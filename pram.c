#include <assert.h>
#include <ctype.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t usize;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

typedef int64_t i64;
typedef int32_t i32;
typedef int16_t i16;
typedef int8_t  i8;

typedef double f64;
typedef float  f32;

#define LENGTH(x) (sizeof(x)/sizeof(*(x)))

struct buffer {
	char *data;
	u32 size;
	u32 start;
};

enum token_type {
	PRAM_EOF,
	PRAM_PLUS,
	PRAM_STAR,
	PRAM_LPAREN,
	PRAM_RPAREN,
	PRAM_NUMBER,
	PRAM_IDENTIFIER,
	PRAM_GET,
	PRAM_SET,
	PRAM_MOV,
	PRAM_STR,
	PRAM_MOV_STAR,
	PRAM_STR_STAR,
	PRAM_ADD,
	PRAM_SUB,
	PRAM_DIV,
	PRAM_JMP,
	PRAM_JIZ,
	PRAM_JIP,

	PRAM_FIRST_INSTRUCTION = PRAM_GET,
};

struct token {
	enum token_type type;
	u32 start;

	union {
		struct {
			char *string;
			u32 length;
		};
		u32 number;
	};
};

struct parser {
	struct token token;
	struct buffer buffer;

	bool is_initialized;
	u32 machine_count;
};

struct pram_memory {
	i32 *inputs;
	i32 *registers;

	u32 input_count;
	u32 register_count;
};

enum pram_expression_type {
	PRAM_EXPR_MACHINE_COUNT,
	PRAM_EXPR_MACHINE_INDEX,
	PRAM_EXPR_NUMBER,
	PRAM_EXPR_ADD,
	PRAM_EXPR_MUL,
};

struct pram_expression {
	enum pram_expression_type type;
	union {
		struct {
			struct pram_expression *lhs;
			struct pram_expression *rhs;
		};

		char *variable;
		i32 number;
	};
};

struct pram_instruction {
	u32 opcode;
	struct pram_expression arg;
};

struct pram_program {
	struct pram_instruction *instructions;
	u32 *counters;

	u32 instruction_count;
	u32 instruction_size;
	u32 machine_count;
};

static const char *token_name[] = {
	[PRAM_EOF] = "EOF",
	[PRAM_PLUS] = "+",
	[PRAM_STAR] = "*",
	[PRAM_NUMBER] = "number",
	[PRAM_IDENTIFIER] = "identifier",
	[PRAM_GET] = "get",
	[PRAM_SET] = "set",
	[PRAM_MOV] = "mov",
	[PRAM_STR] = "str",
	[PRAM_MOV_STAR] = "mov*",
	[PRAM_STR_STAR] = "str*",
	[PRAM_ADD] = "add",
	[PRAM_SUB] = "sub",
	[PRAM_DIV] = "div",
	[PRAM_JMP] = "jmp",
	[PRAM_JIZ] = "jiz",
	[PRAM_JIP] = "jip",
};

static void
die(const char *msg)
{
	perror(msg);
	exit(1);
}

static void *
ecalloc(usize nmemb, usize size)
{
	void *ptr = calloc(nmemb, size);

	if (!ptr) {
		die("calloc");
	}

	return ptr;
}

static bool
tokenize(struct buffer *buffer, struct token *token)
{
	static struct {
		char *string;
		u32 type;
	} keywords[] = {
		{ "get",  PRAM_GET, },
		{ "set",  PRAM_SET, },
		{ "mov",  PRAM_MOV, },
		{ "str",  PRAM_STR, },
		{ "mov*", PRAM_MOV_STAR, },
		{ "str*", PRAM_STR_STAR, },
		{ "add",  PRAM_ADD, },
		{ "sub",  PRAM_SUB, },
		{ "jmp",  PRAM_JMP, },
		{ "jiz",  PRAM_JIZ, },
		{ "jip",  PRAM_JIP, },
	};

	char *at = buffer->data + buffer->start;
	while (*at && isspace(*at)) {
		at++;
	}

	buffer->start = at - buffer->data;
	token->start = buffer->start;
	if (*at == '\0') {
		token->type = PRAM_EOF;
		return false;
	}

	switch (*at) {
	case '+': at++; token->type = PRAM_PLUS;   break;
	case '*': at++; token->type = PRAM_STAR;   break;
	case '(': at++; token->type = PRAM_LPAREN; break;
	case ')': at++; token->type = PRAM_RPAREN; break;
	default:
			  if (isalpha(*at)) {
				  token->type = PRAM_IDENTIFIER;
				  char *start = at;

				  do {
					  at++;
				  } while (*at && isalpha(*at));

				  if (*at == '*') {
					  at++;
				  }

				  token->length = at - start;
				  token->string = malloc(token->length + 1);
				  memcpy(token->string, start, token->length);
				  token->string[token->length] = '\0';

				  for (u32 i = 0; i < LENGTH(keywords); i++) {
					  if (strcmp(keywords[i].string, token->string) == 0) {
						  free(token->string);
						  token->type = keywords[i].type;
						  break;
					  }
				  }
			  } else if (isdigit(*at)) {
				  token->type = PRAM_NUMBER;
				  token->number = 0;

				  do {
					  token->number *= 10;
					  token->number += *at++ - '0';
				  } while (*at && isdigit(*at));
			  } else {
				  fprintf(stderr, "Invalid token\n");
			  }

			  break;
	}

	buffer->start = at - buffer->data;
	return true;
}

bool
accept(struct parser *parser, enum token_type type)
{
	if (!parser->is_initialized) {
		tokenize(&parser->buffer, &parser->token);
		parser->is_initialized = true;
	}

	if (parser->token.type == type) {
		tokenize(&parser->buffer, &parser->token);
		return true;
	} else {
		return false;
	}
}

void
expect(struct parser *parser, enum token_type type)
{
	if (!accept(parser, type)) {
		fprintf(stderr, "Unexpected token: %s\n", token_name[parser->token.type]);
	}
}

bool
buffer_read(struct buffer *buffer, const char *path)
{
	FILE *f = stdin;

	if (path && !(f = fopen(path, "r"))) {
		return false;
	}

	u32 size = 2 * BUFSIZ;
	u32 len = 0;
	usize s;
	char *data = ecalloc(size, 1);

	while ((s = fread(data + len, 1, BUFSIZ, f))) {
		len += s;
		if(BUFSIZ + len + 1 > size) {
			size *= 2;
			if (!(data = realloc(data, size))) {
				die("realloc");
			}
		}
	}

	data[len] = '\0';
	buffer->data = data;
	buffer->size = len;
	buffer->start = 0;

	if (path) {
		fclose(f);
	}

	return true;
}

static void
program_write_instruction(struct pram_program *program, u32 opcode,
	struct pram_expression *expr)
{
	usize size = program->instruction_size;
	usize count = program->instruction_count;

	if (count >= size) {
		if (size == 0) {
			size = 1024 * sizeof(*program->instructions);
			program->instruction_size = size;
			if (!(program->instructions = malloc(size))) {
				die("malloc");
			}
		} else {
			size *= 2;

			program->instruction_size = size;
			if (!(program->instructions = realloc(program->instructions, size))) {
				die("realloc");
			}
		}
	}

	struct pram_instruction *instruction = program->instructions + count;
	instruction->opcode = opcode;
	memcpy(&instruction->arg, expr, sizeof(*expr));

	program->instruction_count++;
}

static bool parse_expression(struct parser *parser, struct pram_expression *expr);

static bool
parse_unary_expression(struct parser *parser, struct pram_expression *expr)
{
	i32 number = parser->token.number;
	char *string = parser->token.string;

	if (accept(parser, PRAM_IDENTIFIER)) {
		if (strcmp(string, "n") == 0) {
			expr->type = PRAM_EXPR_MACHINE_COUNT;
		} else if (strcmp(string, "i") == 0) {
			expr->type = PRAM_EXPR_MACHINE_INDEX;
		} else {
			fprintf(stderr, "Unknown variable '%s'\n", string);
		}

		return true;
	} else if (accept(parser, PRAM_NUMBER)) {
		expr->type = PRAM_EXPR_NUMBER;
		expr->number = number;
		return true;
	} else if (accept(parser, PRAM_LPAREN)) {
		if (!parse_expression(parser, expr)) {
			fprintf(stderr, "Expected expression\n");
		}

		expect(parser, PRAM_RPAREN);
		return true;
	} else {
		return false;
	}
}

static bool
parse_term(struct parser *parser, struct pram_expression *expr)
{
	if (!parse_unary_expression(parser, expr)) {
		return false;
	}

	if (accept(parser, PRAM_STAR)) {
		struct pram_expression *lhs;
		struct pram_expression *rhs;

		lhs = ecalloc(2, sizeof(*lhs));
		rhs = lhs + 1;

		memcpy(lhs, expr, sizeof(*lhs));

		if (!parse_unary_expression(parser, rhs)) {
			fprintf(stderr, "Expected unary expression after '*'\n");
		}

		expr->type = PRAM_EXPR_MUL;
		expr->lhs = lhs;
		expr->rhs = rhs;
	}

	return true;
}

static bool
parse_expression(struct parser *parser, struct pram_expression *expr)
{
	if (!parse_term(parser, expr)) {
		return false;
	}

	if (accept(parser, PRAM_PLUS)) {
		struct pram_expression *lhs = ecalloc(2, sizeof(*lhs));
		struct pram_expression *rhs = lhs + 1;

		memcpy(lhs, expr, sizeof(*lhs));

		if (!parse_term(parser, rhs)) {
			fprintf(stderr, "Expected term after '+'\n");
		}

		expr->type = PRAM_EXPR_ADD;
		expr->lhs = lhs;
		expr->rhs = rhs;
	}

	return true;
}

static bool
parse_program(struct parser *parser, struct pram_program *program)
{
	if (accept(parser, PRAM_EOF)) {
		return false;
	}

	while (parser->token.type >= PRAM_FIRST_INSTRUCTION) {
		u32 opcode = parser->token.type;
		accept(parser, opcode);

		struct pram_expression expr;
		if (!parse_expression(parser, &expr)) {
			fprintf(stderr, "Expected expression\n");
			return true;
		}

		program_write_instruction(program, opcode, &expr);
	}

	return true;
}

static i32
expression_eval(struct pram_expression *expression, u32 machine_index,
	u32 machine_count)
{
	i32 lhs, rhs;

	switch (expression->type) {
	case PRAM_EXPR_ADD:
		lhs = expression_eval(expression->lhs, machine_index, machine_count);
		rhs = expression_eval(expression->rhs, machine_index, machine_count);
		return lhs + rhs;
	case PRAM_EXPR_MUL:
		lhs = expression_eval(expression->lhs, machine_index, machine_count);
		rhs = expression_eval(expression->rhs, machine_index, machine_count);
		return lhs * rhs;
	case PRAM_EXPR_MACHINE_COUNT:
		return machine_count;
	case PRAM_EXPR_MACHINE_INDEX:
		return machine_index;
	case PRAM_EXPR_NUMBER:
		return expression->number;
	default:
		assert(!"Invalid expression type");
		return 0;
	}
}

static void
instruction_execute(struct pram_instruction *instruction,
	struct pram_memory *memory, u32 machine_index, u32 machine_count,
	u32 *counter)
{
	i32 *inputs = memory->inputs;
	i32 *registers = memory->registers;
	i32 *accumulator = &registers[machine_index];

	i32 x = expression_eval(&instruction->arg, machine_index, machine_count);

	// TODO: bounds checking on x.
	switch (instruction->opcode) {
	case PRAM_GET:
		*accumulator = inputs[registers[x]];
		break;
	case PRAM_SET:
		*accumulator = x;
		break;
	case PRAM_MOV:
		*accumulator = registers[x];
		break;
	case PRAM_STR:
		registers[x] = *accumulator;
		break;
	case PRAM_MOV_STAR:
		*accumulator = registers[registers[x]];
		break;
	case PRAM_STR_STAR:
		registers[registers[x]] = *accumulator;
		break;
	case PRAM_ADD:
		*accumulator += registers[x];
		break;
	case PRAM_SUB:
		*accumulator -= registers[x];
		break;
	case PRAM_DIV:
		*accumulator /= x;
		break;
	case PRAM_JMP:
		*counter = x;
		break;
	case PRAM_JIZ:
		if (*accumulator == 0) {
			*counter = x;
		}
		break;
	case PRAM_JIP:
		if (*accumulator > 0) {
			*counter = x;
		}
		break;
	}
}

static bool
program_step(struct pram_program *program, struct pram_memory *memory)
{
	u32 instruction_count = program->instruction_count;
	u32 machine_count = program->machine_count;
	u32 *counters = program->counters;
	bool is_running = false;

	for (u32 machine_index = 0; machine_index < machine_count; machine_index++) {
		if (counters[machine_index] > instruction_count) {
			continue;
		}

		is_running = true;
		u32 *machine_counter = &counters[machine_index];
		struct pram_instruction *instruction = program->instructions +
			*machine_counter;
		*machine_counter += 1;

		instruction_execute(instruction, memory, machine_index, machine_count,
			machine_counter);
	}

	return is_running;
}

static void
program_execute(struct pram_program *program, struct pram_memory *memory)
{
	while (program_step(program, memory)) {

	}
}

static void
print_program(struct pram_program *program)
{
	u32 instruction_count = program->instruction_count;
	struct pram_instruction *instruction = program->instructions;

	while (instruction_count-- > 0) {
		printf("%s\n", token_name[instruction->opcode]);
		instruction++;
	}
}

int
main(int argc, char *argv[])
{
	struct pram_program program = {0};
	struct pram_memory memory = {0};
	struct parser parser = {0};

	if (argc < 2) {
		printf("USAGE: pram [options] file\n\n"
			"OPTIONS\n"
			"    -f FILE load initial inputs from FILE\n"
			"    -i COUNT specify the number of input registers\n"
			"    -r COUNT specify the number of working registers\n");
		return 0;
	}

	argv++;
	argc--;

	/* default values */
	program.machine_count = 4;
	memory.input_count = 32;
	memory.register_count = 32;

	char *code_path = NULL;
	char *input_path = NULL;
	while (argc-- > 0) {
		char *at = *argv++;

		if (at[0] == '-' && at[1] != '\0') {
			switch (at[1]) {
			case 'f':
				input_path = *argv++;
				argc--;
				break;
			case 'i':
				memory.input_count = strtoul(*argv++, NULL, 10);
				argc--;
				break;
			case 'r':
				memory.register_count = strtoul(*argv++, NULL, 10);
				argc--;
				break;
			case 'm':
				program.machine_count = strtoul(*argv++, NULL, 10);
				argc--;
				break;
			}
		} else {
			code_path = at;
			break;
		}
	}

	if (program.machine_count == 0) {
		fprintf(stderr, "Invalid number of machines\n");
		return 1;
	}

	if (memory.input_count == 0) {
		fprintf(stderr, "Invalid number of inputs\n");
		return 1;
	}

	if (memory.register_count == 0 || memory.register_count < program.machine_count) {
		fprintf(stderr, "Invalid number of registers\n");
		return 1;
	}

	/* read in the inputs or allocate memory for the input registers */
	if (input_path) {
		FILE *input = fopen(input_path, "r");
		if (!input) {
			die("Failed to open input file");
		}

		fscanf(input, "%u", &memory.input_count);
		memory.inputs = ecalloc(memory.input_count, sizeof(*memory.inputs));
		for (int i = 0; i < memory.input_count; i++) {
			fscanf(input, "%d", &memory.inputs[i]);
		}

		fclose(input);
	} else {
		memory.inputs = ecalloc(memory.input_count, sizeof(*memory.inputs));
	}

	memory.registers = ecalloc(memory.register_count, sizeof(*memory.registers));
	program.counters = ecalloc(program.machine_count, sizeof(*program.counters));

	if (!buffer_read(&parser.buffer, code_path)) {
		fprintf(stderr, "Failed to read file\n");
		goto error_read;
	}

	if (parse_program(&parser, &program)) {
		printf("Executing program...\n");
		print_program(&program);
		program_execute(&program, &memory);

		for (u32 i = 0; i < memory.register_count; i++) {
			printf("R[%d] = %d\n", i, memory.registers[i]);
		}
	} else {
		fprintf(stderr, "Failed to parse the program\n");
		goto error_parse;
	}

	free(parser.buffer.data);
	free(program.counters);
	free(memory.registers);
	free(memory.inputs);

	return 0;
error_parse:
	free(parser.buffer.data);
error_read:
	free(program.counters);
	free(memory.registers);
	free(memory.inputs);
	return 1;
}
