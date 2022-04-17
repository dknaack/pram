# pram - A simulator for parallel random access machines

pram is a simulator for a [parallel random access machine]. It reads an
assembler-like code and provides a simple shell to step through the code and
see the register values.

Note that this is only a simulator. The processors execute their code
sequentially for now. This means that if two processors write to the same
address that the processor with the higher number wins. This also means that a
processor with a higher number can read from a value from a register even if
another processor wrote that value in the same cycle.

## Building

Simply run the build script:

	./build.sh

This will build a binary called `pram` in the current working directory.

## Usage

The program requires at least a filename for the assembly code. You can
optionally pass an input file with the `-f` option. This is used for the
initial values of the input registers. This file should contain the number of
input registers followed by the input values. You can see the rest of the
options by running the program without any arguments.

After the program starts, you can see the available commands by typing `help`.
In the command line, you don't need to type the same command twice. You can
simply leave it blank and hit enter. This will run the same command as before
(similar to something like gdb).

	./pram example.txt

	example.txt> step
	...
	example.txt>
	example.txt> step

## Assembler Code

The following table gives an overview for the instructions of the assembler for
this simulator. The value `i` is the index of a machine, `n` is the total
number of machines, `I` is the array of input registers, `R` is the array of
working registers and `P` is the program counter.

	Instruction  Equivalent C code
	-----------  -----------------
	set x        R[x] = 0
	get x        R[i] = I[R[x]]
	mov x        R[i] = R[x]
	str x        R[x] = R[i]
	mov* x       R[i] = R[R[x]]
	str* x       R[R[x]] = R[i]
	add x        R[i] = R[i] + R[x]
	sub x        R[i] = R[i] - R[x]
	div x        R[i] = R[i] / x
	jmp x        P = x
	jiz x        if (R[i] == 0) P = x
	jip x        if (R[i] > 0) P = x

[parallel random access machine]: https://en.wikipedia.org/wiki/Parallel_RAM
