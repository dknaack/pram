pram: pram.c
	cc -g -std=c11 -pedantic -Wall -D_POSIX_C_SOURCE=200809L -o pram pram.c
