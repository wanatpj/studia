main : main.c threads.o
	gcc -o main main.c threads.o -g

threads.o : threads.asm
	nasm -felf64 -o threads.o threads.asm -g

clean:
	rm threads.o main
.PHONY: clean
