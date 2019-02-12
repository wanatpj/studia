test.x : test.o ctor.o
	gcc $^ -o $@

test.o : test.c
	gcc -c $^ -O2 -o $@

ctor.o : ctor.asm
	nasm $^ -felf64 -o $@

clean:
	rm ctor.o test.o test.x

.PNONY: clean
