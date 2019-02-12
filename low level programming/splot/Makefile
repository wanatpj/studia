main : main.o filter.o
	gcc -o $@ $^ -g

main.o : main.c
	gcc -c -O2 -o $@ $^ -g

filter.o : filter.asm
	nasm -felf64 -o $@ $^ -g

main-c : main.o filter-c.o
	gcc -o $@ $^

filter-c.o : filter.c
	gcc -c -O2 -o $@ $^

dump: filter-c.o
	objdump -d filter-c.o -M intel

clean:
	rm filter-c.o main-c filter.o main.o main tmp tmp.o

.PHONY: clean
