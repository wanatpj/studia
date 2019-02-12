all: main.x main2.x

main.x: main.o D.o
	g++ -o main.x main.o D.o -g

main2.x: main2.o D.o
	g++ -o main2.x main2.o D.o -g

main.o: main.cpp D.hpp
	g++ -c -O1 main.cpp

main2.o: main2.cpp D.hpp
	g++ -c -O1 main2.cpp

D.o: D.asm
	nasm -felf64 D.asm

clean:
	rm main.x main.o D.o main2.x

.PHONY: all clean
