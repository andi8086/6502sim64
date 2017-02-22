all: main.s
	nasm -felf64 main.s -l main.lst
	ld main.o -lc -I /lib/ld-linux-x86-64.so.2
