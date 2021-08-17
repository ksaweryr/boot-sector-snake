.PHONY: all run clean

all:
	nasm -fbin -o snake.bin snake.asm

run:
	qemu-system-x86_64 -fda snake.bin

clean:
	rm -f *.bin
