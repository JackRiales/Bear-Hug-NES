#!/bin/sh

mainfile=main

# Uncomment if using ca65 6502 assembler
#ca65 $mainfile.s
#ld65 -t nes -o out.nes $mailfile.o

# Uncomment if using nesasm
./toolchain/nesasm $mainfile.s