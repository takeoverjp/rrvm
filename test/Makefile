CROSS_TARGET = riscv32-unknown-elf-
CC = $(CROSS_TARGET)gcc
LD = $(CROSS_TARGET)ld
CFLAGS = -march=rv32im
LDFLAGS = -L/opt/riscv32im/riscv32-unknown-elf/lib -Tlink.ld -nostartfiles -static 

LIBS = -lc
OBJS = hello.o start.o
PROGRAM = hello
OBJCOPY = $(CROSS_TARGET)objcopy
OBJDUMP = $(CROSS_TARGET)objdump

all: $(PROGRAM).bin $(PROGRAM).dump

clean:
	rm -f *.o *.elf *.bin *.dump

.PHONY: all clean

$(PROGRAM).elf: $(OBJS)
	$(LD) $(OBJS) $(LIBS) $(LDFLAGS) -o $(PROGRAM).elf

$(PROGRAM).bin: $(PROGRAM).elf
	$(OBJCOPY) -O binary $(PROGRAM).elf $(PROGRAM).bin

$(PROGRAM).dump: $(PROGRAM).elf
	$(OBJDUMP) -D $(PROGRAM).elf > $(PROGRAM).dump

.S.o:
	$(CC) $(CFLAGS) -c -o $@ $^

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $^

