# Required environment variable

- RISCV
  - Riscv tool install path as the following:

```
$ export RISCV=/opt/riscv/
```

# Howto build

```
$ cargo build
```


# Howto unit test

```
$ cargo test
```

# Howto regression test

## Build and install riscv-tests

```
$ git clone --recursive https://github.com/riscv/riscv-tests
$ mkdir -p riscv-tests/build
$ cd riscv-tests/build
$ ../configure --prefix=${RISCV}/target --with-xlen=32
$ make
$ make install
$ riscv32-unknown-elf-objcopy -O binary ${RISCV}/target/share/riscv-tests/isa/rv32ui-p-addi ${RISCV}/target/share/riscv-tests/isa/rv32ui-p-addi.bin
```

## Run regression tests
```
$ cargo run ${RISCV}/target/share/riscv-tests/isa/rv32ui-p-addi.bin -l debug -o 0x80000000
```

# Howto integration test

## Install dependencies

- Cross compiler and standard libraries
```
$ git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
$ cd riscv-gnu-toolchain
$ ./configure --prefix=${RISCV} --with-arch=rv32gc --with-abi=ilp32d
$ make -j`getconf _NPROCESSORS_ONLN`
```

## Run integration test
```
$ make -C test
$ cargo run test/hello.bin
```


# Howto compare with spike trace log

```
$ cargo run ${RISCV}/target/share/riscv-tests/isa/rv32ui-p-addi.bin -o 0x80000000 -s -l error > rrvm.log
$ spike --isa=rv32gc -l  ${RISCV}/target/share/riscv-tests/isa/rv32ui-p-addi 2>&1 | cut -d")" -f 1 > spike.log
$ diff -u spike.log rrvm.log
```
