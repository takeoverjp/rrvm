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
$ ../configure --prefix=${RISCV}/target
$ make
$ make install
```

## Run regression tests
```
$ cargo run ${RISCV}/target/share/riscv-tests/isa/rv64ui-p-addi.bin
```

# Howto integration test

## Install dependencies

- Cross compiler and standard libraries
```
$ git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
$ cd riscv-gnu-toolchain
$ ./configure --prefix=${RISCV}
$ make -j`getconf _NPROCESSORS_ONLN`
```

## Run integration test
```
$ make -C test
$ cargo run test/hello.bin
```


# Howto compare with spike trace log

```
$ cargo run ${RISCV}/target/share/riscv-tests/isa/rv64ui-p-addi.bin -o 0x80000000 -s -l error > rrvm.log
$ spike --isa=rv64gc -l  ${RISCV}/target/share/riscv-tests/isa/rv64ui-p-addi 2>&1 | cut -d")" -f 1 > spike.log
$ diff -u spike.log rrvm.log
```
