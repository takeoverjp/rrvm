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
$ export RISCV=/opt/riscv/
$ git clone --recursive https://github.com/riscv/riscv-tests
$ mkdir -p riscv-tests/build
$ cd riscv-tests/build
$ ../configure --prefix=${RISCV}/target --with-xlen=32
$ make
$ make install
```

## Run integration test
```
$ export RISCV=/opt/riscv/
$ cargo run ${RISCV}/target/share/riscv-tests/isa/rv32ui-p-addi
```

# Howto integration test

## Install dependencies

- Cross compiler and standard libraries
```
$ export RISCV=/opt/riscv/
$ git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
$ cd riscv-gnu-toolchain
$ ./configure --prefix=${RISCV} --with-arch=rv32gc --with-abi=ilp32d
$ make -j`getconf _NPROCESSORS_ONLN`
```

## Run integration test
```
$ export RISCV=/opt/riscv/
$ make -C test
$ cargo run test/hello.bin
```
