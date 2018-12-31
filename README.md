# Howto build

```
$ cargo build
```


# Howto unit test

```
$ cargo test
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
