# Introduction

The generic-storable-benchmark package is used to benchmark the Storable instances derived using generic-storable package. The methods are measured with the help of [criterion](http://www.serpentine.com/criterion/) library. 

# Usage

```bash
$ cabal build generic-storable-benchmark
$ ln -s dist/build/generic-storable-benchmark/generic-storable-benchmark generic-storable-benchmark
$ ./generic-storable-benchmark -L1  
```

