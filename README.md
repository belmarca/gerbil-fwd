![](https://github.com/belmarca/gerbil-fwd/workflows/.github/workflows/main.yml/badge.svg)
[![Actions Status](https://github.com/belmarca/gerbil-fwd/workflows/main/badge.svg)](https://github.com/belmarca/gerbil-fwd/actions)
# fwd - fast web development
This library is an exploration into fast web development with Gerbil's httpd. Its macros are intended to be composable. You should be able to climb up or down the abstraction ladder and use any level you wish.

```scheme
(defhandler hello
  GET: (response 200 "hello, world!\n"))
```

## TODO

- Implement route variables

## Benchmark

See [woo](https://github.com/fukamachi/woo/blob/master/benchmark.md).

```
Running 10s test @ http://localhost:8080/
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   229.50us  234.10us   8.76ms   96.62%
    Req/Sec     9.44k   807.48    15.84k    76.62%
  377455 requests in 10.10s, 40.68MB read
Requests/sec:  37372.97
Transfer/sec:      4.03MB
```

```
Running 10s test @ http://localhost:8080/
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.51ms   15.60ms 416.72ms   99.44%
    Req/Sec     9.58k     1.88k   18.34k    87.75%
  381354 requests in 10.05s, 41.10MB read
Requests/sec:  37960.75
Transfer/sec:      4.09MB
```

## License
The library itself uses the OpenBSD license.

Gerbil and Gambit are dual licensed under LGPLv2.1 and Apache 2.0.
