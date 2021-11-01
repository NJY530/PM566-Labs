Lab09
================
Jiayi Nie

## Problem 2

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  return(x)
  # x
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n*k,lambda),nrow=n,ncol=k,byrow =TRUE)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(n=1000),
  fun1alt(n=1000), unit="relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq      max neval
    ##     fun1(n = 1000) 34.62936 37.46381 43.18156 39.83534 49.82601 12.79504   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000   100

``` r
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  # position of the max value per row of x
  idx<-max.col(t(x))
  
  # do something to get the actual max value

  x[cbind(idx,1:ncol(x))]
}

# Do we get the same?
all(fun2(x)==fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x),unit="relative"
)
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval
    ##     fun2(x) 9.878051 7.623958 6.499382 7.710009 6.927639 1.245873   100
    ##  fun2alt(x) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100

## Problem 3
