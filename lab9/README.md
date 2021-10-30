Lab 9
================

``` r
library(microbenchmark)
```

## Problem 2:

### This function generates a n x k dataset with all its entries distributed poission with mean lambda.

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  x
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n * k, lambda), nrow = n, ncol= k)
}

microbenchmark::microbenchmark(
  fun1(),
  fun1alt(), units="relative"
)
```

    ## Unit: nanoseconds
    ##       expr    min       lq       mean   median        uq      max neval
    ##     fun1() 544595 791431.0 1325950.50 834226.0 1015334.0 40290155   100
    ##  fun1alt()  34734  38503.5   83228.04  41270.5   48578.5  3634163   100
    ##      units      4     18.5      48.75     26.5      52.5      349   100

### Find the column max (hint: Checkout the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)
# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2alt <- function(x) {
  # Position of the max value per row of x.
  idx <- max.col(t(x)) 
  
  # Do something to get the actual max value
  # x[cbind(1, 15)] ~ x[1, 15]
  # Want to access x[1, 16], x[4, 1]
  # x[rbind(c(1, 16), c(4, 1))]
  # Want to access x[4, 16], x[4, 1]
  # x[cbind(4, c(16, 1))]
  x[ cbind(idx, 1:ncol(x)) ]
}
# Do we get the same?
all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
x <- matrix(rnorm(5e4), nrow=10)
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr      min       lq     mean  median      uq      max neval
    ##     fun2(x) 11.51394 10.78059 10.12181 10.1139 11.5651 3.004268   100
    ##  fun2alt(x)  1.00000  1.00000  1.00000  1.0000  1.0000 1.000000   100

## Problem 3:

``` r
library(parallel)
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  cl <- makePSOCKcluster(ncpus)
  
  # STEP 2: GOES HERE
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
  clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
  
  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl = cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  stopCluster(cl)
  
  ans
  
}
```

### Use the previous pseudocode, and make it work with parallel. Here is just an example for you to try:

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))
# DATA SIM
set.seed(1)
n <- 500; R <- 5e3
x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)
# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)
# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1395732 0.05291612
    ## x            4.8686527 5.04503468

### Check whether your version actually goes faster than the non-parallel version:

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##   0.149   0.029   6.350

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##   0.184   0.032   3.874
