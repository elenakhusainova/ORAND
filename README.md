ORAND
================

### To download, install and test the package run the following:

``` r
library(devtools)
install_github("elenakhusainova/ORAND")
library(ORAND)

params <- list(max_length = 3, alpha = rep(10, 3), beta = rep(10, 3),
         alpha_plus = 500, alpha_minus = 500, beta_plus = 1, beta_minus = 1)
df <- TicTacToe[, -28]
dfClasses <- TicTacToe$outcome
b <- boa(data = df, classes = dfClasses, prior = "BetaBinomial",
         method = "pattern", params, iter_max = 50, cool_rate = 1000,
         p = 0.1)
b
```
