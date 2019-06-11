ORAND
================

### Introduction

The package implements the Bayesian Or's of And's algorithm for Interpretable classification, created by Tong Wang, Cynthia Rudin, Finale Doshi-Velez, Yimin Liu, Erica Klampfl and Perry MacNeille ([the paper](https://pdfs.semanticscholar.org/ee43/4531203bf7609c9648f78eacf4b7ea750dd0.pdf)).

The algorithm takes categorical data (might be more than two levels per variable) and a vector of binary predictions and outputs the set of patterns (rules) for positive prediction.

#### Acknowledgement

The package uses Christian Borgelt's implementation of FP-growth algorithm from **fim4r** package ([link](http://www.borgelt.net/fim4r.html)).

### To download, install and test the package run the following:

``` r
library(devtools)
install_github("elenakhusainova/ORAND")
library(ORAND)

params <- list(max_length = 3, alpha = rep(10, 3), beta = rep(10, 3),
         alpha_plus = 500, alpha_minus = 500, beta_plus = 1, beta_minus = 1)
df1 <- TicTacToe[, -28]
dfClasses <- TicTacToe$outcome
b <- boa(data = df1, classes = dfClasses, prior = "BetaBinomial",
         method = "pattern", metric = "other", params, iter_max = 50, 
         cool_rate = 1000, p = 0.1)
b
```
