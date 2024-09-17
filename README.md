<!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/chuckleong21/sample-criteria/graph/badge.svg)](https://app.codecov.io/gh/chuckleong21/sample-criteria)
  <!-- badges: end -->

# sample.criteria
The goal of `sample.criteria` is to calculate the minimum sample size based on reported clinic prediction models for future work.

## Installaion
The package is not on CRAN, install it through github:

```r
remotes::install_github("jingwen517/sample-criteria")
```

## Usage 
To calculate the minimum sample for multinomial logistic regression model: 

```r
library(sample.criteria)

set.seed(101)
acriteria <- pmsamplesize(Q = 17,
                  k = 5,
                  p = c(2557, 186, 176, 467, 120),
                  r2_nagelkerke = 0.15,
                  shrinkage = 0.9,
                  auc = c(0.85, 0.92, 0.99, 0.95, 0.75, 0.95, 0.87, 0.87, 0.71, 0.82))
```

To learn more about other usage cases, see `vignette("sample-criteria")`.
