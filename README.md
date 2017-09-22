# quantregRanger
Quantile Regression for Ranger


##' y = rnorm(150)
##' x = cbind(y + rnorm(150), rnorm(150))
##' data = data.frame(x,y)
##' mod = quantregRanger(y ~ ., data = data)
##' predict(mod, data = data[1:5, ])

## Installation
The development version
    
    devtools::install_github("PhilippPro/quantregRanger")
    
    
## Usage
Quickstart:

    library(quantregRanger)
    y = rnorm(150)
    x = cbind(y + rnorm(150), rnorm(150))
    data = data.frame(x,y)
    mod = quantregRanger(y ~ ., data = data, quantiles = c(0.1, 0.5, 0,9), params.ranger = list(mtry = 2))
    predict(mod, data = data[1:5, ])