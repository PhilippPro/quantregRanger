##' @title Quantile Regression with Ranger
##' 
##' Creates a quantile regression forest like described in Meinshausen, 2006.##' 
##' 
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit.
##' @param data Training data of class \code{data.frame}, \code{matrix} or \code{gwaa.data} (GenABEL).
##' @param num.trees Number of trees.
##' @param mtry Number of variables to possibly split at in each node. Default is the (rounded down) square root of the number variables. 
##' @param min.node.size Minimal node size. Default is 10.

##' @examples
##' y = rnorm(150)
##' x = cbind(y + rnorm(150), rnorm(150))
##' data = data.frame(x,y)
##' mod = quantregRanger(y ~ ., data = data)
##' predict(mod, data = data[1:5, ])
##' 
##' # QuantregForest
##' library(quantregForest)
##' mod2 = quantregForest(x=as.matrix(x), y=y)
##' predict(mod2, newdata = as.matrix(x[1:5,]), all=TRUE)
##' 
##' @export
quantregRanger = function(formula = NULL, data = NULL,  num.trees = 500, mtry = NULL, min.node.size = NULL){
  cl = match.call()
  cl[[1]] = as.name("quantregRanger")
  qrf = ranger::ranger(formula = formula, data = data,  num.trees = 500, mtry = NULL, write.forest = TRUE, 
                min.node.size = NULL)  
  class(qrf) = c("quantregRanger","ranger")
  
  qrf[["call"]] = cl
  qrf[["origNodes"]] = getnodes(qrf, data)
  qrf[["origObs"]] = y
  qrf
}