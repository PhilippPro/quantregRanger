##' Creates a quantile regression forest like described in Meinshausen, 2006.
##' @title Quantile Regression with Ranger
##' 
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit.
##' @param data Training data of class \code{data.frame}, \code{matrix} or \code{gwaa.data} (GenABEL).
##' @param num.trees Number of trees.
##' @param mtry Number of variables to possibly split at in each node. Default is the (rounded down) square root of the number variables. 
##' @param min.node.size Minimal node size. Default is 10.
##' 
##' @author Philipp Probst
##' @references
##' Meinshausen, Nicolai. "Quantile regression forests." The Journal of Machine Learning Research 7 (2006): 983-999.
##' @seealso \code{\link{predict.quantregRanger}}
##' @useDynLib quantregRanger
##' @importFrom Rcpp evalCpp sourceCpp

##' @examples
##' y = rnorm(150)
##' x = cbind(y + rnorm(150), rnorm(150))
##' data = data.frame(x,y)
##' mod = quantregRanger(y ~ ., data = data)
##' predict(mod, data = data[1:5, ])
##' 
##' @export
quantregRanger = function(formula = NULL, data = NULL,  num.trees = 500, mtry = NULL, min.node.size = NULL, importance = FALSE, quantiles=c(0.1,0.5,0.9)) {
  cl = match.call()
  cl[[1]] = as.name("quantregRanger")
  qrf = ranger::ranger(formula = formula , data = data,  num.trees = num.trees, mtry = mtry, write.forest = TRUE, min.node.size = min.node.size, keep.inbag = TRUE)
  class(qrf) = c("quantregRanger","ranger")
  qrf[["call"]] = cl
  qrf[["origNodes"]] = getnodes(qrf, data)
  qrf[["origObs"]] = model.frame(formula, data)[[1]]
  if(importance==TRUE){
    qrf[["importance"]] = predict.imp(qrf, quantiles = quantiles, formula = formula, data = data)
    qrf[["quantiles"]] = quantiles
  }
  qrf
}
