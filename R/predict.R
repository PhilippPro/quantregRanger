##' @title quantregRanger prediction
##' @param object \code{quantregRanger} object.
##' @param data New test data of class \code{data.frame}
##' @param quantiles Numeric vector of quantiles that should be estimated
##' @return A matrix. The first column contains the conditional quantile 
##' estimates for the first entry in the vector quantiles. The second 
##' column contains the estimates for the second entry of quantiles and so on.
##' @export
predict.quantregRanger = function(object, data = NULL, quantiles = c(0.1,0.5,0.9)) {
  origObs = object$origObs
  origNodes = object$origNodes
  
  quant = matrix(nrow = nrow(data), ncol = length(quantiles))
  nodes = getnodes(object, data)
  ntree = object$num.trees
  
  nobs = length(origObs)
  nnew = nrow(data)
  normalise = 0
  weightvec = rep(0, nobs*nnew)
  counti = rep(0, nobs)
  thres = 5*.Machine$double.eps
  result = Findweights(as.double(as.vector(origNodes)),
                        as.double(as.vector(nodes)),
                        as.double(weightvec),
                        as.integer(nobs),
                        as.integer(nnew),
                        as.integer(ntree),
                        as.double(thres),
                        as.integer(counti),
                        as.integer(normalise))
  
  weights = matrix(result, nrow=nobs)
  
  ord = order(origObs)
  origObs = origObs[ord]
  weights = weights[ord, , drop = FALSE]
  cumweights = apply(weights,2,cumsum)
  cumweights = sweep(cumweights, 2, as.numeric(cumweights[nobs, ]), FUN="/")
  
  #for (qc in 1:length(quantiles)){
  #  larg = cumweights<quantiles[qc]
  #  wc = apply(larg,2,sum)+1
  #  quant[,qc] = origObs[wc]
  #}
  for (qc in 1:length(quantiles)){
    larg = cumweights<quantiles[qc]
    wc = apply(larg, 2, sum) + 1
    ind1 = which(wc < 1.1)
    indn1 = which(wc > 1.1)
    quant[ind1, qc] = rep(origObs[1], length(ind1))
    quantmax = origObs[wc[indn1]]
    quantmin = origObs[wc[indn1]-1]
    weightmax = cumweights[cbind(wc[indn1], indn1)]
    weightmin = cumweights[cbind(wc[indn1]-1, indn1)]
    factor = numeric(length(indn1))
    indz = weightmax-weightmin<10^(-10)
    factor[indz] = 0.5
    factor[!indz] = (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
    quant[indn1,qc] = quantmin+factor*(quantmax-quantmin)
  }
  colnames(quant) = paste("quantile=", quantiles)
  return(quant)
}