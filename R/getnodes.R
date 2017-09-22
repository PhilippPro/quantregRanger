##' @export
getnodes =  function(qrf, data) {
  class(qrf) = "ranger"
  nodes = predict(qrf, data = data, predict.all = TRUE)$predictions
  return(nodes)
}
