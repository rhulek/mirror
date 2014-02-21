quantile75<-function(x) {
  quant<-quantile(x,0.75,na.rm=TRUE)
  return(quant)
}