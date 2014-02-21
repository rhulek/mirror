quantile95<-function(x) {
  quant<-quantile(x,0.95,na.rm=TRUE)
  return(quant)
}