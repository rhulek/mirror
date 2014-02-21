quantile50<-function(x) {
  quant<-quantile(x,0.50,na.rm=TRUE)
  return(quant)
}