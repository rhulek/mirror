quantile25<-function(x) {
  quant<-quantile(x,0.25,na.rm=TRUE)
  return(quant)
}