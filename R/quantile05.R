quantile05<-function(x) {
  quant<-quantile(x,0.05,na.rm=TRUE)
  return(quant)
}