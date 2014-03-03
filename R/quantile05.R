quantile05<-function(x) {
  quant<-unlist(quantile(x,0.05,na.rm=TRUE))[1]
  return(quant)
}