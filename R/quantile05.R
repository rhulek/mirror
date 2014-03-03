quantile05<-function(x) {
  quant<-c()
  quant[1]<-quantile(x,0.05,na.rm=TRUE)
  return(quant)
}