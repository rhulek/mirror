quantile95<-function(x) {
  quant<-c()
  quant[1]<-quantile(x,0.95,na.rm=TRUE)
  return(quant)
}