quantile25<-function(x) {
  quant<-c()
  quant[1]<-quantile(x,0.25,na.rm=TRUE)
  return(quant)
}