quantile50<-function(x) {
  quant<-c()
  quant[1]<-quantile(x,0.50,na.rm=TRUE)
  return(quant)
}