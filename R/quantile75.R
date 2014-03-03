quantile75<-function(x) {
  quant<-c()
  quant[1]<-quantile(x,0.75,na.rm=TRUE)
  return(quant)
}