iqu<-function(x) {
  uq<-quantile50(x)+quantile75(x)-quantile25(x)
  return(uq)
}