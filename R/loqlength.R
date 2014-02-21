loqlength<-function(x) {
  nloq<-length(which(is.na(x)))
  return(nloq)
}