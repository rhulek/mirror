ciu<-function(x) {
  iu<-mean(x,na.rm=TRUE)+qnorm(0.975)*sd(x,na.rm=TRUE)/sqrt(length(x[which(!is.na(x))]))
  return(iu)
}