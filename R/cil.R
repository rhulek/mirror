cil<-function(x) {
  il<-mean(x,na.rm=TRUE)+qnorm(0.025)*sd(x,na.rm=TRUE)/sqrt(length(x,na.rm=TRUE))
  return(il)
}