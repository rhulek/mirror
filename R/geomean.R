geomean<-function(x) {
  geom<-exp(mean(log(x),na.rm=TRUE))
  return(geom)
}