ss<-function(records=NULL,centralValueType="median",whiskerValueType="5_95",transformationType="none") {
  
  library(alldatabrowser)
  library(genasis)  
  
  whisk<-c("5_95","25_75","min_max","2iq","ci")
  whisl<-c("quantile05","quantile25","min","iql","cil")
  whisu<-c("quantile95","quantile75","max","iqu","ciu")
  
  centv<-c("mean","median","geomean")
  centf<-c("arimean","quantile50","geomean")
  
  f1<-centf[which(centv==centralValueType)]
  f2<-whisl[which(whisk==whiskerValueType)]
  f3<-whisu[which(whisk==whiskerValueType)]
  
  results<-list()
  for (i in 1:length(records)) {
    loca<-as.character(records[[i]]$rowLabel)
    value        <-c()
    loqValue     <-c()
    loqMethodCode<-c()
    unit         <-c()
    dateTime     <-c()
    timeLength   <-c()
    
    for (j in 1:length(records[[i]]$values))
      value        <-c(value,        records[[i]]$values[[j]]$value)
    loqValue     <-c(loqValue,     records[[i]]$values[[j]]$loqValue)
    loqMethodCode<-c(loqMethodCode,records[[i]]$values[[j]]$loqMethodCode)
    unit         <-c(unit,         records[[i]]$values[[j]]$unit)
    dateTime     <-c(dateTime,     records[[i]]$values[[j]]$dateTime)
    timeLength   <-c(timeLength,   records[[i]]$values[[j]]$timeLength)
    
    
    # Nahrada LoQ polovinami limitu
    valu<-value
    valu[which(is.na(valu)&loqMethodCode=="INS")]<-loqValue[which(is.na(valu))]*1/2
    
    # Logaritmicka transformace
    if (transformationType=="log") {
      valu<-log(valu)
    }
    
    # Jednotky musi byt stejne
    if (length(unique(unit))>1) {
      stop()
    } else {
      unitu<-unique(unit)
    }
    
    result<-list(label=loca,
                 n=legth(records[[i]]$values),
                 nUnderLOQ=length(which(is.na(value))),
                 unit=unitu,
                 centralValue=sapply(list(valu),FUN=f1),
                 centralValueType=centralValueType,
                 whiskerTopValue=sapply(list(valu),FUN=f3),
                 whiskerBottomValue=sapply(list(valu),FUN=f2),
                 whiskerType=whiskerValueType)
    results<-as.list(c(results,list(result)))
  }
  
  return(results)  
}