ss<-function(records,centralValueType="median",whiskerValueType="5_95",transformationType="none") {
  

  
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
  for (i in 1:nrow(records)) {
    loca<-as.character(records[i,1])
    data<-as.data.frame(records[i,2])
    
    # Nahrada LoQ polovinami limitu
    valu<-data$value
    valu[which(is.na(valu)&data$loqMethodCode=="INS")]<-data$loqValue[which(is.na(valu))]*1/2
    
    # Logaritmicka transformace
    if (transformationType=="log") {
      valu<-log(valu)
    }
    
    # Jednotky musi byt stejne
    if (length(unique(data$unit))>1) {
      stop()
    } else {
      unit<-unique(data$unit)
    }
    
    result<-list(label=loca,
                  n=nrow(data),
                  nUnderLOQ=length(which(is.na(data$value))),
                  unit=unit,
                  centralValue=sapply(list(valu),FUN=f1),
                  centralValueType=centralValueType,
                  whiskerTopValue=sapply(list(valu),FUN=f3),
                  whiskerBottomValue=sapply(list(valu),FUN=f2),
                  whiskerType=whiskerValueType)
    results<-as.list(c(results,list(result)))
  }
  
    return(results)  
}