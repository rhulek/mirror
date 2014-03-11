ts<-function(records,centralValueType="median",whiskerValueType="5_95",transformationType="none") {  
  
  library(genasis)
  
  ## Celkova obalka
  allSeries<-list()
  
  ## Prvni opakovani cyklu - primarni casove rady
  seriesSets<-list()
  labels<-list()
  # i cyklus bezi pres sites
  for (i in 1:length(records)) {
    loca<-as.character(records[[i]]$rowLabel)
    value         <-c()
    loqValue      <-c()
    loqMethodCode <-c()
    unit          <-c()
    dateTime      <-c()
    dateTimeString<-c()
    timeLength    <-c()
    
    for (j in 1:length(records[[i]]$values)) {
      value         <-c(value,         records[[i]]$values[[j]]$value)
      loqValue      <-c(loqValue,      records[[i]]$values[[j]]$loqValue)
      loqMethodCode <-c(loqMethodCode, records[[i]]$values[[j]]$loqMethodCode)
      unit          <-c(unit,          records[[i]]$values[[j]]$unit)
      dateTime      <-c(dateTime,      records[[i]]$values[[j]]$dateTime)
      dateTimeString<-c(dateTimeString,records[[i]]$values[[j]]$dateTimeString)
      timeLength    <-c(timeLength,    records[[i]]$values[[j]]$timeLength)
    }
    
    dateTime<-as.Date(dateTimeString)
    
    value         <-value[order(dateTimeString)]
    loqValue      <-loqValue[order(dateTimeString)]
    loqMethodCode <-loqMethodCode[order(dateTimeString)]
    unit          <-unit[order(dateTimeString)]
    dateTime      <-dateTime[order(dateTimeString)]
    timeLength    <-timeLength[order(dateTimeString)]
    dateTimeString<-dateTimeString[order(dateTimeString)]
    
    # Nahrada LoQ (v promenne valu budou hodnoty vstupujici do vypoctu)
    valu<-value
    valu[which(is.na(valu)&loqMethodCode=="INS")]<-loqValue[which(is.na(valu))]*1/2
    
    # Logaritmicka transformace
    if (transformationType=="log") {
      valu<-log(valu)
    }
   
    if (length(dateTime)>1) {
      hole<-3*mean(dateTime[-1]-dateTime[-length(records[[i]]$values)],trim=0.05)
    } else {
      hole<-0
    }
    
    # k udava poradi segmentu jedne casove rady
    k<-1
    values<-list()
    series<-list()
    
    # j cyklus bezi pres jednotliva mereni
    for (j in 1:length(records[[i]]$values)) {
      cv<-list(value=valu[j],
               loqValue=loqValue[j],
               label=loca,
               loqMethodCode=loqMethodCode[j],
               unit=unit[j],
               dateTime=dateTime[j],
               dateTimeString=dateTimeString[j],
               timeLength=length(records[[i]]$values));
      
      if (j==1) {
        values<-as.list(c(values,list(cv)))
      } else {
        if ((dateTime[j]-dateTime[j-1])<hole) {
          values<-as.list(c(values,list(cv)))
        } else {
          timeSeriesRecord<-list(values=values,label=paste0("site",i," part",k))
          k<-k+1
          series<-as.list(c(series,list(timeSeriesRecord)))
          values<-list(cv)
        }
      }
    }
    
    timeSeriesRecord<-list(values=values,label=paste0("site",i," part",k))
    series<-as.list(c(series,list(timeSeriesRecord)))
    
    # Popis primarnich casovych rad v 1. cyklu    
    if (k<2) {
      res<-genstatistic(valu,dateTime)$res
      
      trendSummary<-list(delta=res$delta,
                         mannKendall=res$"Mann-Kendall",
                         mannKendallP=res$MKp,
                         daniels=res$Daniels,
                         danielsP=res$Dp,
                         mean=res$mean,
                         sd=res$sd,
                         geomean=res$"geom. mean",
                         gsd=res$"geom. sd",
                         median=res$median,
                         min=res$min,
                         max=res$max,
                         perc5=quantile05(valu),
                         perc25=quantile25(valu),
                         perc75=quantile75(valu),
                         perc95=quantile95(valu),
                         geoMean95CIUpperBound=res$"geom. mean"*res$"geom. sd"^qnorm(0.975),
                         geoMean95CILowerBound=res$"geom. mean"*res$"geom. sd"^qnorm(0.025),
                         mean95CIUpperBound=res$mean+qnorm(0.975)*res$sd,
                         mean95CILowerBound=res$mean+qnorm(0.025)*res$sd)
    }
    else {
      trendSummary<-NA
    }
    
    seriesSet<-list(series=series,trendSummary=trendSummary,label=paste0("site",i));
    seriesSets<-as.list(c(seriesSets,list(seriesSet)))
    label<-paste0("Site",i)
    labels<-as.list(c(labels,list(label)))
  }
  
  
  
  
  ## Zabaleni vysledku
  allSeries<-list(seriesSets=seriesSets,
                  labels=labels,
                  allAggregation=1,
                  aggregationLabels=1)
  
  timeSeriesDataSeries<-list(allSeries=allSeries)
  
  return(timeSeriesDataSeries)
}