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
    
    hole<-3*mean(dateTime[-1]-dateTime[-length(records[[i]]$values)],trim=0.05)
    
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
  
  ## Druhe opakovani cyklu - vypocet rad agregaci
  # Vyber agregacnich funkci
  whisk<-c("5_95","25_75","min_max","2iq","ci")
  whisl<-c("quantile05","quantile25","min","iql","cil")
  whisu<-c("quantile95","quantile75","max","iqu","ciu")
  
  centv<-c("mean","median","geomean")
  centf<-c("arimean","quantile50","geomean")
  
  f1<-centf[which(centv==centralValueType)]
  f2<-whisl[which(whisk==whiskerValueType)]
  f3<-whisu[which(whisk==whiskerValueType)]
  
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
    
    # Jednotky musi byt stejne
    if (length(unique(unit))>1) {
      stop("Units of some records differ!")
    } else {
      unit<-unique(unit)
    }
    
    # Rocni agregace do noveho data.frame aggr
    year<-gendate(substr(dateTimeString,1,4))
    aggr<-data.frame(aggregate(valu,by=list(year),FUN=f1)[,2],
                     unit,
                     centralValueType,
                     aggregate(valu,by=list(year),FUN=f3)[,2],
                     aggregate(valu,by=list(year),FUN=f2)[,2],
                     whiskerValueType,
                     gendate(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=length)[,2]),
                     as.character(aggregate(value,by=list(year),FUN=loqlength)[,2]))
    colnames(aggr)<-c("centralValue","unit","centralValueType","whiskerTopValue","whiskerBottomValue","whiskerType","dateTime","dateTimeString","n","nUnderLOQ")
    
    hole<-3*mean(aggr$dateTime[-1]-aggr$dateTime[-nrow(aggr)],trim=0.05)
    
    # k udava poradi segmentu jedne casove rady
    k<-1
    values<-list()
    series<-list()
    
    # j cyklus bezi pres jednotliva mereni
    for (j in 1:nrow(aggr)) {
      aggValue<-list(label=paste0(loca,"_aggr"),
                     n=aggr$n[j],
                     nUnderLOQ=aggr$nUnderLOQ[j],
                     unit=aggr$unit[j],
                     centralValue=aggr$centralValue[j],
                     centralValueType=aggr$centralValueType[j],
                     whiskerTopValue=aggr$whiskerTopValue[j],
                     whiskerBottomValue=aggr$whiskerBottomValue[j],
                     whiskerType=aggr$whiskerType[j],
                     dateTimeString=aggr$dateTimeString[j]);
      
      if (j==1) {
        values<-as.list(c(values,list(aggValue)))
      } else {
        if ((aggr$dateTime[j]-aggr$dateTime[j-1])<hole) {
          values<-as.list(c(values,list(aggValue)))
        } else {
          timeSeriesRecord<-list(values=values,label=paste0("site_aggr",i," part",k))
          k<-k+1
          series<-as.list(c(series,list(timeSeriesRecord)))
          values<-list(aggValue)
        }
      }
    }
    timeSeriesRecord<-list(values=values,label=paste0("site_aggr",i," part",k))
    series<-as.list(c(series,list(timeSeriesRecord)))
    
    # Popis agregovanych casovych rad ve 2. cyklu
    if (k<2) {
      res<-genstatistic(aggr$centralValue,aggr$dateTime)$res
      
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
                         perc5=quantile05(aggr$centralValue),
                         perc25=quantile25(aggr$centralValue),
                         perc75=quantile75(aggr$centralValue),
                         perc95=quantile95(aggr$centralValue),
                         geoMean95CIUpperBound=res$"geom. mean"*res$"geom. sd"^qnorm(0.975),
                         geoMean95CILowerBound=res$"geom. mean"*res$"geom. sd"^qnorm(0.025),
                         mean95CIUpperBound=res$mean+qnorm(0.975)*res$sd,
                         mean95CILowerBound=res$mean+qnorm(0.025)*res$sd)
    }
    else {
      trendSummary<-NA
    }
    
    seriesSet<-list(series=series,trendSummary=trendSummary,label=paste0("site_aggr",i));
    seriesSets<-as.list(c(seriesSets,list(seriesSet)))
    label<-paste0("Site_aggr",i)
    labels<-as.list(c(labels,list(label)))
  }
  
  
  ## Treti opakovani cyklu - vypocet trendu primarnich rad
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
    
    hole<-3*mean(dateTime[-1]-dateTime[-length(records[[i]]$values)],trim=0.05)
    
    if (max(dateTime[-1]-dateTime[-length(records[[i]]$values)])>hole) {
      series<-NA
    } else {
      curve<-data.frame(as.Date(as.numeric(genplot(valu,dateTime,n=20,distr="lnorm",plot=FALSE)$belt[1,]),origin="1970-01-01"),
                        as.numeric(genplot(valu,dateTime,n=20,distr="lnorm",plot=FALSE)$line[1,]),
                        as.numeric(genplot(valu,dateTime,n=20,distr="lnorm",plot=FALSE)$lower[1,]),
                        as.numeric(genplot(valu,dateTime,n=20,distr="lnorm",plot=FALSE)$upper[1,]))
      colnames(curve)<-c("belt","line","lower","upper")
      
      # Logaritmace v pripade log transformace (trend bude linearni)
      if (transformationType=="log") {
        curve$line<-log(curve$line)
        curve$lower<-log(curve$lower)
        curve$upper<-log(curve$upper)
      }
      
      # j cyklus bezi pres jednotlive body krivky
      for (j in 1:nrow(curve)) {
        cv<-list(label=loca,
                 centralValue=curve$line[j],
                 whiskerTopValue=curve$upper[j],
                 whiskerBottomValue=curve$lower[j],
                 dateTimeString=as.character(curve$belt[j]))
        
        values<-as.list(c(values,list(cv)))
      }
    }
    
    timeSeriesRecord<-list(values=values,label=paste0("site_trend",i))
    series<-as.list(c(series,list(timeSeriesRecord)))
    
    # Popis trendovych krivek v 3. cyklu.    
    if (max(dateTime[-1]-dateTime[-length(records[[i]]$values)])>hole) {
      trendSummary<-list(slope=genplot(valu,dateTime,n=20,distr="lnorm",plot=FALSE)$slope,
                         intercept=genplot(valu,dateTime,n=20,distr="lnorm",plot=FALSE)$intercept)
    }
    else {
      trendSummary<-NA
    }
    
    seriesSet<-list(series=series,trendSummary=trendSummary,label=paste0("site_trend",i));
    seriesSets<-as.list(c(seriesSets,list(seriesSet)))
    label<-paste0("Site_trend",i)
    labels<-as.list(c(labels,list(label)))
  }
  
  ## Ctvrte opakovani cyklu - vypocet trendu agregovanych rad
  
  # Vyber agregacnich funkci
  whisk<-c("5_95","25_75","min_max","2iq","ci")
  whisl<-c("quantile05","quantile25","min","iql","cil")
  whisu<-c("quantile95","quantile75","max","iqu","ciu")
  
  centv<-c("mean","median","geomean")
  centf<-c("arimean","quantile50","geomean")
  
  f1<-centf[which(centv==centralValueType)]
  f2<-whisl[which(whisk==whiskerValueType)]
  f3<-whisu[which(whisk==whiskerValueType)]
  
  # i cyklus bezi pres sites
  for (i in 1:length(records)) {
    loca<-as.character(records[[i]]$rowLabel)
    value        <-c()
    loqValue     <-c()
    loqMethodCode<-c()
    unit         <-c()
    dateTime     <-c()
    dateTimeString<-c()
    timeLength   <-c()
    
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
    
    # Jednotky musi byt stejne
    if (length(unique(unit))>1) {
      stop("Units of some records differ!")
    } else {
      unit<-unique(unit)
    }
    
    # Rocni agregace do noveho data.frame aggr
    year<-gendate(substr(dateTimeString,1,4))
    aggr<-data.frame(aggregate(valu,by=list(year),FUN=f1)[,2],
                     unit,
                     centralValueType,
                     aggregate(valu,by=list(year),FUN=f3)[,2],
                     aggregate(valu,by=list(year),FUN=f2)[,2],
                     whiskerValueType,
                     gendate(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=length)[,2]),
                     as.character(aggregate(value,by=list(year),FUN=loqlength)[,2]))
    colnames(aggr)<-c("centralValue","unit","centralValueType","whiskerTopValue","whiskerBottomValue","whiskerType","dateTime","dateTimeString","n","nUnderLOQ")
    
    hole<-3*mean(aggr$dateTime[-1]-aggr$dateTime[-nrow(aggr)],trim=0.05)
    
    if (max(aggr$dateTime[-1]-aggr$dateTime[-nrow(aggr)])>hole) {
      series<-NA
    } else {
      curve<-data.frame(as.Date(as.numeric(genplot(aggr$centralValue,aggr$dateTime,n=20,distr="lnorm",plot=FALSE)$belt[1,]),origin="1970-01-01"),
                        as.numeric(genplot(aggr$centralValue,aggr$dateTime,n=20,distr="lnorm",plot=FALSE)$line[1,]),
                        as.numeric(genplot(aggr$centralValue,aggr$dateTime,n=20,distr="lnorm",plot=FALSE)$lower[1,]),
                        as.numeric(genplot(aggr$centralValue,aggr$dateTime,n=20,distr="lnorm",plot=FALSE)$upper[1,]))
      colnames(curve)<-c("belt","line","lower","upper")
      
      # Logaritmace v pripade log transformace (trend bude linearni)
      if (transformationType=="log") {
        curve$line<-log(curve$line)
        curve$lower<-log(curve$lower)
        curve$upper<-log(curve$upper)
      }
      
      # j cyklus bezi pres jednotlive body krivky
      for (j in 1:nrow(curve)) {
        aggValue<-list(label=loca,
                       centralValue=curve$line[j],
                       whiskerTopValue=curve$upper[j],
                       whiskerBottomValue=curve$lower[j],
                       dateTimeString=as.character(curve$belt[j]))
        
        values<-as.list(c(values,list(aggValue)))
      }
      
      timeSeriesRecord<-list(values=values,label=paste0("site_trend_aggr",i))
      series<-as.list(c(series,list(timeSeriesRecord)))
    }
    
    # Popis trendovych agegovanych krivek ve 4. cyklu.    
    if (max(aggr$dateTime[-1]-aggr$dateTime[-nrow(aggr)])>hole) {
      trendSummary<-list(slope=genplot(valu,aggr$dateTime,n=20,distr="lnorm",plot=FALSE)$slope,
                         intercept=genplot(valu,aggr$dateTime,n=20,distr="lnorm",plot=FALSE)$intercept)
    }
    else {
      trendSummary<-NA
    }
    
    seriesSet<-list(series=series,trendSummary=trendSummary,label=paste0("site_trend_aggr",i));
    seriesSets<-as.list(c(seriesSets,list(seriesSet)))
    label<-paste0("Site_trend_aggr",i)
    labels<-as.list(c(labels,list(label)))
  }
  
  ## Vypocet prostorove agregovane rady z jednotlivych rocnich agregaci (jen jednou pro cely datovy soubor)
  numofrows<-length(seriesSets)/4
  valu<-c()
  data<-c()
  unit<-c()
  
  for (i in 1:numofrows) {
    lengthofrow<-length(seriesSets[[numofrows+i]]$series[[1]]$values)
    
    for (j in 1:lengthofrow) {
      valu<-c(valu,seriesSets[[numofrows+i]]$series[[1]]$values[[j]]$centralValue)
      data<-c(data,as.character(seriesSets[[numofrows+i]]$series[[1]]$values[[j]]$dateTimeString))
      unit<-c(unit,as.character(seriesSets[[numofrows+i]]$series[[1]]$values[[j]]$unit))
    }    
  }
  
  # Jednotky musi byt stejne
  if (length(unique(unit))>1) {
    stop("Units of some records differ!")
  } else {
    unit<-unique(unit)
  }
  
  whisk<-c("5_95","25_75","min_max","2iq","ci")
  whisl<-c("quantile05","quantile25","min","iql","cil")
  whisu<-c("quantile95","quantile75","max","iqu","ciu")
  
  centv<-c("mean","median","geomean")
  centf<-c("arimean","quantile50","geomean")
  
  f1<-centf[which(centv==centralValueType)]
  f2<-whisl[which(whisk==whiskerValueType)]
  f3<-whisu[which(whisk==whiskerValueType)]
  
  # Rocni agregace do noveho data.frame aggr
  year<-gendate(substr(data,1,4))
  aggr<-data.frame(aggregate(valu,by=list(year),FUN=f1)[,2],
                   unit,
                   centralValueType,
                   aggregate(valu,by=list(year),FUN=f3)[,2],
                   aggregate(valu,by=list(year),FUN=f2)[,2],
                   whiskerValueType,
                   gendate(aggregate(valu,by=list(year),FUN=f1)[,1]),
                   as.character(aggregate(valu,by=list(year),FUN=f1)[,1]),
                   as.character(aggregate(valu,by=list(year),FUN=length)[,2]),
                   as.character(aggregate(valu,by=list(year),FUN=loqlength)[,2]))
  colnames(aggr)<-c("centralValue","unit","centralValueType","whiskerTopValue","whiskerBottomValue","whiskerType","dateTime","dateTimeString","n","nUnderLOQ")
  
  hole<-3*mean(aggr$dateTime[-1]-aggr$dateTime[-nrow(aggr)],trim=0.05)
  
  # k udava poradi segmentu jedne casove rady
  k<-1
  values<-list()
  series<-list()
  
  # j cyklus bezi pres jednotliva mereni
  for (j in 1:nrow(aggr)) {
    aggValue<-list(label=paste0("all_aggregation"),
                   n=aggr$n[j],
                   nUnderLOQ=aggr$nUnderLOQ[j],
                   unit=aggr$unit[j],
                   centralValue=aggr$centralValue[j],
                   centralValueType=aggr$centralValueType[j],
                   whiskerTopValue=aggr$whiskerTopValue[j],
                   whiskerBottomValue=aggr$whiskerBottomValue[j],
                   whiskerType=aggr$whiskerType[j],
                   dateTimeString=aggr$dateTimeString[j]);
    
    if (j==1) {
      values<-as.list(c(values,list(aggValue)))
    } else {
      if ((aggr$dateTime[j]-aggr$dateTime[j-1])<hole) {
        values<-as.list(c(values,list(aggValue)))
      } else {
        timeSeriesRecord<-list(values=values,label=paste0("all_aggr",i," part",k))
        k<-k+1
        series<-as.list(c(series,list(timeSeriesRecord)))
        values<-list(aggValue)
      }
    }
  }
  timeSeriesRecord<-list(values=values,label=paste0("all_aggr",i," part",k))
  series<-as.list(c(series,list(timeSeriesRecord)))
  
  # Popis celkove agregovane rady
  if (k<2) {
    res<-genstatistic(aggr$centralValue,aggr$dateTime)$res
    
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
                       perc5=quantile05(aggr$centralValue),
                       perc25=quantile25(aggr$centralValue),
                       perc75=quantile75(aggr$centralValue),
                       perc95=quantile95(aggr$centralValue),
                       geoMean95CIUpperBound=res$"geom. mean"*res$"geom. sd"^qnorm(0.975),
                       geoMean95CILowerBound=res$"geom. mean"*res$"geom. sd"^qnorm(0.025),
                       mean95CIUpperBound=res$mean+qnorm(0.975)*res$sd,
                       mean95CILowerBound=res$mean+qnorm(0.025)*res$sd)
  }
  else {
    trendSummary<-NA
  }
  
  seriesSet<-list(series=series,trendSummary=trendSummary,label="all_agr")
  allAggregation<-list(seriesSet)
  label<-"All_aggr"
  aggregationLabels<-list(label)
  
  
  ## Zabaleni vysledku
  allSeries<-list(seriesSets=seriesSets,
                  labels=labels,
                  allAggregation=allAggregation,
                  aggregationLabels=aggregationLabels)
  
  timeSeriesDataSeries<-list(allSeries=allSeries)
  
  return(timeSeriesDataSeries)
}