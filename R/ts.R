# Testovaci data v JSON reprezentaci
timeSeriesDataJson = '[{"label":"Site 0","values":[{"value":2.58,"loqValue":3.939,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2000-02-26","timeLength":23},{"value":3.147,"loqValue":3.289,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1996-01-26","timeLength":13},{"value":0.114,"loqValue":5.319,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2002-02-23","timeLength":11}]},{"label":"Site 1","values":[{"value":4.661,"loqValue":2.114,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2004-01-06","timeLength":19},{"value":2.935,"loqValue":0.262,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2010-01-05","timeLength":14},{"value":0.465,"loqValue":5.812,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2006-03-11","timeLength":5},{"value":6.879,"loqValue":9.331,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1997-08-02","timeLength":15}]},{"label":"Site 2","values":[{"value":5.747,"loqValue":5.895,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1990-04-08","timeLength":27},{"value":7.306,"loqValue":8.274,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1995-08-03","timeLength":22},{"value":0.157,"loqValue":8.873,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2005-11-23","timeLength":10}]},{"label":"Site 3","values":[{"value":6.558,"loqValue":7.329,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1991-08-23","timeLength":9},{"value":1.929,"loqValue":2.338,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1995-10-25","timeLength":8},{"value":8.103,"loqValue":0.961,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1994-07-14","timeLength":13},{"value":8.053,"loqValue":5.394,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"1991-09-13","timeLength":7},{"value":5.431,"loqValue":2.877,"loqMethodCode":"INS","unit":"ng_m3","dateTime":null,"dateTimeString":"2001-05-01","timeLength":10}]}]';

# prevod testovacich dat do nativni podoby - takto bude strukturovan vstup funkce timeSeries predavany do argumentu records
timeSeriesData = jsonlite::fromJSON(timeSeriesDataJson);
#records<-timeSeriesData
#i<-2

# timeSeriesData$values[[1]]$value
# class(timeSeriesData$values)



# Vytvori datovou strukturu pro report TimeSeries
#
# Pro kazdou casovou radu provede:
# 1. setrideni hodnot podle data
# 2. identifikace casovych der a podle toho rozpad do samostanych useku casove rady
# 3. Vypocte trend summary pro kazdou casouvou radu
# 4. Provede rocni agregaci casovych rad
# 5. Transformuje vysledky do datove struktury TimeSeriesDataSeries
ts <- function(records, centralValueType="median", whiskerValueType="5_95", transformationType="none") {  
  
  library(alldatabrowser)
  library(genasis)  
  
  ## Prvni opakovani cyklu - primarni casove rady
  seriesSets<-list()
  labels<-list()
  # i cyklus bezi pres sites
  for (i in 1:nrow(records)) {
    loca<-as.character(records[i,1])
    data<-as.data.frame(records[i,2])
    
    data$dateTime<-as.Date(data$dateTimeString)
    data<-data[order(data$dateTimeString),]
    
    # Nahrada LoQ (v promenne valu budou hodnoty vstupujici do vypoctu)
    valu<-data$value
    valu[which(is.na(valu)&data$loqMethodCode=="INS")]<-data$loqValue[which(is.na(valu))]*1/2
    
    # Logaritmicka transformace
    if (transformationType=="log") {
      valu<-log(valu)
    }
    
    hole<-3*mean(data$dateTime[-1]-data$dateTime[-nrow(data)],trim=0.05)
    
    # k udava poradi segmentu jedne casove rady
    k<-1
    values<-list()
    series<-list()
    
    # j cyklus bezi pres jednotliva mereni
    for (j in 1:nrow(data)) {
      cv<-list(value=valu[j],
               loqValue=data$loqValue[j],
               label=loca,
               loqMethodCode=data$loqMethodCode[j],
               unit=data$unit[j],
               dateTime=data$dateTime[j],
               dateTimeString=data$dateTimeString[j],
               timeLength=nrow(data));
      
      if (j==1) {
        values<-as.list(c(values,list(cv)))
      } else {
        if ((data$dateTime[j]-data$dateTime[j-1])<hole) {
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
      res<-genstatistic(valu,data$dateTime)$res
      
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
  for (i in 1:nrow(records)) {
    loca<-as.character(records[i,1])
    data<-as.data.frame(records[i,2])
    
    data$dateTime<-as.Date(data$dateTimeString)
    data<-data[order(data$dateTimeString),]
    
    # Nahrada LoQ (v promenne valu budou hodnoty vstupujici do vypoctu)
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
    
    # Rocni agregace do noveho data.frame aggr
    year<-gendate(substr(data$dateTimeString,1,4))
    aggr<-data.frame(aggregate(valu,by=list(year),FUN=f1)[,2],
                     unit,
                     centralValueType,
                     aggregate(valu,by=list(year),FUN=f3)[,2],
                     aggregate(valu,by=list(year),FUN=f2)[,2],
                     whiskerValueType,
                     gendate(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=length)[,2]),
                     as.character(aggregate(data$value,by=list(year),FUN=loqlength)[,2]))
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
  for (i in 1:nrow(records)) {
    loca<-as.character(records[i,1])
    data<-as.data.frame(records[i,2])
    
    data$dateTime<-as.Date(data$dateTimeString)
    data<-data[order(data$dateTimeString),]
    
    # Nahrada LoQ (v promenne valu budou hodnoty vstupujici do vypoctu)
    valu<-data$value
    valu[which(is.na(valu)&data$loqMethodCode=="INS")]<-data$loqValue[which(is.na(valu))]*1/2
    
    hole<-3*mean(data$dateTime[-1]-data$dateTime[-nrow(data)],trim=0.05)
    
    if (max(data$dateTime[-1]-data$dateTime[-nrow(data)])>hole) {
      series<-NA
    } else {
      curve<-data.frame(as.Date(as.numeric(genplot(valu,data$dateTime,n=20,distr="lnorm",plot=FALSE)$belt[1,]),origin="1970-01-01"),
                        as.numeric(genplot(valu,data$dateTime,n=20,distr="lnorm",plot=FALSE)$line[1,]),
                        as.numeric(genplot(valu,data$dateTime,n=20,distr="lnorm",plot=FALSE)$lower[1,]),
                        as.numeric(genplot(valu,data$dateTime,n=20,distr="lnorm",plot=FALSE)$upper[1,]))
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
    if (max(data$dateTime[-1]-data$dateTime[-nrow(data)])>hole) {
      trendSummary<-list(slope=genplot(valu,data$dateTime,n=20,distr="lnorm",plot=FALSE)$slope,
                         intercept=genplot(valu,data$dateTime,n=20,distr="lnorm",plot=FALSE)$intercept)
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
  for (i in 1:nrow(records)) {
    loca<-as.character(records[i,1])
    data<-as.data.frame(records[i,2])
    
    data$dateTime<-as.Date(data$dateTimeString)
    data<-data[order(data$dateTimeString),]
    
    # Nahrada LoQ (v promenne valu budou hodnoty vstupujici do vypoctu)
    valu<-data$value
    valu[which(is.na(valu)&data$loqMethodCode=="INS")]<-data$loqValue[which(is.na(valu))]*1/2
    
    # Jednotky musi byt stejne
    if (length(unique(data$unit))>1) {
      stop()
    } else {
      unit<-unique(data$unit)
    }
    
    # Rocni agregace do noveho data.frame aggr
    year<-gendate(substr(data$dateTimeString,1,4))
    aggr<-data.frame(aggregate(valu,by=list(year),FUN=f1)[,2],
                     unit,
                     centralValueType,
                     aggregate(valu,by=list(year),FUN=f3)[,2],
                     aggregate(valu,by=list(year),FUN=f2)[,2],
                     whiskerValueType,
                     gendate(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=f1)[,1]),
                     as.character(aggregate(valu,by=list(year),FUN=length)[,2]),
                     as.character(aggregate(data$value,by=list(year),FUN=loqlength)[,2]))
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
  
  
  
  timeSeriesDataSeries <- list(seriesSets=seriesSets,
                               labels=labels)
  
  return(timeSeriesDataSeries);
}


# Test
x<-timeSeries(timeSeriesData)

x[[1]][[16]][[1]][[1]][[2]]