
library(plyr)
library(dplyr);
library(data.table)
#library(DT)

library(lubridate)
library(tidyverse);
#library(reshape2)

library(janitor)
library(dataRetrieval)

#####################################

sites0<-fread("WQP_sites_Phosphorus_resultcount100.csv") %>% as.data.frame()
sites<-sites0
sites<-sites %>% filter(StateName %in% c("Texas","Oklahoma","Arkansas"))

# too many requests at once produced failed requests on 2020 Oct 29

int0<-1:length(sites[,1])/20
int<-trunc(int0)+1
for(i in 1:length(unique(int))){
print(paste(i," of ", length(unique(int))))
Sys.sleep(5)
sitesi<-sites$MonitoringLocationIdentifier[which(int==i)]
datai <- readWQPqw(siteNumbers=sitesi,parameterCd="Phosphorus")
if(i==1){datafetched<-datai}
if(i>1){datafetched<-rbind(datafetched,datai)}
}
datafetched$param<-paste(datafetched$CharacteristicName," ",datafetched$ResultSampleFractionText," ", datafetched$ResultMeasure.MeasureUnitCode,sep="")
phosphorusfetched<-datafetched

write.csv(phosphorusfetched,file="wqp_phosphorus_n100.csv")

int0<-1:length(sites[,1])/20
int<-trunc(int0)+1
for(i in 1:length(unique(int))){
print(paste(i," of ", length(unique(int))))
Sys.sleep(4)
datai <- readWQPqw(siteNumbers=sites$MonitoringLocationIdentifier,parameterCd="Nitrogen")
if(i==1){datafetched<-datai}
if(i>1){datafetched<-rbind(datafetched,datai)}
}
datafetched$param<-paste(datafetched$CharacteristicName," ",datafetched$ResultSampleFractionText," ", datafetched$ResultMeasure.MeasureUnitCode,sep="")
nitrogenfetched<-datafetched

write.csv(nitrogenfetched,file="wqp_nitrogen_n100.csv")


for(i in 1:length(sites[,1])){
#for(i in 1:10){
datai <- readWQPqw(siteNumbers=sites$MonitoringLocationIdentifier[i],parameterCd="Phosphorus")
if(i==1){datafetched<-datai}
if(i>1){datafetched<-rbind(datafetched,datai)}
if(i %in% seq(from=100,to=100000,by=100)){print(paste(i," of ", length(sites[,1])))}
}
datafetched$param<-paste(datafetched$CharacteristicName," ",datafetched$ResultSampleFractionText," ", datafetched$ResultMeasure.MeasureUnitCode,sep="")
phosphorusfetched<-datafetched

for(i in 1:length(sites[,1])){
  datai <- readWQPqw(siteNumbers=sites$MonitoringLocationIdentifier[i],parameterCd="Nitrogen")
  if(i==1){datafetched<-datai}
  if(i>1){datafetched<-rbind(datafetched,datai)}
if(i %in% seq(from=100,to=100000,by=100)){print(paste(i," of ", length(sites[,1])))}
}
datafetched$param<-paste(datafetched$CharacteristicName," ",datafetched$ResultSampleFractionText," ", datafetched$ResultMeasure.MeasureUnitCode,sep="")
nitrogenfetched<-datafetched

write.csv(phosphorusfetched,file="wqp_phosphorus_n100.csv")
write.csv(nitrogenfetched,file="wqp_nitrogen_n100.csv")

##################3

#i<-2
#datai<-whatWQPsamples(siteid=sites$MonitoringLocationIdentifier[i])
#datai <- whatWQPmetrics(siteid=sites$MonitoringLocationIdentifier[i])
#datai <- readWQPqw(siteNumbers=sites$MonitoringLocationIdentifier[i],parameterCd="00600")
#datai <- readWQPqw(siteNumbers=sites$MonitoringLocationIdentifier[i],parameterCd="Phosphorus")

#i<-1
#datai <- readWQPqw(siteNumbers=sites$MonitoringLocationIdentifier[i],parameterCd="00600")
#dataireadWQPqw(siteNumbers=sites$MonitoringLocationIdentifier[i],parameterCd="Phosphorus")


#####################################

do<-0
if(do==1){
#latsbbx<-seq(from=24.5,to=49.5,by=2.5)
#lonsbbx<-seq(from=-125.0,to=-65.0,by=2.5)
latsbbx<-seq(from=24.5,to=49.5,by=5)
lonsbbx<-seq(from=-125.0,to=-65.0,by=5)

latsbbx0<-latsbbx[-length(latsbbx)]
latsbbx1<-latsbbx[-1]
lonsbbx0<-lonsbbx[-length(lonsbbx)]
lonsbbx1<-lonsbbx[-1]

lonlower<-c()
latlower<-c()
lonupper<-c()
latupper<-c()
for(i in 1:length(lonsbbx0)){
  lonloweri<-lonsbbx0[i]
  lonupperi<-lonsbbx1[i]
  for(j in 1:length(latsbbx0)){
  latlowerj<-latsbbx0[j] 
  latupperj<-latsbbx1[j]
  lonlower<-c(lonlower,lonloweri)
  lonupper<-c(lonupper,lonupperi)
  latlower<-c(latlower,latlowerj)
  latupper<-c(latupper,latupperj)
  }
}

bboxes<-data.frame(lonlower,latlower,lonupper,latupper)
bboxes


getwhatWQPdatasites<-function(bbox,charname){

gotten<-0
for (i in 1:length(bboxes[,1])){
#for (i in 1:6){
  Sys.sleep(4)
  print(i)
  print(now())
  
  trysitesi <- try(silent=TRUE,whatWQPdata(bBox=as.numeric(bbox[i,]), 
                                           characteristicName = charname))#,  
  if(class(trysitesi)=="try-error" || is.na(trysitesi[1])==TRUE){next()}
  if(class(trysitesi)!="try-error"){
    sitesi<-data.frame(trysitesi)
    gotten<-gotten+1
    if(gotten==1){gottensites<-sitesi}
    if(gotten>1){gottensites<-rbind(gottensites,sitesi)}
  }
}
return(gottensites)
}

}
