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

sites_phosphorus<-getwhatWQPdatasites(bbox=bboxes,charname="Phosphorus")#c("Phosphorus","phosphorus"))
write.csv(sites_phosphorus,"WQP_sites_Phosphorus.csv")
write.csv(sites_phosphorus %>% filter(resultCount>=10),"WQP_sites_Phosphorus_resultcount10.csv")
write.csv(sites_phosphorus %>% filter(resultCount>=20),"WQP_sites_Phosphorus_resultcount20.csv")
write.csv(sites_phosphorus %>% filter(resultCount>=50),"WQP_sites_Phosphorus_resultcount50.csv")
write.csv(sites_phosphorus %>% filter(resultCount>=100),"WQP_sites_Phosphorus_resultcount100.csv")
write.csv(sites_phosphorus %>% filter(resultCount>=500),"WQP_sites_Phosphorus_resultcount500.csv")
write.csv(sites_phosphorus %>% filter(resultCount>=1000),"WQP_sites_Phosphorus_resultcount1000.csv")
sites_nitrogen<-getwhatWQPdatasites(bbox=bboxes,charname="Nitrogen")#c("Nitrogen","nitrogen"))
write.csv(sites_nitrogen,"WQP_sites_Nitrogen.csv")
write.csv(sites_nitrogen %>% filter(resultCount>=10),"WQP_sites_Nitrogen_resultcount10.csv")
write.csv(sites_nitrogen %>% filter(resultCount>=20),"WQP_sites_Nitrogen_resultcount20.csv")
write.csv(sites_nitrogen %>% filter(resultCount>=50),"WQP_sites_Nitrogen_resultcount50.csv")
write.csv(sites_nitrogen %>% filter(resultCount>=100),"WQP_sites_Nitrogen_resultcount100.csv")
write.csv(sites_nitrogen %>% filter(resultCount>=500),"WQP_sites_Nitrogen_resultcount500.csv")
write.csv(sites_nitrogen %>% filter(resultCount>=1000),"WQP_sites_Nitrogen_resultcount1000.csv")


###############################################

do<-0

if(do==1){

sites_phosphorus<-getwhatWQPdatasites(bbox=bboxes,charname="phosphorus")#c("Phosphorus","phosphorus"))
sites_nitrogen<-getwhatWQPdatasites(bbox=bboxes,charname="nitrogen")#c("Nitrogen","nitrogen"))
sites_the<-getwhatWQPdatasites(bbox=bboxes,charname="the")#c("Nitrogen","nitrogen"))


sites_nitrogen<-getwhatWQPdatasites(bbox=bboxes,charname="Nitrogen")

sites_phosphorus<-gottensites

#################################################


gotten<-0
#for (i in 1:length(bboxes[,1])){
for (i in 1:10){
  Sys.sleep(3)
  print(i)
  print(now())
  
  trysitesi <- try(silent=TRUE,whatWQPdata(bBox=as.numeric(bboxes[i,]), 
                                         characteristicName = "Phosphorus"))#,  
#  whatWQPsites
#trysitesi <- try(silent=TRUE,whatWQPsites(bBox=as.numeric(bboxes[i,]), 
#                        characteristicName = "Phosphorus"))#,
#trysitesi <- try(silent=TRUE,whatNWISsites(bBox=as.numeric(bboxes[i,]), 
#                       parameterCd=c("00600")))#,
#                       hasDataTypeCd="dv"))
  
#trysitesi <- try(silent=TRUE,whatNWISsites(bBox=as.numeric(bboxes[i,]), 
#                       parameterCd=c("00010","00060"),
#                       hasDataTypeCd="dv"))
if(class(trysitesi)=="try-error" || is.na(trysitesi[1])==TRUE){next()}
if(class(trysitesi)!="try-error"){
  sitesi<-data.frame(trysitesi)
  gotten<-gotten+1
  if(gotten==1){gottensites<-sitesi}
  if(gotten>1){gottensites<-rbind(gottensites,sitesi)}
}
}

sites<-gottensites

sites <- whatWQPdata(bBox=as.numeric(bboxes[i,]),characteristicName = "Phosphorus")


sites <- whatWQPdata(countycode="US:55:025",characteristicName = "Phosphorus")

sites <- whatNWISsites(bBox=as.numeric(bboxes[20,]), 
                       parameterCd=c("00010","00060"),
                       hasDataTypeCd="dv")


sites <- whatNWISsites(bBox=c(-124.5,25,-65.0,38.5), 
                       parameterCd=c("00010","00060"),
                       hasDataTypeCd="dv")


dataPH <- readWQPdata(characteristicName="pH")



sites <- whatNWISsites(bBox=c(-83.0,36.5,-81.0,49.1), 
                       parameterCd=c("00010","00060"),
                       hasDataTypeCd="dv")


dischargeWI <- readNWISdata(service="dv",
                            stateCd="WI",
                            parameterCd="00060",
                            drainAreaMin="50",
                            statCd="00003")


sitesNJ <- whatWQPsites(statecode="US:34",
                        characteristicName="Chloride")

dataPH <- readWQPdata(statecode="US:55", 
                      characteristicName="pH")

type <- "Stream"
sites <- whatWQPdata(countycode="US:55:025",siteType=type)



site <- whatWQPsamples(siteid="USGS-01594440")


type <- "Stream"
sites <- whatWQPmetrics(countycode="US:55:025",siteType=type)


######################################

specificCond <- readWQPqw('WIDNR_WQX-10032762',
                          'Specific conductance',
                          '2011-05-01','2011-09-30')


parameterCd <- "00618" 
parameterINFO <- readNWISpCode(parameterCd)



siteNumbers <- c("01491000","01645000") 
siteINFO <- readNWISsite(siteNumbers)



whatNWISdata(siteNumber = c('05114000','09423350'),service=c("uv","dv"))


dailyDataAvailable <- whatNWISdata(siteNumber=siteNumbers, service="dv", statCd="00003")


}


