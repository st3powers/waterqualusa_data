
#https://cran.r-project.org/web/packages/nhdplusTools/vignettes/nhdplusTools.html
#https://usgs-r.github.io/nhdplusTools/articles/nhdplusTools.html

library(plyr)
library(dplyr);
library(data.table)
#library(DT)

library(lubridate)
library(tidyverse);
#library(reshape2)

library(janitor)
library(dataRetrieval)

library(sf)
library(nhdplusTools)


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

