#if(getwd()=="/home/ubuntu"){setwd("/home/ubuntu/rivercreeper")}
#

library(shiny)
library(ggvis)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(data.table)
#library(RColorBrewer)
#library(pals)
library(ggplot2)
#library(RCurl)
library(XML)
#library(xml2)
#library(WaterML)
#library(httr)
library(lubridate)

library("curl")
#library(stringi)
library(stringr)

##################

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

################

epsg4326 <- leafletCRS(crsClass = "L.CRS.EPSG4326", code = "EPSG:4326")

# for the full site files
colclasses_sites<-rep("character",56)
colclasses_sites[c(7,8)]<-"numeric"

# for the trimmed site files
colclasses_sites<-rep("character",18)
colclasses_sites[c(10,11)]<-"numeric"
sites<-fread("sites_streams_trim.txt" ,colClasses=colclasses_sites) %>% as.data.frame()

floodstages<-fread("floodstages.csv" ,colClasses="character") %>% as.data.frame()
floodstages<-subset(floodstages,floodstages$stage_unit != "m")
floodstages$floodstage[which(floodstages$floodstage=="")]<-"NA"

sites_trim<-sites %>% select(agency_cd,site_no,station_nm,dec_lat_va,dec_long_va,state_cd,huc_cd) %>% as.data.frame()
sites_trim$site_no<-as.character(sites_trim$site_no)
sites_trim$station_nm<-as.character(sites_trim$station_nm)
sites_trim$state_cd<-as.character(sites_trim$state_cd)
sites_trim$station_state<-paste(sites_trim$station_nm,sites_trim$state_cd)
sites_trim$huc_cd<-as.character(sites_trim$huc_cd)

huclist<-fread("huclist.txt",colClasses=c("character","character")) %>% as.data.frame()
huc6list<-subset(huclist,nchar(huclist$huc)==6)
huc6list$code_basin<-paste(huc6list$huc,"_ ",huc6list$basin,sep="")

huc8list<-subset(huclist,nchar(huclist$huc)==8)
huc8list$code_basin<-paste(huc8list$huc,huc8list$basin)
names(huc6list)[which(names(huc6list)=="huc")]<-"huc6"

huc8list$huc6<-substr(huc8list$huc,1,6)
names(huc8list)[which(names(huc8list)=="huc")]<-"huc8"

hucmerged<-merge(huc8list,huc6list,by="huc6",suffixes=c("_huc8","_huc6"))

huc68<-hucmerged %>% group_by(huc6) %>% 
  mutate(huc8 = paste0(huc8, "," ,collapse = "")) %>% as.data.frame()
huc68$huc8<-substr(huc68$huc8,1,nchar(huc68$huc8)-1)

huc68$nchar<-nchar(huc68$huc8)
# new line to restrict groupings to 10 huc8's, the limit for water services (?)
huc68$huc8[which(huc68$nchar>=9*10)]<-substr(huc68$huc8[which(huc68$nchar>=9*10)],1,(9*10)-1)
huc68$huc8<-str_replace_all(huc68$huc8,",",", ")

huc8_trim<-huc8list %>% select(huc=huc8,code_basin) %>% as.data.frame()
huc6_trim<-huc68 %>% select(huc=huc8,code_basin=code_basin_huc6) %>% as.data.frame()
huc6_trim<-unique(huc6_trim)
huc6_trim$code_basin<-paste(huc6_trim$code_basin," huc8s ",huc6_trim$huc,sep="")

huclist<-rbind(huc8_trim,huc6_trim)

huclist0<-c(
  "Flooded1- a few sites flooded now",
#  "Flooded2- more flooded sites",  
  "Flooded2- all sites flooded now, data since last year",
  "Flooded3- all sites flooded now, data since 2007",  
  "17010304	St. Joe",
  "16050102	Truckee",
#  "Flooding sites",
#  "dams_ID_",
#  "dams_CA_",  
#  "poopoo",
  "18040010	Upper Stanislaus",
#  "18020123	Middle Fork Feather",
#  "18020121	North Fork Feather",
#  "10190003 Middle South Platte-Cherry Creek",
#  "10190004 Clear",
#  "10190005 St. Vrain",
  "170603_ Clearwater huc8s 17060301, 17060302, 17060303, 17060304, 17060305, 17060306, 17060307, 17060308",
  "180500_ San Francisco Bay huc8s 18050001, 18050002, 18050003, 18050004, 18050005, 18050006",
  "18020123	Middle Fork Feather",
  "14010001 Colorado headwaters",
#  "10190006 Big Thompson",
  "12070104	Lower Brazos",
  "12060201	Middle Brazos-Palo Pinto",
  "12060202	Middle Brazos-Lake Whitney",
  "07090001	Upper Rock")

huclist_use<-c(huclist0,huclist$code_basin)
varnames<-c("00060")#,"00600","00665")

zoomstart<-9
#pointradius<-5000
pointradius<-2000

stationgroups<-c("all")
statmonths<-c(5,6,7,8,9,10,11,12,1,2,3,4)
timewindows<-c("recent","all")
maxyear<-as.numeric(year(now()))
minyear<-min(c(as.numeric(year(now()))-12,2007))
years<-maxyear:minyear#s[1]
minyearflood<-minyear
#refyears<-c("none",as.character(years))
refyears<-c("none","all_others",as.character(years))

#############

url_flood<-'https://waterwatch.usgs.gov/webservices/flood?&floodonly&format=csv' 
outflood <- data.frame(fread(url_flood,colClasses="character"))

do<-0

if(do==1){

url_flood_all<-'https://waterwatch.usgs.gov/webservices/flood?&format=csv' 
outflood_all <- data.frame(fread(url_flood_all,colClasses="character"))

outflood_all$dateflood<-substr(as.character(outflood_all$stage_dt),1,10)
outflood_all$stagewunit<-paste(outflood_all$stage,outflood_all$stage_unit,sep="")
outflood_all$abovefloodstage<-as.numeric(outflood_all$stage)-as.numeric(outflood_all$floodstage)
outflood_all$belowfloodstage<-as.numeric(outflood_all$floodstage)-as.numeric(outflood_all$stage)
outflood_all$abovebelow<-"bel"
outflood_all$abovebelow[which(outflood_all$belowfloodstage<0)]<-"abv"
outflood_all$relstage<-round(abs(outflood_all$belowfloodstage),digits=3)
outflood_all$stage_str<-paste(outflood_all$stagewunit,paste(outflood_all$relstage,outflood_all$abovebelow),sep=" / ")
#outflood_all$stage_str[which(substr(outflood_all$stage,1,3)=="Eqp")]<-
#  "equip fail"
outflood_all$stage_str[which(is.na(outflood_all$relstage)==TRUE)]<-paste(outflood_all$stagewunit[which(is.na(outflood_all$relstage)==TRUE)],"NA",sep=" / ")

outflood_all_trim<-outflood_all %>% select(site_no,dateflood,stage_str) %>% as.data.frame()
}

floodsites<-unique(outflood$site_no)
hucpart0<-as.character(paste0(floodsites, "," ,collapse = ""))
hucpartflood<-substr(hucpart0,1,nchar(hucpart0)-1)

# playing with forecasted flows from nws
do<-0
if(do==1){
url_pred<-'https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=stew3&output=xml'

#url_pred<-'https://waterwatch.usgs.gov/webservices/flood?&floodonly&format=csv' 
#url_pred<-'https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=stew3&output=tabular' 
#https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=stew3&output=tabular&time_zone=cdt
#url_pred<-'https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=stew3&format=csv' 
#url_pred<-'https://water.weather.gov/ahps2/hydrograph_to_csv.php?gage=stew3

conpred <- curl(url_pred)
open(conpred)
#      out<-readLines(con,encoding="UTF-8")  
outpred<-read.csv(conpred)#,encoding="UTF-8",colClasses="character")  
close(conpred)
#https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=stew3&output=tabular

pred1<-as.character(names(outpred))
pred2<-as.data.frame(strsplit(pred1,"forecast"))
pred3<-pred2[4,]
pred4<-gsub("..datum..datum..","",pred3)
pred5<-gsub("pedts.valid.timezone.UTC.","",pred4)
pred6<-data.frame(strsplit(pred5,"valid.timezone.UTC."))[2,]
pred7<-gsub("..primary..pedts.","",pred6)
pred8<-gsub("..pedts...datum...","",pred7)
pred9<-gsub("..valid..primary.name.","",pred8)
pred10<-gsub("..valid..primary.name.","",pred9)
pred11<-gsub(".units.ft.","",pred10)
pred12<-gsub(".units.m.","",pred11)
pred13<-gsub("HGIFE..","_",pred12)
pred14<-gsub("HGIFE","_",pred13)
pred15<-gsub("Stage","_",pred14)
predvals<-as.numeric(unlist(strsplit(pred15,"_"))[2*c(1:(0.5*length(unlist(strsplit(pred15,"_")))))])
preddatetime<-unlist(strsplit(pred15,"_"))[-1+2*c(1:(0.5*length(unlist(strsplit(pred15,"_")))))]
preddate<-substr(preddatetime,1,10)
preddate<-gsub("\\.","-",preddate)
predtime<-as.numeric(substr(preddatetime,12,16))
preds.df<-data.frame(preddate,predtime,predvals)
preds.df %>% group_by(preddate) %>%
  dplyr::summarize(max=max(predvals,na.rm=TRUE)) %>% as.data.frame()

}


