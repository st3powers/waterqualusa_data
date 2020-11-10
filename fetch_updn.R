# run this script AFTER running fetchsites
rm(list=ls())

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


sites1<-read.csv("WQP_sites_Nitrogen_resultcount100.csv")
sites2<-read.csv("WQP_sites_Phosphorus_resultcount100.csv")
#sites1<-read.csv("WQP_sites_Nitrogen_resultcount500.csv")
#sites2<-read.csv("WQP_sites_Phosphorus_resultcount500.csv")
sites1<-sites1 %>% select(-activityCount,-resultCount, -siteUrl)
sites2<-sites2 %>% select(-activityCount,-resultCount,-siteUrl)
sites3<-rbind(sites1,sites2)
sites<-unique(sites3)

#sites0<-read.csv("WQP_sites_Nitrogen_resultcount1000.csv")
#sites0<-read.csv("WQP_sites_Phosphorus_resultcount1000.csv")
#sites<-sites0

site<-c()
comid<-c()
lon<-c()
lat<-c()
inletoutlet<-c()
tocomid<-c()
direction<-c()

sitefail<-c()
lonfail<-c()
latfail<-c()
comidfail<-c()
#netwkdistance<-c()

#for(i in 1:length(sites[,1])){
for(i in 6402:length(sites[,1])){
#for(i in 3610:length(sites[,1])){
#for(i in 1241:length(sites[,1])){
#for(i in 677:length(sites[,1])){
#for(i in 77:length(sites[,1])){
#for(i in 83:length(sites[,1])){
print(i)
sitei<-sites$MonitoringLocationIdentifier[i] %>% as.character()
locnamei<-sites$MonitoringLocationName[i] %>% as.character()
print(sitei)
print(locnamei)

lonlati<-c(sites$lon[i],sites$lat[i])
#lonlati<-c(sites$lon[40],sites$lat[40])
#lonlati<-c(-97.1567,31.5843)
#lonlati<-c(-97.1667,31.5893)
#lonlati<-c(-97.3667,31.5693)
#lonlati<-c(-121.5564,39.5138)
#lonlati<-c(-89.3541, 43.0850)

siteloc <- data.table(
  place=c("site"),
  longitude=lonlati[1],
  latitude=lonlati[2])

siteloc_sf <- st_as_sf(siteloc, coords = c("longitude", "latitude"), 
                       crs = 4326, agr = "constant")

start_point <- sf::st_sfc(sf::st_point(lonlati), crs = 4269)

start_comid <- discover_nhdplus_id(start_point)
if(length(start_comid)==0){start_comid<-NA}
#if(length(start_comid)==0 | as.numeric(start_comid)<0){
if(is.na(start_comid)==TRUE | as.numeric(start_comid)<0){
  sitefail<-c(sitefail,sitei)
  lonfail<-c(lonfail,lonlati[1])
  latfail<-c(latfail,lonlati[2])
#  if(length(start_comid)==0){start_comid<-NA}
  comidfail<-c(comidfail,start_comid)
  fail_df<-data.frame(sitefail,lonfail,latfail,comidfail)
  write.csv(fail_df,"updn_fails.csv")
  next()}

flowline_dn <- navigate_nldi(list(featureSource = "comid",
                                  featureID = start_comid),
                             mode = "downstreamMain",
                             distance_km = 20)

#comids_dn<-flowline_dn$nhdplus_comid


subset_file <- tempfile(fileext = ".gpkg")

subset_dn <- try(subset_nhdplus(comids=flowline_dn$nhdplus_comid,                            
                            output_file = subset_file,
                            nhdplus_data = "download", 
                            flowline_only = FALSE,
                            return_data = TRUE,
                            overwrite=TRUE),
                 silent=TRUE)
if(class(subset_dn)=="try-error"){next()}

flowline_dn <- subset_dn$NHDFlowline_Network
catchment_dn <- subset_dn$CatchmentSP
waterbody_dn <- subset_dn$NHDWaterbody

#flowline_waterbody_intersect_dn<-st_intersection(subset_dn$NHDFlowline_Network, waterbody_dn) # %>% as.data.frame()
dn_outlet_comid<-NA
tryit<-try(st_intersection(subset_dn$NHDFlowline_Network, waterbody_dn),silent=TRUE)

if(class(tryit)!="try-error"){
  flowline_waterbody_intersect_dn<-tryit  

dn_inlet_hydroseq<-flowline_waterbody_intersect_dn %>% as.data.frame() %>% filter(ftype=="ArtificialPath") %>%
  dplyr::summarize(maxhydroseq=max(hydroseq)) %>% as.numeric()
dn_inlet_comid<-flowline_dn %>% filter(hydroseq==dn_inlet_hydroseq) %>% as.data.frame() %>% select(comid) %>% as.numeric()

dn_inlet<-flowline_dn %>% filter(comid==dn_inlet_comid)


do<-0
if(do==1){
plotit<-ggplot() + 
  geom_sf(data = waterbody_dn,color="blue") +
  geom_sf(data = flowline_dn, aes(color = as.factor(levelpathi)))+
  guides(color=guide_legend(title="levelpathi"))+
  theme_bw()+
  geom_sf(data = dn_inlet,color = "red",size=3)+
  geom_sf(data = siteloc_sf,color = "black",size=3)
  print(plotit)
}
}
#####################################

flowline_up <- navigate_nldi(list(featureSource = "comid",
                                  featureID = start_comid),
                             mode = "upstreamMain",
                             distance_km = 50)

subset_file <- tempfile(fileext = ".gpkg")

subset_up <- try(subset_nhdplus(comids=flowline_up$nhdplus_comid,                            
                            output_file = subset_file,
                            nhdplus_data = "download", 
                            flowline_only = FALSE,
                            return_data = TRUE,
                            overwrite=TRUE),
                 silent=TRUE)

if(class(subset_up)!="try-error"){

flowline_up <- subset_up$NHDFlowline_Network
catchment_up <- subset_up$CatchmentSP
waterbody_up <- subset_up$NHDWaterbody

#flowline_waterbody_intersect_up<-st_intersection(subset_up$NHDFlowline_Network, waterbody_up) # %>% as.data.frame()
up_outlet_comid<-NA
tryit<-try(st_intersection(subset_up$NHDFlowline_Network, waterbody_up),silent=TRUE)

if(class(tryit)!="try-error"){
flowline_waterbody_intersect_up<-tryit  

up_outlet_hydroseq<-flowline_waterbody_intersect_up %>% as.data.frame() %>% filter(ftype=="ArtificialPath" & comid!=start_comid) %>% #group_by(levelpathi) %>%
  dplyr::summarize(maxhydroseq=min(hydroseq)) %>% as.numeric()
up_outlet_comid<-flowline_up %>% filter(hydroseq==up_outlet_hydroseq) %>% as.data.frame() %>% select(comid) %>% as.numeric()

up_outlet<-flowline_up %>% filter(comid==up_outlet_comid)
}
}

if(class(subset_up)=="try-error"){
up_outlet_comid<-NA
}

do<-0
if(do==1){
plotit<-ggplot() + 
  geom_sf(data = waterbody_up,color="blue") +
  geom_sf(data = flowline_up, aes(color = as.factor(levelpathi)))+
  guides(color=guide_legend(title="levelpathi"))+
  theme_bw()+
  geom_sf(data = up_outlet,color = "red",size=3)+
  geom_sf(data = siteloc_sf,color = "black",size=3)
print(plotit)
}

#if(i>1){
  site<-c(site,rep(sitei,2))
  comid<-c(comid,rep(start_comid,2))
  lon<-c(lon,rep(lonlati[1],2))
  lat<-c(lat,rep(lonlati[2],2))
  inletoutlet<-c(inletoutlet,"inlet","outlet")
  tocomid<-c(tocomid,dn_inlet_comid,up_outlet_comid)
  direction<-c(direction,"dn","up")
#}

  updn_df<-data.frame(site,comid,lon,lat,inletoutlet,tocomid,direction)
  write.csv(updn_df,"updn_df.csv")
}
write.csv(updn_df %>% unique(),"updn_df_unique.csv")
#updn_df<-data.frame(comid,lon,lat,inletoutlet,tocomid,direction)
#write.csv(updn_df,"updn_df.csv")

###############################3
