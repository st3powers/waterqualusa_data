
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


site_phosphorus<-read.csv("WQP_sites_Phosphorus_resultcount1000.csv")
sites<-sites_phosphorus

lonlati<-c(sites$lon[40],sites$lat[40])
lonlati<-c(-97.1567,31.5843)
lonlati<-c(-97.1667,31.5893)
lonlati<-c(-97.3667,31.5693)
lonlati<-c(-121.5564,39.5138)
lonlati<-c(-89.3541, 43.0850)

start_point <- sf::st_sfc(sf::st_point(lonlati), crs = 4269)

start_comid <- discover_nhdplus_id(start_point)

flowline_dn <- navigate_nldi(list(featureSource = "comid",
                                  featureID = start_comid),
                             mode = "downstreamMain",
                             distance_km = 20)

comids<-flowline_dn$nhdplus_comid

subset_file <- tempfile(fileext = ".gpkg")

subset_dn <- subset_nhdplus(comids=flowline_dn$nhdplus_comid,                            output_file = subset_file,
                            nhdplus_data = "download", 
                            flowline_only = FALSE,
                            return_data = TRUE,
                            overwrite=TRUE)

flowline_dn <- subset_dn$NHDFlowline_Network
catchment_dn <- subset_dn$CatchmentSP
waterbody_dn <- subset_dn$NHDWaterbody

flowline_waterbody_intersect_dn<-st_intersection(subset_dn$NHDFlowline_Network, waterbody_dn) # %>% as.data.frame()

dn_inlet_hydroseq<-flowline_waterbody_intersect_dn %>% as.data.frame() %>% filter(ftype=="ArtificialPath") %>%
  dplyr::summarize(maxhydroseq=max(hydroseq)) %>% as.numeric()
dn_inlet_comid<-flowline_dn %>% filter(hydroseq==dn_inlet_hydroseq) %>% as.data.frame() %>% select(comid) %>% as.numeric()

dn_outlet_hydroseq<-flowline_waterbody_intersect_dn %>% as.data.frame() %>% filter(ftype=="ArtificialPath") %>%
  dplyr::summarize(maxhydroseq=min(hydroseq)) %>% as.numeric()
dn_outlet_comid<-flowline_dn %>% filter(hydroseq==dn_outlet_hydroseq) %>% as.data.frame() %>% select(comid) %>% as.numeric()

dn_inlet<-flowline_dn %>% filter(comid==dn_inlet_comid)
dn_outlet<-flowline_dn %>% filter(comid==dn_outlet_comid)

ggplot() + 
  geom_sf(data = waterbody_dn,color="blue") +
  geom_sf(data = flowline, aes(color = as.factor(levelpathi)))+
  guides(color=guide_legend(title="levelpathi"))+
  theme_bw()+
  geom_sf(data = dn_inlet,color = "red",size=3)+
  geom_sf(data = dn_outlet,color = "red",size=3)




###############################################

do<-0

if(do==1){
  
  test<-discover_nldi_characteristics() %>% as.data.frame()
  
  discover_nldi_characteristics(test$MonitoringLocationIdentifier[1])
  
  get_nldi_feature(list("featureSource" = "nwissite", featureID = "USGS-05428500"))
  
  get_nldi_feature(list("featureSource" = "nwissite", featureID = test$MonitoringLocationIdentifier[2]))
  
  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-10336626")
  get_nldi_feature(list("featureSource" = "nwissite", featureID = "USGS-10336626"))
  
  library(sf)
  
  lonlat<-c(sites_phosphorus$lon[1],sites_phosphorus$lat[1])
  
#  discover_nldi_characteristics(sites_phosphorus$MonitoringLocationIdentifier[1])
  
#  lonlat<-c(-77.0374,38.8974)
#  lonlat2<-c(test$lon[2],test$lat[2])
#  lonlat<-rbind(lonlat,lonlat2)
  start_point <- sf::st_sfc(sf::st_point(lonlat), crs = 4269)
  
  start_comid <- discover_nhdplus_id(start_point)
  start_COMID <- discover_nhdplus_id(start_point)
  
  flowline <- navigate_nldi(list(featureSource = "comid",
                                 featureID = start_comid),
                            mode = "upstreamMain",
                            distance_km = 1)
  flowline <- navigate_nldi(list(featureSource = "comid",
                                 featureID = start_comid),
                            mode = "downstreamTributaries",
                            distance_km = 1)
  
  calculate_levelpaths(start_point, status = FALSE)
  
  
  lonlat<-c(test$lon[1],test$lat[1])
  start_point <- sf::st_sfc(sf::st_point(lonlat), crs = 4269)
  
  start_COMID <- discover_nhdplus_id(start_point)
  sample_flines <- read_sf(system.file("extdata",
                                       "petapsco_flowlines.gpkg",
                                       package = "nhdplusTools"))
  DD_COMIDs <- get_DD(sample_flines, start_COMID, distance = 4)
  
  
  library(sf)
  sample_flines <- read_sf(system.file("extdata",
                                       "petapsco_flowlines.gpkg",
                                       package = "nhdplusTools"))
  plot(sample_flines$geom)
  start_COMID <- 11690196
  UM_COMIDs <- get_UM(sample_flines, start_COMID)
  plot(dplyr::filter(sample_flines, COMID %in% UM_COMIDs)$geom,
       col = "red", add = TRUE, lwd = 3)
  UM_COMIDs <- get_UM(sample_flines, start_COMID, distance = 50)
  plot(dplyr::filter(sample_flines, COMID %in% UM_COMIDs)$geom,
       col = "blue", add = TRUE, lwd = 2)
  
  
  sample_data <- system.file("extdata/sample_natseamless.gpkg", package = "nhdplusTools")
  
  plot_nhdplus(list(13293970, 13293750), streamorder = 3, nhdplus_data = sample_data)
  plot_nhdplus(list(list("comid", start_COMID),
                    list("nwissite", "USGS-05428500"),
                    list("huc12pp", "070900020603"),
                    list("huc12pp", "070900020602")),
               streamorder = 2,
               nhdplus_data = sample_data)
 
  
  sample_data <- system.file("extdata/sample_natseamless.gpkg", package = "nhdplusTools")
  plot_nhdplus(list(13293970, 13293750), streamorder = 3, nhdplus_data = sample_data)
  plot_nhdplus(list(list("comid", "13293970"),
                    list("nwissite", "USGS-05428500"),
                    list("huc12pp", "070900020603"),
                    list("huc12pp", "070900020602")),
               streamorder = 2,
               nhdplus_data = sample_data)
  plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
                                       y = 43.08944),
                            coords = c("x", "y"), crs = 4326),
               streamorder = 2,
               nhdplus_data = sample_data)
  plot_nhdplus(list(list("comid", "13293970"),
                    list("nwissite", "USGS-05428500"),
                    list("huc12pp", "070900020603"),
                    list("huc12pp", "070900020602")),
               streamorder = 2,
               nhdplus_data = sample_data,
               plot_config = list(basin = list(lwd = 2),
                                  outlets = list(huc12pp = list(cex = 1.5),
                                                 comid = list(col = "green"))))
  ###############################################
  
  lonlat<-c(sites_phosphorus$lon[1],sites_phosphorus$lat[1])
  start_point <- sf::st_sfc(sf::st_point(lonlat), crs = 4269)
  
  start_comid <- discover_nhdplus_id(start_point)
  start_COMID <- discover_nhdplus_id(start_point)
   
  sub_nhdplus<-subset_nhdplus(comids = start_comid, nhdplus_data = "download")#,output_file=output_file)
  sub_nhdplus_df<-data.frame(sub_nhdplus)
  which_renames<-grep("NHDFlowline_Network.",names(sub_nhdplus_df))
  names(sub_nhdplus_df)[which_renames]<-substr(names(sub_nhdplus_df)[which_renames],21,nchar(names(sub_nhdplus_df)[which_renames]))
    
  sub_nhdplus_df$levelpathi
  
  flowline <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid), 
                            mode = "upstreamTributaries", 
                            distance_km = 1000)
  
#  nhdplus_path(file.path(temp_dir, "natseamless.gpkg"))
#  nhdplus_path()
#  staged_data <- stage_national_data(output_path = tempdir())
#  flowline <- readRDS(staged_data$flowline)
  
  subset_file <- tempfile(fileext = ".gpkg")
  subset <- subset_nhdplus(comids = flowline$nhdplus_comid,
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = FALSE,
                           return_data = TRUE)
  
  flowline <- subset$NHDFlowline_Network
  catchment <- subset$CatchmentSP
  waterbody <- subset$NHDWaterbody
  
  ## Or:
  
  flowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
  catchment <- sf::read_sf(subset_file, "CatchmentSP")
  waterbody <- sf::read_sf(subset_file, "NHDWaterbody")
  
  plot(sf::st_geometry(flowline), col = "blue")
  plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
#  plot(sf::st_geometry(catchment), add = TRUE)
  plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)
  
#  get_DM
  
  DM_COMIDs <- get_DM(flowline, start_COMID, distance = 1000)
  plot(dplyr::filter(flowline, COMID %in% DM_COMIDs)$geom,
       col = "blue", add = TRUE, lwd = 2)
  
#  get_DD(flowline,start_comid)
  
  ##########################
  
  sites_phosphorus$HUCEightDigitCode[1]
  

  get_UM(network=sub_nhdplus$NHDFlowline_Network, comid=start_comid, distance = NULL, sort = FALSE, include = TRUE)
  
#  UM_COMIDs <- get_UM(sub_nhdplus, start_COMID)
  
  ################
  
  plot(sub_nhdplus$NHDFlowline_Network)
  
  
  plot(sub_nhdplus$NHDFlowline_Network$geom,
       col = "blue", add = TRUE, lwd = 2)
  
  DM_COMIDs <- get_DM(sub_nhdplus$NHDFlowline_Network, start_COMID)
  
  fline <- sf::read_sf(sub_nhdplus$NHDFlowline_Network,
                       "NHDFlowline_Network")
  start <- get_node(fline, "start")
  end <- get_node(fline, "end")
  plot(sf::st_zm(fline$geom),
       lwd = fline$StreamOrde, col = "blue")
  plot(sf::st_geometry(start), add = TRUE)
  
  
  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-12307750")
  
  navigate_nldi(nldi_feature = nldi_nwis,
                mode = "UM") %>%
    st_geometry() %>%
    plot(col = "blue", add = TRUE)
  
  geom_col <- attr(sub_nhdplus$NHDFlowline_Network, "sf_column")
  plot(sub_nhdplus$NHDFlowline_Network[[geom_col]],
       lwd = 3)
  
  
  geom_col <- attr(sub_nhdplus$NHDFlowline_Network, "sf_column")
  plot(sub_nhdplus$NHDFlowline_Network[[geom_col]],
       lwd = 3)
  start_point <- sf::st_sfc(sf::st_point(c(-89.362239, 43.090266)),
                            crs = 4326)
  plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
  start_comid <- discover_nhdplus_id(start_point)
  comids <- get_UT(sub_nhdplus$NHDFlowline_Network, start_comid)
  plot(dplyr::filter(sub_nhdplus$NHDFlowline_Network, COMID %in% comids)[[geom_col]],
       add=TRUE, col = "red", lwd = 2)
  
  up_ids <- get_UT(sub_nhdplus$NHDFlowline_Network, start_comid)
  
}


