
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
readWQPqw_batched<-function(sites_use,batchsize,paramtext){
int0<-1:length(sites_use[,1])/batchsize
int<-trunc(int0)+1
for(i in 1:length(unique(int))){
  print(paste(i," of ", length(unique(int))))
  Sys.sleep(5)
  sitesi<-sites_use$MonitoringLocationIdentifier[which(int==i)]
  datai <- readWQPqw(siteNumbers=sitesi,parameterCd=paramtext)
  if(i==1){datafetched<-datai}
  if(i>1){datafetched<-rbind(datafetched,datai)}
}
datafetched$param<-paste(datafetched$CharacteristicName," ",datafetched$ResultSampleFractionText," ", datafetched$ResultMeasure.MeasureUnitCode,sep="")
return(datafetched)
}

#######################################################

sites0<-fread("WQP_sites_Phosphorus_resultcount100.csv") %>% as.data.frame()
sites<-sites0
sites_use<-sites %>% filter(StateName %in% c("Texas"))
phosphorusTX<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Phosphorus")
sites_use<-sites %>% filter(StateName %in% c("Oklahoma"))
phosphorusOK<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Phosphorus")
sites_use<-sites %>% filter(StateName %in% c("Arkansas"))
phosphorusAR<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Phosphorus")
#sites_use<-sites %>% filter(StateName %in% c("LA"))
#phosphorusLA<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Phosphorus")
#sites_use<-sites %>% filter(StateName %in% c("MO"))
#phosphorusMO<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Phosphorus")
#sites_use<-sites %>% filter(StateName %in% c("KS"))
#phosphorusKS<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Phosphorus")
write.csv(phosphorusTX,file="wqp_phosphorus_n100_TX.csv")
write.csv(phosphorusOK,file="wqp_phosphorus_n100_OK.csv")
write.csv(phosphorusAR,file="wqp_phosphorus_n100_AR.csv")
#write.csv(phosphorusLA,file="wqp_phosphorus_n100_LA.csv")
#write.csv(phosphorusMO,file="wqp_phosphorus_n100_MO.csv")
#write.csv(phosphorusKS,file="wqp_phosphorus_n100_KS.csv")


################################################################
sites0<-fread("WQP_sites_Nitrogen_resultcount100.csv") %>% as.data.frame()
sites<-sites0
sites_use<-sites %>% filter(StateName %in% c("Texas"))
nitrogenTX<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Nitrogen")
sites_use<-sites %>% filter(StateName %in% c("Oklahoma"))
nitrogenOK<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Nitrogen")
sites_use<-sites %>% filter(StateName %in% c("Arkansas"))
nitrogenAR<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Nitrogen")
#sites_use<-sites %>% filter(StateName %in% c("LA"))
#nitrogenLA<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Nitrogen")
#sites_use<-sites %>% filter(StateName %in% c("MO"))
#nitrogenMO<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Nitrogen")
#sites_use<-sites %>% filter(StateName %in% c("KS"))
#nitrogenKS<-readWQPqw_batched(sites_use=sites_use,batchsize=20,paramtext="Nitrogen")
write.csv(nitrogenTX,file="wqp_nitrogen_n100_TX.csv")
write.csv(nitrogenOK,file="wqp_nitrogen_n100_OK.csv")
write.csv(nitrogenAR,file="wqp_nitrogen_n100_AR.csv")
#write.csv(nitrogenLA,file="wqp_nitrogen_n100_LA.csv")
#write.csv(nitrogenMO,file="wqp_nitrogen_n100_MO.csv")
#write.csv(nitrogenKS,file="wqp_nitrogen_n100_KS.csv")


#####################################
