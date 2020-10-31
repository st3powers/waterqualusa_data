

library(plyr)
library(shiny);#library(ggvis)
library(dplyr);
library(data.table)
#library(DT)

library(janitor);library(lubridate)
library(tidyverse);
library(ggplot2);
#library(reshape2)

library(janitor)
library(lme4)

library(leaflet)
library(leaflet.extras)
library(viridis)
#library(scales)

library(dataRetrieval)

#####################################


dataPH <- readWQPdata(characteristicName="pH")



sites <- whatNWISsites(bBox=c(-83.0,36.5,-81.0,38.5), 
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





