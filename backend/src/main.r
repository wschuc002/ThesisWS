# Main script for determining Residence Workplace and commuting route from BAG data
# Copyright (C) 2016 William Schuch
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

## TESTED ON WINDOWS 7 (64-bit), R v3.3.1

## TODO:  - ...
##        - ...
##        - Download input data from server (Google Drive).
##        - More residential profiles than "Office worker"
##        - Introduce "spacetime" package and test is
##        - ...

## Note 1: This script only brings different modules in modules/ together.
## No code belonging to one of the modules should go here.

## Note 2: It is expected that the working directory is set to backend/src/

## TERMS:
## PPH: Personal Place History
## CT: Conversion Table

#### Import modules ####

#source("modules/input.r")
source("modules/SaveAsFile.r")
source("modules/RGBtoSingleBand.r")
source("modules/DetermineRoutes.r")
source("modules/ConversionTable.r")
source("modules/PersonalLocationToLocationID.r")
source("modules/LinkPointsToTime.r")
source("modules/HourOfTheYear.r")
source("modules/ReadIDF5files.r")
source("modules/CumulativeExposure.r")

source("modules/TimePhases.r")



ndownload.AQNL("https://drive.google.com/file/d/0B5dbtjRcWbwiSU9tOUQ0TUxZR0E") # bug in downloading files from Google Drive
unzip.AQNL("20161108_pm10_no2.zip")

#RGBtoSingleBand("20161108_vandaag_no2_03.tiff")
RGB.list = list.files(file.path("..", "data", "RIVM"), pattern = ".tiff" )
for (i in RGB.list)
{
  RGBtoSingleBand(i)
}

DetermineRoutesNL(c("Utrecht", "Gelderland"), 100, 1000)

CRAB_Doel = DetermineAddressGoals_FL("Antwerpen")
SaveAsFile(CRAB_Doel, "CRAB_OUT_Antwerpen", "Shapefile", TRUE)
CRAB_Doel = readOGR(file.path("..", "output", "CRAB_OUT_Antwerpen.shp"), layer = "CRAB_OUT_Antwerpen")
DetermineRoutesFL(CRAB_Doel, "Antwerpen", 15, 1000, "simplified")
#rm(CRAB_Doel)



CT = CreateConversionTable()
CT.SP = MakeCTSpatial(CT)

PPH.C1_in = file.path("..", "output", "CommutingRoutesOutwards_Antwerpen.geojson")
PPH.C1 = readOGR(PPH.C1_in, layer = 'OGRGeoJSON')
PPH.C2_in = file.path("..", "output", "CommutingRoutesInwards_Antwerpen.geojson")
PPH.C2 = readOGR(PPH.C2_in, layer = 'OGRGeoJSON')
PPH.R_in = file.path("..", "output", "Residence_Antwerpen.geojson")
PPH.R = readOGR(PPH.R_in, layer = 'OGRGeoJSON')
PPH.W_in = file.path("..", "output", "Workplace_Antwerpen.geojson")
PPH.W = readOGR(PPH.W_in, layer = 'OGRGeoJSON')
LocationIDs.C1 = PersonalLocationToLocationID(PPH.C1, CT.SP, 1)
LocationIDs.C2 = PersonalLocationToLocationID(PPH.C2, CT.SP, 1)
LocationIDs.R = PersonalLocationToLocationID(PPH.R, CT.SP, 1)
LocationIDs.W = PersonalLocationToLocationID(PPH.W, CT.SP, 1)

PPH.C1@data$duration = PPH.C1@data$duration * 1.2 # duration correction
PPH.C2@data$duration = PPH.C2@data$duration * 1.2 # duration correction


Leave.R = 8
Leave.W = 17

TimeVertex.C1 = LinkPointsToTime.Commuting(PPH.C1, LocationIDs.C1, 2009, Leave.R) # Time of the Commuting routes vertices Outwards
TimeVertex.C2 = LinkPointsToTime.Commuting(PPH.C2, LocationIDs.C2, 2009, Leave.W) # Time of the Commuting routes vertices Inwards

HOURVertex.C1 = HourOfTheYear2(2009, TIMEVertex.C1, 0)
HOURVertex.C2 = HourOfTheYear2(2009, TIMEVertex.C2, 0)

# ExposureValue = ExtractExposureValue("no2", LocationIDs.C1[[2]][1], HourVertex[[2]][3]) # LocationIDs.C1[[<individual>]][<vertex(route)>]
# ExposureValue                                                                           # HourVertex[[<individual>]][<vertex(route)>]
# 
# # test what is more eddicient: from main script or inside "ExtractExposureValue"
# for (i in seq(1,1)) # seq_along(LocationIDs.C1)
# {
#   for (v in seq_along(LocationIDs.C1[[i]]))
#   {
#     
#     ExposureValue = ExtractExposureValue("no2", LocationIDs.C1[[i]][v], HourVertex[[i]][v])
#     #print(i,v, ExposureValue)
#     #paste("Ind:",(i),"Ver:",(v), "Exp:", ExposureValue)
#     print(ExposureValue)
#   }
# }


YearDates = YearDates(2009)
BusinesDates = BusinesDates(YearDates)

PPH.Phases.Times = TimePhaser(Leave.R, Leave.W, TimeVertex.C1, TimeVertex.C2)
PPH.Phases.DateTimes = PPH.Phases.Times

PHASES = TimePhaserList(BusinesDates, PPH.Phases.DateTimes)
PHASES[[1]][1,1] #[[businesday#]][individual,]

TIME.R = AtResidenceOrWork("Residence", PHASES, BusinesDates, Correct = T)
TIME.W = AtResidenceOrWork("Workplace", PHASES, BusinesDates, Correct = T)
TIME.R[[14]][[200]] #[[individual]][[businesday#]]
TIME.W[[14]][[200]] #[[individual]][[businesday#]]

TIMEVertex.C1 = LinkPointsToTime.Commuting2("Outwards", PPH.C1, LocationIDs.C1, PHASES) # Time of the Commuting routes vertices Outwards
TIMEVertex.C2 = LinkPointsToTime.Commuting2("Inwards", PPH.C2, LocationIDs.C2, PHASES) # Time of the Commuting routes vertices Inwards
TIMEVertex.C1[[14]][[200]] #[[individual]][[businesday#]]
TIMEVertex.C2[[14]][[200]] #[[individual]][[businesday#]]

HOURS.R = HourOfTheYear4(2009, TIME.R, 0)
HOURS.W = HourOfTheYear4(2009, TIME.W, 0)
HOURS.C1 = HourOfTheYear4(2009, TIMEVertex.C1, 0)
HOURS.C2 = HourOfTheYear4(2009, TIMEVertex.C2, 0)
HOURS.R[[14]][[200]] #[[individual]][[businesday#]]
HOURS.W[[14]][[200]] #[[individual]][[businesday#]]
HOURS.C1[[14]][[200]] #[[individual]][[businesday#]]
HOURS.C2[[14]][[200]] #[[individual]][[businesday#]]

rm(CT, CT.SP, PPH.C1, PPH.C1_in, PPH.C2, PPH.C2_in, PPH.R, PPH.R_in ,PPH.W, PPH.W_in, PPH.Phases.DateTimes, PPH.Phases.Times,
   YearDates, BusinesDates, Leave.W, Leave.R, PHASES, TIME.R, TIME.W, TimeVertex.C1, TimeVertex.C2, TIMEVertex.C1, TIMEVertex.C2,
   list.of.packages, new.packages)

ExposureValue.R = ExtractExposureValue1("no2", LocationIDs.R, HOURS.R)
ExposureValue.W = ExtractExposureValue1("no2", LocationIDs.W, HOURS.W)
ExposureValue.R[[1]][[2]] # [[individual#]][[BusinesDay#]]
ExposureValue.W[[3]][[200]] # [[individual#]][[BusinesDay#]]

# R_path = file.path("..", "output", "R.csv")
# R_csv = write.csv(ExposureValue.R, R_path)


ExposureValue.C1 = ExtractExposureValue2("no2", LocationIDs.C1, HOURS.C1)

ExposureValue.C2 = ExtractExposureValue2("no2", LocationIDs.C2, HOURS.C2)





SaveAsFile(CT2, "CT2", "GeoJSON", TRUE)
RESO.BE = CalculateResolution(CT)



## Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages = c("data.table","sp","rgdal","foreign","rgeos","osrm", "futile.options", "lambda.r", "sensorweb4R")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
if(length(new.packages)) devtools::install_github("52North/sensorweb4R") # The sensorweb4R package is not on CRAN yet.


## Load the packages
library(data.table)
library(sp)
library(rgdal)
library(foreign)
library(rgeos)
library(osrm)

library(sensorweb4R)

## Clear the workspace
#rm(list = ls()) 

#getwd()