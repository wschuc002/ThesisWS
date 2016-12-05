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

## TODO:  - Make separate modules.
##        - DetermineRoutes for Flanders
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

source("modules/input.r")
source("modules/RGBtoSingleBand.r")
source("modules/DetermineRoutes.r")
source("modules/CumulativeExposure.r")
source("modules/ConversionTable.r")
source("modules/ReadIDF5files.r")
source("modules/HourOfTheYear.r")
source("modules/PersonalLocationToLocationID.r")
source("modules/LinkPointsToTime.r")
source("modules/SaveAsFile.r")

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
LocationIDs.C1 = PersonalLocationToLocationID(PPH.C1, CT.SP, 1)
LocationIDs.C2 = PersonalLocationToLocationID(PPH.C2, CT.SP, 1)
LocationIDs.R = PersonalLocationToLocationID(PPH.R, CT.SP, 1)

PPH.W_in = file.path("..", "output", "Workplace_Antwerpen.geojson")
PPH.W = readOGR(PPH.W_in, layer = 'OGRGeoJSON')

PPH.C1@data$duration = PPH.C1@data$duration * 1.2 # duration correction
PPH.C2@data$duration = PPH.C2@data$duration * 1.2 # duration correction

TimeVertex.POSIXct = LinkPointsToTime(PPH.C1, LocationIDs.C1, 2009) # Time of the Commuting routes vertices Outwards
#TimeVertex.POSIXct = LinkPointsToTime(PPH.R, LocationIDs.R, 2009) # Time of the Commuting routes vertices Outwards
HourVertex = HourOfTheYear2(2009, TimeVertex.POSIXct, 0) # replace date with date PPH

ExposureValue = ExtractExposureValue("no2", LocationIDs.C1[[2]][1], HourVertex[[2]][3]) # LocationIDs.C1[[<individual>]][<vertex(route)>]
ExposureValue                                                                           # HourVertex[[<individual>]][<vertex(route)>]

# test what is more eddicient: from main script or inside "ExtractExposureValue"
for (i in seq(1,3)) # seq_along(LocationIDs.C1)
{
  for (v in seq_along(LocationIDs.C1[[i]]))
  {
    
    ExposureValue = ExtractExposureValue("no2", LocationIDs.C1[[i]][v], HourVertex[[i]][v])
    #print(i,v, ExposureValue)
    #paste("Ind:",(i),"Ver:",(v), "Exp:", ExposureValue)
    print(ExposureValue)
  }
}




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