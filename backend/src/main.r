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

#### Import modules ####

source("modules/input.r")
source("modules/RGBtoSingleBand.r")
source("modules/DetermineRoutes.r")
source("modules/CumulativeExposure.r")
source("modules/ReadIDF5files.r")
source("modules/HourOfTheYear.r")

ndownload.AQNL("https://drive.google.com/file/d/0B5dbtjRcWbwiSU9tOUQ0TUxZR0E") # bug in downloading files from Google Drive
unzip.AQNL("20161108_pm10_no2.zip")

#RGBtoSingleBand("20161108_vandaag_no2_03.tiff")
RGB.list = list.files(file.path("..", "data", "RIVM2"), pattern = ".tiff" )
for (i in RGB.list)
{
  RGBtoSingleBand(i)
}

DetermineRoutesNL(c("Utrecht", "Gelderland"), 100, 1000)

CRAB_Doel = DetermineAddressGoals_FL("Antwerpen")
SaveAsFile(CRAB_Doel, "CRAB_OUT_Antwerpen", "GeoJSON", TRUE)
DetermineRoutesFL(CRAB_Doel, 100, 1000)

#rm(CRAB_Doel)

CumulativeExposure()

CT = CreateConversionTable()

LocationID = PersonalLocationToLocationID()
ActiveH5FLocation = ReadHDF5("no2", 68083) # replace number with LocationID
Hour = HourOfTheYear(2009, as.POSIXct("2009-01-07 23:17:00", tz = "GMT"))
ExposureValue = ExtractExposureValue(ActiveH5FLocation, Hour) # Value of space-time intersection



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