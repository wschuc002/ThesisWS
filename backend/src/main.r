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

## TESTED ON WINDOWS 7 (64-bit), R v3.3.1, tz UTC+01:00

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

source("modules/TimePhases.r")
source("modules/WeightCR.r")

source("modules/SummaryStatistics.r")

#source("modules/TimeDifferenceCalculation.r")

#source("modules/CumulativeExposure.r")

#library("rgdal")

ndownload.AQNL("https://drive.google.com/file/d/0B5dbtjRcWbwiSU9tOUQ0TUxZR0E") # bug in downloading files from Google Drive
unzip.AQNL("20161108_pm10_no2.zip")

#RGBtoSingleBand("20161108_vandaag_no2_03.tiff")
RGB.list = list.files(file.path("..", "data", "RIVM"), pattern = ".tiff" )
for (i in RGB.list)
{
  RGBtoSingleBand(i)
}

DetermineRoutesNL(c("Utrecht", "Gelderland"), 100, 1000)

## FLANDERS ##

Subset.Gemeente = "" # empty = no subset = all municipalities | c("Gent","Antwerpen")
Names = paste(Subset.Gemeente, collapse="_")
Name = paste("CRAB_Doel", Names, sep = "_")

if (file.exists(file.path("..", "output", paste0("CRAB_Doel_",Names,".shp"))))
{
  CRAB_Doel = readOGR(file.path("..", "output", paste0(Name,".shp")), layer = Name) # Bug in .geojson, read .shp
} else
{
  if (Subset.Gemeente == "")
  {
    CRAB_Doel = DetermineAddressGoals_FL()
  } else
  {
    CRAB_Doel = DetermineAddressGoals_FL(Subset.Gemeente)
  }
  SaveAsFile(CRAB_Doel, Name, "Shapefile", TRUE) #"GeoJSON"
}

dir.R = file.path("..", "output", paste0("Residence_",Names,".geojson"))
dir.W = file.path("..", "output", paste0("Workplace_",Names,".geojson"))
dir.C1s = file.path("..", "output", paste0("CommutingRoutesOutwards_",Names,"_s", ".geojson"))
dir.C1f = file.path("..", "output", paste0("CommutingRoutesOutwards_",Names,"_f", ".geojson"))
dir.C2s = file.path("..", "output", paste0("CommutingRoutesInwards_",Names,"_s", ".geojson"))
dir.C2f = file.path("..", "output", paste0("CommutingRoutesIntwards_",Names,"_f", ".geojson"))
# Check if data already exists
if (!file.exists(dir.R)&!file.exists(dir.W)&(!file.exists(dir.C1s)|!file.exists(dir.C1f))&(!file.exists(dir.C2s)|!file.exists(dir.C2f)))
{
  DeterminePPH_FL(CRAB_Doel, Names, 1000, 5000, "simplified")
}


data_in = file.path("..", "data", "BE", "ATMOSYS", "atmosys-timeseries_2.data")
#data_in = file.path("G:", "ATMOSYS", "atmosys-timeseries_2.data")

#Name = paste("CT", Names, sep = "_")
Name = "CT"

if (file.exists(file.path("..", "output", paste0(Name,".shp"))))
{
  CRAB_Doel = readOGR(file.path("..", "output", paste0(Name,".shp")), layer = Name) # Bug in .geojson, read .shp
} else
{
  CT = CreateConversionTable(data_in)
  CT.SP = MakeCTSpatial(CT)
  SaveAsFile(CT.SP, Name, "Shapefile", TRUE) #"GeoJSON"
}

PPH.R = readOGR(dir.R, layer = 'OGRGeoJSON')
PPH.W = readOGR(dir.W, layer = 'OGRGeoJSON')
PPH.C1 = readOGR(dir.C1s, layer = 'OGRGeoJSON') #PPH.C1f
PPH.C2 = readOGR(dir.C2s, layer = 'OGRGeoJSON') #PPH.C2f

LocationIDs.R = PersonalLocationToLocationID(PPH.R, CT.SP, 1)
LocationIDs.W = PersonalLocationToLocationID(PPH.W, CT.SP, 1)
LocationIDs.C1 = PersonalLocationToLocationID(PPH.C1, CT.SP, 1)
LocationIDs.C2 = PersonalLocationToLocationID(PPH.C2, CT.SP, 1)

PPH.C1@data$duration = PPH.C1@data$duration * 1.2 # duration correction
PPH.C2@data$duration = PPH.C2@data$duration * 1.2 # duration correction

Leave.R = 8
Leave.W = 17

TimeVertex.C1 = LinkPointsToTime.Commuting(PPH.C1, LocationIDs.C1, 2009, Leave.R) # Time of the Commuting routes vertices Outwards
TimeVertex.C2 = LinkPointsToTime.Commuting(PPH.C2, LocationIDs.C2, 2009, Leave.W) # Time of the Commuting routes vertices Inwards

# HOURVertex.C1 = HourOfTheYear2(2009, TIMEVertex.C1, 0)
# HOURVertex.C2 = HourOfTheYear2(2009, TIMEVertex.C2, 0)

YearDates = YearDates1(2009)
BusinesDates = BusinesDates1(YearDates)

PPH.Phases.Times = TimePhaser(Leave.R, Leave.W, TimeVertex.C1, TimeVertex.C2)
PPH.Phases.DateTimes = PPH.Phases.Times

PHASES = TimePhaserList(BusinesDates, PPH.Phases.DateTimes)
as.POSIXct(PHASES[[60]][15,2], origin = "1970-01-01", tz = "CET")
as.POSIXct(PHASES[[70]][15,1], origin = "1970-01-01", tz = "CET")
as.POSIXct(PHASES[[70]][15,2], origin = "1970-01-01", tz = "CET")

PHASES[[200]][1,1] #[[businesday#]][individual,]

Correct = T
if (Correct == T) # Summertime correction correction (CET vs. CEST | The S can be ignored after this correction)
{
  PHASES = TimePhaserListC(PHASES)
}

TIME.R = AtResidenceOrWork2("Residence", PHASES, BusinesDates, Correct = F)
TIME.W = AtResidenceOrWork2("Workplace", PHASES, BusinesDates, Correct = F)
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

HOURS.C1_3d = HourOfTheYear4(2009, TIMEVertex.C1, 3)
HOURS.C2_3d = HourOfTheYear4(2009, TIMEVertex.C2, 3)

rm(CT, CT.SP, PPH.C1, PPH.C2, PPH.R, PPH.R_in ,PPH.W, PPH.W_in, PPH.Phases.DateTimes, PPH.Phases.Times,
   YearDates, BusinesDates, Leave.W, Leave.R, PHASES, TIME.R, TIME.W, TimeVertex.C1, TimeVertex.C2, TIMEVertex.C1, TIMEVertex.C2,
 list.of.packages, new.packages)

pol = "no2"
polFile = paste0(pol, "-gzip.hdf5")
h5f_dir = file.path("..", "data", "BE", "ATMOSYS", polFile)
#h5f_dir = file.path("G:", "ATMOSYS", polFile)            
                    
ExposureValue.R = ExtractExposureValue.Static(h5f_dir, LocationIDs.R, HOURS.R)
ExposureValue.W = ExtractExposureValue.Static(h5f_dir, LocationIDs.W, HOURS.W)
ExposureValue.R[[25]][[2]] # [[individual#]][[BusinesDay#]]
ExposureValue.W[[13]][[200]] # [[individual#]][[BusinesDay#]]

# R_path = file.path("..", "output", "R.csv")
# R_csv = write.csv(ExposureValue.R, R_path)

# start.time = Sys.time()
# ExposureValue.C1 = ExtractExposureValue.Dynamic2(h5f_dir, LocationIDs.C1, HOURS.C1)
# end.time = Sys.time()
# time.taken = end.time - start.time
# time.taken # 40-50 minutes
# 
# ExposureValue.C1[[1]][[1]]
# LocationIDs.C1[[1]][2]
# 
# start.time = Sys.time()
# ExposureValue.C2 = ExtractExposureValue.Dynamic2(h5f_dir, LocationIDs.C2, HOURS.C2)
# end.time = Sys.time()
# time.taken = end.time - start.time
# time.taken # 1 hour # 25.6 mins

# Kan sneller wannneer (R,W,) C1 en C2 tegelijk worden berekend:
start.time = Sys.time()
ExposureValue.C12 = ExtractExposureValue.Dynamic3(h5f_dir, LocationIDs.C1, LocationIDs.C2, HOURS.C1, HOURS.C2)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken # 25 minutes (15) ,33 minutes (30)

ExposureValue.C1 = ExposureValue.C12[[1]]
ExposureValue.C2 = ExposureValue.C12[[2]]

WEIGHTS.C1 = WeightCommutingRouteVertices(HOURS.C1_3d, HOURS.R, Leave.R)
WEIGHTS.C2 = WeightCommutingRouteVertices(HOURS.C2_3d, HOURS.W, Leave.W)

WEIGHTS.C1[[1]]
sum(WEIGHTS.C1[[1]])
tail(HOURS.C1_3d[[1]][[1]], n=1) - HOURS.C1_3d[[1]][[1]][1]

# TOEVOEGEN: Koppeling W aan R, zodat lenght(W)=lenght(R) | When there is a many:1 relation
PPH.R@data$RWlink = PPH.R@data$koppeling
PPH.W@data$RWlink = PPH.W@data$NR
PPH.W@data$WNR = seq_along(PPH.W)

RW = NA
for (k in seq_along(PPH.R))
{
  for (f in seq_along(PPH.W))
  {
    if (PPH.W@data$RWlink[f] == PPH.R@data$RWlink[k])
    {
      RW[k] = PPH.W@data$WNR[f]
    }
  }
}

for (i in seq_along(RW))
{
  ExposureValue.W[[i]] = ExposureValue.W[[RW[i]]]
}

ExposureValue.R.WM = Weighted.Static(ExposureValue.R, "WeightedMean")
ExposureValue.W.WM = Weighted.Static(ExposureValue.W, "WeightedMean")
ExposureValue.C1.WM = Weighted.Dynamic(ExposureValue.C1, WEIGHTS.C1, "WeightedMean")
ExposureValue.C2.WM = Weighted.Dynamic(ExposureValue.C2, WEIGHTS.C2, "WeightedMean")

for (i in seq_along(ExposureValue.R.WM))
{
  print(paste("Individual", i, ":", mean(ExposureValue.R.WM[[i]]), mean(ExposureValue.W.WM[[i]]), mean(ExposureValue.C1.WM[[i]]), mean(ExposureValue.C2.WM[[i]])))
}

mean(ExposureValue.R.WM[[1]])
mean(ExposureValue.W.WM[[1]])
mean(ExposureValue.C1.WM[[1]])
mean(ExposureValue.C2.WM[[1]])

hist(ExposureValue.R.WM[[3]], breaks = 50)




EXP.R.mean = list()
EXP.R.sum = list()
for (i in seq_along(ExposureValue.C1)) # per individual
{
  for (d in seq_along(BusinesDates)) # per day
  {
    #Exp.R.mean = mean(ExposureValue.R[[i]][[d]])
    #Exp.W.mean = mean(ExposureValue.W[[i]][[d]])  
    
    Exp.R.sum[[d]] = sum(ExposureValue.R[[i]][[d]])
    
  }
  
  
  
  #EXP.R.mean[[i]] = Exp.R.mean
  EXP.R.sum[[i]] = Exp.R.sum[[d]]
  
#   EXP.W[[i]] = 
#   EXP.C[[i]] = 
  
}
sum(Exp.R.sum)


sum(ExposureValue.C1[[1]][[1]] * WEIGHTS.C1[[1]], na.rm = TRUE)


TEST = TimeDifference(HourOfTheYear4(2009, TIMEVertex.C1, 3))

ExposureValue.C2 = ExtractExposureValue2("no2", LocationIDs.C2, HOURS.C2)



h5f.active_WS = h5read(h5f_dir, as.character(16))
h5f.active_WS$data[HOURS.C1[[5]][[1]][1]+1, 5187]


smoothingSpline = smooth.spline(x=HOURS.C1_3d[[2]][[70]], ExposureValue.C1[[2]][[70]], spar=0.035)
plot(x=HOURS.C1_3d[[2]][[70]], y=ExposureValue.C1[[2]][[70]], ylim=c(0, 100))
lines(smoothingSpline)




library(ggplot2)
qplot(HOURS.C1_3d[[2]][[70]],ExposureValue.C1[[2]][[70]], geom='smooth', span =0.5, ylim=c(0, 100))

start.time = Sys.time()

length(HOURS.C1[[15]][[1]])

end.time = Sys.time()
time.taken = end.time - start.time
paste("The script has finished running in", time.taken, "seconds.")

length(ExposureValue.C1[[12]][[1]])


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