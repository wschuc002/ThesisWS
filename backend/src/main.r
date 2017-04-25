# Main script for determining Primary (residence), Secondary (workplace) and
# Transport routes from BAG data to combine these with air quality data NO2 and
# PM2.5.
# Copyright (C) 2017 William Schuch
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

## TESTED ON WINDOWS 7 (64-bit), 4GB RAM, R v3.3.2, Timezone CET

## TODO:  - ...
##        - ...
##        - Improve SummaryStatistics for profile and phase type comparison.
##        - ?More residential profiles than "Office worker", "Home Office" and "School Pupil".
##        - Simplify "full" OSRM method, based on duration.
##        - ?Introduce "spacetime" package and test is.
##        - Download input data from cloud server (Google Drive or Dropbox).
##        - Documentation

## Note 1: This script only brings different modules in modules/ together.
## No code belonging to one of the modules should go here.

## Note 2: It is expected that the working directory is set to backend/src/


## TERMS:
## PPH: Personal Place History
## CT: Conversion Table, which is used for closest measuring point (Location IDs).

#### Import modules ####

#source("modules/input.r")
source("modules/SaveAsFile.r")
source("modules/DetermineRoutes.r")
source("modules/ConversionTable.r")
source("modules/PersonalLocationToLocationID.r")
source("modules/LinkPointsToTime.r")
source("modules/HourOfTheYear.r")
source("modules/ReadIDF5files.r")
source("modules/TimePhases.r")
source("modules/IncludeWeekends.r")
source("modules/SecondaryRelation.r")
source("modules/DBFreader.r")
source("modules/SummaryStatistics.r")

source("modules/util.r")
source("modules/Interpolate.r")
source("modules/IntersectsBoolean.r")

#source("modules/WeightCR.r")
#source("modules/RGBtoSingleBand.r")
#source("modules/TimeDifferenceCalculation.r")
#source("modules/CumulativeExposure.r")


## Download data from cloud service (Dropbox)

# install.packages('rdrop2')
# library(rdrop2)
# drop_auth()
# 
# Dropbox.dir = file.path("..", "data", "Dropbox")
# if (!dir.exists(Dropbox.dir))
# {
#   dir.create(Dropbox.dir)
# }
# 
# CRAB_Adressenlijst_in = "CRAB_Adressenlijst_DropboxTest.zip"
# 
# if (!file.exists(Dropbox.dir) & !file.exists(shp_in))
# {
#   stop(paste("CRAB addresses not found (.shp)"))
# }
# if (!file.exists(shp_in))
# {
#   unzip(zip_in, exdir= file.path("..", "data", "BE_FL"))
# }
# 
# drop_get("ThesisWS/data/CRAB_Adressenlijst.zip", file.path(Dropbox.dir, CRAB_Adressenlijst))
# 
# https://dl.dropboxusercontent.com/u/56774223/ThesisWS/data/CRAB_Adressenlijst.zip
# 
# ## Google Drive
# 
# https://drive.google.com/open?id=0B5dbtjRcWbwiMFNLTUZRNGhWbWs

#### FLANDERS ####

### General ###
# Use the official address database of Flanders and add the correct attribute 'Goal of use'

Subset.Gemeente = NULL # empty = NULL = no subset = all municipalities |  example subset: c("Gent","Antwerpen")
#Subset.Gemeente = "Antwerpen" # c("Antwerpen", "Gent")

BE_crs = CRS("+init=epsg:31370")

if (is.null(Subset.Gemeente))
{
  Names = paste("")
  CRAB.Name = paste("CRAB_Doel")
} else
{
  Names = paste(Subset.Gemeente, collapse="_")
  CRAB.Name = paste("CRAB_Doel", Names, sep = "_")
}

if (file.exists(file.path("..", "output", paste0(CRAB.Name,".shp"))))
{
  CRAB_Doel = readOGR(file.path("..", "output", paste0(CRAB.Name,".shp")), layer = CRAB.Name) # Bug in .geojson, read .shp
  CRAB_Doel@proj4string = BE_crs
} else
{
  CRAB_Doel = DetermineAddressGoals_FL(Subset.Gemeente,2)
  CRAB_Doel@proj4string = BE_crs
  SaveAsFile(CRAB_Doel, CRAB.Name, "Shapefile", TRUE) #"GeoJSON"
}

# read Residential profile CSV
csv.ResidentialProfiles_in = file.path("..", "data", "ResidentialProfiles.csv")
ResidentialProfiles = fread(csv.ResidentialProfiles_in, sep=",", header=TRUE)

# Select active Residential Profile
Active.Type = "01.OW"
Active.Profile = ResidentialProfiles[ResidentialProfiles$Abbreviation == Active.Type,]

OSRM.Level = "full" # "simplified" or "full" version of vectors in routes (OSRM package)

dir.P = file.path("..", "output", paste(Active.Type, paste0("Primary", Names,".geojson"), sep = "_"))
if (Active.Profile$Dynamics == "dynamic")
{
  dir.T1s = file.path("..", "output", paste(Active.Type, paste0("TransportOutwards", Names,".geojson"), sep = "_"))
  dir.T2s = file.path("..", "output", paste(Active.Type, paste0("_TransportInwards", Names,".geojson"), sep = "_"))
  
  dir.T1f = file.path("..", "output", paste(Active.Type, paste0("TransportOutwards", Names,".geojson"), sep = "_"))
  dir.T2f = file.path("..", "output", paste(Active.Type, paste0("TransportInwards", Names,".geojson"), sep = "_"))
  
  dir.S = file.path("..", "output", paste(Active.Type, paste0("Secondary", Names,".geojson"), sep = "_"))
}

if (OSRM.Level != "full" & OSRM.Level != "simplified")
{
  stop(paste("OSRM.Level should be 'full' or 'simplified'."))
}

# new statistics on Commuting (Office Worker only)
csv.Commuting_in = file.path("..", "data", "BE_FL", "CommutingStats.csv")
Commuting = fread(csv.Commuting_in, sep=",", header=TRUE)

# Read Flanders polygon (improve unzip)
gml.Flanders_in = file.path("..", "data", "BE_FL", "Refgew.gml")
Flanders = readOGR(gml.Flanders_in, layer = "Refgew")
Flanders@proj4string = BE_crs

# Determine PPH for the active profile.
# Check if data already exists. If so, it will not run.
#if (!file.exists(dir.P)&!file.exists(dir.S)&(!file.exists(dir.T1s)|!file.exists(dir.T1f))&(!file.exists(dir.T2s)|!file.exists(dir.T2f)))
DeterminePPH_FL(CRAB_Doel, Names, 250, OSRM.Level, Active.Type, "detailed")


Name = "CT"
if (file.exists(file.path("..", "output", paste0(Name,".shp"))))
{
  CT.SPDF = readOGR(file.path("..", "output", paste0(Name,".shp")), layer = Name) # Bug in .geojson, read .shp
  CT.SPDF@proj4string = BE_crs
} else
{
  data_in = file.path("..", "data", "BE", "ATMOSYS", "atmosys-timeseries_2.data")
  #data_in = file.path("H:", "ATMOSYS", "atmosys-timeseries_2.data")
  
  CT = CreateConversionTable(data_in)
  #CT.SP = MakeCTSpatial(CT)
  CT.SPDF = MakeCTSpatial(CT)
  
  SaveAsFile(CT.SPDF, Name, "Shapefile", TRUE) #"GeoJSON"
}

if (!is.null(Subset.Gemeente))
{
  CT.SPDF = SubsetCTSpatial(CT.SPDF, Subset.Gemeente)
}
SaveAsFile(CT.SPDF, paste(Name, Subset.Gemeente, sep = "_"), "Shapefile", TRUE)


# Set year of pollutant dataset, determine dates and date types (Workdays~Weekends)
year.active = 2015
YearDates = YearDates1(year.active)
BusinesDates = DateType(YearDates,"Workdays")
WeekendDates = DateType(YearDates,"Weekends")
#HolidayDates = DateType(YearDates,"Holidays")

# Read PPH and determine the Location ID corresponding to the pollutant dataset (Spatial ConversionTable = CT.SP)
PPH.P = readOGR(dir.P, layer = 'OGRGeoJSON')
PPH.P@proj4string = BE_crs
LocationIDs.P = PersonalLocationToLocationID(PPH.P, CT.SP, 1)

if (Active.Profile$Dynamics == "dynamic")
{
  PPH.S = readOGR(dir.S, layer = 'OGRGeoJSON')
  
  if (OSRM.Level == "simplified")
  {
    PPH.T1 = readOGR(dir.T1s, layer = 'OGRGeoJSON')
    PPH.T2 = readOGR(dir.T2s, layer = 'OGRGeoJSON')
  }
  if (OSRM.Level == "full")
  {
    PPH.T1 = readOGR(dir.T1f, layer = 'OGRGeoJSON')
    PPH.T2 = readOGR(dir.T2f, layer = 'OGRGeoJSON')
  }
  
  LocationIDs.S = PersonalLocationToLocationID(PPH.S, CT.SP, 1)
  LocationIDs.T1 = PersonalLocationToLocationID(PPH.T1, CT.SP, 1)
  LocationIDs.T2 = PersonalLocationToLocationID(PPH.T2, CT.SP, 1)
  
  PPH.T1@data$duration = PPH.T1@data$duration * 1.2 # duration correction
  PPH.T2@data$duration = PPH.T2@data$duration * 1.2 # duration correction
  
  Leave.P = 8
  Leave.S = 17
  
  TimeVertex.T1 = LinkPointsToTime.Commuting(PPH.T1, LocationIDs.T1, 2009, Leave.P) # Time of the Transport route vertices Outwards
  TimeVertex.T2 = LinkPointsToTime.Commuting(PPH.T2, LocationIDs.T2, 2009, Leave.S) # Time of the Transport route vertices Inwards
  
  PPH.Phases.Times = TimePhaser(PPH.P,Leave.P, Leave.S, TimeVertex.T1, TimeVertex.T2)
  PPH.Phases.DateTimes = PPH.Phases.Times
  
  PHASES = TimePhaserList(BusinesDates, PPH.Phases.DateTimes)
  as.POSIXct(PHASES[[60]][15,2], origin = "1970-01-01", tz = "CET")
  as.POSIXct(PHASES[[70]][15,1], origin = "1970-01-01", tz = "CET")
  as.POSIXct(PHASES[[70]][15,2], origin = "1970-01-01", tz = "CET")
  
  #PHASES[[200]][1,1] #[[businesday#]][individual,]
  
  Correct = T
  if (Correct == T) # Summertime correction correction (CET vs. CEST | The S can be ignored after this correction)
  {
    PHASES = TimePhaserListC(PHASES, Leave.P, PPH.Phases.Times)
  }
  as.POSIXct(PHASES[[60]][15,2], origin = "1970-01-01", tz = "CET")
  as.POSIXct(PHASES[[70]][15,1], origin = "1970-01-01", tz = "CET")
  as.POSIXct(PHASES[[70]][15,2], origin = "1970-01-01", tz = "CET")
  
  TIME.P = AtPrimaryOrSecondary2("Primary", PHASES, BusinesDates, "Workdays")
  TIME.S = AtPrimaryOrSecondary2("Secondary", PHASES, BusinesDates)
  
  TIMEVertex.T1 = LinkPointsToTime.Commuting2("Outwards", PPH.T1, LocationIDs.T1, PHASES) # Time of the Transport routes vertices Outwards
  TIMEVertex.T2 = LinkPointsToTime.Commuting2("Inwards", PPH.T2, LocationIDs.T2, PHASES) # Time of the Transport routes vertices Inwards
  
  # Weekends
  Include.Weekends = TRUE
  if (Include.Weekends == TRUE)
  {
    TIME.P = IncludeWeekends("Primary", TIME.P, YearDates, BusinesDates, WeekendDates)
    TIME.S = IncludeWeekends("Secondary", TIME.S, YearDates, BusinesDates, WeekendDates)
    TIMEVertex.T1 = IncludeWeekends("T1", TIMEVertex.T1, YearDates, BusinesDates, WeekendDates)
    TIMEVertex.T2 = IncludeWeekends("T2", TIMEVertex.T2, YearDates, BusinesDates, WeekendDates)
  }
  
  # Hours of the year
  HOURS.P = HourOfTheYear4(2009, TIME.P, 0)
  HOURS.S = HourOfTheYear4(2009, TIME.S, 0)
  HOURS.T1 = HourOfTheYear4(2009, TIMEVertex.T1, 0)
  HOURS.T2 = HourOfTheYear4(2009, TIMEVertex.T2, 0)
  HOURS.T1_3d = HourOfTheYear4(2009, TIMEVertex.T1, 3)
  HOURS.T2_3d = HourOfTheYear4(2009, TIMEVertex.T2, 3)
}

if (Active.Profile$Dynamics == "static")
{
  #   TIME.P = seq(YearDates[1], tail((YearDates), 1)+1*60**2*24, by = 1*60**2)
  #   length(TIME.P_test)
  #   tail((TIME.P), 2)
  
  Time.P = NULL
  for (d in seq(2, length(YearDates), 1))
  {
    Time.P[[1]] = seq(YearDates[1], YearDates[1]+1*60**2*24, by = 1*60**2)
    Time.P[[d]] = seq(YearDates[d]+1*60**2, YearDates[d]+1*60**2*24, by = 1*60**2)
  }
  
  TIME.P = list()
  for (i in seq_along(PPH.P))
  {
    TIME.P[[i]] = Time.P
  }
  
  Hours.P = HourOfTheYear5(2009, Time.P, 0)
  HOURS.P = HourOfTheYear5(2009, TIME.P, 0)
}

# Write TIME to disk
WriteToDisk = TRUE
if (WriteToDisk == TRUE)
{
  SaveAsDBF(TIME.P, "TIME_P", Active.Type)
  
  if (Active.Profile$Dynamics == "dynamic")
  {
    SaveAsDBF(TIME.S, "TIME_S", Active.Type)
    SaveAsDBF(TIMEVertex.T1, "TIME_T1", Active.Type)
    SaveAsDBF(TIMEVertex.T2, "TIME_T2", Active.Type)
  }
}

#rm(dir.P, dir.S, dir.T1f, dir.T1s, dir.T2f, dir.T2s)

## Read Air Quality data

# Create buffer of AoI (=Location history)
m = 250
AoI_buff1 = gBuffer(merge(PRI,SEC), byid = F, id = NULL, width = m)
AoI_buff2 = gBuffer(merge(CommutingRoutes1_SLDF,CommutingRoutes2_SLDF), byid = F, id = NULL, width = m)
AoI = union(AoI_buff1, AoI_buff2)
AoI = rgeos::gUnaryUnion(AoI, id = NULL)
#AoI.uSP = rgeos::gUnaryUnion(AoI_buff1, AoI_buff2)
#AoI.uSP = maptools::unionSpatialPolygons(AoI)
AoI_SPDF = SpatialPolygonsDataFrame(AoI, data = data.frame(1:(length(AoI.UU))), match.ID = T)
SaveAsFile(AoI_SPDF, paste("AreaOfInterest", paste0(m,"m"), sep = "_"), "GeoJSON", TRUE)


AoI = AreaOfInterest.PPH(PRI, SecondaryPaired, PPH.T1, PPH.T2, 250)
plot(AoI)

AoI.path_in = file.path("..", "output", paste("AreaOfInterest", paste0(m,"m", ".geojson"), sep = "_"))
AoI = readOGR(AoI.path_in, layer = 'OGRGeoJSON')
AoI = rgeos::gUnaryUnion(AoI, id = NULL)
AoI@proj4string = BE_crs

## TXT structure RIO-IFDM
txt.Points_in = file.path("..", "data", "BE", "IRCELINE", "20150101_1_NO2.txt")
Points = fread(txt.Points_in, sep=";", header=TRUE)

## Read from compressed bz2 file
library(R.utils)

hourFile = "20150101_1_NO2.txt"
bz2.Points_in = file.path("..", "data", "BE", "IRCELINE", paste0(hourFile,".bz2"))
txt.Points = file.path("..", "data", "BE", "IRCELINE", hourFile)
if (!file.exists(txt.Points))
{
  bunzip2(bz2.Points_in, txt.Points, remove = FALSE, skip = TRUE)
}
Points = fread(txt.Points, sep=";", header=TRUE)
coordinates(Points) = ~x+y
Points@proj4string = BE_crs


AoI.AQ = AreaOfInterest.AQ(Points)
plot(AoI.AQ)
points(Points)
lines(PPH.T2, col = "red")
lines(Flanders, col = "orange")

# Make subset Area of Interest (AoI) (of Flanders)

# Read Flanders polygon (improve unzip)
gml.Flanders_in = file.path("..", "data", "BE_FL", "Refgew.gml")
Flanders = readOGR(gml.Flanders_in, layer = "Refgew")
Flanders@proj4string = BE_crs

# Create buffer of Flanders
Flanders_buff = gBuffer(Flanders, byid = F, id = NULL, width = 5000)
lines(Flanders_buff, col = "blue")

# Read Belgian polygon (improve unzip)
## Read the input data
BE_zip_in = file.path("..", "data", "BE", "Belgium_shapefile.zip")
scale = 1 # (in km) 1, 10 or 100
BE_shp_name = paste0("be_", scale, "km")
Extentions = c(".shp", ".dbf", ".prj", ".shx")
BE_shp_in = file.path("..", "data", "BE", paste0(BE_shp_name,Extentions[1]))

# Check if input data is available
if (!file.exists(BE_zip_in) & !file.exists(BE_shp_in))
{
  stop(paste("Belgium shapefile does not found."))
}
if (!file.exists(BE_shp_in))
{
  unzip(BE_zip_in, exdir= file.path("..", "data", "BE"), files = paste0(BE_shp_name,Extentions))
}
Belgium = readOGR(BE_shp_in, layer = BE_shp_name)
Belgium = spTransform(Belgium, BE_crs)
Belgium = rgeos::gUnaryUnion(Belgium, id = NULL)

# Create buffer of Belgium
Belgium_buff = gBuffer(Belgium.UU, byid = F, id = NULL, width = -10000)
plot(PRI, col = "green")
points(Points)
lines(PPH.T1, col = "red")
lines(Belgium, col = "blue")
lines(Belgium_buff, col = "pink")

PPH.T1.co = coordinates(PPH.T1)

o = over(PPH.T1.co, Belgium_buff)
inter = gIntersects(PPH.T1[1:20,], Belgium_buff, byid = TRUE)
inter = gIntersection(PPH.T1[1:20,], Belgium_buff, byid = TRUE)

coordinates(PPH.T1[1,]@lines[[1]])

# AoI & Subset area Municipality (AoI2) | use for testing
AoI2 = gIntersection(AoI_SPDF, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",])

AoI2_SPDF = SpatialPolygonsDataFrame(AoI2, data = data.frame(1:(length(AoI2))), match.ID = T)
SaveAsFile(AoI2_SPDF, "AoI2", "GeoJSON", TRUE)
#AoI2 = gIntersection(AoI_SPDF, Flanders_buff)

## Simplify AoI
# s = 25
# AoI2.simp = gSimplify(AoI2, s)
# plot(AoI2.simp, col = "blue")
# AoI2_SPDF.simp = SpatialPolygonsDataFrame(AoI2.simp, data = data.frame(1:(length(AoI2.simp))), match.ID = T)
# SaveAsFile(AoI2_SPDF.simp, paste("AreaOfInterest_simp", paste0("x", s), sep = "_"), "GeoJSON", TRUE)

# Create the Boolean for the base 
Points.TF = IntersectsBoolean(Points, AoI2)
# Create the base: All the RIO-IFDM points inside the Area of Interest
Points.AoI = Points[Points.TF,]
plot(Points.AoI)
SaveAsFile(Points.AoI, "Points_AoI", "GeoJSON", TRUE)


# Create grid for Flanders

# Grid on whole number coordinates 10x10m inside AoI
!!


# data intensive method
sgrid = GridMaker(Flanders_buff, AoI, 100, 10)
#plot(sgrid)

sgrid.TF = IntersectsBoolean(sgrid, AoI)

# Create Boolean and base for the Municipality of Antwerp | use for testing
# Gemeente.RIO_IFDM_TF = gIntersects(Points,
#                                    Municipalities[Municipalities@data$NAAM %in% "Antwerpen",],
#                                    byid = TRUE)
# Gemeente.RIO_IFDM_TF_logi = as.logical(Gemeente.RIO_IFDM_TF)
# Gemeente.RIO_IFDM = Points[Gemeente.RIO_IFDM_TF_logi,]

# AoI2.RIO_IFDM_TF = gIntersects(Gemeente.RIO_IFDM, AoI_SPDF, byid = T)
# AoI2.RIO_IFDM_logi = MatrixToLogical(AoI2.RIO_IFDM_TF)
# 
# MatrixToLogical <-function(Sub, ...)
# {
#   TF = NA
#   for (c in 1:ncol(Sub))
#   {
#     TF[c] = any(Sub[,c]==T)
#   }
#   return(TF)
# }
# 
# AoI2.RIO_IFDM = Gemeente.RIO_IFDM[AoI2.RIO_IFDM_logi,]
# spplot(AoI2.RIO_IFDM, "values")
# 
# SaveAsFile(AoI.RIO_IFDM, "AoI-RIO_IFDM", "GeoJSON", TRUE)
# 
# AoI.path_in = file.path("..", "output", "AoI-RIO_IFDM.geojson")
# AoI.RIO_IFDM = readOGR(AoI.path_in, layer = 'OGRGeoJSON')
# AoI.RIO_IFDM@proj4string = BE_crs


# Read the values and place them in the Points SPDF
hourFile = "20150101_19_NO2.txt"
bz2.Points_in = file.path("..", "data", "BE", "IRCELINE", paste0(hourFile,".bz2"))
txt.Points = file.path("..", "data", "BE", "IRCELINE", hourFile)

for (d in 1:length(hourFile))
{
  if (!file.exists(txt.Points[d]))
  {
    bunzip2(bz2.Points_in[d], txt.Points[d], remove = FALSE, skip = TRUE)
  }
}

Values = fread(txt.Points, sep=";", header=TRUE, select = "values")[Points.TF,]
colnames(Values) = "CON20150101_19_NO2"

Points.AoI@data = cbind(Points.AoI@data, Values)
colnames(Points.AoI@data)

spplot(Points.AoI, "values")
spplot(Points.AoI, "CON20150101_19_NO2")


## Interpolating the points

# Remove duplicates
Points.AoI.Dups = Points.AoI[duplicated(Points.AoI@coords), ]
Points.AoI.NoDup = Points.AoI[!duplicated(Points.AoI@coords), ]
SaveAsFile(Points.AoI.NoDup, paste("Points_AoI_RIO-IFDM", "CON20150101_19_NO2", sep = "_"), "GeoJSON", TRUE)


# Calculate a raster from RIO-IFDM points with the Triangulation method
res = 10
AoI2.Raster = PointsToRasterTIN(SPDF = Points.AoI.NoDup, value = "CON20150101_19_NO2",
                                AoI = AoI,
                                dmax = 20, mpp = res, dup = "error")
plot(AoI2.Raster)
lines(AoI2, col = "red")
points(Points.AoI2.NoDup)
SaveAsFile(AoI2.Raster, paste("Antwerpen", paste0(res,"x",res), "CON20150101_19_NO2", sep = "_"), "GeoTIFF", TRUE)

# Compare with Antwerpen NO2 hour19 raster
raster_in = file.path("..", "output", "Raster_10x10_Antwerpen.tif")
Raster.Antwerpen = raster(raster_in, layer = "Raster_10x10_Antwerpen.tif")
projection(Raster.Antwerpen) = BE_crs
plot(Raster.Antwerpen)

DiffRaster = Raster.Antwerpen - AoI2.Raster
values(DiffRaster)
plot(DiffRaster)
SaveAsFile(DiffRaster, paste("DiffRaster", paste0(res,"x",res), sep = "_"), "GeoTIFF", TRUE)




Gemeente.Raster.Cut = gIntersects(Gemeente.Raster, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",])

SaveAsFile(Gemeente.Raster, paste("Raster", "Antwerpen", sep = "_"), "Shapefile", TRUE)


## Reading the HDF5 structure
pol = "no2"
polFile = paste0(pol, "-gzip.hdf5")
h5f_dir = file.path("..", "data", "BE", "ATMOSYS", polFile)
#h5f_dir = file.path("I:", "ATMOSYS", polFile)

## Where the magic happens
ExposureValue.All = ExtractExposureValue.Integral(h5f_dir, LocationIDs.P, LocationIDs.S, LocationIDs.T1, LocationIDs.T2,
                                                  HOURS.P, HOURS.S, HOURS.T1, HOURS.T2)
ExposureValue.P = ExposureValue.All[[1]]
ExposureValue.S = ExposureValue.All[[2]]
ExposureValue.T1 = ExposureValue.All[[3]]
ExposureValue.T2 = ExposureValue.All[[4]]


ExposureValue.P[[5]][[200]]
ExposureValue.T1[[1]][[204]]
ExposureValue.T2[[100]][[250]]
ExposureValue.S[[100]][[200]]

# # TOEVOEGEN: Koppeling W aan R, zodat lenght(W)=lenght(R) | When there is a many:1 relation
ExposureValue.S = SecondaryRelation(PPH.P, PPH.S, ExposureValue.S)
ExposureValue.S[[80]][[203]]

## num [1:v] NA -> num NA or logi NA for Transport (T1&T2)
ExposureValue.T1 = NAWeekends(ExposureValue.T1, YearDates, BusinesDates, WeekendDates)
ExposureValue.T2 = NAWeekends(ExposureValue.T2, YearDates, BusinesDates, WeekendDates)

# Write Exposurevalues to disk
WriteToDisk = TRUE
if (WriteToDisk == TRUE)
{
  SaveAsDBF(ExposureValue.P, "ExposureValue_P", Active.Type)
  
  if (Active.Profile$Dynamics == "dynamic")
  {
    SaveAsDBF(ExposureValue.S, "ExposureValue_S", Active.Type)
    SaveAsDBF(ExposureValue.T1, "ExposureValue_T1", Active.Type)
    SaveAsDBF(ExposureValue.T2, "ExposureValue_T2", Active.Type)
  }
}




#Read DBF file with TIME 
TIME.P = DBFreader("Time", "Primary", PPH.P, YearDates, Active.Type)
if (Active.Profile$Dynamics == "dynamic")
{
  TIME.S = DBFreader("Time", "Secondary", PPH.P, YearDates, Active.Type)
  TIMEVertex.T1 = DBFreader("Time", "T1", PPH.P, YearDates, Active.Type)
  TIMEVertex.T2 = DBFreader("Time", "T2", PPH.P, YearDates, Active.Type)
}

#Read DBF file with ExposureValues
ExposureValue.P = DBFreader("Exposure", "Primary", PPH.P, YearDates, Active.Type)
if (Active.Profile$Dynamics == "dynamic")
{
  ExposureValue.S = DBFreader("Exposure", "Secondary", PPH.P, YearDates, Active.Type)
  ExposureValue.T1 = DBFreader("Exposure", "T1", PPH.P, YearDates, Active.Type)
  ExposureValue.T2 = DBFreader("Exposure", "T2", PPH.P, YearDates, Active.Type)
}


# Plotting results
Ind = 265
Plot.PersonalExposureGraph(Ind, 70, 6) # (Individual, Start(working)Day, Amount of days)

Plot.Group(Active.Type, 1, 7, 100, TRUE)


# Saving plots on hard rive
Plot_dir = file.path("..", "output", "plots")
if (!dir.exists(Plot_dir)) 
{
  dir.create(Plot_dir)
}

png(filename = file.path(Plot_dir, paste(Active.Type, "ExposureValues", "Individual", paste0(Ind, ".png"), sep = "_")),
    width = 1208, height = 720, units = "px", pointsize = 12)



## Air Quality Health Standards

# read Air Quality Health Standards CSV
csv.HealthStandards_in = file.path("..", "data", "AirQualityHealthStandards.csv")
HealthStandards = fread(csv.HealthStandards_in, sep=",", header=TRUE)


# query
ExposureValue.P[[1]] > HealthStandards$Concentration[2]

EXC.P = ExposureValue.P # use same structure
TIME2.P = TIME.P

# pick random date
rdm.ind = sample(seq_along(TIME.P),1)
rdm.day = sample(seq_along(TIME.P[[rdm.ind]]),1)
rdm.hour = sample(seq_along(TIME.P[[rdm.ind]][[rdm.day]]),1)
rdm.TIME = TIME.P[[rdm.ind]][[rdm.day]][rdm.hour]
rdm.EXP = ExposureValue.P[[rdm.ind]][[rdm.day]][rdm.hour]

DF.EXC = data.frame(rdm.TIME,rdm.EXP)
colnames(DF.EXC) = c("TIME", "EXC")

for (i in seq_along(ExposureValue.P))
  #for (i in seq(1,10))
{
  for (d in seq_along(ExposureValue.P[[i]]))
    #for (d in seq(1,30))
  {
    EXC.P[[i]][[d]] = ExposureValue.P[[i]][[d]][ExposureValue.P[[i]][[d]] > HealthStandards$Concentration[2]]
    TIME2.P[[i]][[d]] = TIME.P[[i]][[d]][ExposureValue.P[[i]][[d]] > HealthStandards$Concentration[2]]
    
    
    #     if (ExposureValue.P[[i]][[d]] > HealthStandards$Concentration[2])
    #     {
    #       #DF.EXC$TIME[r] = TIME.P[[i]][[d]][ExposureValue.P[[i]][[d]][h] > HealthStandards$Concentration[2]]
    #       #DF.EXC$EXC[r] = ExposureValue.P[[i]][[d]][ExposureValue.P[[i]][[d]][h] > HealthStandards$Concentration[2]]
    #       #r = r+1
    #       
    #       print(ExposureValue.P[[i]][[d]])
    #     }
  }
  TIME2.P[[i]][lapply(TIME2.P[[i]],length)>0]
}

for (i in seq_along(ExposureValue.P))
{
  print(paste("Individual:", i, "with", length(which(unlist(ExposureValue.P[[i]]) > HealthStandards$Concentration[2])),
              "exceedances", "CRAB ID:", PPH.P@data$ID[i], "in", PPH.P@data$GEMEENTE[i])) # x times exposed over the year
}

length(which(unlist(ExposureValue.P[[2]]) > HealthStandards$Concentration[2])) # x times exposed over the year

which(unlist(ExposureValue.P[[2]]) > 200)
which(unlist(TIME.P[[2]]) > 200)

TIME2.P[lengths(TIME2.P[[2]]) > 0]

unlist(ExposureValue.P[[1]])
unlist(TIME.P[[1]])

unlist(TIME.P[[2]])[unlist(TEST.P[[2]]) == TRUE]

test = as.POSIXct(unlist(TIME.P[[2]])[unlist(TEST.P[[2]]) == TRUE], origin = "1970-01-01", tz = "CET")




#Remove all, exept...
rm(list=setdiff(ls(), "ExposureValue.All, TIME.P, TIME.S, TIMEVertex.T1, TIMEVertex.T2,
                HOURS.P, HOURS.S, HOURS.T1, HOURS.T2"))




## Summary calculations

# Place weights on Transport vertices
WEIGHTS.T1 = WeightCommutingRouteVertices(HOURS.T1_3d, HOURS.P, Leave.P)
WEIGHTS.T2 = WeightCommutingRouteVertices(HOURS.T2_3d, HOURS.S, Leave.S)

WEIGHTS.T1[[1]]
sum(WEIGHTS.T1[[1]])
tail(HOURS.T1_3d[[1]][[1]], n=1) - HOURS.T1_3d[[1]][[1]][1]



# Mean per day
HO.02.mean. = Weighted.Static(ExposureValue.P, "WeightedMean.Day")

# Mean per year (per individual)
HO.02_1 = mean(HO.02[[1]])
HO.02_2 = mean(HO.02[[2]])
HO.02_281 = mean(HO.02.mean.[[281]])

# Cummulative sum per day
HO.02.CumSum.Day = Weighted.Static(ExposureValue.P, "CumSum.Day")

TIME.unlisted.P = TIME.P
ExposureValueCum.P = ExposureValue.P # use same structure
ExposureValueCumYear.P = NA
ExposureValue.unlisted.P = ExposureValue.P # use same structure


for (i in seq_along(ExposureValue.P)) # per individual
{
  ExposureValueCum.P[[i]] = cumsum(unlist(ExposureValue.P[[i]]))
  TIME.unlisted.P[[i]] = unlist(TIME.P[[i]])
  
  ExposureValueCumYear.P[i] = tail(ExposureValueCum.P[[i]],1)
  
  ExposureValue.unlisted.P[[i]] = unlist(ExposureValue.P[[i]])
}

hist(ExposureValueCumYear.P,100)

Plot.CumExposureGraph(1:300)
Plot.CumExposureGraph(1:length(ExposureValue.P))


# daily average (24h)
for (i in seq_along(ExposureValue.unlisted.P))
{
  
}

# slope method (using cumulative)
Standard.24h = 35
Hours = 24
norm = Standard.24h * Hours

ExposureValueDiff = ExposureValueCum.P # use same structure
ExposureValueDiff2 = ExposureValueDiff

#for (i in seq_along(ExposureValueCum.P))
for (i in seq(1,3))
{
  ExposureValueDiff[[i]] = NA
  ExposureValueDiff2[[i]] = NA
  for (h in seq_along(ExposureValueCum.P[[i]]))
  {
    diff = ExposureValueCum.P[[i]][h+Hours-1]-ExposureValueCum.P[[i]][h]
    ExposureValueDiff[[i]][h] = diff > norm # only first hour of serie
    
    #ExposureValueDiff2[[i]][h:(h+Hours-1)] = ExposureValueDiff[[i]][h] # for complete serie
    
  }
}

for (o in seq(1, length(ExposureValueCum.P[[3]])-(Hours-1)))
{
  ExposureValueDiff2[[3]][o:(o+Hours-1)] = ExposureValueDiff[[3]][o] # for complete serie
}

ExposureValueDiff[[3]][50:100]
ExposureValueDiff2[[3]][50:100]

ExposureValueCum.P[[3]][64]
ExposureValueCum.P[[3]][64+24-1]

ExposureValueCum.P[[3]][64+24-1] - ExposureValueCum.P[[3]][64]

ExposureValueDiff[[3]][65]
ExposureValueDiff2[[3]][65]

ExposureValueDiff[[3]][64:(64+Hours-1)]
ExposureValueDiff2[[3]][64:(64+Hours-1)] = ExposureValueDiff[[3]][64]

ExposureValueDiff2[[3]][64:(64+Hours-1+10)]

Plot.CumExposureGraph(1:10, "monthly")

length(which(ExposureValueDiff[[3]] == T))

NumericWS = 1:10








# Mean for all 100 individuals
ExposureValue.P[[1]][[1]] + ExposureValue.P[[2]][[1]]

HourBasedExposure = ExposureValue.P # use same structure
for (d in seq_along(ExposureValue.P[[1]]))
{
  for (i in seq_along(ExposureValue.P))
  {
    for (h in (seq_along(ExposureValue.P[[i]][[d]])))
    {
      HourBasedExposure[[d]][[h]][i] = ExposureValue.P[[i]][[d]][h]
    }
  }
}


#transpose: [[individual]][[day]][hour] -> [[day]][[individual]][hour]
n <- length(ExposureValue.P[[1]]) # assuming all lists in before have the same length
ExposureValue.P.tr = lapply(1:n, function(i) lapply(ExposureValue.P, "[[", i))

#transpose2: [[day]][[individual]][hour] -> [[day]][[hour]][individual]
ExposureValue.P.tr2 = list(list())
for (d in seq_along(ExposureValue.P.tr))
{
  ExposureValue.P.tr2[[d]] = transpose(ExposureValue.P.tr[[d]])
}

# Mean
# ExposureValue.P100 = data.frame()
# for (d in seq_along(ExposureValue.P.tr2))
# {
#   for (h in seq_along(ExposureValue.P.tr2[[d]]))
#   {
#     ExposureValue.P100[d,h] = mean(ExposureValue.P.tr2[[d]][[h]])
#   }
# }

ExposureValue.P100 = ExposureValue.P.tr2
for (d in seq_along(ExposureValue.P.tr2))
{
  for (h in seq_along(ExposureValue.P.tr2[[d]]))
  {
    ExposureValue.P100[[d]][h] = mean(ExposureValue.P.tr2[[d]][[h]])
    #ExposureValue.P100[[d]] = unlist(ExposureValue.P100[[d]])
  }
  ExposureValue.P100[[d]] = unlist(ExposureValue.P100[[d]])
}

TIME.P100 = TIME.P

#Plotting Summary statistics results
Plot.PersonalExposureGraph.P.summary(1, length(ExposureValue.P100)) # whole year
Plot.PersonalExposureGraph.P.summary(1, 7) # first week


Plot.PersonalExposureGraph.P(76,1,7)


# Mean per day
ExposureValue.P100.DailyMean = NA
for (d in seq_along(ExposureValue.P100))
{
  ExposureValue.P100.DailyMean[d] = mean(ExposureValue.P100[[d]])
}

# Mean over the year
ExposureValue.P100.YearlyMean = mean(ExposureValue.P100.DailyMean)

#! Combine the 4 types in OW.01 and calculate mean

#! Include weekends for OW.01 (and SP.03)

## WOON-WERKVERPLAATSING 46.21 km (http://www.mobiliteitsmanagement.be/ndl/woonwerkverkeer/)


## CHECK the ExposureValues. Should give 8761 values? Are these values in the right time?
head(TIME.P[[1]], 2)
head(HOURS.P[[1]], 2)
tail(HOURS.P[[1]], 2)

unlist(TIME.P[[1]]) %in% as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET") | 
  unlist(TIME.P[[1]]) %in% as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET")

length(unlist(TIME.P[[1]]))
length(unlist(HOURS.P[[1]]))
length(unlist(ExposureValue.P[[1]]))

tail(ExposureValue.P[[1]],1)
head(ExposureValue.P[[1]],2)

tail(ExposureValue.P_01.OW[[1]],1)

H5.active = h5read(h5f_dir, as.character(1))
H5.active$data[HOURS.P[[1]][[365]][25], 7701]
H5.active$data[HOURS.P[[1]][[365]][24], 7701]
H5.active$data[8760, 7701]
DF = data.frame(H5.active)
H5close()

tail(H5.active@data,20)

ExposureValue.P.WM = Weighted.Static(ExposureValue.P, "WeightedMean")
ExposureValue.S.WM = Weighted.Static(ExposureValue.S, "WeightedMean")
ExposureValue.T1.WM = Weighted.Dynamic(ExposureValue.T1, WEIGHTS.T1, "WeightedMean")
ExposureValue.T2.WM = Weighted.Dynamic(ExposureValue.T2, WEIGHTS.T2, "WeightedMean")

for (i in seq_along(ExposureValue.P.WM))
{
  print(paste("Individual", i, ":", mean(ExposureValue.P.WM[[i]]), mean(ExposureValue.S.WM[[i]]), mean(ExposureValue.T1.WM[[i]]), mean(ExposureValue.T2.WM[[i]])))
}

mean(ExposureValue.P.WM[[1]])
mean(ExposureValue.S.WM[[1]])
mean(ExposureValue.T1.WM[[1]])
mean(ExposureValue.T2.WM[[1]])

hist(ExposureValue.P.WM[[99]], breaks = 50)

rm(ExposureValue.T12,ExposureValue.T1.WM, ExposureValue.T2.WM, ExposureValue.P.WM, ExposureValue.S.WM, WEIGHTS.T1, WEIGHTS.T2)


EXP.P.mean = list()
EXP.P.sum = list()
for (i in seq_along(ExposureValue.T1)) # per individual
{
  for (d in seq_along(BusinesDates)) # per day
  {
    #Exp.P.mean = mean(ExposureValue.P[[i]][[d]])
    #Exp.S.mean = mean(ExposureValue.S[[i]][[d]])  
    
    Exp.P.sum[[d]] = sum(ExposureValue.P[[i]][[d]])
    
  }
  
  
  
  #EXP.P.mean[[i]] = Exp.P.mean
  EXP.P.sum[[i]] = Exp.P.sum[[d]]
  
  #   EXP.S[[i]] = 
  #   EXP.C[[i]] = 
  
}
sum(Exp.P.sum)


sum(ExposureValue.T1[[1]][[1]] * WEIGHTS.T1[[1]], na.rm = TRUE)


TEST = TimeDifference(HourOfTheYear4(2009, TIMEVertex.T1, 3))

ExposureValue.T2 = ExtractExposureValue2("no2", LocationIDs.T2, HOURS.T2)



h5f.active_WS = h5read(h5f_dir, as.character(16))
h5f.active_WS$data[HOURS.T1[[5]][[1]][1]+1, 5187]


smoothingSpline = smooth.spline(x=HOURS.T1_3d[[99]][[70]], ExposureValue.T1[[99]][[70]], spar=0.035)
plot(x=HOURS.T1_3d[[99]][[70]], y=ExposureValue.T1[[99]][[70]], ylim=c(0, 100))
lines(smoothingSpline)

plot(x=c(TIMEVertex.T1[[99]][[70]]), y=ExposureValue.T1[[99]][[70]], ylim=c(0, 100))

plot(x=c(TIME.P[[99]][[70]],TIME.S[[99]][[70]],TIMEVertex.T1[[99]][[70]],TIMEVertex.T2[[99]][[70]]),
     y=c(ExposureValue.P[[99]][[70]],ExposureValue.S[[99]][[70]],ExposureValue.T1[[99]][[70]],ExposureValue.T2[[99]][[70]]),
     ylim=c(0, 100))



library(ggplot2)
qplot(HOURS.T1_3d[[2]][[70]],ExposureValue.T1[[2]][[70]], geom='smooth', span =0.5, ylim=c(0, 100))

start.time = Sys.time()

length(HOURS.T1[[15]][[1]])

end.time = Sys.time()
time.taken = end.time - start.time
paste("The script has finished running in", time.taken, "seconds.")

length(ExposureValue.T1[[12]][[1]])


SaveAsFile(CT2, "CT2", "GeoJSON", TRUE)
RESO.BE = CalculateResolution(CT)


#! Use parallel processing

install.packages("rmarkdown")

library(parallel)

mclapply(1:30, rnorm)
# use the same random numbers for all values
set.seed(12345)
mclapply(1:30, rnorm, mc.preschedule=FALSE, mc.set.seed=FALSE)
# something a bit bigger - albeit still useless :P
unlist(mclapply(1:32, function(x) sum(rnorm(1e7))))





#### The Netherlands ####

## Convert RGB images to single band GeoTIFF

#RGBtoSingleBand("20161108_vandaag_no2_03.tiff")
RGB.list = list.files(file.path("..", "data", "RIVM"), pattern = ".tiff" )
for (i in RGB.list)
{
  RGBtoSingleBand(i)
}

## Determine routes
DetermineRoutesNL(c("Utrecht", "Gelderland"), 100, 1000)


# H5close()
# start.time = Sys.time()                    
# ExposureValue.P_ = ExtractExposureValue.Static(h5f_dir, LocationIDs.P, HOURS.P) # LocationIDs.P[1:5]
# end.time = Sys.time()
# time.taken = end.time - start.time
# time.taken # 6.7 min (5,100) # 43.8 min (100) # 47.5 min (100)

# uses a quicker method with hard drive
ExposureValue.P = ExtractExposureValue.Static2(h5f_dir, LocationIDs.P, HOURS.P) # LocationIDs.P[1:5]

start.time = Sys.time()
ExposureValue.S = ExtractExposureValue.Static(h5f_dir, LocationIDs.S, HOURS.S)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken # 11.5 min (5,100)

# Kan sneller wannneer (R,W,) C1 en C2 tegelijk worden berekend:
start.time = Sys.time()
ExposureValue.T12 = ExtractExposureValue.Dynamic3(h5f_dir, LocationIDs.T1, LocationIDs.T2, HOURS.T1, HOURS.T2) # LocationIDs.P[1:5]
end.time = Sys.time()
time.taken = end.time - start.time
time.taken # 25 minutes (15) ,33 minutes (30), 7 hours (100), 1.5 hours (1,100, f), 3.7 hours (5,100,f)

ExposureValue.T1 = ExposureValue.T12[[1]]
ExposureValue.T2 = ExposureValue.T12[[2]]






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