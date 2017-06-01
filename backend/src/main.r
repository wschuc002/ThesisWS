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

## TESTED ON WINDOWS 7 (64-bit), 4GB RAM, R v3.3.2, Timezone GMT

## TODO:  - ...
##        - ...
##        - Improve SummaryStatistics for profile and phase type comparison.
##        - Speed and route options for cyclists (School Pupil).
##        - ?Introduce "spacetime" package and test is.
##        - Documentation

## Clear the workspace?
Clear.WorkSpace = FALSE
if (Clear.WorkSpace) {rm(list = ls())}

## Note 1: This script only brings different modules in modules/ together.
## No code belonging to one of the modules should go here.

## Note 2: It is expected that the working directory is set to "../ThesisWS/backend/src"
## Check with: getwd() | example how to change: setwd("C:/git/ThesisWS/backend/src")

## Note 3a: To download the input data from OneDrive you need a KEY file (KEY_InputData.csv),
## which should be placed in backend/data/.
## Note 3b: To download the input data from the irceline ftp server, you need a password.
## This password should we filled between the double quotes.
ftp.pwd = "<passwordhere>" # fill in the password, without < >

## ABBREVIATIONS:
# PPH: Personal Place History
# P, S, T1, T2: Primary, Secondary, Transport Outwards, Transport Inwards
# dir: directory
# crs: coordinate reference system
# CRAB: Centraal Referentieadressenbestand

## WARNING: Every modules checks if the required R-libraries are installed.
## If not, they will be installed automatically.

#### Import modules ####
source("modules/DownloadInputFilesFromOneDrive.r")
source("modules/DownloadInputFilesFromIrcelineFTP.r")
source("modules/SaveAsFile.r")
source("modules/DetermineRoutes.r")
source("modules/LinkPointsToTime.r")
source("modules/HourOfTheYear.r")
source("modules/Interpolate.r")
source("modules/AreaOfInterest.r")
source("modules/ExtractBZ2.r")
source("modules/SimplifyRoutes.r")
source("modules/CreateCorrespondingDateAndTime.r")
source("modules/DataFraming.r")
source("modules/SummaryStatistics.r")
source("modules/util.r")

#source("modules/IntersectsBoolean.r")
#source("modules/ConversionTable.r")
#source("modules/PersonalLocationToLocationID.r")
#source("modules/ReadIDF5files.r")
#source("modules/TimePhases.r")
#source("modules/IncludeWeekends.r")
#source("modules/SecondaryRelation.r")
#source("modules/DBFreader.r")
#source("modules/WeightCR.r")
#source("modules/RGBtoSingleBand.r")
#source("modules/TimeDifferenceCalculation.r")
#source("modules/CumulativeExposure.r")

### Download data from OneDrive or irceline ftp server
DownloadMode = "OneDrive" # "FPT"

# create 'data' and 'data/BE' folder in case it does not exist
data.dir = file.path("..", "data")
if (!dir.exists(data.dir)) { dir.create(data.dir) }
BE.dir = file.path("..", "data", "BE")
if (!dir.exists(BE.dir)) { dir.create(BE.dir) }

## ONE DRIVE

# Read OneDrive KEY
if (DownloadMode == "OneDrive")
{
  key.InputData.dir = file.path("..", "data", "KEY_InputData.csv") # This file is required to download input files.
  if (file.exists(key.InputData.dir))
  {
    KEY.InputData = fread(key.InputData.dir, sep=";", header=TRUE)
    Filenames = KEY.InputData$Filename
    
    DownloadInputFilesFromOneDrive(Filenames, KEY.InputData)
  } else
  {
    stop(paste("KEY_InputData.csv missing. Please contact William Schuch (william.schuch@wur.nl) for file request."))
  }
}

## irceline ftp server (as alternative for OneDrive) | not foolproof yet
year.active = 2015
pol = "no2" # "pm25"

if (DownloadMode == "FTP")
{
  out.dir = file.path("..", "data", "IRCELINE_test")
  ftp.filenames = c("20150101_1_NO2.txt.bz2", "20150101_2_NO2.txt.bz2")
  DownloadInputFilesFromIrcelineFTP(ftp.filenames, ftp.pwd, out.dir)
}

#### FLANDERS ####

### General ###
# Use the official address database of Flanders and add the correct attribute 'Goal of use'

Subset.Gemeente = NULL # empty = NULL = no subset = all municipalities |  example subset: c("Gent","Antwerpen")

BE.epsg = 31370
BE_crs = CRS(paste0("+init=epsg:", BE.epsg))

if (is.null(Subset.Gemeente))
{
  Names = paste("")
  CRAB.Name = paste("CRAB_Doel")
} else
{
  Names = paste(Subset.Gemeente, collapse="_")
  CRAB.Name = paste("CRAB_Doel", Names, sep = "_")
}

# read Residential profile CSV
csv.ResidentialProfiles_in = file.path("..", "data", "ResidentialProfiles.csv")
ResidentialProfiles = fread(csv.ResidentialProfiles_in, sep=",", header = TRUE)

for (Active.Type in unique(ResidentialProfiles$Type))
{
  print(Active.Type)
}

# Select active Residential Profile
Active.Type = "01.OW"
Active.Subtype = "01.OW_WS1"

Active.Profile = ResidentialProfiles[ResidentialProfiles$Type == Active.Type,]
Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]

OSRM.Level = "full" # "simplified" or "full" version of vectors in routes (OSRM package)

dir.P = file.path("..", "output", paste(Active.Type, paste0("Primary", Names,".geojson"), sep = "_"))

if (Active.Subprofile$Dynamics == "dynamic")
{
  dir.S = file.path("..", "output", paste(Active.Type, paste0("Secondary", Names,".geojson"), sep = "_"))
  
  if (is.null(Subset.Gemeente))
  {
    dir.T1 = file.path("..", "output", paste(Active.Type, paste0("TransportOutwards", Names, "_", substr(OSRM.Level, 1, 1), ".geojson"), sep = "_"))
    dir.T2 = file.path("..", "output", paste(Active.Type, paste0("TransportInwards", Names, "_", substr(OSRM.Level, 1, 1), ".geojson"), sep = "_"))
  } else
  {
    dir.T1 = file.path("..", "output", paste0(Active.Type,"_TransportOutwards_", Names, "_", substr(OSRM.Level, 1, 1)))
    dir.T2 = file.path("..", "output", paste0(Active.Type,"_TransportInwards_", Names, "_", substr(OSRM.Level, 1, 1)))
  }
}

# create 'output' folder in case it does not exist
output.dir = file.path("..", "output")
if (!dir.exists(output.dir)) { dir.create(output.dir) }

if (!exists("CRAB_Doel") & !file.exists(dir.P))
{
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
}

# if (OSRM.Level != "full" & OSRM.Level != "simplified")
# {
#   stop(paste("OSRM.Level should be 'full' or 'simplified'."))
# }

# new statistics on Commuting (Office Worker only)
csv.Commuting_in = file.path("..", "data", "BE_FL", "CommutingStats.csv")
Commuting = fread(csv.Commuting_in, sep=",", header=TRUE)

# Read Flanders polygon (improve unzip)
gml.Flanders_in = file.path("..", "data", "BE_FL", "Refgew.gml")
Flanders = readOGR(gml.Flanders_in, layer = "Refgew")
Flanders@proj4string = BE_crs

## Determine PPH for the active profile.
# Check if data already exists. If so, it will not run.
if (!file.exists(dir.P) & !file.exists(dir.S))
{
  DeterminePPH_FL(CRAB_Doel, Names, 25, OSRM.Level, Active.Type, Plot = TRUE, SaveResults = TRUE)
}

# Remove the data in the environment that will not be used from this point.
rm(CRAB_Doel, KEY.InputData, DownloadMode, keyInputData.dir, Filenames)

if (!exists("PPH.P") & file.exists(dir.P))
{
  PPH.P = readOGR(dir.P, layer = 'OGRGeoJSON')
  PPH.P@proj4string = BE_crs
}

if (!exists("PPH.S") & file.exists(dir.S))
{
  PPH.S = readOGR(dir.S, layer = 'OGRGeoJSON') # Error: FIDs not unique
  PPH.S@proj4string = BE_crs
}

if (!exists("PPH.T1") & file.exists(dir.T1))
{
  PPH.T1 = readOGR(dir.T1, layer = 'OGRGeoJSON')
  PPH.T1@proj4string = BE_crs
}

if (!exists("PPH.T2") & file.exists(dir.T2))
{
  PPH.T2 = readOGR(dir.T2, layer = 'OGRGeoJSON')
  PPH.T2@proj4string = BE_crs
}

# Set year of pollutant dataset, determine dates and date types (Workdays~Weekends)
# Sys.setenv(TZ="Europe/Brussels")
# BE_tz = "Europe/Brussels"
OriginalTimezone = Sys.timezone(location = TRUE) # All system settings should be changed to its original state at the end of the code
Sys.setenv(TZ = "GMT")
YearDates = YearDates2(year.active)

if (Active.Type == "01.OW")
{
  # Official holydays (for all profiles)
  csv.OfficialHolidays_in = file.path("..", "data", "BE", "OfficialHolidays.csv")
  OfficialHolidays = fread(csv.OfficialHolidays_in, sep = ";", header=TRUE)
  HoliDates = as.POSIXct(OfficialHolidays$Datum)
}
if (Active.Type == "03.SP")
{
  # School holidays (School Pupils only)
  csv.SchoolHolidays_in = file.path("..", "data", "BE_FL", "SchoolHolidays.csv")
  SchoolHolidays = fread(csv.SchoolHolidays_in, sep = ";", header=TRUE)
  HoliDates = HolidayGenerator(SchoolHolidays) #2015 only
}

if (Active.Subprofile$Dynamics == "dynamic")
{
  BusinesDates = DateType(YearDates, "Workdays", HoliDates)
  WeekendDates = DateType(YearDates, "Weekends")
  
  
  # Convert Transport to points for equal durations
  PPH.T1.Pnt.eq.Li = SimplifyRoutes(PPH.T1, TRUE, Factor = 100) # Factor should be desirable resulting amount
  PPH.T2.Pnt.eq.Li = SimplifyRoutes(PPH.T2, FALSE, Factor = 100)
  
  Simplify = TRUE
  
  if (Simplify)
  {
    PPH.T1.Simp = gSimplify(PPH.T1, 75, topologyPreserve = TRUE)
    PPH.T2.Simp = gSimplify(PPH.T2, 75, topologyPreserve = TRUE)
    
    PPH.T1.Simp.Pnt.Li = list()
    PPH.T2.Simp.Pnt.Li = list()
  }
  PPH.T1.Pnt.Li = list()
  PPH.T2.Pnt.Li = list()
  
  for (i in seq_along(PPH.T1))
  {
    if (Simplify)
    {
      PPH.T1.Simp.Pnt.Li[[i]] = as(PPH.T1.Simp[i,], "SpatialPoints")
      PPH.T2.Simp.Pnt.Li[[i]] = as(PPH.T2.Simp[i,], "SpatialPoints")
      
      points(PPH.T1.Simp.Pnt.Li[[i]], col = "blue")
      points(PPH.T2.Simp.Pnt.Li[[i]], col = "blue")
    }
    PPH.T1.Pnt.Li[[i]] = as(PPH.T1[i,], "SpatialPoints")
    PPH.T2.Pnt.Li[[i]] = as(PPH.T2[i,], "SpatialPoints")
    
    points(PPH.T1.Pnt.Li[[i]])
    points(PPH.T2.Pnt.Li[[i]])
  }
  
  PPH.T1.PNT.RS = RandomSampleRoutesYears(PPH.T1.Pnt.eq.Li, TRUE, 25, YearDates, BusinesDates)
  PPH.T2.PNT.RS = RandomSampleRoutesYears(PPH.T2.Pnt.eq.Li, FALSE, 25, YearDates, BusinesDates)

  for (i in seq_along(PPH.P))
  {
    print(length(PPH.T1.PNT.RS[[i]][[6]]))
    print(length(PPH.T2.PNT.RS[[i]][[6]]))
  }
  
  # some test plots
  i = 5
  day = 2
  plot(PPH.T1[i,])
  points(PPH.T1.Pnt.eq.Li[[i]], col = "blue")
  points(PPH.T1.PNT.RS[[i]][[day]], col = "red")
  
  # Basic time element per vertex
  TimeVertex.T1 = LinkPointsToTime.Transport("Outwards", PPH.T1, PPH.T1.Pnt.Li, year.active, Active.Subprofile)
  TimeVertex.T2 = LinkPointsToTime.Transport("Inwards", PPH.T2, PPH.T2.Pnt.Li, year.active, Active.Subprofile)
}

TIME = CreateCorrespondingDateAndTime(Active.Type, Active.Subprofile, PPH.P, YearDates, BusinesDates, WeekendDates, HoliDates,
                                      TimeVertex.T1, TimeVertex.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS)
PHASES = TIME[[1]]
TIME.P = TIME[[2]]
if (Active.Subprofile$Dynamics == "dynamic")
{
  TIME.S = TIME[[3]]
  TIME.T1 = TIME[[4]]
  TIME.T2 = TIME[[5]]
}
rm(TIME)

DurationCorrection = FALSE
if (DurationCorrection)
{
  PPH.T1@data$duration = PPH.T1@data$duration * 1.2 # duration correction
  PPH.T2@data$duration = PPH.T2@data$duration * 1.2 # duration correction
}

# Hours of the year
HOURS.P = HourOfTheYear7(year.active, TIME.P, 0)
if (Active.Subprofile$Dynamics == "dynamic")  #Active.Profile$Dynamics == "dynamic"
{
  HOURS.S = HourOfTheYear7(year.active, TIME.S, 0)
  HOURS.T1 = HourOfTheYear7(year.active, TIME.T1, 0)
  HOURS.T2 = HourOfTheYear7(year.active, TIME.T2, 0)
  
  HOURS.T1_3d = HourOfTheYear7(year.active, TIME.T1, 3)
  HOURS.T2_3d = HourOfTheYear7(year.active, TIME.T2, 3)
}

# Write TIME to disk
WriteToDisk = FALSE
if (WriteToDisk)
{
  SaveAsDBF(TIME.P, "TIME_P", Active.Type)
  
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    SaveAsDBF(TIME.S, "TIME_S", Active.Type)
    SaveAsDBF(TIME.T1, "TIME_T1", Active.Type)
    SaveAsDBF(TIME.T2, "TIME_T2", Active.Type)
  }
}

#rm(dir.P, dir.S, dir.T1f, dir.T1s, dir.T2f, dir.T2s)

#Read DBF file with TIME 
TIME.P = DBFreader("Time", "Primary", PPH.P, YearDates, Active.Type)
if (Active.Subprofile$Dynamics == "dynamic")
{
  TIME.S = DBFreader("Time", "Secondary", PPH.P, YearDates, Active.Type)
  TIME.T1 = DBFreader("Time", "T1", PPH.P, YearDates, Active.Type)
  TIME.T2 = DBFreader("Time", "T2", PPH.P, YearDates, Active.Type)
}

## Read Air Quality data



## TXT structure RIO-IFDM

# # (encrypted) ZIP
# BaseFile = paste0(year.active, "0101_", toupper(pol))
# zip.Points_in = file.path("..", "data", "BE", "IRCELINE", paste0(BaseFile,".zip"))
# 
# Pol.dir_in = file.path("..", "data", "BE", "IRCELINE", "test")
# Pol.zip = file.path("..", "data", "BE", "IRCELINE", "test", BaseFile)
# 
# PassWord = "WilliamS"
# 
# zipPsw <- function(dir, fn = tempfile(fileext = ".zip"), psw, addFlags="")
# {
#   stopifnot(Sys.which("zip")!="")
#   
#   zip(zipfile = fn, files = path.expand(dir), flags = paste0("-r --password ", psw, " ", addFlags))
#   
#   return(fn)
# }
# 
# zipPsw(Pol.dir_in, fn = Pol.zip , psw = PassWord, addFlags = "-j")
# 
# zip(zipfile = fn, files = path.expand(dir), flags = paste0("-r --password ", psw, " ", addFlags))



# Read the values and place them in the Points SPDF
ExternalDrive = TRUE
if (ExternalDrive)
{
  DriveLetter = "T"
  PolDir = file.path(paste0(DriveLetter, ":"), "RIO-IFDM", toupper(pol))
} else 
{
  PolDir = file.path("..", "data", "BE", "IRCELINE")
}

txt.Points = ExtractBZ2(pol, PolDir, 1, 50)

# Read the base | # Read from compressed bz2 file
BaseFile = paste0(year.active, "0101_1_", toupper(pol), ".txt")
#bz2.Points_in = file.path("..", "data", "BE", "IRCELINE", paste0(BaseFile,".bz2"))
#txt.Points = file.path("..", "data", "BE", "IRCELINE", BaseFile)
txt.Points = file.path(PolDir, BaseFile)

if (!file.exists(txt.Points))
{
  bunzip2(bz2.Points_in, txt.Points, remove = FALSE, skip = TRUE)
}
Points = fread(txt.Points, sep=";", header=TRUE)
coordinates(Points) = ~x+y
Points@proj4string = BE_crs

Points.NoVal = Points
colnames(Points.NoVal@data) = NA
Points.NoVal@data[,1] = NA
rm(Points)

# for (p in 1:length(txt.Points))
# {
#   Values = fread(txt.Points[p], sep=";", header=TRUE, select = "values") #[Points.T,]
#   
#   Points.AoI_test@data = cbind(Points.AoI_test@data, Values)
# }
# ColNames = paste0("CON_", regmatches(txt.Points, regexpr(paste0("[0-9]*_[0-9]*_", toupper(pol)), txt.Points)))
# Points.AoI_test@data[,1] = NULL
# colnames(Points.AoI_test@data) = ColNames
# 
# spplot(Points.AoI_test, colnames(Points.AoI_test@data)[25])

# # Remove duplicates
# #Points.AoI.Dups = Points.AoI[duplicated(Points.AoI@coords), ]
# Points.AoI.NoDup = Points.AoI[!duplicated(Points.AoI@coords), ]
# #SaveAsFile(Points.AoI.NoDup, paste("Points_AoI_RIO-IFDM", "CON_20150101_19_NO2", sep = "_"), "GeoJSON", TRUE)

## Interpolating the points

start.time = Sys.time()
ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P, PPH.S, PPH.T1.PNT.RS, PPH.T2.PNT.RS, Points.NoVal, PolDir, Plot = FALSE,
                                            pol, StartHour = 1, EndHour = 21*24,
                                            HOURS.P, HOURS.S, HOURS.T1, HOURS.T2, 50)
ExposureValue.P = ExposureValue.All[[1]]
if (Active.Subprofile$Dynamics == "dynamic")
{
  ExposureValue.S = ExposureValue.All[[2]]
  ExposureValue.T1 = ExposureValue.All[[3]]
  ExposureValue.T2 = ExposureValue.All[[4]]
}
end.time = Sys.time()
print(end.time - start.time)

rm(ExposureValue.All)

head(ExposureValue.P[[1]])
head(ExposureValue.S[[1]])
head(ExposureValue.T1[[1]])
head(ExposureValue.T2[[1]])

rm(Primary, Primary_random, Secondary, src1, src2, dst1, dst2)

## Data Frame structure and stats

ST.DF.P = DF.Structure2(TIME.P, TIME.P, ExposureValue.P)
ST.DF.S = DF.Structure2(TIME.P, TIME.S, ExposureValue.S)
ST.DF.T1 = DF.Structure2(TIME.P, TIME.T1, ExposureValue.T1)
ST.DF.T2 = DF.Structure2(TIME.P, TIME.T2, ExposureValue.T2)

stats.EXP.P = DF.Stats(ST.DF.P)
stats.EXP.S = DF.Stats(ST.DF.S)
stats.EXP.T1 = DF.Stats(ST.DF.T1)
stats.EXP.T2 = DF.Stats(ST.DF.T2)


## Temporal aggregation

Time = seq(as.POSIXct(paste0(year.active,"-01-01 00:00:00")),
           (as.POSIXct(paste0(year.active+1,"-01-01 00:00:00")) - 1*60**2), 1*60**2) + 1*60**2

TIME.HR = list()
ExposureValueCombined = list()
for (i in seq_along(PPH.P))
{
  TIME.HR[[i]] = Time
  
  ExposureValueCombined[[i]] = 1:(length(Time))
  ExposureValueCombined[[i]][ExposureValueCombined[[i]] > 0] = NA
}

#for (h in 1:(24*length(YearDates)))
for (h in 1:(21*24))
{
  print(paste0("Series Hour ", h))
  day = ceiling(h/24)
  
  wP = list()
  wS = list()
  wT1 = list()
  wT2 = list()
  
  for (i in seq_along(PPH.P))
  {
    wP[[i]] = which(h == HOURS.P[[i]][[day]])
    wS[[i]] = which(h == HOURS.S[[i]][[day]])
    wT1[[i]] = which(h == HOURS.T1[[i]][[day]])
    wT2[[i]] = which(h == HOURS.T2[[i]][[day]])
    
    if (length(wP[[i]]) > 0 & !length(wT1[[i]]) > 0 & !length(wT2[[i]]) > 0) # Only P, no hour overlap
    {
      ExposureValueCombined[[i]][h] = ExposureValue.P[[i]][[day]][wP[[i]]]
    }
    
    if (length(wS[[i]]) > 0 & !length(wT1[[i]]) > 0 & !length(wT2[[i]]) > 0) # Only S, no hour overlap
    {
      ExposureValueCombined[[i]][h] = ExposureValue.S[[i]][[day]][wS[[i]]]
    }
    
    if (length(wT1[[i]]) > 0 & length(wS[[i]]) == 1) # Hour overlap T1 & S
    {
      Mean.T1 = mean(ExposureValue.T1[[i]][[day]][wT1[[i]]])
      Weight.T1 = as.numeric(tail(TIME.T1[[i]][[day]][wT1[[i]]],1) - 
                               floor_date(tail(TIME.T1[[i]][[day]][wT1[[i]]],1), 'hours')) / 60
      
      Weight.S = as.numeric(TIME.S[[i]][[day]][1] - tail(TIME.T1[[i]][[day]][wT1[[i]]],1)) / 60
      #Weight.T1 + Weight.S # should be 1
      
      ExposureValueCombined[[i]][h] = Mean.T1 * Weight.T1 + ExposureValue.S[[i]][[day]][wS[[i]]] * Weight.S
    }
    
    if (length(wT1[[i]]) > 0 & length(wS[[i]]) == 0) # Only T1, no hour overlap
    {
      Mean.T1 = mean(ExposureValue.T1[[i]][[day]][wT1[[i]]])
      
      ExposureValueCombined[[i]][h] = Mean.T1
    }
    
    
    if (length(wT2[[i]]) > 0 & length(wP[[i]]) == 1) # Hour overlap T2 & P
    {
      Mean.T2 = mean(ExposureValue.T2[[i]][[day]][wT2[[i]]])
      Weight.T2 = as.numeric(tail(TIME.T2[[i]][[day]][wT2[[i]]],1) -
                               floor_date(tail(TIME.T2[[i]][[day]][wT2[[i]]],1), 'hours')) / 60
      
      Weight.P = as.numeric(TIME.P[[i]][[day]][wP[[i]]] - tail(TIME.T2[[i]][[day]][wT2[[i]]],1)) / 60
      #Weight.T2 + Weight.P # should be 1
      
      ExposureValueCombined[[i]][h] = Mean.T2 * Weight.T2 + ExposureValue.P[[i]][[day]][wP[[i]]] * Weight.P
    }
    
    if (length(wT2[[i]]) > 0 & length(wP[[i]]) == 0) # Only T2, no hour overlap
    {
      Mean.T2 = mean(ExposureValue.T2[[i]][[day]][wT2[[i]]])
      
      ExposureValueCombined[[i]][h] = Mean.T2
    }
    
    #ExposureValueCombined[[i]]
  } # closing i
  
} # closing h

for (i in seq_along(PPH.P))
{
  print(head(ExposureValueCombined[[i]], 21*24))
}

ST.DF.HR = DF.Structure2(TIME.P, Time, ExposureValueCombined)
stats.EXP.HR = DF.Stats(ST.DF.HR)



wP = list()
wS = list()
wT1 = list()
wT2 = list()

for (i in seq_along(PPH.P))
#for (i in 1:2)  
{
  P = list()
  S = list()
  T1 = list()
  T2 = list()
  for (h in seq_along(Time))
  #for (h in 1:(7*24))
  {
    day = ceiling(h/24)
    print(paste0("Year Hour ", h))
    print(paste0("Day ", day))
    
    P[[h]] = which(h == HOURS.P[[i]][[day]])
    S[[h]] = which(h == HOURS.S[[i]][[day]])
    T1[[h]] = which(h == HOURS.T1[[i]][[day]])
    T2[[h]] = which(h == HOURS.T2[[i]][[day]])
  }
  wP[[i]] = P
  wS[[i]] = S
  wT1[[i]] = T1
  wT2[[i]] = T2
}
wP[[1]]
wT1[[2]][[33]]
HOURS.T1[[1]][[2]]



# Plotting results
Ind = 25
Plot.PersonalExposureGraph(Ind, 2, 5) # (Individual, Start(working)Day, Amount of days)

Plot.Group2(Active.Type, 1, 7, 25, TRUE)




# Calculate a raster from RIO-IFDM points with the Triangulation method
res = 100
AoI2.Raster = PointsToRasterTIN(SPDF = Points.AoI.NoDup, value = "CON20150101_19_NO2",
                                AoI = AoI2,
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



## Undo system settings changes
Sys.setenv(TZ = OriginalTimezone)




# 
# 
# # Mean for all 100 individuals
# ExposureValue.P[[1]][[1]] + ExposureValue.P[[2]][[1]]
# 
# HourBasedExposure = ExposureValue.P # use same structure
# for (d in seq_along(ExposureValue.P[[1]]))
# {
#   for (i in seq_along(ExposureValue.P))
#   {
#     for (h in (seq_along(ExposureValue.P[[i]][[d]])))
#     {
#       HourBasedExposure[[d]][[h]][i] = ExposureValue.P[[i]][[d]][h]
#     }
#   }
# }
# 
# 
# #transpose: [[individual]][[day]][hour] -> [[day]][[individual]][hour]
# n <- length(ExposureValue.P[[1]]) # assuming all lists in before have the same length
# ExposureValue.P.tr = lapply(1:n, function(i) lapply(ExposureValue.P, "[[", i))
# 
# #transpose2: [[day]][[individual]][hour] -> [[day]][[hour]][individual]
# ExposureValue.P.tr2 = list(list())
# for (d in seq_along(ExposureValue.P.tr))
# {
#   ExposureValue.P.tr2[[d]] = transpose(ExposureValue.P.tr[[d]])
# }
# 
# # Mean
# # ExposureValue.P100 = data.frame()
# # for (d in seq_along(ExposureValue.P.tr2))
# # {
# #   for (h in seq_along(ExposureValue.P.tr2[[d]]))
# #   {
# #     ExposureValue.P100[d,h] = mean(ExposureValue.P.tr2[[d]][[h]])
# #   }
# # }
# 
# ExposureValue.P100 = ExposureValue.P.tr2
# for (d in seq_along(ExposureValue.P.tr2))
# {
#   for (h in seq_along(ExposureValue.P.tr2[[d]]))
#   {
#     ExposureValue.P100[[d]][h] = mean(ExposureValue.P.tr2[[d]][[h]])
#     #ExposureValue.P100[[d]] = unlist(ExposureValue.P100[[d]])
#   }
#   ExposureValue.P100[[d]] = unlist(ExposureValue.P100[[d]])
# }
# 
# TIME.P100 = TIME.P
# 
# #Plotting Summary statistics results
# Plot.PersonalExposureGraph.P.summary(1, length(ExposureValue.P100)) # whole year
# Plot.PersonalExposureGraph.P.summary(1, 7) # first week
# 
# 
# Plot.PersonalExposureGraph.P(76,1,7)
# 
# 
# # Mean per day
# ExposureValue.P100.DailyMean = NA
# for (d in seq_along(ExposureValue.P100))
# {
#   ExposureValue.P100.DailyMean[d] = mean(ExposureValue.P100[[d]])
# }
# 
# # Mean over the year
# ExposureValue.P100.YearlyMean = mean(ExposureValue.P100.DailyMean)
# 
# #! Combine the 4 types in OW.01 and calculate mean
# 
# #! Include weekends for OW.01 (and SP.03)
# 
# ## WOON-WERKVERPLAATSING 46.21 km (http://www.mobiliteitsmanagement.be/ndl/woonwerkverkeer/)
# 
# 
# 
# ## OLD CODE | Placed 2017-05-04
# 
# 
# # # Conversion Table for HDF5 structure
# # Name = "CT"
# # if (file.exists(file.path("..", "output", paste0(Name,".shp"))))
# # {
# #   CT.SPDF = readOGR(file.path("..", "output", paste0(Name,".shp")), layer = Name) # Bug in .geojson, read .shp
# #   CT.SPDF@proj4string = BE_crs
# # } else
# # {
# #   data_in = file.path("..", "data", "BE", "ATMOSYS", "atmosys-timeseries_2.data")
# #   #data_in = file.path("H:", "ATMOSYS", "atmosys-timeseries_2.data")
# #   
# #   CT = CreateConversionTable(data_in)
# #   #CT.SP = MakeCTSpatial(CT)
# #   CT.SPDF = MakeCTSpatial(CT)
# #   
# #   SaveAsFile(CT.SPDF, Name, "Shapefile", TRUE) #"GeoJSON"
# # }
# # 
# # if (!is.null(Subset.Gemeente))
# # {
# #   CT.SPDF = SubsetCTSpatial(CT.SPDF, Subset.Gemeente)
# # }
# # SaveAsFile(CT.SPDF, paste(Name, Subset.Gemeente, sep = "_"), "Shapefile", TRUE)
# 
# # # Read PPH and determine the Location ID corresponding to the pollutant dataset (Spatial ConversionTable = CT.SP)
# # LocationIDs.P = PersonalLocationToLocationID(PPH.P, CT.SP, 1)
# # 
# # if (Active.Profile$Dynamics == "dynamic")
# # {
# #   LocationIDs.S = PersonalLocationToLocationID(PPH.S, CT.SP, 1)
# #   LocationIDs.T1 = PersonalLocationToLocationID(PPH.T1, CT.SP, 1)
# #   LocationIDs.T2 = PersonalLocationToLocationID(PPH.T2, CT.SP, 1)
# #   
# #   PPH.T1@data$duration = PPH.T1@data$duration * 1.2 # duration correction
# #   PPH.T2@data$duration = PPH.T2@data$duration * 1.2 # duration correction
# #   
# #   Leave.P = 8
# #   Leave.S = 17
# #   
# #   TimeVertex.T1 = LinkPointsToTime.Commuting(PPH.T1, LocationIDs.T1, year.active, Leave.P) # Time of the Transport route vertices Outwards
# #   TimeVertex.T2 = LinkPointsToTime.Commuting(PPH.T2, LocationIDs.T2, year.active, Leave.S) # Time of the Transport route vertices Inwards
# #   
# #   PPH.Phases.Times = TimePhaser(PPH.P,Leave.P, Leave.S, TimeVertex.T1, TimeVertex.T2)
# #   PPH.Phases.DateTimes = PPH.Phases.Times
# #   
# #   PHASES = TimePhaserList(BusinesDates, PPH.Phases.DateTimes)
# #   as.POSIXct(PHASES[[60]][15,2], origin = "1970-01-01", tz = "CET")
# #   as.POSIXct(PHASES[[70]][15,1], origin = "1970-01-01", tz = "CET")
# #   as.POSIXct(PHASES[[70]][15,2], origin = "1970-01-01", tz = "CET")
# #   
# #   #PHASES[[200]][1,1] #[[businesday#]][individual,]
# #   
# #   Correct = T
# #   if (Correct == T) # Summertime correction correction (CET vs. CEST | The S can be ignored after this correction)
# #   {
# #     PHASES = TimePhaserListC(PHASES, Leave.P, PPH.Phases.Times)
# #   }
# #   as.POSIXct(PHASES[[60]][15,2], origin = "1970-01-01", tz = "CET")
# #   as.POSIXct(PHASES[[70]][15,1], origin = "1970-01-01", tz = "CET")
# #   as.POSIXct(PHASES[[70]][15,2], origin = "1970-01-01", tz = "CET")
# #   
# #   TIME.P = AtPrimaryOrSecondary2("Primary", PHASES, BusinesDates, "Workdays")
# #   TIME.S = AtPrimaryOrSecondary2("Secondary", PHASES, BusinesDates)
# #   
# #   TIMEVertex.T1 = LinkPointsToTime.Commuting2("Outwards", PPH.T1, LocationIDs.T1, PHASES) # Time of the Transport routes vertices Outwards
# #   TIMEVertex.T2 = LinkPointsToTime.Commuting2("Inwards", PPH.T2, LocationIDs.T2, PHASES) # Time of the Transport routes vertices Inwards
# #   
# #   # Weekends
# #   Include.Weekends = TRUE
# #   if (Include.Weekends == TRUE)
# #   {
# #     TIME.P = IncludeWeekends("Primary", TIME.P, YearDates, BusinesDates, WeekendDates)
# #     TIME.S = IncludeWeekends("Secondary", TIME.S, YearDates, BusinesDates, WeekendDates)
# #     TIMEVertex.T1 = IncludeWeekends("T1", TIMEVertex.T1, YearDates, BusinesDates, WeekendDates)
# #     TIMEVertex.T2 = IncludeWeekends("T2", TIMEVertex.T2, YearDates, BusinesDates, WeekendDates)
# #   }
# #   
# #   # Hours of the year
# #   HOURS.P = HourOfTheYear4(2009, TIME.P, 0)
# #   HOURS.S = HourOfTheYear4(2009, TIME.S, 0)
# #   HOURS.T1 = HourOfTheYear4(2009, TIMEVertex.T1, 0)
# #   HOURS.T2 = HourOfTheYear4(2009, TIMEVertex.T2, 0)
# #   HOURS.T1_3d = HourOfTheYear4(2009, TIMEVertex.T1, 3)
# #   HOURS.T2_3d = HourOfTheYear4(2009, TIMEVertex.T2, 3)
# # }
# 
# # if (Active.Profile$Dynamics == "static")
# # {
# #   #   TIME.P = seq(YearDates[1], tail((YearDates), 1)+1*60**2*24, by = 1*60**2)
# #   #   length(TIME.P_test)
# #   #   tail((TIME.P), 2)
# #   
# #   Time.P = NULL
# #   for (d in seq(2, length(YearDates), 1))
# #   {
# #     Time.P[[1]] = seq(YearDates[1], YearDates[1]+1*60**2*24, by = 1*60**2)
# #     Time.P[[d]] = seq(YearDates[d]+1*60**2, YearDates[d]+1*60**2*24, by = 1*60**2)
# #   }
# #   
# #   TIME.P = list()
# #   for (i in seq_along(PPH.P))
# #   {
# #     TIME.P[[i]] = Time.P
# #   }
# #   
# #   Hours.P = HourOfTheYear5(2009, Time.P, 0)
# #   HOURS.P = HourOfTheYear5(2009, TIME.P, 0)
# # }
# 
# 
# # # Create buffer of AoI (=Location history)
# # 
# # mBuffer = 250
# # start.time = Sys.time()
# # AoI = AreaOfInterest.PPH(PPH.P, PPH.S, PPH.T1, PPH.T2, mBuffer)
# # end.time = Sys.time()
# # print(end.time - start.time)
# # SaveAsFile(AoI, paste(Active.Type,"AreaOfInterest", paste0(mBuffer,"m"), sep = "_"), "GeoJSON", TRUE)
# # 
# # start.time = Sys.time()
# # AoI2 = AreaOfInterest.PPH.2(PPH.P, PPH.S, PPH.T1, PPH.T2, mBuffer, Plot = TRUE)
# # end.time = Sys.time()
# # print(end.time - start.time)
# # SaveAsFile(AoI2, paste(Active.Type,"AreaOfInterest", paste0(mBuffer,"m"), "simp", sep = "_"), "GeoJSON", TRUE)
# # 
# # 
# # AoI.path_in = file.path("..", "output", paste(Active.Type, "AreaOfInterest", paste0(mBuffer,"m", ".geojson"), sep = "_"))
# # AoI = readOGR(AoI.path_in, layer = 'OGRGeoJSON')
# # AoI = rgeos::gUnaryUnion(AoI, id = NULL)
# # AoI@proj4string = BE_crs
# 
# # AoI.AQ = AreaOfInterest.AQ(Points)
# # plot(AoI.AQ)
# # points(Points)
# # lines(PPH.T2, col = "red")
# # lines(Flanders, col = "orange")
# 
# # # Make subset Area of Interest (AoI) (of Flanders)
# # 
# # # Create buffer of Flanders
# # Flanders_buff = gBuffer(Flanders, byid = F, id = NULL, width = 5000)
# # lines(Flanders_buff, col = "blue")
# # 
# # # Read Belgian polygon (improve unzip)
# # ## Read the input data
# # BE_zip_in = file.path("..", "data", "BE", "Belgium_shapefile.zip")
# # scale = 1 # (in km) 1, 10 or 100
# # BE_shp_name = paste0("be_", scale, "km")
# # Extentions = c(".shp", ".dbf", ".prj", ".shx")
# # BE_shp_in = file.path("..", "data", "BE", paste0(BE_shp_name,Extentions[1]))
# # 
# # # Check if input data is available
# # if (!file.exists(BE_zip_in) & !file.exists(BE_shp_in))
# # {
# #   stop(paste("Belgium shapefile does not found."))
# # }
# # if (!file.exists(BE_shp_in))
# # {
# #   unzip(BE_zip_in, exdir= file.path("..", "data", "BE"), files = paste0(BE_shp_name,Extentions))
# # }
# # Belgium = readOGR(BE_shp_in, layer = BE_shp_name)
# # Belgium = spTransform(Belgium, BE_crs)
# # Belgium = rgeos::gUnaryUnion(Belgium, id = NULL)
# # 
# # # Create buffer of Belgium
# # Belgium_buff = gBuffer(Belgium.UU, byid = F, id = NULL, width = -10000)
# # plot(PRI, col = "green")
# # points(Points)
# # lines(PPH.T1, col = "red")
# # lines(Belgium, col = "blue")
# # lines(Belgium_buff, col = "pink")
# # 
# # PPH.T1.co = coordinates(PPH.T1)
# # 
# # o = over(PPH.T1.co, Belgium_buff)
# # inter = gIntersects(PPH.T1[1:20,], Belgium_buff, byid = TRUE)
# # inter = gIntersection(PPH.T1[1:20,], Belgium_buff, byid = TRUE)
# # 
# # coordinates(PPH.T1[1,]@lines[[1]])
# 
# # # AoI & Subset area Municipality (AoI2) | use for testing
# # AoI2 = gIntersection(AoI_SPDF, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",])
# # 
# # AoI2_SPDF = SpatialPolygonsDataFrame(AoI2, data = data.frame(1:(length(AoI2))), match.ID = T)
# # SaveAsFile(AoI2_SPDF, "AoI2", "GeoJSON", TRUE)
# # #AoI2 = gIntersection(AoI_SPDF, Flanders_buff)
# 
# ##
# # Points.T.txt.path_in = file.path("..", "output", paste(Active.Type,"AoI_TRUES.txt", sep = "_"))
# # if (!exists("Points.T"))
# # {
# #   if (file.exists(Points.T.txt.path_in))
# #   {
# #     Points.T = as.integer(fread(Points.T.txt.path_in, sep = ";", header = FALSE))
# #     Points.T = Points.T[1:(length(Points.T)-1)] # remove last 'fake' value
# #   } else
# #   {
# #     Points.TF = IntersectsBoolean(Points, AoI)
# #     Points.T = which(Points.TF == TRUE)
# #     
# #     #create and write to txt file
# #     file.create(Points.T.txt.path_in)
# #     Points.T.txt = file(Points.T.txt.path_in)
# #     writeLines(paste(Points.T), Points.T.txt, sep = ";")
# #     close(Points.T.txt)
# #   }
# # }
# 
# # # Create the base: All the RIO-IFDM points inside the Area of Interest
# # Points.AoI = Points[Points.T,]
# # # plot(Points.AoI)
# # # SaveAsFile(Points.AoI, paste(Active.Type, "Points_AoI", sep = "_"), "GeoJSON", TRUE)
# # 
# # AoI.path_in = file.path("..", "output", paste(Active.Type, "Points_AoI", sep = "_"))
# # if (!exists("Points.AoI") & file.exists(AoI.path_in))
# # {
# #   Points.AoI = readOGR(AoI.path_in, layer = 'OGRGeoJSON')
# #   Points.AoI@proj4string = BE_crs
# # }
# 
# # # Create grid for Flanders
# # 
# # # Grid on whole number coordinates 10x10m inside AoI
# # !!
# #   
# #   
# #   # data intensive method
# #   sgrid = GridMaker(Flanders_buff, AoI, 100, 10)
# # #plot(sgrid)
# # 
# # sgrid.TF = IntersectsBoolean(sgrid, AoI)
# 
# # Create Boolean and base for the Municipality of Antwerp | use for testing
# # Gemeente.RIO_IFDM_TF = gIntersects(Points,
# #                                    Municipalities[Municipalities@data$NAAM %in% "Antwerpen",],
# #                                    byid = TRUE)
# # Gemeente.RIO_IFDM_TF_logi = as.logical(Gemeente.RIO_IFDM_TF)
# # Gemeente.RIO_IFDM = Points[Gemeente.RIO_IFDM_TF_logi,]
# 
# # AoI2.RIO_IFDM_TF = gIntersects(Gemeente.RIO_IFDM, AoI_SPDF, byid = T)
# # AoI2.RIO_IFDM_logi = MatrixToLogical(AoI2.RIO_IFDM_TF)
# # 
# # MatrixToLogical <-function(Sub, ...)
# # {
# #   TF = NA
# #   for (c in 1:ncol(Sub))
# #   {
# #     TF[c] = any(Sub[,c]==T)
# #   }
# #   return(TF)
# # }
# # 
# # AoI2.RIO_IFDM = Gemeente.RIO_IFDM[AoI2.RIO_IFDM_logi,]
# # spplot(AoI2.RIO_IFDM, "values")
# # 
# # SaveAsFile(AoI.RIO_IFDM, "AoI-RIO_IFDM", "GeoJSON", TRUE)
# # 
# # AoI.path_in = file.path("..", "output", "AoI-RIO_IFDM.geojson")
# # AoI.RIO_IFDM = readOGR(AoI.path_in, layer = 'OGRGeoJSON')
# # AoI.RIO_IFDM@proj4string = BE_crs
# 
# # ## Reading the HDF5 structure
# # pol = "no2"
# # polFile = paste0(pol, "-gzip.hdf5")
# # h5f_dir = file.path("..", "data", "BE", "ATMOSYS", polFile)
# # #h5f_dir = file.path("I:", "ATMOSYS", polFile)
# # 
# # ## Where the magic happens
# # ExposureValue.All = ExtractExposureValue.Integral(h5f_dir, LocationIDs.P, LocationIDs.S, LocationIDs.T1, LocationIDs.T2,
# #                                                   HOURS.P, HOURS.S, HOURS.T1, HOURS.T2)
# # ExposureValue.P = ExposureValue.All[[1]]
# # ExposureValue.S = ExposureValue.All[[2]]
# # ExposureValue.T1 = ExposureValue.All[[3]]
# # ExposureValue.T2 = ExposureValue.All[[4]]
# # 
# # 
# # ExposureValue.P[[5]][[200]]
# # ExposureValue.T1[[1]][[204]]
# # ExposureValue.T2[[100]][[250]]
# # ExposureValue.S[[100]][[200]]
# # 
# # # # TOEVOEGEN: Koppeling W aan R, zodat lenght(W)=lenght(R) | When there is a many:1 relation
# # ExposureValue.S = SecondaryRelation(PPH.P, PPH.S, ExposureValue.S)
# # ExposureValue.S[[80]][[203]]
# # 
# # ## num [1:v] NA -> num NA or logi NA for Transport (T1&T2)
# # ExposureValue.T1 = NAWeekends(ExposureValue.T1, YearDates, BusinesDates, WeekendDates)
# # ExposureValue.T2 = NAWeekends(ExposureValue.T2, YearDates, BusinesDates, WeekendDates)
# # 
# # # Write Exposurevalues to disk
# # WriteToDisk = TRUE
# # if (WriteToDisk == TRUE)
# # {
# #   SaveAsDBF(ExposureValue.P, "ExposureValue_P", Active.Type)
# #   
# #   if (Active.Profile$Dynamics == "dynamic")
# #   {
# #     SaveAsDBF(ExposureValue.S, "ExposureValue_S", Active.Type)
# #     SaveAsDBF(ExposureValue.T1, "ExposureValue_T1", Active.Type)
# #     SaveAsDBF(ExposureValue.T2, "ExposureValue_T2", Active.Type)
# #   }
# # }
# # 
# # #Read DBF file with TIME 
# # TIME.P = DBFreader("Time", "Primary", PPH.P, YearDates, Active.Type)
# # if (Active.Profile$Dynamics == "dynamic")
# # {
# #   TIME.S = DBFreader("Time", "Secondary", PPH.P, YearDates, Active.Type)
# #   TIMEVertex.T1 = DBFreader("Time", "T1", PPH.P, YearDates, Active.Type)
# #   TIMEVertex.T2 = DBFreader("Time", "T2", PPH.P, YearDates, Active.Type)
# # }
# # 
# # #Read DBF file with ExposureValues
# # ExposureValue.P = DBFreader("Exposure", "Primary", PPH.P, YearDates, Active.Type)
# # if (Active.Profile$Dynamics == "dynamic")
# # {
# #   ExposureValue.S = DBFreader("Exposure", "Secondary", PPH.P, YearDates, Active.Type)
# #   ExposureValue.T1 = DBFreader("Exposure", "T1", PPH.P, YearDates, Active.Type)
# #   ExposureValue.T2 = DBFreader("Exposure", "T2", PPH.P, YearDates, Active.Type)
# # }
# # 
# # 
# # # Plotting results
# # Ind = 265
# # Plot.PersonalExposureGraph(Ind, 70, 6) # (Individual, Start(working)Day, Amount of days)
# # 
# # Plot.Group(Active.Type, 1, 7, 100, TRUE)
# 
# 
# 
# 
# ## CHECK the ExposureValues. Should give 8761 values? Are these values in the right time?
# head(TIME.P[[1]], 2)
# head(HOURS.P[[1]], 2)
# tail(HOURS.P[[1]], 2)
# 
# unlist(TIME.P[[1]]) %in% as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET") | 
#   unlist(TIME.P[[1]]) %in% as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET")
# 
# length(unlist(TIME.P[[1]]))
# length(unlist(HOURS.P[[1]]))
# length(unlist(ExposureValue.P[[1]]))
# 
# tail(ExposureValue.P[[1]],1)
# head(ExposureValue.P[[1]],2)
# 
# tail(ExposureValue.P_01.OW[[1]],1)
# 
# H5.active = h5read(h5f_dir, as.character(1))
# H5.active$data[HOURS.P[[1]][[365]][25], 7701]
# H5.active$data[HOURS.P[[1]][[365]][24], 7701]
# H5.active$data[8760, 7701]
# DF = data.frame(H5.active)
# H5close()
# 
# tail(H5.active@data,20)
# 
# ExposureValue.P.WM = Weighted.Static(ExposureValue.P, "WeightedMean")
# ExposureValue.S.WM = Weighted.Static(ExposureValue.S, "WeightedMean")
# ExposureValue.T1.WM = Weighted.Dynamic(ExposureValue.T1, WEIGHTS.T1, "WeightedMean")
# ExposureValue.T2.WM = Weighted.Dynamic(ExposureValue.T2, WEIGHTS.T2, "WeightedMean")
# 
# for (i in seq_along(ExposureValue.P.WM))
# {
#   print(paste("Individual", i, ":", mean(ExposureValue.P.WM[[i]]), mean(ExposureValue.S.WM[[i]]), mean(ExposureValue.T1.WM[[i]]), mean(ExposureValue.T2.WM[[i]])))
# }
# 
# mean(ExposureValue.P.WM[[1]])
# mean(ExposureValue.S.WM[[1]])
# mean(ExposureValue.T1.WM[[1]])
# mean(ExposureValue.T2.WM[[1]])
# 
# hist(ExposureValue.P.WM[[99]], breaks = 50)
# 
# rm(ExposureValue.T12,ExposureValue.T1.WM, ExposureValue.T2.WM, ExposureValue.P.WM, ExposureValue.S.WM, WEIGHTS.T1, WEIGHTS.T2)
# 
# 
# EXP.P.mean = list()
# EXP.P.sum = list()
# for (i in seq_along(ExposureValue.T1)) # per individual
# {
#   for (d in seq_along(BusinesDates)) # per day
#   {
#     #Exp.P.mean = mean(ExposureValue.P[[i]][[d]])
#     #Exp.S.mean = mean(ExposureValue.S[[i]][[d]])  
#     
#     Exp.P.sum[[d]] = sum(ExposureValue.P[[i]][[d]])
#     
#   }
#   
#   
#   
#   #EXP.P.mean[[i]] = Exp.P.mean
#   EXP.P.sum[[i]] = Exp.P.sum[[d]]
#   
#   #   EXP.S[[i]] = 
#   #   EXP.C[[i]] = 
#   
# }
# sum(Exp.P.sum)
# 
# 
# sum(ExposureValue.T1[[1]][[1]] * WEIGHTS.T1[[1]], na.rm = TRUE)
# 
# 
# TEST = TimeDifference(HourOfTheYear4(2009, TIMEVertex.T1, 3))
# 
# ExposureValue.T2 = ExtractExposureValue2("no2", LocationIDs.T2, HOURS.T2)
# 
# 
# 
# h5f.active_WS = h5read(h5f_dir, as.character(16))
# h5f.active_WS$data[HOURS.T1[[5]][[1]][1]+1, 5187]
# 
# 
# smoothingSpline = smooth.spline(x=HOURS.T1_3d[[99]][[70]], ExposureValue.T1[[99]][[70]], spar=0.035)
# plot(x=HOURS.T1_3d[[99]][[70]], y=ExposureValue.T1[[99]][[70]], ylim=c(0, 100))
# lines(smoothingSpline)
# 
# plot(x=c(TIMEVertex.T1[[99]][[70]]), y=ExposureValue.T1[[99]][[70]], ylim=c(0, 100))
# 
# plot(x=c(TIME.P[[99]][[70]],TIME.S[[99]][[70]],TIMEVertex.T1[[99]][[70]],TIMEVertex.T2[[99]][[70]]),
#      y=c(ExposureValue.P[[99]][[70]],ExposureValue.S[[99]][[70]],ExposureValue.T1[[99]][[70]],ExposureValue.T2[[99]][[70]]),
#      ylim=c(0, 100))
# 
# 
# 
# library(ggplot2)
# qplot(HOURS.T1_3d[[2]][[70]],ExposureValue.T1[[2]][[70]], geom='smooth', span =0.5, ylim=c(0, 100))
# 
# start.time = Sys.time()
# 
# length(HOURS.T1[[15]][[1]])
# 
# end.time = Sys.time()
# time.taken = end.time - start.time
# paste("The script has finished running in", time.taken, "seconds.")
# 
# length(ExposureValue.T1[[12]][[1]])
# 
# 
# SaveAsFile(CT2, "CT2", "GeoJSON", TRUE)
# RESO.BE = CalculateResolution(CT)
# 
# 
# #! Use parallel processing
# 
# install.packages("rmarkdown")
# 
# library(parallel)
# 
# mclapply(1:30, rnorm)
# # use the same random numbers for all values
# set.seed(12345)
# mclapply(1:30, rnorm, mc.preschedule=FALSE, mc.set.seed=FALSE)
# # something a bit bigger - albeit still useless :P
# unlist(mclapply(1:32, function(x) sum(rnorm(1e7))))
# 
# 
# 
# 
# 
# #### The Netherlands ####
# 
# ## Convert RGB images to single band GeoTIFF
# 
# #RGBtoSingleBand("20161108_vandaag_no2_03.tiff")
# RGB.list = list.files(file.path("..", "data", "RIVM"), pattern = ".tiff" )
# for (i in RGB.list)
# {
#   RGBtoSingleBand(i)
# }
# 
# ## Determine routes
# DetermineRoutesNL(c("Utrecht", "Gelderland"), 100, 1000)
# 
# 
# # H5close()
# # start.time = Sys.time()                    
# # ExposureValue.P_ = ExtractExposureValue.Static(h5f_dir, LocationIDs.P, HOURS.P) # LocationIDs.P[1:5]
# # end.time = Sys.time()
# # time.taken = end.time - start.time
# # time.taken # 6.7 min (5,100) # 43.8 min (100) # 47.5 min (100)
# 
# # uses a quicker method with hard drive
# ExposureValue.P = ExtractExposureValue.Static2(h5f_dir, LocationIDs.P, HOURS.P) # LocationIDs.P[1:5]
# 
# start.time = Sys.time()
# ExposureValue.S = ExtractExposureValue.Static(h5f_dir, LocationIDs.S, HOURS.S)
# end.time = Sys.time()
# time.taken = end.time - start.time
# time.taken # 11.5 min (5,100)
# 
# # Kan sneller wannneer (R,W,) C1 en C2 tegelijk worden berekend:
# start.time = Sys.time()
# ExposureValue.T12 = ExtractExposureValue.Dynamic3(h5f_dir, LocationIDs.T1, LocationIDs.T2, HOURS.T1, HOURS.T2) # LocationIDs.P[1:5]
# end.time = Sys.time()
# time.taken = end.time - start.time
# time.taken # 25 minutes (15) ,33 minutes (30), 7 hours (100), 1.5 hours (1,100, f), 3.7 hours (5,100,f)
# 
# ExposureValue.T1 = ExposureValue.T12[[1]]
# ExposureValue.T2 = ExposureValue.T12[[2]]