# Main script for determining Primary (residence), Secondary (workplace) and
# Transport routes from address data to combine these with high resolution
# air quality data NO2 and PM2.5 to produce individual exposure values.
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

## TODO:  - Weights in S-T aggregation for overlaps in 03.SP
##        - Improve SummaryStatistics for profile and phase type comparison.
##        - 
##        - ?Introduce "spacetime" package and test is.
##        - Documentation
##        - ...

##        - Interactive graph (optional)
##        - Air Quality Health Standards Exceedance analysis (optional)

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
source("modules/ExtractBZ2.r")
source("modules/SimplifyRoutes.r")
source("modules/CreateCorrespondingDateAndTime.r")
source("modules/DataFraming.r")
source("modules/BiWeekly.r")
source("modules/ToHourValues.r")
source("modules/DBFreader.r")
#source("modules/AreaOfInterest.r")
source("modules/SummaryStatistics.r")
#source("modules/util.r")
source("modules/DrivingLinearDistanceRatio.r")
source("modules/BaseAQnetwork.r")

OriginalTimezone = Sys.timezone(location = TRUE) # All system settings should be changed to its original state at the end of the code
Sys.setenv(TZ = "GMT")


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
#pol = "pm25" # "no2", "pm25"

if (DownloadMode == "FTP")
{
  ftp.pwd = "" # password here
  out.dir = file.path("..", "data", "IRCELINE_test")
  ftp.filenames = c("20150101_1_", toupper(pol), ".txt.bz2", "20150101_2_", toupper(pol), ".txt.bz2")
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

# new statistics on Commuting (Office Worker only)
csv.Commuting_in = file.path("..", "data", "BE_FL", "CommutingStats.csv")
Commuting = fread(csv.Commuting_in, sep=",", header=TRUE)

# create 'output' folder in case it does not exist
output.dir = file.path("..", "output")
if (!dir.exists(output.dir)) { dir.create(output.dir) }

# Read Flanders polygon)
Flanders = getData("GADM",country = "Belgium", level = 1, path = output.dir)
Flanders = Flanders[Flanders@data$NAME_1 == "Vlaanderen",]
Flanders = spTransform(Flanders, BE_crs)
Flanders@proj4string = BE_crs
Flanders@data$NAME_1_EN = "Flanders"
#SaveAsFile(Flanders, "Flanders", "GeoJSON", TRUE)

# Set year of pollutant dataset, determine dates and date types (Workdays~Weekends)
YearDates = YearDates2(year.active)

Time = seq(as.POSIXct(paste0(year.active,"-01-01 01:00:00")),
           (as.POSIXct(paste0(year.active+1,"-01-01 00:00:00"))), 1*60**2)

# Official holydays (for all profiles)
csv.OfficialHolidays_in = file.path("..", "data", "BE", "OfficialHolidays.csv")
OfficialHolidays = fread(csv.OfficialHolidays_in, sep = ";", header=TRUE)

# School holidays (School Pupils only)
csv.SchoolHolidays_in = file.path("..", "data", "BE_FL", "SchoolHolidays.csv")
SchoolHolidays = fread(csv.SchoolHolidays_in, sep = ";", header=TRUE)

# Set the seed for reproducible results
ReproduceMode = FALSE
if (ReproduceMode)
{
  # Separate SeedNr to prevent PPH.P being identical for all profiles
  SeedNr.BiWeekly = 123
} else
{
  SeedNr.BiWeekly = NULL
}

# Biweekly generator
if (!exists("BIWEEKLY"))
{
  BIWEEKLY = BiWeekly(year.active, YearDates, SchoolHolidays, Time, SeedNr.BiWeekly)
}

ReproduceMode = TRUE

# End of general code
# - - - 
# Beginning of profile based code


# # Select active Residential Profile
# Active.Type = "01.OW" # "01.OW" "02.HO" or "03.SP"
# Active.Subtype = paste0(Active.Type, "_WS","1")
# 
# Active.Profile = ResidentialProfiles[ResidentialProfiles$Type == Active.Type,]
# Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]

#for (Active.Subtype in ResidentialProfiles$Subtype[c(1,3,5,9,11)])
for (Active.Subtype in ResidentialProfiles$Subtype[c(1,3,5,9,11)][4])
{
  #Active.Subtype = ResidentialProfiles$Subtype[c(1,3,5,9,11)][4]
  print(Active.Subtype)
  
  Active.Type = ResidentialProfiles$Type[ResidentialProfiles$Subtype == Active.Subtype]
  Active.Profile = ResidentialProfiles[ResidentialProfiles$Type == Active.Type,]
  Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]
  
  # Set the seed for reproducible results
  if (ReproduceMode)
  {
    # Separate SeedNr to prevent PPH.P being identical for all profiles
    Active.SetSeedNr = Active.Subprofile$SeedNr
  } else
  {
    Active.SetSeedNr = NULL
  }
  
  dir.P = file.path("..", "output", paste(Active.Type, paste0("Primary", Names,".geojson"), sep = "_"))
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    dir.S = file.path("..", "output", paste(Active.Type, paste0("Secondary", Names,".geojson"), sep = "_"))
    
    if (is.null(Subset.Gemeente))
    {
      dir.T1 = file.path("..", "output", paste(Active.Type, paste0("TransportOutwards", Names, ".geojson"), sep = "_"))
      dir.T2 = file.path("..", "output", paste(Active.Type, paste0("TransportInwards", Names, ".geojson"), sep = "_"))
    } else
    {
      dir.T1 = file.path("..", "output", paste0(Active.Type,"_TransportOutwards_", Names, "_"))
      dir.T2 = file.path("..", "output", paste0(Active.Type,"_TransportInwards_", Names, "_"))
    }
    
    # Use stats from previous results
    if (Active.Type == "01.OW") {DrivingDistanceLinearDistance = DrivingLinearDistanceRatio()}
    
  }
  
  if (!exists("CRAB_Doel") & !file.exists(dir.P))
  {
    if (file.exists(file.path("..", "output", paste0(CRAB.Name,".shp")))) # .geojson
    {
      CRAB_Doel = readOGR(file.path("..", "output", paste0(CRAB.Name,".shp")), layer = CRAB.Name) # Bug in .geojson, read .shp
      CRAB_Doel@proj4string = BE_crs
    } else
    {
      CRAB_Doel = DetermineAddressGoals_FL(Subset.Gemeente,2)
      CRAB_Doel@proj4string = BE_crs
      SaveAsFile(CRAB_Doel, CRAB.Name, "Shapefile", TRUE) # "GeoJSON" fails
    }
  }
  
  # Check if data already exists. If so, it will not run.
  if (!file.exists(dir.P))
  {
    # Read Belgium polygon
    Belgium = getData("GADM",country="Belgium", level = 0, path = output.dir)
    Belgium = spTransform(Belgium, BE_crs)
    Belgium@proj4string = BE_crs
    #SaveAsFile(Belgium, "Belgium", "GeoJSON", FALSE)
    
    # Create buffer to mimic the range of the RIO-IFDM points
    Belgium = gBuffer(Belgium, byid = F, id = NULL, width = 2000)
    
    ## Determine PPH for the active profile.
    DeterminePPH_FL(CRAB_Doel, Names, 1000, Active.Type, Active.Subprofile,
                    Plot = TRUE, SaveResults = TRUE, Belgium, Active.SetSeedNr, Commuting, DrivingDistanceLinearDistance)
  }
  
  # Remove the data in the environment that will not be used from this point.
  if (file.exists(dir.P))
  {
    if (exists("CRAB_Doel")) {rm(CRAB_Doel)}
    if (exists("Belgium")) {rm(Belgium)}
    #, KEY.InputData, DownloadMode, keyInputData.dir, Filenames)
    gc()
  }
  
  # Read the Personal Place History (PPH)
  if (file.exists(dir.P)) # (!exists("PPH.P")
  {
    PPH.P = readOGR(dir.P, layer = 'OGRGeoJSON')
    PPH.P@proj4string = BE_crs
  }
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    if (file.exists(dir.S)) # !exists("PPH.S") & 
    {
      PPH.S = readOGR(dir.S, layer = 'OGRGeoJSON')
      PPH.S@proj4string = BE_crs
    }
    
    if (file.exists(dir.T1)) # !exists("PPH.T1") & 
    {
      PPH.T1 = readOGR(dir.T1, layer = 'OGRGeoJSON')
      PPH.T1@proj4string = BE_crs
    }
    
    if (file.exists(dir.T2)) # !exists("PPH.T2") & 
    {
      PPH.T2 = readOGR(dir.T2, layer = 'OGRGeoJSON')
      PPH.T2@proj4string = BE_crs
    }
  }
  
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    # Correcting the cycling durations: use constant speed of 10km/h
    
    # hist(PPH.T1@data$duration, 1000)
    # hist(PPH.T1@data$duration / PPH.T1@data$distance, 1000)
    # hist(PPH.T2@data$duration / PPH.T2@data$distance, 1000)
    # hist(PPH.T1@data$distance, 1000)
    # hist(PPH.T2@data$distance, 1000)
    
    if (Active.Type == "03.SP")
    {
      MeanCyclingSpeed = 10 # km/h
      PPH.T1@data$duration = PPH.T1@data$distance * (60/MeanCyclingSpeed)
      PPH.T2@data$duration = PPH.T2@data$distance * (60/MeanCyclingSpeed)
      
      # hist(PPH.T1@data$duration / PPH.T1@data$distance, 1000)
      # hist(PPH.T2@data$duration / PPH.T2@data$distance, 1000)
      # PPH.T1@data$distance / PPH.T1@data$duration * 60
    }
  }
  
  if (Active.Type == "01.OW")
  {
    HoliDates = as.POSIXct(OfficialHolidays$Datum)
    SimplifyRemainingPoints = 100 # desirable resulting amount
    RandomSamplePoints = 25 # desirable resulting amount of points, after random sampling the equal points
    
    BusinesDates = DateType(YearDates, "Workdays", HoliDates)
    WeekendDates = DateType(YearDates, "Weekends")
  }
  if (Active.Type == "03.SP")
  {
    HoliDates = HolidayGenerator(SchoolHolidays, Time) #2015 only
    SimplifyRemainingPoints = 10 # should be desirable resulting amount
    RandomSamplePoints = 5 # desirable resulting amount of points, after random sampling the equal points
    
    BusinesDates = DateType(YearDates, "Workdays", HoliDates)
    WeekendDates = DateType(YearDates, "Weekends")
  }
  
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    PPH.T1.Pnt.Li = list()
    PPH.T2.Pnt.Li = list()
    for (i in seq_along(PPH.P))
    {
      PPH.T1.Pnt.Li[[i]] = as(PPH.T1[i,], "SpatialPoints")
      PPH.T2.Pnt.Li[[i]] = as(PPH.T2[i,], "SpatialPoints")
    }
    
    # Convert Transport to points for equal durations
    PPH.T1.Pnt.eq.Li = SimplifyRoutes(PPH.T1, FALSE, Factor = SimplifyRemainingPoints) # Factor should be desirable resulting amount
    PPH.T2.Pnt.eq.Li = SimplifyRoutes(PPH.T2, FALSE, Factor = SimplifyRemainingPoints)
    
    # Basic time element per vertex
    TimeVertex.T1 = LinkPointsToTime.Transport("Outwards", PPH.T1, PPH.T1.Pnt.Li, year.active, Active.Subprofile)
    TimeVertex.T2 = LinkPointsToTime.Transport("Inwards", PPH.T2, PPH.T2.Pnt.Li, year.active, Active.Subprofile)
  }
  
  # 
  # # Check if folder with fragments already exist
  # TimeExp.lst = list.files(path = output.dir, pattern = paste0(Active.Subtype, "_", "[0-9]*"))
  # # remove _ and backups in the lst
  # NChar = NA
  # for (c in seq_along(TimeExp.lst))
  # {
  #   NChar[c] = nchar(TimeExp.lst[c])
  # }
  # TimeExp.lst = TimeExp.lst[NChar == nchar(Active.Subtype)+2 | NChar == nchar(Active.Subtype)+3]
  # TimeExp.lst = mixedsort(TimeExp.lst) # fixes string order 1 10 11 -> 1 2 3
  # 
  # if (Active.Subprofile$Dynamics == "dynamic")
  # {
  #   Frag.lst = gsub(x = TimeExp.lst, pattern = paste0(Active.Subtype, "_"), replacement = "")
  #   Fragments = as.numeric(Frag.lst)
  #   
  #   if (length(Fragments) == tail(Fragments,1))
  #   {
  #     DaySplit = length(YearDates)/length(Fragments)
  #     SeqFragment = floor(seq(0, length(YearDates), DaySplit))
  #   } else
  #   {
  #     stop(paste("Fragments are not correct."))
  #   }
  #   
  # } else # if static
  # {
  #   if (is.na(TimeExp.lst))
  #   {
  #     Fragments = 1
  #     SeqFragment = 0
  #   }
  # }
  
  pollutants = c("no2", "pm25")
  
  #for (pol in pollutants)
  for (pol in pollutants[2]) #[2] = pm25 only | [1] = no2 only
  {
    BASEAQ = BaseAQnetwork(pol, ExternalDrive = TRUE, DriveLetter = "T")
    Points.NoVal = BASEAQ[[1]]
    PolDir = BASEAQ[[2]]
    
    Fragments = 1:10
    DaySplit = length(YearDates)/length(Fragments)
    SeqFragment = floor(seq(0, length(YearDates), DaySplit))
    
    
    TimeTakenInterpolation = NA
    for (f in Fragments)
    { # implement EXP_pol ST.DF and HR recognition
      print(paste("Testing if HR exists of Fragment", f))
      if (file.exists(file.path(output.dir, paste0(Active.Subtype, "_", f),
                                paste0("HR_", toupper(pol), ".dbf"))))
      {
        print(paste("HR already exists of Fragment", f))
        next
      }
      print(paste("Starting Fragment", f))
      
      if (length(Fragments) > 1)
      {
        YearDates.Sub = YearDates2(year.active)[(SeqFragment[f]+1):(SeqFragment[f+1])]
        Time.Sub = Time[Time > YearDates.Sub[1] & Time <= (tail(YearDates.Sub,1) + 24*60**2)]
        
        if (exists("PPH.T1.PNT.RS")) {rm(PPH.T1.PNT.RS)}
        if (exists("PPH.T2.PNT.RS")) {rm(PPH.T2.PNT.RS)}
        gc()
        
        if (Active.Subprofile$Dynamics == "dynamic")
        {
          print(paste("Calculating (pseudo) random points in routes..."))
          PPH.T1.PNT.RS = RandomSampleRoutesYears(PPH.T1, PPH.T1.Pnt.eq.Li, FALSE, RandomSamplePoints,
                                                  YearDates.Sub, BusinesDates, Active.SetSeedNr, f)
          PPH.T2.PNT.RS = RandomSampleRoutesYears(PPH.T2, PPH.T2.Pnt.eq.Li, FALSE, RandomSamplePoints,
                                                  YearDates.Sub, BusinesDates, Active.SetSeedNr, f)
        }
        FolderName = paste0(Active.Subtype, "_", f)
      } else # length(Fragments) <= 1
      {
        YearDates.Sub = YearDates2(year.active)
        Time.Sub = Time
        
        FolderName = Active.Subtype
      }
      
      if (!file.exists(file.path(output.dir, FolderName)))
      {
        dir.create(file.path(output.dir, FolderName))
      }
      
      if (Active.Subprofile$Dynamics == "static") {TIME_EXP = c("TIME_P_")}
      if (Active.Subprofile$Dynamics == "dynamic") {TIME_EXP = c("TIME_P_", "TIME_S_","TIME_T1_","TIME_T2_")}
      
      if (all(file.exists(file.path(output.dir, FolderName, paste0(TIME_EXP, length(PPH.P), ".dbf")))))
      {
        # read from file
        TIME.P_F = DBFreader("Time", "Primary", PPH.P, YearDates.Sub, FolderName)
        if (Active.Subprofile$Dynamics == "dynamic")
        {
          TIME.S_F = DBFreader("Time", "Secondary", PPH.P, YearDates.Sub, FolderName)
          TIME.T1_F = DBFreader("Time", "T1", PPH.P, YearDates.Sub, FolderName)
          TIME.T2_F = DBFreader("Time", "T2", PPH.P, YearDates.Sub, FolderName)
        }
      } else # TIME files do not exist
      {
        dir.create(file.path(output.dir, FolderName))
        
        #! not tested in this context
        TIME = CreateCorrespondingDateAndTime(Active.Type, Active.Subprofile, PPH.P,
                                              YearDates.Sub, BusinesDates, WeekendDates, HoliDates,
                                              PPH.T1.Pnt.Li, PPH.T2.Pnt.Li, TimeVertex.T1, TimeVertex.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS,
                                              year.active, SeqFragment, f)
        TIME.P_F = TIME[[2]]
        if (Active.Subprofile$Dynamics == "dynamic")
        {
          PHASES_F = TIME[[1]]
          TIME.S_F = TIME[[3]]
          TIME.T1_F = TIME[[4]]
          TIME.T2_F = TIME[[5]]
        }
        rm(TIME)
        gc()
        
        # Write TIME to disk
        WriteToDisk = TRUE
        OW = FALSE
        if (WriteToDisk)
        {
          SaveAsDBF(TIME.P_F, "Time", "Primary", FolderName, OW, pol, 0)
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            SaveAsDBF(TIME.S_F, "Time", "Secondary", FolderName, OW, pol, 0)
            SaveAsDBF(TIME.T1_F, "Time", "T1", FolderName, OW, pol, 0)
            SaveAsDBF(TIME.T2_F, "Time", "T2", FolderName, OW, pol, 0)
          }
        }
      }
      
      # Hours of the year
      HOURS.P_F = HourOfTheYear7(year.active, TIME.P_F, 0)
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        HOURS.S_F = HourOfTheYear7(year.active, TIME.S_F, 0)
        HOURS.T1_F = HourOfTheYear7(year.active, TIME.T1_F, 0)
        HOURS.T2_F = HourOfTheYear7(year.active, TIME.T2_F, 0)
      }
      
      # Detect which hours belong to which points | change name systematically to HoP (Hour of Point) = HoP.P etc.
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        HOP = WhichHourForWhichPoint(PPH.P, Time.Sub, HOURS.P_F, HOURS.S_F, HOURS.T1_F, HOURS.T2_F,
                                     Print = FALSE, Active.Subprofile, SeqFragment , f)
        HoP.P_F = HOP[[1]]
        HoP.S_F = HOP[[2]]
        HoP.T1_F = HOP[[3]]
        HoP.T2_F = HOP[[4]]
      } else
      {
        HOP = WhichHourForWhichPoint(PPH.P, Time, HOURS.P_F, Print = FALSE,
                                     Active.Subprofile = Active.Subprofile, SeqFragment = 0)
        HoP.P_F = HOP[[1]]
      }
      rm(HOP)
      gc()
      
      ## Interpolating the points
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P, PPH.S, PPH.T1.PNT.RS, PPH.T2.PNT.RS,
                                                    Points.NoVal, PolDir, Plot = FALSE, pol,
                                                    StartHour = SeqFragment[f]*24+1, # Time[(SeqFragment[f]*24+1)]
                                                    EndHour = (SeqFragment[f+1])*24, # Time[(SeqFragment[f+1])*24]
                                                    HOURS.P_F, HOURS.S_F, HOURS.T1_F, HOURS.T2_F, 50,
                                                    HoP.P_F, HoP.S_F, HoP.T1_F, HoP.T2_F, Active.Subprofile, SeqFragment[f]#,
                                                    #Include_P = FALSE, Include_S = FALSE,
                                                    #Include_T1 = TRUE, Include_T2 = TRUE
                                                    )
      } else # if static
      {
        ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P = PPH.P, POL = Points.NoVal,
                                                    PolDir = PolDir,
                                                    Plot = FALSE, pol = pol,
                                                    StartHour = 1, EndHour = length(Time),
                                                    HOURS.P = HOURS.P_F,  NearestPoints = 50,
                                                    wP = HoP.P_F,
                                                    Active.Subprofile = Active.Subprofile,
                                                    seq = 0)
      }
      
      ExposureValue.P_F = ExposureValue.All[[1]]
      ExposureValue.S_F = ExposureValue.All[[2]]
      ExposureValue.T1_F = ExposureValue.All[[3]]
      ExposureValue.T2_F = ExposureValue.All[[4]]
      TimeTakenInterpolation[f] = ExposureValue.All[[5]]
      print(TimeTakenInterpolation[f])
      
      rm(ExposureValue.All)
      gc()
      
      # Write EXP to disk
      WriteToDisk = TRUE
      OW = FALSE
      if (WriteToDisk)
      {
        SaveAsDBF(ExposureValue.P_F, "Exposure", "Primary", FolderName, OW, pol, 0)
        if (Active.Subprofile$Dynamics == "dynamic")
        {
          SaveAsDBF(ExposureValue.S_F, "Exposure", "Secondary", FolderName, OW, pol, 0)
          SaveAsDBF(ExposureValue.T1_F, "Exposure", "T1", FolderName, OW, pol, 0)
          SaveAsDBF(ExposureValue.T2_F, "Exposure", "T2", FolderName, OW, pol, 0)
        }
      }
      
      ST.DF.P_F = DF.Structure2(PPH.P, TIME.P_F, TIME.P_F, ExposureValue.P_F) #, rm.na = TRUE
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        ST.DF.S_F = DF.Structure2(PPH.P, TIME.P_F, TIME.S_F, ExposureValue.S_F)
        ST.DF.T1_F = DF.Structure2(PPH.P, TIME.P_F, TIME.T1_F, ExposureValue.T1_F)
        ST.DF.T2_F = DF.Structure2(PPH.P, TIME.P_F, TIME.T2_F, ExposureValue.T2_F)
      }
      
      # Save DF structure
      SaveAsDBF(ST.DF.P_F, "DF", "Primary", FolderName, OW, pol, 0)
      if (Active.Subprofile$Dynamics == "dynamic")
      {  
        SaveAsDBF(ST.DF.S_F, "DF", "Secondary", FolderName, OW, pol, 0)
        SaveAsDBF(ST.DF.T1_F, "DF", "T1", FolderName, OW, pol, 0)
        SaveAsDBF(ST.DF.T2_F, "DF", "T2", FolderName, OW, pol, 0)
      }
      
      # if (Active.Subprofile$Dynamics == "dynamic")
      # {  
      #   # To HR structure
      #   ExposureValueCombined_F = ToHourValues(PPH.P, Time.Sub, ExposureValue.P_F, ExposureValue.S_F, ExposureValue.T1_F, ExposureValue.T2_F,
      #                                          TIME.P_F, TIME.S_F, TIME.T1_F, TIME.T2_F, HoP.P_F, HoP.S_F, HoP.T1_F, HoP.T2_F)
      #   
      #   if (length(Time.Sub) * length(PPH.P) == length(unlist(ExposureValueCombined_F)))
      #   {
      #     ST.DF.HR_F = DF.Structure2(PPH.P, TIME.P_F, Time.Sub, ExposureValueCombined_F, rm.na = FALSE)
      #     SaveAsDBF(ST.DF.HR_F, "HR", "HR", FolderName, OW, pol, 0)
      #   }
      # } else # if static
      # {
      #   SaveAsDBF(ST.DF.P_F, "HR", "HR", FolderName, OW, pol, 0)
      # }
      
      gc()
      # } # closing folder exists
      
      # Remove from memory/environment
      rm(TIME.P_F, HOURS.P_F, HoP.P_F, ExposureValue.P_F, ST.DF.P_F)
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        rm(TIME.S_F, TIME.T1_F, TIME.T2_F)
        rm(HOURS.S_F, HOURS.T1_F, HOURS.T2_F)
        rm(HoP.S_F, HoP.T1_F, HoP.T2_F)
        rm(ExposureValue.S_F, ExposureValue.T1_F, ExposureValue.T2_F)
        rm(ST.DF.S_F, ST.DF.T1_F, ST.DF.T2_F)
        rm(ST.DF.HR_F)
      }
      gc()
      
    } # closing f
    
  } # closing pol
  
  rm(f, dir.P, PPH.P, TimeExp.lst, Fragments, SeqFragment)
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    rm(dir.S, dir.T1, dir.T2)
    rm(PPH.S, PPH.T1, PPH.T2)
    rm(PPH.T1.Pnt.Li, PPH.T2.Pnt.Li)
    rm(PPH.T1.Pnt.eq.Li, PPH.T2.Pnt.eq.Li)
    rm(PPH.T1.PNT.RS, PPH.T2.PNT.RS)
    rm(TimeVertex.T1, TimeVertex.T2)
    rm(Frag.lst, DaySplit)
  }
  gc()

} # closing Active Subtype


for (f in Fragments)
{
  FolderName = paste0(Active.Subtype, "_", f)
  
  YearDates.Sub = YearDates2(year.active)[(SeqFragment[f]+1):(SeqFragment[f+1])]
  Time.Sub = Time[Time > YearDates.Sub[1] & Time <= (tail(YearDates.Sub,1) + 24*60**2)]
  
  
  # read from file
  TIME.P_F = DBFreader("Time", "Primary", PPH.P, YearDates.Sub, FolderName)
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    TIME.S_F = DBFreader("Time", "Secondary", PPH.P, YearDates.Sub, FolderName)
    TIME.T1_F = DBFreader("Time", "T1", PPH.P, YearDates.Sub, FolderName)
    TIME.T2_F = DBFreader("Time", "T2", PPH.P, YearDates.Sub, FolderName)
  }
  
  
  ST.DF.P_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_P_", toupper(pol), ".dbf")))
  ST.DF.S_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_S_", toupper(pol), ".dbf")))
  ST.DF.T1_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T1_", toupper(pol), ".dbf")))
  ST.DF.T2_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T2_", toupper(pol), ".dbf")))
  
  ExposureValue.P_F = list()
  ExposureValue.S_F = list()
  ExposureValue.T1_F = list()
  ExposureValue.T2_F = list()
  
  for (i in seq_along(PPH.P))
  {
    ExposureValue.P_F[[i]] = rep(list(), length(YearDates.Sub))
    for (d in seq_along(TIME.P_F[[i]]))
    {
      ExposureValue.P_F[[i]][[d]] = ST.DF.P_F[ST.DF.P_F$IND %in% i &
                                                ST.DF.P_F$TIME %in% TIME.P_F[[i]][[d]],]$EXP
    }
    
    ExposureValue.S_F[[i]] = rep(list(), length(YearDates.Sub))
    for (d in seq_along(TIME.S_F[[i]]))
    {
      ExposureValue.S_F[[i]][[d]] = ST.DF.S_F[ST.DF.S_F$IND %in% i &
                                                ST.DF.S_F$TIME %in% TIME.S_F[[i]][[d]],]$EXP
    }
    
    ExposureValue.T1_F[[i]] = rep(list(), length(YearDates.Sub))
    for (d in seq_along(TIME.T1_F[[i]]))
    {
      ExposureValue.T1_F[[i]][[d]] = ST.DF.T1_F[ST.DF.T1_F$IND %in% i &
                                                  ST.DF.T1_F$TIME %in% TIME.T1_F[[i]][[d]],]$EXP
    }
    
    ExposureValue.T2_F[[i]] = rep(list(), length(YearDates.Sub))
    for (d in seq_along(TIME.T2_F[[i]]))
    {
      ExposureValue.T2_F[[i]][[d]] = ST.DF.T2_F[ST.DF.T2_F$IND %in% i &
                                                  ST.DF.T2_F$TIME %in% TIME.T2_F[[i]][[d]],]$EXP
    }
    
  }
  
  #! Save EXP
  
  
  # Hours of the year
  HOURS.P_F = HourOfTheYear7(year.active, TIME.P_F, 0)
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    HOURS.S_F = HourOfTheYear7(year.active, TIME.S_F, 0)
    HOURS.T1_F = HourOfTheYear7(year.active, TIME.T1_F, 0)
    HOURS.T2_F = HourOfTheYear7(year.active, TIME.T2_F, 0)
  }
  
  # Detect which hours belong to which points | change name systematically to HoP (Hour of Point) = HoP.P etc.
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    HOP = WhichHourForWhichPoint(PPH.P, Time.Sub, HOURS.P_F, HOURS.S_F, HOURS.T1_F, HOURS.T2_F,
                                 Print = FALSE, Active.Subprofile, SeqFragment , f)
    HoP.P_F = HOP[[1]]
    HoP.S_F = HOP[[2]]
    HoP.T1_F = HOP[[3]]
    HoP.T2_F = HOP[[4]]
  } else
  {
    HOP = WhichHourForWhichPoint(PPH.P, Time, HOURS.P_F, Print = FALSE,
                                 Active.Subprofile = Active.Subprofile, SeqFragment = 0)
    HoP.P_F = HOP[[1]]
  }
  rm(HOP)
  gc()
  
  
  # Convert to HR
  # To HR structure
  ExposureValueCombined_F = ToHourValues(PPH.P, Time.Sub, ExposureValue.P_F, ExposureValue.S_F, ExposureValue.T1_F, ExposureValue.T2_F,
                                         TIME.P_F, TIME.S_F, TIME.T1_F, TIME.T2_F, HoP.P_F, HoP.S_F, HoP.T1_F, HoP.T2_F)
  
  if (length(Time.Sub) * length(PPH.P) == length(unlist(ExposureValueCombined_F)))
  {
    ST.DF.HR_F = DF.Structure2(PPH.P, TIME.P_F, Time.Sub, ExposureValueCombined_F, rm.na = FALSE)
    SaveAsDBF(ST.DF.HR_F, "HR", "HR", FolderName, OW, pol, 0)
  }
  
}



#! if TimeExp already exists.. tetect fragments


# split time in Fragments (only dynamic)
Fragments = 20

if (Fragments > 1)
{
  DaySplit = length(YearDates)/Fragments
  SeqFragment = floor(seq(0, length(YearDates), DaySplit))
  TimeTakenInterpolation = NA
  
  for (f in 1:Fragments)
    #for (f in 19:Fragments)
  {
    print(paste0("Starting fragment ", f, " of ", Fragments))
    
    if (exists("YearDates.Sub")) {rm(YearDates.Sub)}
    if (exists("Time.Sub ")) {rm(Time.Sub)}
    
    YearDates.Sub = YearDates2(year.active)[(SeqFragment[f]+1):(SeqFragment[f+1])]
    Time.Sub = Time[Time > YearDates.Sub[1] & Time <= (tail(YearDates.Sub,1) + 24*60**2)]
    
    if (exists("PPH.T1.PNT.RS")) {rm(PPH.T1.PNT.RS)}
    if (exists("PPH.T2.PNT.RS")) {rm(PPH.T2.PNT.RS)}
    
    PPH.T1.PNT.RS = RandomSampleRoutesYears(PPH.T1, PPH.T1.Pnt.eq.Li, FALSE, RandomSamplePoints,
                                            YearDates.Sub, BusinesDates, Active.SetSeedNr, f)
    PPH.T2.PNT.RS = RandomSampleRoutesYears(PPH.T2, PPH.T2.Pnt.eq.Li, FALSE, RandomSamplePoints,
                                            YearDates.Sub, BusinesDates, Active.SetSeedNr, f)
    
    Parts = 5
    StepSize = length(PPH.P)/Parts
    SeqParts = floor(seq(0, length(PPH.P), StepSize))
    
    for (p in (1:Parts))
    {
      if (exists("TIME.P")) {rm(TIME.P)}
      if (exists("TIME.S")) {rm(TIME.S)}
      if (exists("TIME.T1")) {rm(TIME.T1)}
      if (exists("TIME.T2")) {rm(TIME.T2)}
      
      TIME = CreateCorrespondingDateAndTime(Active.Type, Active.Subprofile, PPH.P[(SeqParts[p]+1):(SeqParts[p+1]),],
                                            YearDates.Sub, BusinesDates, WeekendDates, HoliDates,
                                            PPH.T1.Pnt.Li, PPH.T2.Pnt.Li, TimeVertex.T1, TimeVertex.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS,
                                            year.active, SeqFragment, f, SeqParts, p)
      TIME.P = TIME[[2]]
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        PHASES = TIME[[1]]
        TIME.S = TIME[[3]]
        TIME.T1 = TIME[[4]]
        TIME.T2 = TIME[[5]]
      }
      rm(TIME)
      
      # Write TIME to disk
      WriteToDisk = TRUE
      OW = FALSE
      if (WriteToDisk)
      {
        SaveAsDBF(TIME.P, "Time", "Primary",
                  paste0(Active.Subtype, "_", f), OW, pol, SeqParts[p])
        if (Active.Subprofile$Dynamics == "dynamic")
        {
          SaveAsDBF(TIME.S, "Time", "Secondary",
                    paste0(Active.Subtype, "_", f), OW, pol, SeqParts[p])
          SaveAsDBF(TIME.T1, "Time", "T1",
                    paste0(Active.Subtype, "_", f), OW, pol, SeqParts[p])
          SaveAsDBF(TIME.T2, "Time", "T2",
                    paste0(Active.Subtype, "_", f), OW, pol, SeqParts[p])
        }
      }
      
      
    } # closing p(arts)
    
    
    print(paste0("Starting fragment ", f, " of ", Fragments))
    
    if (exists("TIME.P")) {rm(TIME.P)}
    if (exists("TIME.S")) {rm(TIME.S)}
    if (exists("TIME.T1")) {rm(TIME.T1)}
    if (exists("TIME.T2")) {rm(TIME.T2)}
    
    # attach TIME parts
    TIME.P = DBFreader("Time", "Primary", PPH.P, YearDates.Sub, FolderName)
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      TIME.S = DBFreader("Time", "Secondary", PPH.P, YearDates.Sub, FolderName)
      TIME.T1 = DBFreader("Time", "T1", PPH.P, YearDates.Sub, FolderName)
      TIME.T2 = DBFreader("Time", "T2", PPH.P, YearDates.Sub, FolderName)
    }
    
    if (exists("HOURS.P")) {rm(HOURS.P)}
    if (exists("HOURS.S")) {rm(HOURS.S)}
    if (exists("HOURS.T1")) {rm(HOURS.T1)}
    if (exists("HOURS.T2")) {rm(HOURS.T2)}
    
    # Hours of the year
    HOURS.P = HourOfTheYear7(year.active, TIME.P, 0)
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      HOURS.S = HourOfTheYear7(year.active, TIME.S, 0)
      HOURS.T1 = HourOfTheYear7(year.active, TIME.T1, 0)
      HOURS.T2 = HourOfTheYear7(year.active, TIME.T2, 0)
    }
    
    if (exists("HoP.P")) {rm(HoP.P)}
    if (exists("HoP.S")) {rm(HoP.S)}
    if (exists("HoP.T1")) {rm(HoP.T1)}
    if (exists("HoP.T2")) {rm(HoP.T2)}
    
    # Detect which hours belong to which points | change name systematically to HoP (Hour of Point) = HoP.P etc.
    HOP = WhichHourForWhichPoint(PPH.P, Time.Sub, HOURS.P, HOURS.S, HOURS.T1, HOURS.T2,
                                 Print = FALSE, Active.Subprofile, SeqFragment , f) 
    HoP.P = HOP[[1]]
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      HoP.S = HOP[[2]]
      HoP.T1 = HOP[[3]]
      HoP.T2 = HOP[[4]]
    }
    rm(HOP)
    
    ## Interpolating the points
    start.time = Sys.time()
    ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P, PPH.S, PPH.T1.PNT.RS, PPH.T2.PNT.RS,
                                                Points.NoVal, PolDir, Plot = FALSE, pol,
                                                StartHour = SeqFragment[f]*24+1, # Time[(SeqFragment[f]*24+1)]
                                                EndHour = (SeqFragment[f+1])*24, # Time[(SeqFragment[f+1])*24]
                                                HOURS.P, HOURS.S, HOURS.T1, HOURS.T2, 50,
                                                HoP.P, HoP.S, HoP.T1, HoP.T2, Active.Subprofile, SeqFragment[f])
    ExposureValue.P = ExposureValue.All[[1]]
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      ExposureValue.S = ExposureValue.All[[2]]
      ExposureValue.T1 = ExposureValue.All[[3]]
      ExposureValue.T2 = ExposureValue.All[[4]]
    }
    rm(ExposureValue.All)
    end.time = Sys.time()
    TimeTakenInterpolation[f] = end.time - start.time
    print(TimeTakenInterpolation[f])
    
    rm(HOURS.P, HOURS.S, HOURS.T1, HOURS.T2)
    rm(HoP.P, HoP.S, HoP.T1, HoP.T2)
    rm(PPH.T1.PNT.RS, PPH.T2.PNT.RS)
    
    # Write EXP to disk
    WriteToDisk = TRUE
    OW = FALSE
    if (WriteToDisk)
    {
      SaveAsDBF(ExposureValue.P, "Exposure", "Primary",
                paste0(Active.Subtype, "_", f), OW, pol, 0)
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        SaveAsDBF(ExposureValue.S, "Exposure", "Secondary",
                  paste0(Active.Subtype, "_", f), OW, pol, 0)
        SaveAsDBF(ExposureValue.T1, "Exposure", "T1",
                  paste0(Active.Subtype, "_", f), OW, pol, 0)
        SaveAsDBF(ExposureValue.T2, "Exposure", "T2",
                  paste0(Active.Subtype, "_", f), OW, pol, 0)
      }
    }
    
    gc()
    
  } # closing f(ragments)
  
  # # Connect TIME and EXP
  # 
  # TIME.P = list(list())
  # TIME.S = list(list())
  # TIME.T1 = list(list())
  # TIME.T2 = list(list())
  # ExposureValue.P = list(list())
  # ExposureValue.S = list(list())
  # ExposureValue.T1 = list(list())
  # ExposureValue.T2 = list(list())
  # 
  # for (i in seq_along(PPH.P))
  # {
  #   TIME.P[[i]] = rep(list(), length(YearDates)) # use same structure
  #   TIME.S[[i]] = rep(list(), length(YearDates)) # use same structure
  #   TIME.T1[[i]] = rep(list(), length(YearDates)) # use same structure
  #   TIME.T2[[i]] = rep(list(), length(YearDates)) # use same structure
  #   ExposureValue.P[[i]] = rep(list(), length(YearDates)) # use same structure
  #   ExposureValue.S[[i]] = rep(list(), length(YearDates)) # use same structure
  #   ExposureValue.T1[[i]] = rep(list(), length(YearDates)) # use same structure
  #   ExposureValue.T2[[i]] = rep(list(), length(YearDates)) # use same structure
  # }
  
  for (f in 1:Fragments)
    #for (f in Fragments)
  {
    YearDates.Sub = YearDates2(year.active)[(SeqFragment[f]+1):(SeqFragment[f+1])]
    Time.Sub = Time[Time > YearDates.Sub[1] & Time <= (tail(YearDates.Sub,1) + 24*60**2)]
    
    # read from file
    TIME.P_F = DBFreader("Time", "Primary", PPH.P, YearDates.Sub, FolderName)
    TIME.S_F = DBFreader("Time", "Secondary", PPH.P, YearDates.Sub, FolderName)
    TIME.T1_F = DBFreader("Time", "T1", PPH.P, YearDates.Sub, FolderName)
    TIME.T2_F = DBFreader("Time", "T2", PPH.P, YearDates.Sub, FolderName)
    ExposureValue.P_F = DBFreader("Exposure", "Primary", PPH.P, YearDates.Sub, FolderName, pol)
    ExposureValue.S_F = DBFreader("Exposure", "Secondary", PPH.P, YearDates.Sub, FolderName, pol)
    ExposureValue.T1_F = DBFreader("Exposure", "T1", PPH.P, YearDates.Sub, FolderName, pol)
    ExposureValue.T2_F = DBFreader("Exposure", "T2", PPH.P, YearDates.Sub, FolderName, pol)
    
    
    # Check if the lengths of TIME and EXP match
    if (length(unlist(TIME.P_F)) != length(unlist(ExposureValue.P_F)) |
        length(unlist(TIME.S_F)) != length(unlist(ExposureValue.S_F)) |
        length(unlist(TIME.T1_F)) != length(unlist(ExposureValue.T1_F)) |
        length(unlist(TIME.T2_F)) != length(unlist(ExposureValue.T2_F)))
    {
      stop(paste("Lengths of TIME and EXP do not match."))
    } #else 
    # {
    #   print(paste0("Lengths of TIME and EXP match."))
    # }
    
    # Hours of the year
    HOURS.P_F = HourOfTheYear7(year.active, TIME.P_F, 0)
    HOURS.S_F = HourOfTheYear7(year.active, TIME.S_F, 0)
    HOURS.T1_F = HourOfTheYear7(year.active, TIME.T1_F, 0)
    HOURS.T2_F = HourOfTheYear7(year.active, TIME.T2_F, 0)
    
    # Detect which hours belong to which points | change name systematically to HoP (Hour of Point) = HoP.P etc.
    HOP = WhichHourForWhichPoint(PPH.P, Time.Sub, HOURS.P_F, HOURS.S_F, HOURS.T1_F, HOURS.T2_F,
                                 Print = FALSE, Active.Subprofile, SeqFragment , f) 
    HoP.P_F = HOP[[1]]
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      HoP.S_F = HOP[[2]]
      HoP.T1_F = HOP[[3]]
      HoP.T2_F = HOP[[4]]
    }
    rm(HOP)
    
    ST.DF.P_F = DF.Structure2(PPH.P, TIME.P_F, TIME.P_F, ExposureValue.P_F) #, rm.na = TRUE
    ST.DF.S_F = DF.Structure2(PPH.P, TIME.P_F, TIME.S_F, ExposureValue.S_F)
    ST.DF.T1_F = DF.Structure2(PPH.P, TIME.P_F, TIME.T1_F, ExposureValue.T1_F)
    ST.DF.T2_F = DF.Structure2(PPH.P, TIME.P_F, TIME.T2_F, ExposureValue.T2_F)
    
    # Save DF structure
    OW = TRUE
    SaveAsDBF(ST.DF.P_F, "DF", "Primary", paste0(Active.Subtype, "_", f), OW, pol, 0)
    SaveAsDBF(ST.DF.S_F, "DF", "Secondary", paste0(Active.Subtype, "_", f), OW, pol, 0)
    SaveAsDBF(ST.DF.T1_F, "DF", "T1", paste0(Active.Subtype, "_", f), OW, pol, 0)
    SaveAsDBF(ST.DF.T2_F, "DF", "T2", paste0(Active.Subtype, "_", f), OW, pol, 0)
    
    # stats.EXP.P_F = DF.Stats(ST.DF.P_F)
    # stats.EXP.S_F = DF.Stats(ST.DF.S)
    # stats.EXP.T1_F = DF.Stats(ST.DF.T1)
    # stats.EXP.T2_F = DF.Stats(ST.DF.T2)
    
    ExposureValueCombined_F = ToHourValues(PPH.P, Time.Sub, ExposureValue.P_F, ExposureValue.S_F, ExposureValue.T1_F, ExposureValue.T2_F,
                                           TIME.P_F, TIME.S_F, TIME.T1_F, TIME.T2_F, HoP.P_F, HoP.S_F, HoP.T1_F, HoP.T2_F)
    
    
    length(Time.Sub) * length(PPH.P) == length(unlist(ExposureValueCombined_F))
    
    
    ST.DF.HR_F = DF.Structure2(PPH.P, TIME.P_F, Time.Sub, ExposureValueCombined_F, rm.na = FALSE)
    #stats.EXP.HR_F = DF.Stats(ST.DF.HR_F)
    
    # Validity check
    if (nrow(ST.DF.HR_F) != length(Time.Sub) * length(PPH.P))
    {
      paste("missing", (length(Time.Sub) * length(PPH.P) - nrow(ST.DF.HR_F)), "values!")
    } else
    {
      paste("valid!")
    }
    
    # ValCheck = NA
    # for (i in seq_along(PPH.P))
    # {
    #   ValCheck[i] = nrow(ST.DF.HR_F[ST.DF.HR_F$IND == i,])
    # }
    # 
    # Flawed = which(ValCheck != length(Time.Sub))
    # 
    # PPH.T1[Flawed,]@data
    # 
    # mean(PPH.T1[Flawed,]@data$distance)
    # mean(PPH.T1@data$distance)
    # 
    # length(unlist(HoP.P_F))
    # 
    # HoP.P_F[[Flawed[1]]]
    # HoP.P_F[[Flawed[1]]][[832]]
    # TIME.P_F[[Flawed[1]]][[ceiling(832/24)]][HoP.P_F[[Flawed[1]]][[832]]]
    # ExposureValue.P_F[[Flawed[1]]][[ceiling(832/24)]][HoP.P_F[[Flawed[1]]][[832]]]
    # 
    
    
    # Save Hourly corrected
    OW = TRUE
    SaveAsDBF(ST.DF.HR_F, "HR", "HR", paste0(Active.Subtype, "_", f), OW, pol, 0)
    
    rm(YearDates.Sub, Time.Sub, TIME.P_F, TIME.S_F, TIME.T1_F, TIME.T2_F,
       HOURS.P_F, HOURS.S_F, HOURS.T1_F, HOURS.T2_F,
       HoP.P_F, HoP.S_F, HoP.T1_F, HoP.T2_F,
       ExposureValue.P_F, ExposureValue.S_F, ExposureValue.T1_F, ExposureValue.T2_F,
       ST.DF.P_F, ST.DF.S_F, ST.DF.T1_F, ST.DF.T2_F,
       ExposureValueCombined_F, ST.DF.HR_F)
    gc()
  }
}
} else # close dynamic, start static
{
  
  #! Detect if the TIME (and EXP) dbf's already exist
  
  
  TIME = CreateCorrespondingDateAndTime(Active.Type, Active.Subprofile, PPH.P,
                                        YearDates, year.active)
  TIME.P = TIME[[2]]
  rm(TIME)
  gc()
  
  # Write TIME to disk
  WriteToDisk = TRUE
  OW = FALSE
  if (WriteToDisk)
  {
    SaveAsDBF(TIME.P, "Time", "Primary", Active.Subtype, OW, pol, 0)
  }
  
  #! When TIME dbf's exist
  
  TIME.P = DBFreader("Time", "Primary", PPH.P, YearDates, Active.Subtype)
  
  # Hours of the year
  HOURS.P = HourOfTheYear7(year.active, TIME.P, 0)
  
  # Detect which hours belong to which points | change name systematically to HoP (Hour of Point) = HoP.P etc.
  HOP = WhichHourForWhichPoint(PPH.P, Time, HOURS.P, Print = FALSE,
                               Active.Subprofile = Active.Subprofile, SeqFragment = 0)
  HoP.P = HOP[[1]]
  rm(HOP)
  gc()
  
  
  ## Interpolating the points
  start.time = Sys.time()
  ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P = PPH.P, POL = Points.NoVal,
                                              PolDir = PolDir,
                                              Plot = FALSE, pol = pol,
                                              StartHour = 1, EndHour = length(Time),
                                              HOURS.P = HOURS.P,  NearestPoints = 50,
                                              wP = HoP.P,
                                              Active.Subprofile = Active.Subprofile,
                                              seq = 0)
  ExposureValue.P = ExposureValue.All[[1]]
  rm(ExposureValue.All)
  end.time = Sys.time()
  TimeTakenInterpolation = end.time - start.time
  print(TimeTakenInterpolation)
  
  # Write EXP to disk
  WriteToDisk = TRUE
  OW = FALSE
  if (WriteToDisk)
  {
    SaveAsDBF(ExposureValue.P, "Exposure", "Primary", Active.Subtype, OW, pol, 0)
  }
  
  ExposureValue.P = DBFreader("Exposure", "Primary", PPH.P, YearDates, Active.Subtype, pol)
  
  
  # HR structure (skip ST.DF.P in static PPH... ST.DF.P = ST.DF.HR)
  ST.DF.HR = DF.Structure2(PPH.P, TIME.P, Time, ExposureValue.P)
  
  SaveAsDBF(ST.DF.HR, "HR", "HR", Active.Subtype, OW, pol, 0)
  
  
  ST.DF.HR = 
    
    
} # close static





# for (i in seq_along(PPH.P))
# {
#   TIME.P[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = TIME.P_F[[i]]
#   TIME.S[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = TIME.S_F[[i]]
#   TIME.T1[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = TIME.T1_F[[i]]
#   TIME.T2[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = TIME.T2_F[[i]]
#   ExposureValue.P[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = ExposureValue.P_F[[i]]
#   ExposureValue.S[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = ExposureValue.S_F[[i]]
#   ExposureValue.T1[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = ExposureValue.T1_F[[i]]
#   ExposureValue.T2[[i]][(SeqFragment[f]+1):(SeqFragment[f+1])] = ExposureValue.T2_F[[i]]
# }


#Read DBF file with TIME 
TIME.P = DBFreader("Time", "Primary", PPH.P, YearDates, BusinesDates, Active.Subtype)
if (Active.Subprofile$Dynamics == "dynamic")
{
  TIME.S = DBFreader("Time", "Secondary", PPH.P, YearDates, BusinesDates, Active.Subtype)
  TIME.T1 = DBFreader("Time", "T1", PPH.P, YearDates, BusinesDates, Active.Subtype)
  TIME.T2 = DBFreader("Time", "T2", PPH.P, YearDates, BusinesDates, Active.Subtype)
}

# Hours of the year
HOURS.P = HourOfTheYear7(year.active, TIME.P, 0)
if (Active.Subprofile$Dynamics == "dynamic")
{
  HOURS.S = HourOfTheYear7(year.active, TIME.S, 0)
  HOURS.T1 = HourOfTheYear7(year.active, TIME.T1, 0)
  HOURS.T2 = HourOfTheYear7(year.active, TIME.T2, 0)
  #HOURS.T1_3d = HourOfTheYear7(year.active, TIME.T1, 3)
  #HOURS.T2_3d = HourOfTheYear7(year.active, TIME.T2, 3)
}

#install.packages("pryr")
library(pryr)
mem_used()


# # some test plots
# i = 10
# day = 5
# plot(PPH.T1[i,])
# points(PPH.T1.Pnt.Li[[i]], pch = "+", col = "gray")
# points(PPH.T1.Pnt.Li[[i]][1,], pch = "O", col = "gray")
# points(PPH.P[i,], col = "green", pch = "O")
# points(PPH.S[i,], col = "orange", pch = "O")
# 
# Clr.RIO = rgb(red=0.8, green=0.8, blue=0.8, alpha=0.75)
# # points(Points.NoVal, pch = "+", col = Clr.RIO)
# 
# points(PPH.T1.Pnt.eq.Li[[i]], col = "blue")
# points(PPH.T1.Pnt.eq.Li[[i]][4,], col = "blue", pch = "O")
# 
# points(PPH.T1.PNT.RS[[i]][[day]], col = "orange")
# points(PPH.T1.PNT.RS[[i]][[day]][5,], col = "red", pch = "O")
# 
# # select proximity coordinates
# inds = knnLookup(tree, newdat = coordinates(PPH.P[i,]), k = 50) # gives the matrix
# inds = as.vector(inds)
# POL.sel = POL[inds,]
# points(POL.sel, pch = "+", col = Clr.RIO)
# 
# inds = knnLookup(tree, newdat = coordinates(PPH.S[i,]), k = 50) # gives the matrix
# inds = as.vector(inds)
# POL.sel = POL[inds,]
# points(POL.sel, pch = "+", col = Clr.RIO)
# 
# inds = knnLookup(tree, newdat = coordinates(PPH.T1.PNT.RS[[i]][[day]]), k = 50) # gives the matrix
# inds = as.vector(inds)
# POL.sel = POL[inds,]
# points(POL.sel, pch = "+", col = Clr.RIO)
# 
# CoordsOfInterest = PPH.T1.Pnt[[i]][[day]][v,]@coords
# inds = knnLookup(tree, newdat = CoordsOfInterest, k = NearestPoints)
# inds = as.vector(inds)
# POL.sel = POL[inds,]
# plot(POL.sel, pch = "+", col = Clr.RIO)
# Exp.T1 = unlist(akima::interp(x = POL.sel@coords[,1], y = POL.sel@coords[,2], z = unlist(POL.sel@data[,1]),
#                               xo = CoordsOfInterest[1], yo = CoordsOfInterest[2], extrap = FALSE, duplicate = "strip",
#                               linear = TRUE))[3]
# 
# points(PPH.T1.PNT.RS[[i]][[day]][17], col="purple", font = 2, pch = 19)
# text(PPH.T1.PNT.RS[[i]][[day]][17], labels = round(Exp.T1,3), pos = 1, cex = 1, font = 2, col = "purple")
# 
# points(PPH.T1.PNT.RS[[i]][[day]][17])



#   # Order the PPH.S to match [i,]
#   PPH.S.Li = list()
#   for (i in seq_along(PPH.P))
#   {
#     WS = which(PPH.S@data$object_id %in% PPH.T1@data$dst[i])
#     PPH.S.Li[[i]] = PPH.S[WS[1],]
#   }
#   #points(PPH.S.Li[[i]], col = "purple", pch = "L")
#   PPH.S = do.call(rbind, PPH.S.Li)



#rm(dir.P, dir.S, dir.T1f, dir.T1s, dir.T2f, dir.T2s)


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




# SaveAsFile(Points.NoVal, "RIO_IFDM", "GeoJSON", TRUE)

# Detect which hours belong to which points | change name systematically to HoP (Hour of Point) = HoP.P etc.
HOP = WhichHourForWhichPoint(PPH.P, Time, HOURS.P, HOURS.S, HOURS.T1, HOURS.T2,
                             Print = FALSE, Active.Subprofile) 
wP = HOP[[1]] # HoP.P = HOP[[1]]
if (Active.Subprofile$Dynamics == "dynamic")
{
  wS = HOP[[2]] # HoP.S = HOP[[2]]
  wT1 = HOP[[3]] # HoP.T1 = HOP[[3]]
  wT2 = HOP[[4]] # HoP.T2 = HOP[[4]]
}
rm(HOP)

## Interpolating the points

start.time = Sys.time()
ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P, PPH.S, PPH.T1.PNT.RS, PPH.T2.PNT.RS,
                                            Points.NoVal, PolDir, Plot = FALSE, pol,
                                            StartHour = 1, EndHour = length(Time),
                                            #StartHour = 5*24+1, EndHour = 6*24,
                                            #StartHour = 1, EndHour = 1*24,
                                            HOURS.P, HOURS.S, HOURS.T1, HOURS.T2, 50,
                                            wP, wS, wT1, wT2, Active.Subprofile) # HoP.P, HoP.S, HoP.T1, HoP.T2, Active.Subprofile)
ExposureValue.P = ExposureValue.All[[1]]
if (Active.Subprofile$Dynamics == "dynamic")
{
  ExposureValue.S = ExposureValue.All[[2]]
  ExposureValue.T1 = ExposureValue.All[[3]]
  ExposureValue.T2 = ExposureValue.All[[4]]
}
end.time = Sys.time()
print(end.time - start.time)

# Write EXP to disk
WriteToDisk = TRUE
OW = TRUE
if (WriteToDisk)
{
  SaveAsDBF(ExposureValue.P, "Exposure", "Primary", YearDates, Active.Subtype, OW, pol)
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    SaveAsDBF(ExposureValue.S, "Exposure", "Secondary", YearDates, Active.Subtype, OW, pol)
    SaveAsDBF(ExposureValue.T1, "Exposure", "T1", YearDates, Active.Subtype, OW, pol)
    SaveAsDBF(ExposureValue.T2, "Exposure", "T2", YearDates, Active.Subtype, OW, pol)
  }
}

#Read DBF file with Exposurevalues 
ExposureValue.P = DBFreader("Exposure", "Primary", PPH.P, YearDates, BusinesDates, Active.Subtype, pol)
if (Active.Subprofile$Dynamics == "dynamic")
{
  ExposureValue.S = DBFreader("Exposure", "Secondary", PPH.P, YearDates, BusinesDates, Active.Subtype, pol)
  ExposureValue.T1 = DBFreader("Exposure", "T1", PPH.P, YearDates, BusinesDates, Active.Subtype, pol)
  ExposureValue.T2 = DBFreader("Exposure", "T2", PPH.P, YearDates, BusinesDates, Active.Subtype, pol)
}

## Data Frame structure and stats

rm(ST.DF.P, ST.DF.S, ST.DF.T1, ST.DF.T2,
   stats.EXP.P, stats.EXP.S, stats.EXP.T1, stats.EXP.T2)

ST.DF.P = DF.Structure2(PPH.P, TIME.P, TIME.P, ExposureValue.P)
stats.EXP.P = DF.Stats(ST.DF.P)
if (Active.Subprofile$Dynamics == "dynamic")
{
  ST.DF.S = DF.Structure2(PPH.P, TIME.P, TIME.S, ExposureValue.S)
  ST.DF.T1 = DF.Structure2(PPH.P, TIME.P, TIME.T1, ExposureValue.T1)
  ST.DF.T2 = DF.Structure2(PPH.P, TIME.P, TIME.T2, ExposureValue.T2)
  
  stats.EXP.S = DF.Stats(ST.DF.S)
  stats.EXP.T1 = DF.Stats(ST.DF.T1)
  stats.EXP.T2 = DF.Stats(ST.DF.T2)
}


## Temporal aggregation (only for dynamic profiles)
if (Active.Subprofile$Dynamics == "static")
{
  ST.DF.HR = ST.DF.P
  stats.EXP.HR = stats.EXP.P
}

if (Active.Subprofile$Dynamics == "dynamic")
{
  ExposureValueCombined = ToHourValues(PPH.P, Time, ExposureValue.P, ExposureValue.S, ExposureValue.T1, ExposureValue.T2,
                                       TIME.P, TIME.S, TIME.T1, TIME.T2, wP, wS, wT1, wT2)
  #   for (i in seq_along(PPH.P))
  #   {
  #     print(head(ExposureValueCombined[[i]], 5*24))
  #   }
  #   head(ExposureValueCombined[[66]], 7*24)
  
  ST.DF.HR = DF.Structure2(PPH.P, TIME.P, Time, ExposureValueCombined)
  stats.EXP.HR = DF.Stats(ST.DF.HR)
}


# Remove from memory/environment
rm(dir.P, PPH.P, TIME, HOURS.P, HOP, HoP.P, ExposureValue.P)
if (Active.Subprofile$Dynamics == "dynamic")
{
  rm(dir.S, dir.T1, dir.T2)
  rm(PPH.S, PPH.T1, PPH.T2)
  
  rm(TIME.P, TIME.S, TIME.T1, TIME.T2)
  rm(HOURS.S, HOURS.T1, HOURS.T2)
  
  rm(wS ,wT1, wT2)
  
  rm(ExposureValue.S, ExposureValue.T1, ExposureValue.T2)
}


#Fragments = 1:10

# dynamic (merge)
ST.DF.HR.Li = list()

if (file.exists(file.path(output.dir, paste0(Active.Subtype, "_", f))))
{
  for (f in 1:Fragments)
  {
    ST.DF.HR.Li[[f]] = read.dbf(file.path(output.dir, paste0(Active.Subtype, "_", f), paste0("HR_", toupper(pol),".dbf")))
  }
  ST.DF.HR = do.call(rbind, ST.DF.HR.Li)
  rm(ST.DF.HR.Li)
  gc()
}

# static
ST.DF.HR = read.dbf(file.path(output.dir, Active.Subtype, paste0("HR_", toupper(pol),".dbf")))


# Subset biweekly
ST.DF.HR.BiWe = ST.DF.HR[ST.DF.HR$TIME >= BIWEEKLY[[1]][1] & ST.DF.HR$TIME <= BIWEEKLY[[1]][14] |
                           ST.DF.HR$TIME >= BIWEEKLY[[2]][1] & ST.DF.HR$TIME <= BIWEEKLY[[2]][14] |
                           ST.DF.HR$TIME >= BIWEEKLY[[3]][1] & ST.DF.HR$TIME <= BIWEEKLY[[3]][14],]

## Individual based calculations
# Mean calculations
for (i in seq_along(PPH.P))
{
  if (Active.Subprofile$Subtype == "01.OW_WS1") # change to WS
  {
    if (!exists("OW_WS1.INDmean")) { OW_WS1.INDmean = NA }
    if (!exists("OW_WS2.INDmean")) { OW_WS2.INDmean = NA }
    if (!exists("OW_WS1.INDsum")) { OW_WS1.INDsum = NA }
    
    OW_WS1.INDmean[i] = mean(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
    OW_WS2.INDmean[i] = mean(ST.DF.HR.BiWe[ST.DF.HR.BiWe$IND == i,]$EXP, na.rm = TRUE)
    # OW_WS1.INDmax[i] = max(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
    # OW_WS2.INDmax[i] = max(ST.DF.HR.BiWe[ST.DF.HR.BiWe$IND == i,]$EXP, na.rm = TRUE)
    OW_WS1.INDsum[i] = sum(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
  }
  
  if (Active.Subprofile$Subtype == "01.OW_C1") # change to C
  {
    if (!exists("OW_C1.INDmean")) { OW_C1.INDmean = NA }
    if (!exists("OW_C2.INDmean")) { OW_C2.INDmean = NA }
    
    OW_C1.INDmean[i] = mean(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
    OW_C2.INDmean[i] = mean(ST.DF.HR.BiWe[ST.DF.HR.BiWe$IND == i,]$EXP, na.rm = TRUE)
  }
  
  if (Active.Subprofile$Subtype == "02.HO_WS1")
  {
    if (!exists("HO_WS1.INDmean")) { HO_WS1.INDmean = NA }
    if (!exists("HO_WS2.INDmean")) { HO_WS2.INDmean = NA }
    
    HO_WS1.INDmean[i] = mean(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
    HO_WS2.INDmean[i] = mean(ST.DF.HR.BiWe[ST.DF.HR.BiWe$IND == i,]$EXP, na.rm = TRUE)
  }
  
  if (Active.Subprofile$Subtype == "03.SP_WS1")
  {
    if (!exists("SP_WS1.INDmean")) { SP_WS1.INDmean = NA }
    if (!exists("SP_WS2.INDmean")) { SP_WS2.INDmean = NA }
    
    SP_WS1.INDmean[i] = mean(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
    SP_WS2.INDmean[i] = mean(ST.DF.HR.BiWe[ST.DF.HR.BiWe$IND == i,]$EXP, na.rm = TRUE)
  }
  
  if (Active.Subprofile$Subtype == "03.SP_C1")
  {
    if (!exists("SP_C1.INDmean")) { SP_C1.INDmean = NA }
    if (!exists("SP_C2.INDmean")) { SP_C2.INDmean = NA }
    
    SP_C1.INDmean[i] = mean(ST.DF.HR[ST.DF.HR$IND == i,]$EXP, na.rm = TRUE)
    SP_C2.INDmean[i] = mean(ST.DF.HR.BiWe[ST.DF.HR.BiWe$IND == i,]$EXP, na.rm = TRUE)
  }
}


# Plot and save the (13) Correlation plot graphs
CorPlotGraphAndSave("01.OW", OW_WS1.INDmean, OW_WS2.INDmean, "WS1", "WS2", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("01.OW", OW_WS1.INDmean, OW_C1.INDmean, "WS1", "C1", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("01.OW", OW_WS1.INDmean, OW_C2.INDmean, "WS1", "C2", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("01.OW", OW_WS2.INDmean, OW_C1.INDmean, "WS2", "C1", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("01.OW", OW_WS2.INDmean, OW_C2.INDmean, "WS2", "C2", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("01.OW", OW_C1.INDmean, OW_C2.INDmean, "C1", "C2", Width = 720, Height = 480, pol = toupper(pol))

CorPlotGraphAndSave("02.HO", HO_WS1.INDmean, HO_WS2.INDmean, "WS1", "WS2", Width = 720, Height = 480, pol = toupper(pol))

CorPlotGraphAndSave("03.SP", SP_WS1.INDmean, SP_WS2.INDmean, "WS1", "WS2", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("03.SP", SP_WS1.INDmean, SP_C1.INDmean, "WS1", "C1", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("03.SP", SP_WS1.INDmean, SP_C2.INDmean, "WS1", "C2", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("03.SP", SP_WS2.INDmean, SP_C1.INDmean, "WS2", "C1", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("03.SP", SP_WS2.INDmean, SP_C2.INDmean, "WS2", "C2", Width = 720, Height = 480, pol = toupper(pol))
CorPlotGraphAndSave("03.SP", SP_C1.INDmean, SP_C2.INDmean, "C1", "C2", Width = 720, Height = 480, pol = toupper(pol))

# Plot and save the (3) Correlation plot tables
CorPlotTableAndSave("01.OW", OW_WS1.INDmean, OW_WS2.INDmean, OW_C1.INDmean, OW_C2.INDmean, Width = 720, Height = 480, pol = toupper(pol))
CorPlotTableAndSave("02.HO", HO_WS1.INDmean, HO_WS2.INDmean, Width = 720, Height = 480, pol = toupper(pol))
CorPlotTableAndSave("03.SP", SP_WS1.INDmean, SP_WS2.INDmean, SP_C1.INDmean, SP_C2.INDmean, Width = 720, Height = 480, toupper(pol))

# CorPlotTable("01.OW", "Numbers", OW_WS1.INDmean, OW_WS2.INDmean, OW_C1.INDmean, OW_C2.INDmean)
# CorPlotTable("02.HO", "Numbers", HO_WS1.INDmean, HO_WS2.INDmean)
# CorPlotTable("03.SP", "Numbers", SP_WS1.INDmean, SP_WS2.INDmean, SP_C1.INDmean, SP_C2.INDmean)



#? Correlation on HR based values
DF.WS1.collect = data.frame(cbind(OW_WS1.ST.DF.HR, HO_WS1.ST.DF.HR, SP_WS1.ST.DF.HR))
C.WS1 = cor(DF.WS1.collect)
corrplot(C.WS1, method="ellipse")
corrplot(C.WS1, method="number", number.digits = 5)



plot(WS1.INDmean, WS2.INDmean, main = paste(Active.Subtype,"WS1 vs. WS2 (µg/m³)", length(PPH.P), "individuals") , pch = "+")
R.squared = cor(WS1.INDmean, WS2.INDmean)**2
text(min(WS1.INDmean), max(WS1.INDmean), pos = 1, labels = "R²:", font = 2)
text(min(WS1.INDmean)+4, max(WS1.INDmean), pos = 1, labels = R.squared)


plot(WS1.INDmax, WS2.INDmax, main = "WS1 vs. WS2 (µg/m³)", pch = "+")



# Add fit lines
abline(lm(WS1.INDmean~WS2.INDmean), col="red") # regression line (y~x)
lines(lowess(WS2.INDmean,WS1.INDmean), col="blue") # lowess line (x,y) 

plot(WS1.INDmean, WS2.INDmean)
str(summary(M.lm))

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

## Undo system settings changes
Sys.setenv(TZ = OriginalTimezone)




# 
# 
# 
# ## Summary calculations
# 
# # Place weights on Transport vertices
# WEIGHTS.T1 = WeightCommutingRouteVertices(HOURS.T1_3d, HOURS.P, Leave.P)
# WEIGHTS.T2 = WeightCommutingRouteVertices(HOURS.T2_3d, HOURS.S, Leave.S)
# 
# WEIGHTS.T1[[1]]
# sum(WEIGHTS.T1[[1]])
# tail(HOURS.T1_3d[[1]][[1]], n=1) - HOURS.T1_3d[[1]][[1]][1]
# 
# 
# 
# # Mean per day
# HO.02.mean. = Weighted.Static(ExposureValue.P, "WeightedMean.Day")
# 
# # Mean per year (per individual)
# HO.02_1 = mean(HO.02[[1]])
# HO.02_2 = mean(HO.02[[2]])
# HO.02_281 = mean(HO.02.mean.[[281]])
# 
# # Cummulative sum per day
# HO.02.CumSum.Day = Weighted.Static(ExposureValue.P, "CumSum.Day")
# 
# TIME.unlisted.P = TIME.P
# ExposureValueCum.P = ExposureValue.P # use same structure
# ExposureValueCumYear.P = NA
# ExposureValue.unlisted.P = ExposureValue.P # use same structure
# 
# 
# for (i in seq_along(ExposureValue.P)) # per individual
# {
#   ExposureValueCum.P[[i]] = cumsum(unlist(ExposureValue.P[[i]]))
#   TIME.unlisted.P[[i]] = unlist(TIME.P[[i]])
#   
#   ExposureValueCumYear.P[i] = tail(ExposureValueCum.P[[i]],1)
#   
#   ExposureValue.unlisted.P[[i]] = unlist(ExposureValue.P[[i]])
# }
# 
# hist(ExposureValueCumYear.P,100)
# 
# Plot.CumExposureGraph(1:300)
# Plot.CumExposureGraph(1:length(ExposureValue.P))
# 
# 
# # daily average (24h)
# for (i in seq_along(ExposureValue.unlisted.P))
# {
#   
# }
# 
# # slope method (using cumulative)
# Standard.24h = 35
# Hours = 24
# norm = Standard.24h * Hours
# 
# ExposureValueDiff = ExposureValueCum.P # use same structure
# ExposureValueDiff2 = ExposureValueDiff
# 
# #for (i in seq_along(ExposureValueCum.P))
# for (i in seq(1,3))
# {
#   ExposureValueDiff[[i]] = NA
#   ExposureValueDiff2[[i]] = NA
#   for (h in seq_along(ExposureValueCum.P[[i]]))
#   {
#     diff = ExposureValueCum.P[[i]][h+Hours-1]-ExposureValueCum.P[[i]][h]
#     ExposureValueDiff[[i]][h] = diff > norm # only first hour of serie
#     
#     #ExposureValueDiff2[[i]][h:(h+Hours-1)] = ExposureValueDiff[[i]][h] # for complete serie
#     
#   }
# }
# 
# for (o in seq(1, length(ExposureValueCum.P[[3]])-(Hours-1)))
# {
#   ExposureValueDiff2[[3]][o:(o+Hours-1)] = ExposureValueDiff[[3]][o] # for complete serie
# }
# 
# ExposureValueDiff[[3]][50:100]
# ExposureValueDiff2[[3]][50:100]
# 
# ExposureValueCum.P[[3]][64]
# ExposureValueCum.P[[3]][64+24-1]
# 
# ExposureValueCum.P[[3]][64+24-1] - ExposureValueCum.P[[3]][64]
# 
# ExposureValueDiff[[3]][65]
# ExposureValueDiff2[[3]][65]
# 
# ExposureValueDiff[[3]][64:(64+Hours-1)]
# ExposureValueDiff2[[3]][64:(64+Hours-1)] = ExposureValueDiff[[3]][64]
# 
# ExposureValueDiff2[[3]][64:(64+Hours-1+10)]
# 
# Plot.CumExposureGraph(1:10, "monthly")
# 
# length(which(ExposureValueDiff[[3]] == T))
# 
# NumericWS = 1:10