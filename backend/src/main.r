# Main script for the Geoscientific Information System for Calculating Air
# Pollution Exposure (GISCAPE). GISCAPE simulates personal place histories,
# based on activity-patterns with 4 location phases: Primary (residence),
# Secondary (workplace/school), Transport out and Transport in. These simu-
# lated personal place histories are then combined with high-resolution
# air quality data Nitrogen Dioxide (NO2) and Fine Particulate Matter (PM2.5)
# to produce individual exposure values.
# Copyright (C) 2017-2018 William Schuch
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

## TESTED ON WINDOWS 7 (64-bit), 8GB RAM, R v3.3.3, Timezone UTC+01:00
## TESTED ON WINDOWS 10 (64-bit), 4GB RAM, R v3.4.0, Timezone UTC+01:00

## TODO:  - Improve readability;
##        - Test reproducibility of main exposure calculation (PC1 vs. PC2);
##        - Support OneDrive download for AQ data;
##        - Test DownloadInputFilesFromIrcelineFTP when bz2-files are (partly) missing;
##        - ...

## Clear the workspace?
Clear.WorkSpace = TRUE
if (Clear.WorkSpace)
{
  rm(list = ls())
  gc()
}

## Note 1: This script only brings different modules in modules/ together.
## No code belonging to one of the modules should go here.

## Note 2: It is expected that the working directory is set to "../ThesisWS/backend/src"
## Check with: getwd() | example how to change: setwd("C:/git/ThesisWS/backend/src")

## Note 3a: To download the input data from OneDrive you need a KEY file (KEY_InputData.csv),
## which should be placed in "../backend/data" manually.
## Note 3b: To download the input data from the Irceline ftp server, you need a password.
## This password should we filled between the double quotes.
ftp.pwd = "schuch:<passwordhere>" # fill in the password, without < >

## ABBREVIATIONS:
# PPH: Personal Place History
# P, S, T1, T2: Primary, Secondary, Transport Outwards, Transport Inwards
# dir: directory
# crs: coordinate reference system
# CRAB: Centraal Referentieadressenbestand

## WARNING: Every modules checks if the required R-libraries are installed.
## If not, they will be installed automatically.

#### Import modules ####
# source("modules/Rupdate.r")
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
source("modules/SummaryStatistics.r")
source("modules/DrivingLinearDistanceRatio.r")
source("modules/BaseAQnetwork.r")
source("modules/MeanMunicipality.r")
#source("modules/AreaOfInterest.r")
#source("modules/RasterMunicipality.r")
#source("modules/util.r")

### Set time zone ###
OriginalTimezone = Sys.timezone(location = TRUE) # All system settings should be changed to its original state at the end of the code
Sys.setenv(TZ = "GMT")

### Settings
DownloadMode = "OneDrive" # "FPT"
DriveLetter = "I" # NA (default/local/repository) or e.g. "T" (this is the letter of the external drive to use)
OutputsOnExternalDrive = FALSE
# The required free space is 400 GB

ReproduceMode = TRUE
pollutants = c("no2", "pm25")
GroupSize = 1000 # size per residential profile
PlotResults = FALSE
OrgFragments = 1:20

### Download data from OneDrive or irceline ftp server ###
# create 'data' and 'data/BE' folder in case it does not exist
data.dir = file.path("..", "data")
if (!dir.exists(data.dir)) { dir.create(data.dir) }
BE.dir = file.path("..", "data", "BE")
if (!dir.exists(BE.dir)) { dir.create(BE.dir) }

# Select main folder for hi-res Air Quality data
if (is.na(DriveLetter)) { aq.dir = file.path(data.dir, "BE", "RIO-IFDM") }
if (!is.na(DriveLetter))
{
  aq.dir = file.path(paste0(DriveLetter, ":" ), "RIO-IFDM")
}
if (!dir.exists(aq.dir)) { dir.create(aq.dir) }

# create 'output' folder in case it does not exist
if (OutputsOnExternalDrive == FALSE){ output.dir = file.path("..", "output")}
if (OutputsOnExternalDrive == TRUE) { output.dir = file.path(paste0(DriveLetter, ":" ), "output")}
if (!dir.exists(output.dir)) { dir.create(output.dir) }

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

#### FLANDERS ####

### General ###
# Uses the official address database of Flanders and adds the correct attribute 'Goal of use'

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
Types = unique(ResidentialProfiles$Type)

# new statistics on Commuting (Office Worker only)
csv.Commuting_in = file.path("..", "data", "BE_FL", "CommutingStats.csv")
Commuting = fread(csv.Commuting_in, sep=",", header=TRUE)

# Read Flanders polygon)
Flanders = getData("GADM",country = "Belgium", level = 1, path = output.dir)
Flanders = Flanders[Flanders@data$NAME_1 == "Vlaanderen",]
Flanders = spTransform(Flanders, BE_crs)
Flanders@proj4string = BE_crs
Flanders@data$NAME_1_EN = "Flanders"
if (PlotResults) {plot (Flanders)}
#SaveAsFile(Flanders, "Flanders", "GeoJSON", TRUE)

# Set year of pollutant dataset, determine dates and date types (Workdays~Weekends)
year.active = 2015
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

LogCSVpath = file.path("..", "output", "log.csv")
# check if logfile already exists
if (!file.exists(LogCSVpath))
{
  file.create(LogCSVpath)
  base = paste("Date", "Subtype", "Pollutant", "Fragment", "Time taken interpolation", "Time taken", sep = ", ")
  write.table(base, file = LogCSVpath, sep = ",", quote = FALSE, append = FALSE, col.names = FALSE, row.names = FALSE)
}

# read Air Quality Health Standards CSV
csv.HealthStandards_in = file.path("..", "data", "AirQualityHealthStandards.csv")
HealthStandards = fread(csv.HealthStandards_in, sep=",", header=TRUE)

# End of general code
# - - - 
# Beginning of profile based code

MoI = c("PST1", "P1", "P7") # Methods of Interest
DownloadMode = "FTP" # In case you still want to use the FTP server for the AQ data.

# Perform main exposure calculations per type (OW, HO and SP)
for (Active.Type in Types)
{
  #Active.Type = Types[3]
  print(Active.Type)
  
  # ## Check which output data is already present inside Active Type
  # # HR
  # HR_present = NA
  # for (FC in OrgFragments)
  # { 
  #   
  #   if (file.exists(file.path(output.dir, paste0(Active.Subtype, "_", FC),
  #                             paste0("HR_", "ALL", ".dbf"))))
  #   {
  #     HR_present[FC] = 1
  #   } else
  #   {
  #     HR_present[FC] = 0
  #   }
  # }
  # Fragments = which(HR_present %in% 0)
  # if (length(Fragments) < 1) {next}
  
  # # DF
  # DF_present = NA
  # for (FC in Fragments)
  # { 
  #   
  #   # if dynamic
  #   if Active.Type
  #   
  #   # if static
  #   
  #   if (file.exists(file.path(output.dir, paste0(Active.Subtype, "_", FC),
  #                             paste0("HR_", "ALL", ".dbf"))))
  #   {
  #     HR_present[FC] = 1
  #   } else
  #   {
  #     HR_present[FC] = 0
  #   }
  # }
  
  # EXP
  
  
  FirstSubtype = ResidentialProfiles[ResidentialProfiles$Type == Active.Type][1]
  
  # Set the seed for reproducible results
  if (ReproduceMode)
  {
    # Separate SeedNr to prevent PPH.P being identical for all profiles
    Active.SetSeedNr = FirstSubtype$SeedNr
  } else
  {
    Active.SetSeedNr = NULL
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
  
  dir.P = file.path("..", "output", paste(Active.Type, paste0("Primary", Names,".geojson"), sep = "_"))
  if (FirstSubtype$Dynamics == "dynamic")
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
      CRAB_Doel = DetermineAddressGoals_FL(Subset.Gemeente, 2)
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
    DeterminePPH_FL(CRAB_Doel, Names, GroupSize, Active.Type, FirstSubtype,
                    Plot = PlotResults, SaveResults = TRUE, Belgium, Active.SetSeedNr, Commuting, DrivingDistanceLinearDistance)
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
  if (FirstSubtype$Dynamics == "dynamic")
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
  
  SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
                                                        ResidentialProfiles$Type == Active.Type]
  
  SubProfilesFullPeriodMethodsOfInterest = SubProfilesFullPeriod[SubProfilesFullPeriod %in%
                                                                   paste0(Active.Type, "_", MoI)]
  
  # SpecificSubprofile = NA
  # if (length(SubProfilesFullPeriod) == 4) {SpecificSubprofile = SpecificSubprofile - 4}
  # if (is.na(SpecificSubprofile)) {SpecificSubprofile = 1:length(SubProfilesFullPeriod)}
  
  for (Active.Subtype in SubProfilesFullPeriodMethodsOfInterest)
    #for (Active.Subtype in SubProfilesFullPeriod[SpecificSubprofile])  
  {
    #Active.Subtype = SubProfilesFullPeriodMethodsOfInterest[1]
    print(Active.Subtype)
    
    ## Check which output data is already present inside Active Type
    # HR
    HR_present = NA
    for (FC in OrgFragments)
    { 
      
      if (file.exists(file.path(output.dir, paste0(Active.Subtype, "_", FC),
                                paste0("HR_", "ALL", ".dbf"))))
      {
        HR_present[FC] = 1
      } else
      {
        HR_present[FC] = 0
      }
    }
    HR_Fragments = which(HR_present %in% 0)
    if (length(HR_Fragments) < 1) {next}
    
    
    # Subtype base (for TIME: one for PST, one for P)
    Active.SubtypeBase = str_sub(Active.Subtype, 1, -2L)
    
    Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]
    
    
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      # Correcting the cycling durations: use constant speed of 10km/h
      if (Active.Type == "03.SP")
      {
        MeanCyclingSpeed = 10 # km/h
        PPH.T1@data$duration = PPH.T1@data$distance * (60/MeanCyclingSpeed)
        PPH.T2@data$duration = PPH.T2@data$distance * (60/MeanCyclingSpeed)
      }
    }
    
    if (Active.Subprofile$Dynamics == "dynamic" & !exists("TimeVertex.T1") & !exists("TimeVertex.T2"))
    {
      PPH.T1.Pnt.Li = list()
      PPH.T2.Pnt.Li = list()
      for (i in seq_along(PPH.P))
      {
        PPH.T1.Pnt.Li[[i]] = as(PPH.T1[i,], "SpatialPoints")
        PPH.T2.Pnt.Li[[i]] = as(PPH.T2[i,], "SpatialPoints")
      }
      
      # Convert Transport to points for equal durations
      # Factor should be desirable resulting amount. Warning message when this factor is larger than amount of vertices.
      PPH.T1.Pnt.eq.Li = SimplifyRoutes(PPH.T1, FALSE, Factor = SimplifyRemainingPoints)  
      PPH.T2.Pnt.eq.Li = SimplifyRoutes(PPH.T2, FALSE, Factor = SimplifyRemainingPoints)
      
      # Basic time element per vertex
      TimeVertex.T1 = LinkPointsToTime.Transport("Outwards", PPH.T1, PPH.T1.Pnt.Li, year.active, Active.Subprofile)
      TimeVertex.T2 = LinkPointsToTime.Transport("Inwards", PPH.T2, PPH.T2.Pnt.Li, year.active, Active.Subprofile)
    }
    
    for (pol in pollutants)
      # for (pol in pollutants[2]) #[2] = pm25 only | [1] = no2 only
    {
      print(paste("Starting", toupper(pol)))
      
      ## Check which output data is already present inside Active Type
      # DF
      if (Active.Subprofile$Dynamics == "static") {DF = c("DF_P_")}
      if (Active.Subprofile$Dynamics == "dynamic") {DF = c("DF_P_", "DF_S_","DF_T1_","DF_T2_")}
      
      DF_present = NA
      for (FC in OrgFragments)
      { 
        if (all(file.exists(file.path(output.dir, paste0(Active.Subtype, "_", FC),
                                  paste0(DF, toupper(pol), ".dbf")))))
        {
          DF_present[FC] = 1
        } else
        {
          DF_present[FC] = 0
        }
      }
      DF_Fragments = which(DF_present %in% 0)

      # Combine the HR and DF fragments
      Fragments = HR_Fragments[which(HR_Fragments %in% DF_Fragments)]
      
      ## irceline ftp server for downloading hi-res Air Quality data (as alternative for OneDrive)
      if (DownloadMode == "FTP")
      {
        # Check if TXT (uncompressed) files already exist
        
        txt.filenames = paste0(format(Time, "%Y"), format(Time, "%m"), format(Time, "%d"),
                                     "_", 1:24, "_", toupper(pol), ".txt")
        # tail(txt.filenames2,30)
        if (!all(format(Time, "%Y") != year.active))
        {
          Order_txt.filenames = order(txt.filenames)
          txt.filename = paste0(format(Time[Order_txt.filenames], "%Y"), format(Time[Order_txt.filenames], "%m"), format(Time[Order_txt.filenames], "%d"),
                                  "_", 1:24, "_", toupper(pol), ".txt")
          Wrong = which(format(Time, "%Y") != year.active)
          txt.filenames[Wrong] = paste0(format(Time[Wrong-1], "%Y"), format(Time[Wrong-1], "%m"), format(Time[Wrong-1], "%d"),
                                         "_", 24, "_", toupper(pol), ".txt")
          # tail(txt.filenames2,30)
        }
        
        if (all(file.exists(file.path(aq.dir, toupper(pol), txt.filenames))))
        {
          paste0("All the TXT-files for ", toupper(pol), " already exist on the hard drive. Continuing scipt...")
        } else
        {
          DownloadInputFilesFromIrcelineFTP(ftp.filenames, ftp.pwd, aq.dir, pol)
          # stop(paste(""))
        }

      }
      
      Points.NoVal = BaseAQnetwork(pol, aq.dir)
      PolDir = file.path(aq.dir, toupper(pol))
      
      if (Active.Subprofile$`S-aggr` == 1) # Municipality related
      {
        if (!exists("Municipalities"))
        {
          # Read municipalities
          Municipalities = getData("GADM", country = "Belgium", level = 4, path = output.dir)
          Municipalities = spTransform(Municipalities, BE_crs)
          Municipalities@proj4string = BE_crs
        }
        Municipality.RIO_IFDM.Li = PointsPerMunicipality(pol, Points.NoVal, PolDir, Municipalities)
        
        
        dbf_out = file.path(file.path(output.dir, paste0("MuniDF_", toupper(pol), ".dbf")))
        if (!file.exists(dbf_out))
        {
          MuniDF = PreMeanMunicipality(Points.NoVal, PolDir, pol, StartHour = 1, EndHour = length(Time),
                                       Municipalities, Municipality.RIO_IFDM.Li)
          
          MuniDF_T = transpose(MuniDF)
          colnames(MuniDF_T) = c(paste0("M", 1:ncol(MuniDF_T)))
          write.dbf(MuniDF_T, dbf_out)
        } else
        {
          #read
          MuniDF = transpose(read.dbf(dbf_out))
        }
        
        if (Active.Subprofile$`T-aggr` == 1) # daily and municipality average
        {
          MuniDF_Daily = data.frame(MuniDF[,1:length(YearDates)])
          MuniDF_Daily[,] = NA
          
          SeqHourDay = seq(1, ncol(MuniDF), 24)
          
          for (m in 1:nrow(MuniDF_Daily))
          {
            for (h in SeqHourDay)
            {
              hr = which(SeqHourDay %in% h)
              
              MuniDF_Daily[m,hr] = mean(as.numeric(MuniDF[m,(h):(h+23)]))
            }
          }
        }
      } # closing Municipality related
      
      # Fragments = 1:20
      DaySplit = length(YearDates)/length(OrgFragments)
      SeqFragment = floor(seq(0, length(YearDates), DaySplit))
      
      TimeTakenInterpolation = NA
      for (f in Fragments)
      # for (f in 1)
      {
        
        print(paste("Testing if HR exists of Fragment", f))
        if (file.exists(file.path(output.dir, paste0(Active.Subtype, "_", f),
                                  paste0("HR_", "ALL", ".dbf"))))
          #paste0("HR_", toupper(pol), ".dbf"))))
        {
          print(paste("HR already exists of Fragment", f))
          next # f+1
        }
        
        # print(paste("Testing if DFs exists of Fragment", f))
        # 
        # if (Active.Subprofile$Dynamics == "static") {DF = c("DF_P_")}
        # if (Active.Subprofile$Dynamics == "dynamic") {DF = c("DF_P_", "DF_S_","DF_T1_","DF_T2_")}
        # 
        # if (all(file.exists(file.path(output.dir, paste0(Active.Subtype, "_", f),
        #                           paste0(DF, toupper(pollutants), ".dbf")))))
        #   #paste0("HR_", toupper(pol), ".dbf"))))
        # {
        #   print(paste("DFs already exist of Fragment", f))
        #   next # f+1
        # }
        
        print(paste("Starting Fragment", f))
        
        YearDates.Sub = YearDates2(year.active)[(SeqFragment[f]+1):(SeqFragment[f+1])]
        Time.Sub = Time[Time > YearDates.Sub[1] & Time <= (tail(YearDates.Sub,1) + 24*60**2)]
        #MuniDF.Sub = MuniDF[,Time > YearDates.Sub[1] & Time <= (tail(YearDates.Sub,1) + 24*60**2)]
        
        # if (exists("PPH.T1.PNT.RS")) {rm(PPH.T1.PNT.RS)}
        # if (exists("PPH.T2.PNT.RS")) {rm(PPH.T2.PNT.RS)}
        # gc()
        
        CalculateRS = FALSE
        if (Active.Subprofile$Dynamics == "dynamic") {CalculateRS = TRUE}
        
        # Check the number from the current FolderName
        if (Active.Subprofile$Dynamics == "dynamic" & exists("FolderName"))
        {
          if (FolderName == paste0(Active.Subtype, "_", f))
          {
            CalculateRS = FALSE
            print(paste("RandomSampleRoutesYears (RS) are already in memory.
                          Skipping process."))
          }
        }
        
        if (CalculateRS)
        {
          print(paste("Calculating (pseudo) random points in routes..."))
          PPH.T1.PNT.RS = RandomSampleRoutesYears(PPH.T1, PPH.T1.Pnt.eq.Li, FALSE, RandomSamplePoints,
                                                  YearDates.Sub, BusinesDates, Active.SetSeedNr, f)
          PPH.T2.PNT.RS = RandomSampleRoutesYears(PPH.T2, PPH.T2.Pnt.eq.Li, FALSE, RandomSamplePoints,
                                                  YearDates.Sub, BusinesDates, Active.SetSeedNr, f)
        } # end Calculate RS
        
        FolderName = paste0(Active.Subtype, "_", f)
        FolderNameBase = paste0(Active.SubtypeBase, "_", f)
          
        # if (Active.Subprofile$Dynamics == "static")
        # {
        #   YearDates.Sub = YearDates2(year.active)
        #   Time.Sub = Time
        #   
        #   FolderName = Active.Subtype
        #   FolderNameBase = Active.SubtypeBase
        # }

        if (!file.exists(file.path(output.dir, FolderName)))
        {
          dir.create(file.path(output.dir, FolderName))
        }
        if (!file.exists(file.path(output.dir, FolderNameBase)))
        {
          dir.create(file.path(output.dir, FolderNameBase))
        }
        
        if (Active.Subprofile$Dynamics == "static") {TIME_EXP = c("TIME_P_")}
        if (Active.Subprofile$Dynamics == "dynamic") {TIME_EXP = c("TIME_P_", "TIME_S_","TIME_T1_","TIME_T2_")}
        
        if (all(file.exists(file.path(output.dir, FolderNameBase, paste0(TIME_EXP, 1:length(PPH.P), ".dbf"))))) #FolderNameBase
        {
          # read from file
          TIME.P_F = DBFreader("Time", "Primary", PPH.P, YearDates.Sub, FolderNameBase) #FolderNameBase
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            TIME.S_F = DBFreader("Time", "Secondary", PPH.P, YearDates.Sub, FolderNameBase) #FolderNameBase
            TIME.T1_F = DBFreader("Time", "T1", PPH.P, YearDates.Sub, FolderNameBase) #FolderNameBase
            TIME.T2_F = DBFreader("Time", "T2", PPH.P, YearDates.Sub, FolderNameBase) #FolderNameBase
          }
        } else # TIME files do not exist
        {
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            TIME = CreateCorrespondingDateAndTime(Active.Type, Active.Subprofile, PPH.P,
                                                  YearDates.Sub, BusinesDates, WeekendDates, HoliDates,
                                                  PPH.T1.Pnt.Li, PPH.T2.Pnt.Li, TimeVertex.T1, TimeVertex.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS,
                                                  year.active, SeqFragment, f)
          } else
          {
            TIME = CreateCorrespondingDateAndTime(Active.Type, Active.Subprofile, PPH.P,
                                                  YearDates.Sub, year.active, SeqFragment, f)
          }
          
          TIME.P_F = TIME[[2]]
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            #PHASES_F = TIME[[1]]
            TIME.S_F = TIME[[3]]
            TIME.T1_F = TIME[[4]]
            TIME.T2_F = TIME[[5]]
          }
          rm(TIME)
          gc()
          
          
          # Write TIME to disk
          WriteToDisk = TRUE
          OW = TRUE
          if (WriteToDisk)
          {
            SaveAsDBF(TIME.P_F, "Time", "Primary", FolderNameBase, OW, pol, 0)
            if (Active.Subprofile$Dynamics == "dynamic")
            {
              SaveAsDBF(TIME.S_F, "Time", "Secondary", FolderNameBase, OW, pol, 0)
              SaveAsDBF(TIME.T1_F, "Time", "T1", FolderNameBase, OW, pol, 0)
              SaveAsDBF(TIME.T2_F, "Time", "T2", FolderNameBase, OW, pol, 0)
            }
          }
        }
        
        # # correct time for daily
        # if (Active.Subprofile$'T-aggr' == 1) #daily
        # {
        #   for (i in 1:GroupSize)
        #   {
        #     for (d in seq_along(TIME.P_F[[i]]))
        #     {
        #       TIME.P_F[[i]][[d]] = TIME.P_F[[i]][[d]][1]-1*60**2
        #     }
        #   }
        # }
        
        if (Active.Subprofile$Dynamics == "static") {EXP = c("EXP_P_")}
        if (Active.Subprofile$Dynamics == "dynamic") {EXP = c("EXP_P_", "EXP_S_","EXP_T1_","EXP_T2_")}
        
        if (all(file.exists(file.path(output.dir, FolderName, paste0(EXP, (1:length(PPH.P)), "_", toupper(pol), ".dbf")))))
        {
          ExposureValue.P_F = DBFreader("Exposure", "Primary", PPH.P, YearDates.Sub, FolderName, pol)
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            ExposureValue.S_F = DBFreader("Exposure", "Secondary", PPH.P, YearDates.Sub, FolderName, pol)
            ExposureValue.T1_F = DBFreader("Exposure", "T1", PPH.P, YearDates.Sub, FolderName, pol)
            ExposureValue.T2_F = DBFreader("Exposure", "T2", PPH.P, YearDates.Sub, FolderName, pol)
          }
        } else
        {
          if (Active.Subprofile$'T-aggr' == 0) # hourly
          {
            # Hours of the year
            HOURS.P_F = HourOfTheYear7(year.active, TIME.P_F, 0)
            if (Active.Subprofile$Dynamics == "dynamic")
            {
              HOURS.S_F = HourOfTheYear7(year.active, TIME.S_F, 0)
              HOURS.T1_F = HourOfTheYear7(year.active, TIME.T1_F, 0)
              HOURS.T2_F = HourOfTheYear7(year.active, TIME.T2_F, 0)
            }
            
            # Detect which hours belong to which points | HoP (Hour of Point)
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
              HOP = WhichHourForWhichPoint(PPH.P = PPH.P, Time = Time.Sub, HOURS.P = HOURS.P_F, Print = FALSE,
                                           Active.Subprofile = Active.Subprofile, SeqFragment = SeqFragment, f = f)
              HoP.P_F = HOP[[1]]
            }
            rm(HOP)
            gc()
          }
          
          ## Interpolating the points
          if (Active.Subprofile$`S-gaps` == 0 & Active.Subprofile$`S-aggr` == 0 & Active.Subprofile$'T-aggr' == 0) # PST1
          {
            ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P, PPH.S, PPH.T1.PNT.RS, PPH.T2.PNT.RS
                                                        ,Points.NoVal, PolDir, Plot = PlotResults, pol
                                                        ,StartHour = SeqFragment[f]*24+1 # Time[(SeqFragment[f]*24+1)]
                                                        ,EndHour = (SeqFragment[f+1])*24 # Time[(SeqFragment[f+1])*24]
                                                        ,HOURS.P = HOURS.P_F, HOURS.S = HOURS.S_F
                                                        ,HOURS.T1 = HOURS.T1_F, HOURS.T2 = HOURS.T2_F, NearestPoints = 50
                                                        ,wP = HoP.P_F, wS = HoP.S_F, wT1 = HoP.T1_F, wT2 = HoP.T2_F
                                                        ,Active.Subprofile = Active.Subprofile
                                                        ,seq = SeqFragment[f]
                                                        #,Include_P = FALSE, Include_S = FALSE
                                                        #,Include_T1 = FALSE, Include_T2 = TRUE
            )
          }
          if (Active.Subprofile$`S-gaps` == 1 & Active.Subprofile$`S-aggr` == 0 & Active.Subprofile$'T-aggr' == 0) # P1
          {
            ExposureValue.All = PPH.TIN.InterpolationWS(PPH.P = PPH.P, POL = Points.NoVal
                                                        ,PolDir = PolDir
                                                        ,Plot = PlotResults, pol = pol
                                                        ,StartHour = SeqFragment[f]*24+1
                                                        ,EndHour = (SeqFragment[f+1])*24
                                                        ,HOURS.P = HOURS.P_F, NearestPoints = 50
                                                        ,wP = HoP.P_F
                                                        ,Active.Subprofile = Active.Subprofile
                                                        ,seq = SeqFragment[f],
                                                        Include_P = TRUE, Include_S = FALSE,
                                                        Include_T1 = FALSE, Include_T2 = FALSE)
          }
          
          ## Points from Municipality
          if (Active.Subprofile$`S-gaps` == 0 & Active.Subprofile$`S-aggr` == 1 & Active.Subprofile$'T-aggr' == 0) # PST3
          {
            ExposureValue.All = MeanMunicipality(PPH.P, PPH.S, PPH.T1, PPH.T2, PPH.T1.PNT.RS, PPH.T2.PNT.RS
                                                 ,PolDir, Points.NoVal, pol
                                                 ,StartHour = SeqFragment[f]*24+1 # Time[(SeqFragment[f]*24+1)]
                                                 ,EndHour = (SeqFragment[f+1])*24 # Time[(SeqFragment[f+1])*24]
                                                 ,HOURS.P = HOURS.P_F, HOURS.S = HOURS.S_F
                                                 ,HOURS.T1 = HOURS.T1_F, HOURS.T2 = HOURS.T2_F
                                                 ,wP = HoP.P_F, wS = HoP.S_F, wT1 = HoP.T1_F, wT2 = HoP.T2_F
                                                 ,Active.Subprofile = Active.Subprofile
                                                 ,seq = SeqFragment[f]
                                                 ,Municipalities, Municipality.RIO_IFDM, MuniDF
                                                 #,Include_P = FALSE, Include_S = TRUE
                                                 #,Include_T1 = FALSE, Include_T2 = FALSE
            )
          }
          
          #! not tested
          if (Active.Subprofile$`S-gaps` == 1 & Active.Subprofile$`S-aggr` == 1 & Active.Subprofile$'T-aggr' == 0) # P3
          {
            ExposureValue.All = MeanMunicipalityIndividualCentric(PPH.P = PPH.P
                                                                  ,PolDir = PolDir, POL = Points.NoVal, pol = pol
                                                                  ,StartHour = SeqFragment[f]*24+1 # Time[(SeqFragment[f]*24+1)]
                                                                  ,EndHour = (SeqFragment[f+1])*24 # Time[(SeqFragment[f+1])*24]
                                                                  ,HOURS.P = HOURS.P_F
                                                                  ,wP = HoP.P_F
                                                                  ,Active.Subprofile = Active.Subprofile
                                                                  ,seq = SeqFragment[f]
                                                                  ,Municipalities = Municipalities
                                                                  ,Municipality.RIO_IFDM = Municipality.RIO_IFDM
                                                                  ,MuniDF = MuniDF
                                                                  #,Include_P = FALSE, Include_S = TRUE
                                                                  #,Include_T1 = FALSE, Include_T2 = FALSE
            )
          }
          
          #! not tested
          if (Active.Subprofile$`S-gaps` == 0 & Active.Subprofile$`S-aggr` == 0 & Active.Subprofile$'T-aggr' == 1) # PST5
          {
          }
          
          #! not tested
          if (Active.Subprofile$`S-gaps` == 1 & Active.Subprofile$`S-aggr` == 0 & Active.Subprofile$'T-aggr' == 1) # P5
          {
          }
          
          #! not tested
          if (Active.Subprofile$`S-gaps` == 0 & Active.Subprofile$`S-aggr` == 1 & Active.Subprofile$'T-aggr' == 1) # PST7
          {
          }
          ## Points from Municipality and daily means
          if (Active.Subprofile$`S-gaps` == 1 & Active.Subprofile$`S-aggr` == 1 & Active.Subprofile$'T-aggr' == 1) # P7
          {
            ExposureValue.All = MeanMunicipalityIndividualCentric(PPH.P = PPH.P
                                                                  ,PolDir = PolDir, POL = Points.NoVal, pol = pol
                                                                  ,StartHour = SeqFragment[f]*24+1 # Time[(SeqFragment[f]*24+1)]
                                                                  ,EndHour = (SeqFragment[f+1])*24 # Time[(SeqFragment[f+1])*24]
                                                                  ,TIME.P = TIME.P_F
                                                                  #,wP = HoP.P_F
                                                                  ,Active.Subprofile = Active.Subprofile
                                                                  ,seq = SeqFragment[f]
                                                                  ,Municipalities = Municipalities
                                                                  ,Municipality.RIO_IFDM = Municipality.RIO_IFDM
                                                                  ,MuniDF = MuniDF_Daily
                                                                  ,Include_P = TRUE, Include_S = FALSE
                                                                  ,Include_T1 = FALSE, Include_T2 = FALSE
            )
          }
          
          ExposureValue.P_F = ExposureValue.All[[1]]
          ExposureValue.S_F = ExposureValue.All[[2]]
          ExposureValue.T1_F = ExposureValue.All[[3]]
          ExposureValue.T2_F = ExposureValue.All[[4]]
          TimeTakenInterpolation[f] = ExposureValue.All[[5]]
          print(TimeTakenInterpolation[f])
          
          info = paste(Sys.time(), Active.Subprofile$Subtype, pol, f, TimeTakenInterpolation[f], NA, sep = ", ")
          write.table(info, file = LogCSVpath, sep = ",", quote = FALSE, append = TRUE, col.names = FALSE,
                      row.names = FALSE, eol = "\n")
          
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
          
        } # closing when EXP does not exist on HD
        
        if (Active.Subprofile$Dynamics == "static") {DF = c("DF_P_")}
        if (Active.Subprofile$Dynamics == "dynamic") {DF = c("DF_P_", "DF_S_","DF_T1_","DF_T2_")}
        
        
        if (all(file.exists(file.path(output.dir, FolderName, paste0(DF, toupper(pol), ".dbf")))))
        {
          ST.DF.P_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_P_", toupper(pol), ".dbf")))
          class(ST.DF.P_F$TIME) = class(Time)
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            ST.DF.S_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_S_", toupper(pol), ".dbf")))
            ST.DF.T1_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T1_", toupper(pol), ".dbf")))
            ST.DF.T2_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T2_", toupper(pol), ".dbf")))
            class(ST.DF.S_F$TIME) = class(Time)
            class(ST.DF.T1_F$TIME) = class(Time)
            class(ST.DF.T2_F$TIME) = class(Time)
          }
        } else
        {
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
          
        } # closing when ST.DF does not exist on HD
        
        if (!file.exists(file.path(output.dir, FolderName, paste0("HR_", "ALL", ".dbf"))) &
            (all(file.exists(file.path(output.dir, FolderName, paste0(DF, toupper(pollutants), ".dbf"))))))
        {
          if (Active.Subprofile$Dynamics == "dynamic")
          {
            ST.DF.HR_F = ToHourValuesFromDF.Dynamic(PPH.P, Time.Sub, output.dir, FolderName,
                                                    TIME.P_F, TIME.S_F, TIME.T1_F, TIME.T2_F, pollutants)
            
            #SaveAsDBF(ST.DF.HR_F, "HR", "HR", FolderName, OW, toupper(pol), 0)
          } else # if static
          {
            ST.DF.HR_F = ToHourValuesFromDF.Static(PPH.P, output.dir, FolderName, TIME.P_F, toupper(pollutants))
          }
          
          SaveAsDBF(ST.DF.HR_F, "HR", "HR", FolderName, T, "ALL", 0)
          
          
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
          # gc()
          
        } # closing folder exists
        
        # Remove from memory/environment
        rm(TIME.P_F, HOURS.P_F, HoP.P_F, ExposureValue.P_F, ST.DF.P_F)
        if (Active.Subprofile$Dynamics == "dynamic")
        {
          rm(PPH.T1.PNT.RS, PPH.T2.PNT.RS)
          rm(TIME.S_F, TIME.T1_F, TIME.T2_F)
          rm(HOURS.S_F, HOURS.T1_F, HOURS.T2_F)
          rm(HoP.S_F, HoP.T1_F, HoP.T2_F)
          rm(ExposureValue.S_F, ExposureValue.T1_F, ExposureValue.T2_F)
          rm(ST.DF.S_F, ST.DF.T1_F, ST.DF.T2_F)
          rm(ExposureValueCombined_F, ST.DF.HR_F)
        }
        gc()
        
      } # closing f
      
      info = paste(Sys.time(), Active.Subprofile$Subtype, pol, "Total", NA, sum(TimeTakenInterpolation), sep = ", ")
      write.table(info, file = LogCSVpath, sep = ",", quote = FALSE, append = TRUE, col.names = FALSE,
                  row.names = FALSE, eol = "\n")
      
    } # closing pol
    
    rm(BASEAQ, Points.NoVal, MuniDF)
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      rm(Fragments, SeqFragment, DaySplit)
    }
    gc()
    
  } # closing Subprofile
  
  rm(dir.P, PPH.P)
  rm(HoliDates, SimplifyRemainingPoints, RandomSamplePoints, BusinesDates, WeekendDates)
  if (FirstSubtype$Dynamics == "dynamic")
  {
    rm(dir.S, dir.T1, dir.T2)
    rm(PPH.S, PPH.T1, PPH.T2)
    rm(PPH.T1.Pnt.Li, PPH.T2.Pnt.Li)
    rm(PPH.T1.Pnt.eq.Li, PPH.T2.Pnt.eq.Li)
    rm(TimeVertex.T1, TimeVertex.T2)
  }
  gc()
  
} # closing Type


## test EXP/DF/HR on strange outputs
TEST_F1 = 1
TEST_F2 = 2
TEST_Type = "03.SP"
TEST_Subber = "PST"
TEST_Nr = 1 # 1 7
TEST_pol = "no2"

Test.DifferentOutcomesFragments <- function(TEST_F1, TEST_F2, TEST_Type, TEST_Subber, TEST_Nr, TEST_pol)
{
  YearDates.Sub_F1 = YearDates2(year.active)[(SeqFragment[TEST_F1]+1):(SeqFragment[TEST_F1+1])]
  YearDates.Sub_F2 = YearDates2(year.active)[(SeqFragment[TEST_F2]+1):(SeqFragment[TEST_F2+1])]
  
  Time.Sub_F1 = Time[Time > YearDates.Sub_F1[1] & Time <= (tail(YearDates.Sub_F1,1) + 24*60**2)]
  Time.Sub_F2 = Time[Time > YearDates.Sub_F2[1] & Time <= (tail(YearDates.Sub_F2,1) + 24*60**2)]
  
  
  TEST_SubType = paste0(TEST_Type, "_", TEST_Subber)
  
  FolderNameBase_F1 = paste0(TEST_SubType, "_", TEST_F1)
  FolderNameBase_F2 = paste0(TEST_SubType, "_", TEST_F2)
  FolderName_F1 = paste0(TEST_SubType, TEST_Nr, "_", TEST_F1)
  FolderName_F2 = paste0(TEST_SubType, TEST_Nr, "_", TEST_F2)
  
  TEST_dir.P = file.path("..", "output", paste(TEST_Type, paste0("Primary", paste(""),".geojson"), sep = "_"))
  TEST_PPH.P = readOGR(TEST_dir.P, layer = 'OGRGeoJSON')
  TEST_PPH.P@proj4string = BE_crs
  
  TEST_TIME.P_F1 = DBFreader("Time", "Primary", TEST_PPH.P[1,], YearDates.Sub_F1, FolderNameBase_F1)
  TEST_TIME.P_F2 = DBFreader("Time", "Primary", TEST_PPH.P[1,], YearDates.Sub_F2, FolderNameBase_F2)
  
  TEST_ExposureValue.P_F1 = DBFreader("Exposure", "Primary", TEST_PPH.P[1,], YearDates.Sub_F1, FolderName_F1, TEST_pol)
  TEST_ExposureValue.P_F2 = DBFreader("Exposure", "Primary", TEST_PPH.P[1,], YearDates.Sub_F2, FolderName_F2, TEST_pol)
  
  TEST = TEST_ExposureValue.P_F1[[1]][[1]][1] == TEST_ExposureValue.P_F2[[1]][[1]][1]
  if (TEST) {warning(paste0("Did not past the test."))}
  if (!TEST) {print(paste0("Passed the test."))}
  
  return(list(TEST_ExposureValue.P_F1[[1]][[1]], TEST_ExposureValue.P_F2[[1]][[1]]))
  
}

TEST = Test.DifferentOutcomesFragments(15, 16, "02.HO", "P", 1, "no2")
TEST[[1]]
TEST[[2]]



### Collect different calculation methods per profile (Saving 1 file HR_ALL per Type)
# for (Active.Type in Types)
for (Active.Type in Types)
{
  #Active.Type = Types[1]
  TypeNr = which(Types %in% Active.Type)
  
  # SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
  #                                                       ResidentialProfiles$Type == Active.Type]
  MethodsOfInterest = c("PST1", "P1", "P7")
  if (TypeNr == 2) {MethodsOfInterest = c("P1", "P7")}
  
  MethodsOfInterest2 = paste0(Active.Type, "_", MethodsOfInterest)
  print(MethodsOfInterest2)
  
  for (Active.Subtype in MethodsOfInterest2)
  {
    print(Active.Subtype)
    #Active.Subtype = MethodsOfInterest2[1]
    s = which(MethodsOfInterest2 %in% Active.Subtype)
    
    # Check if folder with fragments already exist
    TimeExp.lst = list.files(path = output.dir, pattern = paste0(Active.Subtype, "_", "[0-9]*"))
    # remove _ and backups in the lst
    NChar = NA
    for (c in seq_along(TimeExp.lst))
    {
      NChar[c] = nchar(TimeExp.lst[c])
    }
    TimeExp.lst = TimeExp.lst[NChar == nchar(Active.Subtype)+2 | NChar == nchar(Active.Subtype)+3]
    TimeExp.lst = mixedsort(TimeExp.lst) # fixes string order 1 10 11 -> 1 2 3
    
    Frag.lst = gsub(x = TimeExp.lst, pattern = paste0(Active.Subtype, "_"), replacement = "")
    Fragments = as.numeric(Frag.lst)
    
    if (s == 1)
    {
      if (!file.exists(file.path(output.dir, Active.Subtype, paste0("HR_ALL", ".dbf"))))
      {
        if (all(is.na(Fragments)))
        {
          
          
        } else
        {
          # Connect the fragments
          HR_ALL.Li = list()
          for (f in Fragments)
          {
            print(f)
            FolderName = paste0(Active.Subtype, "_", f)
            #FolderNameBase = paste0(Active.SubtypeBase, "_", f)
            
            #HR_ALL.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_", toupper(pol), ".dbf")))[,1:4] #c("TIME", "IND")
            
            # HR_ALL.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_ALL", ".dbf")))[,1:4] #c("TIME", "IND")
            HR_ALL.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_ALL", ".dbf"))) #c("TIME", "IND")
            
            #colnames(HR_ALL.Li[[f]])[3:4] = paste0("PST1_", colnames(HR_ALL.Li[[f]])[3:4])
          }
          HR_ALL = do.call(rbind, HR_ALL.Li)
          HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
          
          rm(HR_ALL.Li)
          gc()
        }
        
        if (nrow(HR_ALL) == length(Time)*GroupSize)
        {
          SaveAsDBF(HR_ALL, "HR", "HR", Active.Subtype, FALSE, "ALL", 0)
        } else
        {
          stop(print(paste0("Data frame should have ", length(Time)*GroupSize, " observations/rows.")))
        }
      } else
      {
        # HR_ALL = read.dbf(file = file.path(output.dir, Active.Subtype, paste0("HR_ALL", ".dbf")))[,1:4]
        HR_ALL = read.dbf(file = file.path(output.dir, Active.Subtype, paste0("HR_ALL", ".dbf")))
        
        HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
      }
      
      class(HR_ALL$TIME) = class(YearDates)
      
    } else # closing (s == 1) opening (s != 1)
    {
      if (!file.exists(file.path(output.dir, Active.Subtype, paste0("HR_ALL", ".dbf"))))
      {
        if (all(is.na(Fragments)))
        {
          HR_ALL.Li = list()
          for (pol in pollutants)
          {
            p = which(pollutants %in% pol)
            HR_ALL.Li[[p]] = read.dbf(file = file.path(output.dir, Active.Subtype, paste0("HR_", toupper(pol), ".dbf")))
          }
          
          if (all(HR_ALL.Li[[1]][,"TIME"] == HR_ALL.Li[[2]][,"TIME"]))
          {
            HR = cbind(HR_ALL.Li[[1]][,c("TIME", "IND", "EXP")], HR_ALL.Li[[2]]$EXP)
          }
          HR = HR[order(HR$TIME),] # order on time
          
        } else
        {
          HR.Li = list()
          for (f in Fragments)
          {
            print(f)
            FolderName = paste0(Active.Subtype, "_", f)
            
            HR.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_ALL", ".dbf")))
          }
          HR = do.call(rbind, HR.Li)
          
          if (!all(HR$IND == HR_ALL$IND))
          {
            HR2 = HR[order(HR$TIME),]
            
            if (!all(HR2$TIME == HR_ALL$TIME) | !all(HR2$IND == HR_ALL$IND))
            {
              stop(print(paste0("Cannot make correct order for data frame merging.")))
            }
            HR = HR2
            rm(HR2)
          }
        }
        
        HR_ALL = cbind(HR_ALL, HR[,3:length(HR)])
        HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
        
        rm(HR.Li, HR)
        gc()
        
      } else
      {
        if (all(is.na(Fragments)))
        {
          FolderName = Active.Subtype
          HR.Li = list()
          HR2.Li = list()
          for (pol in pollutants)
          {
            p = which(pollutants %in% pol)
            HR.Li[[p]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_", toupper(pol), ".dbf")))
            
            if (!all(HR.Li[[p]]$IND == HR_ALL$IND))
            {
              HR2.Li[[p]] = HR.Li[[p]][order(HR.Li[[p]]$TIME),]
              all(HR2.Li[[p]]$IND == HR_ALL$IND)
            }
            #HR_ALL = do.call(rbind, HR2.Li)
            HR_ALL = cbind(HR_ALL[,1:(2*s+p-1)], HR2.Li[[p]]$EXP)
            HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
          }
          rm(HR.Li, HR2.Li)
          gc()
        } else
        {
          HR_SUB.Li = list()
          for (pol in pollutants)
          {
            EXP.Li = list()
            p = which(pollutants %in% pol)
            for (f in Fragments)
            {
              print(f)
              FolderName = paste0(Active.Subtype, "_", f)
              
              EXP.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_", toupper(pol), ".dbf")))#$EXP
            }
            #HR_SUB.Li[[p]] = EXP.Li
            HR_SUB.Li[[p]] = do.call(rbind, EXP.Li)
            
            if (!all(HR_SUB.Li[[p]]$IND == HR_ALL$IND))
            {
              HR_SUB.Li[[p]] = HR_SUB.Li[[p]][order(HR_SUB.Li[[p]]$TIME),]
              all(HR_SUB.Li[[p]]$IND == HR_ALL$IND)
            }
            
            HR_ALL = cbind(HR_ALL[,1:(2*s+p-1)], HR_SUB.Li[[p]]$EXP)
            HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
          } # closing p
          
          ## Connect HR_ALL(base) and second
          #HR_ALL = cbind(HR_ALL, unlist(HR_SUB.Li[[1]]), unlist(HR_SUB.Li[[2]]))
          
          rm(EXP.Li, HR_SUB.Li)
          gc()
        }
      }
      
    } # closing (s != 1)
    
    # regex the calc method
    StrSPlt = strsplit(MethodsOfInterest2, paste0(Active.Type, "_"))
    EvenNr = seq(2, length(unlist(StrSPlt)), 2)
    MethodsOfInterest = unlist(StrSPlt)[EvenNr]
    
    colnames(HR_ALL)[2*s+1] = paste0(MethodsOfInterest[s], "_", toupper(pollutants[1]))
    colnames(HR_ALL)[2*s+2] = paste0(MethodsOfInterest[s], "_", toupper(pollutants[2]))
    
    head(HR_ALL)
    
  } # closing Active.Subtype
  
  SaveAsDBF(HR_ALL, "HR", "HR", Active.Type, F, "ALL", 0)
  
} # closing Active.Type

# #!! Check if order is correct
# HR_ALL.OW = read.dbf(file = file.path(output.dir, "01.OW", paste0("HR_ALL", ".dbf")))
# HR_ALL.HO = read.dbf(file = file.path(output.dir, "02.HO", paste0("HR_ALL", ".dbf")))
# HR_ALL.SP = read.dbf(file = file.path(output.dir, "03.SP", paste0("HR_ALL", ".dbf")))
# 
# all(HR_ALL.OW$TIME == HR_ALL.SP$TIME)
# all(HR_ALL.OW$IND == HR_ALL.SP$IND)


### Collect different calculation methods per profile (Saving 1 file DF per Type)
for (Active.Type in Types)
{
  #Active.Type = Types[1]
  print(Active.Type)
  TypeNr = which(Types %in% Active.Type)
  
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
  
  MethodsOfInterest = c("PST1", "P1", "P7")
  if (TypeNr == 2) {MethodsOfInterest = c("P1", "P7")}
  
  MethodsOfInterest2 = paste0(Active.Type, "_", MethodsOfInterest)
  print(MethodsOfInterest2)
  
  for (Active.Subtype in MethodsOfInterest2)
  {
    #Active.Subtype = MethodsOfInterest2[1]
    print(Active.Subtype)
    Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]
    
    s = which(MethodsOfInterest2 %in% Active.Subtype)
    
    # Check if folder with fragments already exist
    TimeExp.lst = list.files(path = output.dir, pattern = paste0(Active.Subtype, "_", "[0-9]*"))
    # remove _ and backups in the lst
    NChar = NA
    for (c in seq_along(TimeExp.lst))
    {
      NChar[c] = nchar(TimeExp.lst[c])
    }
    TimeExp.lst = TimeExp.lst[NChar == nchar(Active.Subtype)+2 | NChar == nchar(Active.Subtype)+3]
    TimeExp.lst = mixedsort(TimeExp.lst) # fixes string order 1 10 11 -> 1 2 3
    
    Frag.lst = gsub(x = TimeExp.lst, pattern = paste0(Active.Subtype, "_"), replacement = "")
    Fragments = as.numeric(Frag.lst)
    
    DF.name = c("DF_P", "DF_S","DF_T1","DF_T2")
    
    if (grepl("PST", Active.Subtype))
    {
      for (lp in seq_along(DF.name)) # per Location Phase
      {
        # lp = 1
        print(DF.name[lp])
        #if (length(list.files(path = file.path(output.dir, Active.Subtype), pattern = DF[lp])) < 1)
        if (!file.exists(file.path(output.dir, Active.Subtype, paste0(DF.name[lp], "_ALL", ".dbf"))))
        {
          DF_ALL.Li = list()
          for (f in Fragments)
            #for (f in 11:14)
          {
            print(f)
            FolderName = paste0(Active.Subtype, "_", f)
            
            DaySplit = length(YearDates)/length(Fragments)
            SeqFragment = floor(seq(0, length(YearDates), DaySplit))
            YearDates.Sub = YearDates2(year.active)[(SeqFragment[f]+1):(SeqFragment[f+1])]
            
            # check if the fragment includes a business day
            if (!any(YearDates.Sub %in% BusinesDates) & lp > 1)
            {
              next # f+1
            }
            
            DF.Li = list()
            for (pol in pollutants)
            {
              p = which(pollutants %in% pol)
              DF.Li[[p]] = read.dbf(file = file.path(output.dir, FolderName, paste0(DF.name[lp],"_", toupper(pol), ".dbf")))
            }
            CHECK = NA
            for (p in 1:(length(pollutants)-1))
            {
              
              CHECK[p] = all(c(all(DF.Li[[p]]$TIME == DF.Li[[p+1]]$TIME), all(DF.Li[[p]]$IND == DF.Li[[p+1]]$IND)))
            }
            if (!all(CHECK))
            {
              stop(print("DID NOT PASS CHECKPOINT"))
              #sort
              DF.Li[[1]] = DF.Li[[1]][order(DF.Li[[1]]$IND),]
              # DF.Li[[1]][order(DF.Li[[1]]$TIME),] # order on time
              
            } else
            {
              # merge dataframes
              DF = cbind(DF.Li[[1]], DF.Li[[2]]$EXP)
              DF[,2] = DF.Li[[1]]$IND
              colnames(DF)[2] = "IND"
              DF[,3] = DF.Li[[1]]$EXP
              colnames(DF)[3] = paste0("EXP_", toupper(pollutants[1]))
              colnames(DF)[length(DF)] = paste0("EXP_", toupper(pollutants[2]))
            }
            DF_ALL.Li[[f]] = DF
            rm(DF)
          } # closing f
          DF_ALL = do.call(rbind, DF_ALL.Li)
          DF_ALL = DF_ALL[order(DF_ALL$TIME),] # order on time
          
          rm(DF_ALL.Li)
          gc()
          
          # if (nrow(DF_ALL) == length(Time)*GroupSize)
          # {
          SaveAsDBF(DF_ALL, "DF", DF.name[lp], Active.Subtype, FALSE, "ALL", 0)
          # } else
          # {
          #   stop(print(paste0("Data frame should have ", length(Time)*GroupSize, " observations/rows.")))
          # }
        } # closing file existance
      } # closing location phase (lp)
    } else # if not "PST"
    {
      
    }
  } # closing Active.Subtype
} # closing Active.Type

#       
#       class(DF_ALL$TIME) = class(YearDates)
#       
#     } else # closing (s == 1) opening (s != 1)
#     {
#       if (!file.exists(file.path(output.dir, Active.Subtype, paste0("HR_ALL", ".dbf"))))
#       {
#         if (all(is.na(Fragments)))
#         {
#           HR_ALL.Li = list()
#           for (pol in pollutants)
#           {
#             p = which(pollutants %in% pol)
#             HR_ALL.Li[[p]] = read.dbf(file = file.path(output.dir, Active.Subtype, paste0("HR_", toupper(pol), ".dbf")))
#           }
#           
#           if (all(HR_ALL.Li[[1]][,"TIME"] == HR_ALL.Li[[2]][,"TIME"]))
#           {
#             HR = cbind(HR_ALL.Li[[1]][,c("TIME", "IND", "EXP")], HR_ALL.Li[[2]]$EXP)
#           }
#           HR = HR[order(HR$TIME),] # order on time
#           
#         } else
#         {
#           HR.Li = list()
#           for (f in Fragments)
#           {
#             print(f)
#             FolderName = paste0(Active.Subtype, "_", f)
#             
#             HR.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_ALL", ".dbf")))
#           }
#           HR = do.call(rbind, HR.Li)
#           
#           if (!all(HR$IND == HR_ALL$IND))
#           {
#             HR2 = HR[order(HR$TIME),]
#             
#             if (!all(HR2$TIME == HR_ALL$TIME) | !all(HR2$IND == HR_ALL$IND))
#             {
#               stop(print(paste0("Cannot make correct order for data frame merging.")))
#             }
#             HR = HR2
#             rm(HR2)
#           }
#         }
#         
#         HR_ALL = cbind(HR_ALL, HR[,3:length(HR)])
#         HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
#         
#         rm(HR.Li, HR)
#         gc()
#         
#       } else
#       {
#         if (all(is.na(Fragments)))
#         {
#           FolderName = Active.Subtype
#           HR.Li = list()
#           HR2.Li = list()
#           for (pol in pollutants)
#           {
#             p = which(pollutants %in% pol)
#             HR.Li[[p]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_", toupper(pol), ".dbf")))
#             
#             if (!all(HR.Li[[p]]$IND == HR_ALL$IND))
#             {
#               HR2.Li[[p]] = HR.Li[[p]][order(HR.Li[[p]]$TIME),]
#               all(HR2.Li[[p]]$IND == HR_ALL$IND)
#             }
#             #HR_ALL = do.call(rbind, HR2.Li)
#             HR_ALL = cbind(HR_ALL[,1:(2*s+p-1)], HR2.Li[[p]]$EXP)
#             HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
#           }
#           rm(HR.Li, HR2.Li)
#           gc()
#         } else
#         {
#           HR_SUB.Li = list()
#           for (pol in pollutants)
#           {
#             EXP.Li = list()
#             p = which(pollutants %in% pol)
#             for (f in Fragments)
#             {
#               print(f)
#               FolderName = paste0(Active.Subtype, "_", f)
#               
#               EXP.Li[[f]] = read.dbf(file = file.path(output.dir, FolderName, paste0("HR_", toupper(pol), ".dbf")))#$EXP
#             }
#             #HR_SUB.Li[[p]] = EXP.Li
#             HR_SUB.Li[[p]] = do.call(rbind, EXP.Li)
#             
#             if (!all(HR_SUB.Li[[p]]$IND == HR_ALL$IND))
#             {
#               HR_SUB.Li[[p]] = HR_SUB.Li[[p]][order(HR_SUB.Li[[p]]$TIME),]
#               all(HR_SUB.Li[[p]]$IND == HR_ALL$IND)
#             }
#             
#             HR_ALL = cbind(HR_ALL[,1:(2*s+p-1)], HR_SUB.Li[[p]]$EXP)
#             HR_ALL = HR_ALL[order(HR_ALL$TIME),] # order on time
#           } # closing p
#           
#           ## Connect HR_ALL(base) and second
#           #HR_ALL = cbind(HR_ALL, unlist(HR_SUB.Li[[1]]), unlist(HR_SUB.Li[[2]]))
#           
#           rm(EXP.Li, HR_SUB.Li)
#           gc()
#         }
#       }
#       
#     } # closing (s != 1)
#     
#     # regex the calc method
#     StrSPlt = strsplit(MethodsOfInterest2, paste0(Active.Type, "_"))
#     EvenNr = seq(2, length(unlist(StrSPlt)), 2)
#     MethodsOfInterest = unlist(StrSPlt)[EvenNr]
#     
#     colnames(HR_ALL)[2*s+1] = paste0(MethodsOfInterest[s], "_", toupper(pollutants[1]))
#     colnames(HR_ALL)[2*s+2] = paste0(MethodsOfInterest[s], "_", toupper(pollutants[2]))
#     
#     head(HR_ALL)
#     
#   } # closing Active.Subtype
#   
#   SaveAsDBF(HR_ALL, "HR", "HR", Active.Type, F, "ALL", 0)
#   
# } # closing Active.Type


### Stats & Plots 
for (Active.Type in Types)
{
  # Active.Type = Types[3]
  
  HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL", ".dbf")))
  
  Stats.HR_ALL_IndividualBased = DF.Stats2(HR_ALL, BasedOn = "IND", Time)
  Stats.HR_ALL_HourBased = DF.Stats2(HR_ALL, BasedOn = "TIME", Time)
  
  MethodsOfInterest = c("PST1", "P1", "P7")
  if (Active.Type == "02.HO") {MethodsOfInterest = c("P1", "P7")}
  
  # for (cm in 3:length(IndividualBasedMean))
  # {
  #   print(cm)
  #   print(colnames(IndividualBasedMean[cm]))
  #   
  #   hist(IndividualBasedMean[,cm], 100, xlim = c(0, 50), ylim = c(0, 150), plot = TRUE, 
  #        main = paste("Histogram of", Active.Type, ": ", colnames(IndividualBasedMean)[cm], "(IndividualBasedMean)"),
  #        xlab = colnames(IndividualBasedMean)[cm])
  #   
  #   hist(HR_ALL[,cm], 100, xlim = c(0, 50), plot = TRUE, 
  #        main = paste("Histogram of", Active.Type, ": ", colnames(IndividualBasedMean)[cm]),
  #        xlab = colnames(IndividualBasedMean)[cm])
  # }
  
  StatsMethod = "mean" # mean, min, max, sd
  StatsMethodWithMethodsOfInterest1 = paste(StatsMethod, MethodsOfInterest, sep = "_")
  StatsMethodWithMethodsOfInterest = c(StatsMethodWithMethodsOfInterest1, StatsMethodWithMethodsOfInterest1)
  #StatsMethodWithMethodsOfInterestPol = paste(StatsMethodWithMethodsOfInterest, toupper(pollutants), sep = "_")
  
  #hist(Stats.HR_ALL_IndividualBased[,StatsMethodWithMethodsOfInterestPol], 100)
  # hist(HR_ALL, 100, xlim = c(0, 50), ylim = c(0, 50), plot = TRUE)
  
  PlotSave = FALSE
  
  for (pol in pollutants)
  {
    # pol = pollutants[2]
    
    MethodsOfInterestPol = paste(MethodsOfInterest, toupper(pol), sep = "_")
    StatsMethodWithMethodsOfInterestPol = paste(StatsMethod, MethodsOfInterestPol, sep = "_")
    
    for (cm in MethodsOfInterest)
    {
      # cm = MethodsOfInterest[1]
      
      Active.Subtype = paste0(Active.Type, "_", cm)
      
      ST.DF.P = read.dbf(file = file.path(output.dir, Active.Subtype, "DF_P_ALL.dbf"))
      ST.DF.S = read.dbf(file = file.path(output.dir, Active.Subtype, "DF_S_ALL.dbf"))
      ST.DF.T1 = read.dbf(file = file.path(output.dir, Active.Subtype, "DF_T1_ALL.dbf"))
      ST.DF.T2 = read.dbf(file = file.path(output.dir, Active.Subtype, "DF_T2_ALL.dbf"))
      class(ST.DF.P$TIME) = class(Time)
      class(ST.DF.S$TIME) = class(Time)
      class(ST.DF.T1$TIME) = class(Time)
      class(ST.DF.T2$TIME) = class(Time)
      
      
      
      Plot.RawExposure(Active.Type, 1, 1, GroupSize, PlotMinMax = FALSE, ST.DF.P, ST.DF.S, ST.DF.T1, ST.DF.T2)
      
      Plot.HourExposure(Active.Type, 5, 1, HR_ALL, Stats.HR_ALL_HourBased, cm, PlotMinMax = TRUE, pol = pol)
      
    } #closing cm
    
    # ScatterplotMatrixAndSave(Active.Type, Stats.HR_ALL_IndividualBased, StatsMethodWithMethodsOfInterestPol,
    #                          PlotSave = PlotSave, Width = 720, Height = 480, toupper(pol), StatsMethod)
    
    MethodsOfInterest2 = paste(StatsMethod, MethodsOfInterest, sep = "_")
    Plot.DeltaProposedAndSave(Active.Type, 1, 21, MethodsOfInterest2, toupper(pol), Stats.HR_ALL_HourBased,
                              PlotSave = PlotSave, Width = 720, Height = 480, PointSize = 15, StatsMethod)
    
    # Plot.DeltaProposed(Active.Type, 1, 21, MethodsOfInterest2, toupper(pol), Stats.HR_ALL_HourBased)
    
    # # How many individuals above 150?
    # HR_ALL[HR_ALL$PST1_NO2 > 150,]
    # HR_ALL[HR_ALL$PST1_NO2 > 150,]
    # 
    # # How many individuals of 1000?
    # nrow(HR_ALL[HR_ALL$PST1_NO2 > 100,])
    # nrow(HR_ALL[HR_ALL$PST1_PM25 > 100,])
    
    # CorPlotTable2(Active.Type, "Mixed", StatsMethodWithMethodsOfInterest, toupper(pol), Stats.HR_ALL_IndividualBased)
    
    # ScatterPlotMatrix(Active.Type, StatsMethodWithMethodsOfInterest, Stats.HR_ALL_IndividualBased, pol)
    ScatterplotMatrixAndSave(Type, Stats.HR_ALL_IndividualBased, StatsMethodWithMethodsOfInterestPol,
                             PlotSave = FALSE, pol = pol, StatsMethod = StatsMethod)
    
    # CorPlotGraph(Active.Type, paste(StatsMethod, MethodsOfInterest[1], toupper(pol), sep = "_"),
    #              paste(StatsMethod, MethodsOfInterest[2], toupper(pol), sep = "_"),
    #              Width = 1208, Height = 720, pol, GroupSize, Stats.HR_ALL_IndividualBased)
    # CorPlotGraph(Active.Type, paste(StatsMethod, MethodsOfInterest[1], toupper(pol), sep = "_"),
    #              paste(StatsMethod, MethodsOfInterest[3], toupper(pol), sep = "_"),
    #              Width = 1208, Height = 720, pol, GroupSize, Stats.HR_ALL_IndividualBased)
    
    p = Plot.DeltaProposed(Active.Type, MethodsOfInterest, 1, 365, StatsMethodWithMethodsOfInterest1, pol, Stats.HR_ALL_HourBased, InteractivePlot = TRUE,
                           Location = "Online") #Location = "Online"
    p
    
    
    
  } # closing pol
  
} # closign Active.Type

## Exceedance analysis

# pol = pollutants[1]
# if (pol == "pm25") {pol2 = "pm2.5"}
# if (pol == "no2") {pol2 = "no2"}

# read Health Standards (HeSt)
HeSt_1H_NO2 = HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 hour" &
                                              HealthStandards$Pollutant == toupper("no2") &
                                              HealthStandards$Agency == "EU"]
HeSt_1H_PM25 = 60

HeSt_1Y_NO2 = HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                              HealthStandards$Pollutant == toupper("no2") &
                                              HealthStandards$Agency == "WHO"]

HeSt_1Y_PM25 = HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                               HealthStandards$Pollutant == toupper("pm2.5") &
                                               HealthStandards$Agency == "WHO"]

## Home Office
Active.Type = Types[2]
HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL", ".dbf")))
Stats.HR_ALL_IndividualBased = DF.Stats2(HR_ALL, BasedOn = "IND", Time)

EXC_HO_1H_NO2_P1 = HR_ALL[HR_ALL$P1_NO2 > HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 hour" &
                                                                          HealthStandards$Pollutant == toupper("no2") &
                                                                          HealthStandards$Agency == "EU"],][,1:2]
EXC_HO_1H_NO2_P7 = HR_ALL[HR_ALL$P7_NO2 > HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 hour" &
                                                                          HealthStandards$Pollutant == toupper("no2") &
                                                                          HealthStandards$Agency == "EU"],][,1:2]
EXC_HO_1H_PM25_P1 = HR_ALL[HR_ALL$P1_PM25 > HeSt_1H_PM25,][,1:2]
EXC_HO_1H_PM25_P7 = HR_ALL[HR_ALL$P7_PM25 > HeSt_1H_PM25,][,1:2]

EXC_HO_1Y_NO2_P1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P1_NO2 >
                                                  HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                                                                  HealthStandards$Pollutant == toupper("no2") &
                                                                                  HealthStandards$Agency == "WHO"],]$IND
EXC_HO_1Y_NO2_P7 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P7_NO2 > 
                                                  HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                                                                  HealthStandards$Pollutant == toupper("no2") &
                                                                                  HealthStandards$Agency == "WHO"],]$IND
EXC_HO_1Y_PM25_P1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P1_PM25 >
                                                   HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                                                                   HealthStandards$Pollutant == toupper("pm2.5") &
                                                                                   HealthStandards$Agency == "WHO"],]$IND
EXC_HO_1Y_PM25_P7 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P7_PM25 >
                                                   HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                                                                   HealthStandards$Pollutant == toupper("pm2.5") &
                                                                                   HealthStandards$Agency == "WHO"],]$IND
# Office Worker
Active.Type = Types[1]
HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL", ".dbf")))
Stats.HR_ALL_IndividualBased = DF.Stats2(HR_ALL, BasedOn = "IND", Time)

EXC_OW_1H_NO2_PST1 = HR_ALL[HR_ALL$PST1_NO2 > HeSt_1H_NO2,][,c(1,2, which(colnames(HR_ALL) == "PST1_NO2"))]
EXC_OW_1H_NO2_P1 = HR_ALL[HR_ALL$P1_NO2 > HeSt_1H_NO2,][,c(1,2, which(colnames(HR_ALL) == "P1_NO2"))]
EXC_OW_1H_NO2_P7 = HR_ALL[HR_ALL$P7_NO2 > HeSt_1H_NO2,][,c(1,2, which(colnames(HR_ALL) == "P7_NO2"))]

EXC_OW_1H_PM25_PST1 = HR_ALL[HR_ALL$PST1_PM25 > HeSt_1H_PM25,][,c(1,2, which(colnames(HR_ALL) == "PST1_PM25"))]
EXC_OW_1H_PM25_P1 = HR_ALL[HR_ALL$P1_PM25 > HeSt_1H_PM25,][,c(1,2, which(colnames(HR_ALL) == "P1_PM25"))]
EXC_OW_1H_PM25_P7 = HR_ALL[HR_ALL$P7_PM25 > HeSt_1H_PM25,][,c(1,2, which(colnames(HR_ALL) == "P7_PM25"))]

EXC_OW_1Y_NO2_PST1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_PST1_NO2 > HeSt_1Y_NO2,]$IND
EXC_OW_1Y_NO2_P1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P1_NO2 > HeSt_1Y_NO2,]$IND
EXC_OW_1Y_NO2_P7 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P7_NO2 > HeSt_1Y_NO2,]$IND
EXC_OW_1Y_PM25_PST1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_PST1_PM25 > HeSt_1Y_PM25,]$IND
EXC_OW_1Y_PM25_P1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P1_PM25 > HeSt_1Y_PM25,]$IND
EXC_OW_1Y_PM25_P7 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P7_PM25 > HeSt_1Y_PM25,]$IND

# School pupil
Active.Type = Types[3]
HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL", ".dbf")))
Stats.HR_ALL_IndividualBased = DF.Stats2(HR_ALL, BasedOn = "IND", Time)

EXC_SP_1H_NO2_PST1 = HR_ALL[HR_ALL$PST1_NO2 > HeSt_1H_NO2,][,c(1,2, which(colnames(HR_ALL) == "PST1_NO2"))]
EXC_SP_1H_NO2_P1 = HR_ALL[HR_ALL$P1_NO2 > HeSt_1H_NO2,][,c(1,2, which(colnames(HR_ALL) == "P1_NO2"))]
EXC_SP_1H_NO2_P7 = HR_ALL[HR_ALL$P7_NO2 > HeSt_1H_NO2,][,c(1,2, which(colnames(HR_ALL) == "P7_NO2"))]

EXC_SP_1H_PM25_PST1 = HR_ALL[HR_ALL$PST1_PM25 > HeSt_1H_PM25,][,c(1,2, which(colnames(HR_ALL) == "PST1_PM25"))]
EXC_SP_1H_PM25_P1 = HR_ALL[HR_ALL$P1_PM25 > HeSt_1H_PM25,][,c(1,2, which(colnames(HR_ALL) == "P1_PM25"))]
EXC_SP_1H_PM25_P7 = HR_ALL[HR_ALL$P7_PM25 > HeSt_1H_PM25,][,c(1,2, which(colnames(HR_ALL) == "P7_PM25"))]

EXC_SP_1Y_NO2_PST1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_PST1_NO2 > HeSt_1Y_NO2,]$IND
EXC_SP_1Y_NO2_P1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P1_NO2 > HeSt_1Y_NO2,]$IND
EXC_SP_1Y_NO2_P7 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P7_NO2 > HeSt_1Y_NO2,]$IND
EXC_SP_1Y_PM25_PST1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_PST1_PM25 > HeSt_1Y_PM25,]$IND
EXC_SP_1Y_PM25_P1 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P1_PM25 > HeSt_1Y_PM25,]$IND
EXC_SP_1Y_PM25_P7 = Stats.HR_ALL_IndividualBased[Stats.HR_ALL_IndividualBased$mean_P7_PM25 > HeSt_1Y_PM25,]$IND


class(EXC_OW_1H_NO2_PST1$TIME) = class(Time)
class(EXC_OW_1H_PM25_PST1$TIME) = class(Time)

# class(EXC_OW_1H_NO2_P1$TIME) = class(Time)
# class(EXC_OW_1H_PM25_P1$TIME) = class(Time)
# 
# class(EXC_OW_1H_NO2_P7$TIME) = class(Time)
# class(EXC_OW_1H_PM25_P7$TIME) = class(Time)

class(EXC_SP_1H_NO2_PST1$TIME) = class(Time)
class(EXC_SP_1H_PM25_PST1$TIME) = class(Time)

class(EXC_HO_1H_NO2_P1$TIME) = class(Time)
class(EXC_HO_1H_PM25_P1$TIME) = class(Time)

EXC_OW_1H_NO2_PST1.Li = list()
EXC_OW_1H_PM25_PST1.Li = list()
EXC_OW_1H_PM25 = NA
EXC_OW_1H_NO2 = NA

# EXC_OW_1H_NO2_P1.Li = list()
# EXC_OW_1H_PM25_P1.Li = list()
# EXC_OW_1H_PM25_P1 = NA
# EXC_OW_1H_NO2_P1 = NA

EXC_SP_1H_NO2_PST1.Li = list()
EXC_SP_1H_PM25_PST1.Li = list()
EXC_SP_1H_PM25 = NA
EXC_SP_1H_NO2 = NA

EXC_HO_1H_NO2_P1.Li = list()
EXC_HO_1H_PM25_P1.Li = list()
EXC_HO_1H_PM25 = NA
EXC_HO_1H_NO2 = NA

for (i in (1:GroupSize))
{
  EXC_OW_1H_NO2_PST1.Li[[i]] = EXC_OW_1H_NO2_PST1[EXC_OW_1H_NO2_PST1$IND == i,]
  EXC_OW_1H_NO2[i] = length(EXC_OW_1H_NO2_PST1.Li[[i]]$IND)
  EXC_OW_1H_PM25_PST1.Li[[i]] = EXC_OW_1H_PM25_PST1[EXC_OW_1H_PM25_PST1$IND == i,]
  EXC_OW_1H_PM25[i] = length(EXC_OW_1H_PM25_PST1.Li[[i]]$IND)
  
  # EXC_OW_1H_NO2_P1.Li[[i]] = EXC_OW_1H_NO2_P1[EXC_OW_1H_NO2_P1$IND == i,]
  # EXC_OW_1H_NO2_P1[i] = length(EXC_OW_1H_NO2_P1.Li[[i]]$IND)
  # EXC_OW_1H_PM25_P1.Li[[i]] = EXC_OW_1H_PM25_P1[EXC_OW_1H_PM25_P1$IND == i,]
  # EXC_OW_1H_PM25_P1[i] = length(EXC_OW_1H_PM25_P1.Li[[i]]$IND)
  
  EXC_SP_1H_NO2_PST1.Li[[i]] = EXC_SP_1H_NO2_PST1[EXC_SP_1H_NO2_PST1$IND == i,]
  EXC_SP_1H_NO2[i] = length(EXC_SP_1H_NO2_PST1.Li[[i]]$IND)
  EXC_SP_1H_PM25_PST1.Li[[i]] = EXC_SP_1H_PM25_PST1[EXC_SP_1H_PM25_PST1$IND == i,]
  EXC_SP_1H_PM25[i] = length(EXC_SP_1H_PM25_PST1.Li[[i]]$IND)

  EXC_HO_1H_NO2_P1.Li[[i]] = EXC_HO_1H_NO2_P1[EXC_HO_1H_NO2_P1$IND == i,]
  EXC_HO_1H_NO2[i] = length(EXC_HO_1H_NO2_P1.Li[[i]]$IND)
  EXC_HO_1H_PM25_P1.Li[[i]] = EXC_HO_1H_PM25_P1[EXC_HO_1H_PM25_P1$IND == i,]
  EXC_HO_1H_PM25[i] = length(EXC_HO_1H_PM25_P1.Li[[i]]$IND)
}

length(which(EXC_OW_1H_NO2 > 0))
length(which(EXC_OW_1H_NO2 > 20))
length(which(EXC_OW_1H_PM25 > 0))
length(which(EXC_OW_1H_PM25 > 20))
sum(EXC_OW_1H_NO2)
sum(EXC_OW_1H_PM25)

length(which(EXC_SP_1H_NO2 > 0))
length(which(EXC_SP_1H_NO2 > 20))
length(which(EXC_SP_1H_PM25 > 0))
length(which(EXC_SP_1H_PM25 > 20))
sum(EXC_SP_1H_NO2)
sum(EXC_SP_1H_PM25)

length(which(EXC_HO_1H_NO2 > 0))
length(which(EXC_HO_1H_NO2 > 20))
length(which(EXC_HO_1H_PM25 > 0))
length(which(EXC_HO_1H_PM25 > 20))
sum(EXC_HO_1H_NO2)
sum(EXC_HO_1H_PM25)

# %'s of group
EXC_OW_1Y_NO2_PST1.perc = length(EXC_OW_1Y_NO2_PST1)/GroupSize*100
EXC_OW_1Y_NO2_P1.perc = length(EXC_OW_1Y_NO2_P1)/GroupSize*100
EXC_OW_1Y_NO2_P7.perc = length(EXC_OW_1Y_NO2_P7)/GroupSize*100

EXC_OW_1Y_PM25_PST1.perc = length(EXC_OW_1Y_PM25_PST1)/GroupSize*100
EXC_OW_1Y_PM25_P1.perc = length(EXC_OW_1Y_PM25_P1)/GroupSize*100
EXC_OW_1Y_PM25_P7.perc = length(EXC_OW_1Y_PM25_P7)/GroupSize*100

EXC_SP_1Y_NO2_PST1.perc = length(EXC_SP_1Y_NO2_PST1)/GroupSize*100
EXC_SP_1Y_NO2_P1.perc = length(EXC_SP_1Y_NO2_P1)/GroupSize*100
EXC_SP_1Y_NO2_P7.perc = length(EXC_SP_1Y_NO2_P7)/GroupSize*100

EXC_SP_1Y_PM25_PST1.perc = length(EXC_SP_1Y_PM25_PST1)/GroupSize*100
EXC_SP_1Y_PM25_P1.perc = length(EXC_SP_1Y_PM25_P1)/GroupSize*100
EXC_SP_1Y_PM25_P7.perc = length(EXC_SP_1Y_PM25_P7)/GroupSize*100

EXC_HO_1Y_NO2_P1.perc = length(EXC_HO_1Y_NO2_P1)/GroupSize*100
EXC_HO_1Y_NO2_P7.perc = length(EXC_HO_1Y_NO2_P7)/GroupSize*100

EXC_HO_1Y_PM25_P1.perc = length(EXC_HO_1Y_PM25_P1)/GroupSize*100
EXC_HO_1Y_PM25_P7.perc = length(EXC_HO_1Y_PM25_P7)/GroupSize*100


## Basic statistics
Active.Type = Types[1]

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

# Which hours
`%ni%` <- Negate(`%in%`)

BasicStatsCalculator <- function(Active.Type, HR_ALL, BusinesDates, ...)
{
  # Active.Type = "01.OW"
  OneHour = 60**2
  
  # HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL", ".dbf")))
  class(HR_ALL$TIME) = class(Time)
  
  SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
                                                        ResidentialProfiles$Type == Active.Type]
  
  Type.Dynamics = ResidentialProfiles$Dynamics[ResidentialProfiles$Type == Active.Type][1]
  
  if (Type.Dynamics == "dynamic") { MoI = c("PST1", "P1", "P7")} else
  {MoI = c("P1", "P7")}
  
  SubProfilesFullPeriod_MoI = SubProfilesFullPeriod[SubProfilesFullPeriod %in%
                                                      paste0(Active.Type, "_", MoI)]
  
  DF = data.frame("MoI" = NA, "Phase" = NA, "Pollutant" = NA, "Mean" = NA, "SD" = NA, "pPST" = NA, "pwPST" = NA,
                  "Median" = NA, "Min" = NA, "Max" = NA)
  
  for (M in SubProfilesFullPeriod_MoI)
  {
    # M = SubProfilesFullPeriod_MoI[1]
    print(paste0(M, ":" ))
    m = which(SubProfilesFullPeriod_MoI %in% M)
    
    if (ResidentialProfiles$Dynamics[ResidentialProfiles$Subtype == M] == "dynamic")
    {
      Leave.P.raw = as.numeric(ResidentialProfiles$TimeLeavingPrimary[ResidentialProfiles$Subtype == M]) / 100
      Leave.P.Minutes = (Leave.P.raw %% 1) * 100 / 60
      Leave.P.StartTime = Leave.P.raw - (Leave.P.raw %% 1) + Leave.P.Minutes
      Leave.S.raw = as.numeric(ResidentialProfiles$TimeLeavingSecondary[ResidentialProfiles$Subtype == M]) / 100
      Leave.S.Minutes = (Leave.S.raw %% 1) * 100 / 60
      Leave.S.StartTime = Leave.S.raw - (Leave.S.raw %% 1) + Leave.S.Minutes
      
      T1 = which(HR_ALL$TIME %in% (BusinesDates + (ceiling(Leave.P.StartTime+1))*OneHour))
      T2 = which(HR_ALL$TIME %in% (BusinesDates + (ceiling(Leave.S.StartTime+1))*OneHour))
      
      Sseq = seq(ceiling(Leave.P.StartTime)+2, Leave.S.StartTime)
      S.List = list()
      for (s in Sseq)
      {
        S.List[[s]] = BusinesDates + s*OneHour
      }
      Sdates = unlist(S.List)
      S = which(HR_ALL$TIME %in% Sdates)
      
      P = which(seq_along(HR_ALL$TIME) %ni% c(T1, T2, S))
      
      length(S)+length(P)+length(T1)+length(T2) == nrow(HR_ALL)
      
    } else # when static
    {
      
    }
    
    for (pol in pollutants)
    {
      # pol = pollutants[1]
      print(paste0(toupper(pol), ":" ))
      
      p = which(pollutants %in% pol)
      
      if (p == 1) {RowExtra = -1}
      if (p == 2) {RowExtra = 0}
      
      CoI = paste0(MoI[m], "_", toupper(pol))
      # CoI2 = which(colnames(HR_ALL) %in% CoI)
      
      if (ResidentialProfiles$Dynamics[ResidentialProfiles$Subtype == M] == "dynamic")
      {
        PST.mean = mean(HR_ALL[,CoI])
        PST.sd = sd(HR_ALL[,CoI])
        PST.med = median(HR_ALL[,CoI])
        PST.min = min(HR_ALL[,CoI])
        PST.max = max(HR_ALL[,CoI])
        
        P.mean = mean(HR_ALL[P,CoI])
        P.sd = sd(HR_ALL[P,CoI])
        P.med = median(HR_ALL[P,CoI])
        P.min = min(HR_ALL[P,CoI])
        P.max = max(HR_ALL[P,CoI])
        
        S.mean = mean(HR_ALL[S,CoI])
        S.sd = sd(HR_ALL[S,CoI])
        S.med = median(HR_ALL[S,CoI])
        S.min = min(HR_ALL[S,CoI])
        S.max = max(HR_ALL[S,CoI])
        
        T1.mean = mean(HR_ALL[T1,CoI])
        T1.sd = sd(HR_ALL[T1,CoI])
        T1.med = median(HR_ALL[T1,CoI])
        T1.min = min(HR_ALL[T1,CoI])
        T1.max = max(HR_ALL[T1,CoI])
        
        T2.mean = mean(HR_ALL[T2,CoI])
        T2.sd = sd(HR_ALL[T2,CoI])
        T2.med = median(HR_ALL[T2,CoI])
        T2.min = min(HR_ALL[T2,CoI])
        T2.max = max(HR_ALL[T2,CoI])
        
        T.mean = mean(HR_ALL[c(T1,T2),CoI])
        T.sd = sd(HR_ALL[c(T1,T2),CoI])
        T.med = median(HR_ALL[c(T1,T2),CoI])
        T.min = min(HR_ALL[c(T1,T2),CoI])
        T.max = max(HR_ALL[c(T1,T2),CoI])
        
        PST_PST = PST.mean / PST.mean * 100
        P_PST = P.mean / PST.mean * 100
        S_PST = S.mean / PST.mean * 100
        T1_PST = T1.mean / PST.mean * 100
        T2_PST = T2.mean / PST.mean * 100
        T_PST = T.mean / PST.mean * 100
        
        lPST = length(S)+length(P)+length(T1)+length(T2)
        
        wPST_PST = PST.mean*(lPST/lPST) / PST.mean*(lPST/lPST) * 100
        wP_PST = P.mean*(length(P)/lPST) / PST.mean*(lPST/lPST) * 100
        wS_PST = S.mean*(length(S)/lPST) / PST.mean*(lPST/lPST) * 100
        wT1_PST = T1.mean*(length(T1)/lPST) / PST.mean*(lPST/lPST) * 100
        wT2_PST = T2.mean*(length(T2)/lPST) / PST.mean*(lPST/lPST) * 100
        wT_PST = T.mean*(length(c(T1,T2))/lPST) / PST.mean*(lPST/lPST) * 100
        
        as.character(sum(wP_PST, wS_PST, wT_PST)) == as.character(100)
        
        print(c(P.mean, P.sd, P_PST, wP_PST, P.med, P.min, P.max))
        print(c(S.mean, S.sd, S_PST, wS_PST, S.med, S.min, S.max))
        print(c(T1.mean, T1.sd, T1_PST, wT1_PST, T1.med, T1.min, T1.max))
        print(c(T2.mean, T2.sd, T2_PST, wT2_PST, T2.med, T2.min, T2.max))
        print(c(T.mean, T.sd, T_PST, wT_PST, T.med, T.min, T.max))
        
        # if (m == 1) {RowBase = 0}
        # if (m == 2) {RowBase = 12}
        # if (m == 3) {RowBase = 14}
        
        if (is.na(DF[1,1])) {R = 1}
        if (!is.na(DF[1,1])) {R = nrow(DF)+1}
        
        DF[R,] = c(MoI[m], "PST", pol, PST.mean, PST.sd, PST_PST, wPST_PST, PST.med, PST.min, PST.max)
        DF[nrow(DF)+1,] = c(MoI[m], "P",pol, P.mean, P.sd, P_PST, wP_PST, P.med, P.min, P.max)
        DF[nrow(DF)+1,] = c(MoI[m], "S",pol, S.mean, S.sd, S_PST, wS_PST, S.med, S.min, S.max)
        DF[nrow(DF)+1,] = c(MoI[m], "T1",pol, T1.mean, T1.sd, T1_PST, wT1_PST, T1.med, T1.min, T1.max)
        DF[nrow(DF)+1,] = c(MoI[m], "T2",pol, T2.mean, T2.sd, T2_PST, wT2_PST, T2.med, T2.min, T2.max)
        DF[nrow(DF)+1,] = c(MoI[m], "T",pol, T.mean, T.sd, T_PST, wT_PST, T.med, T.min, T.max)
        
      } else # if static
      {
        P.mean = mean(HR_ALL[,CoI])
        P.sd = sd(HR_ALL[,CoI])
        P.med = median(HR_ALL[,CoI])
        P.min = min(HR_ALL[,CoI])
        P.max = max(HR_ALL[,CoI])
        
        print(c(P.mean, P.sd, P.med, P.min, P.max))
        
        # if (m == 1) {RowBase = 0}
        # if (m == 2) {RowBase = 2}
        
        if (is.na(DF[1,1])) {R = 1}
        if (!is.na(DF[1,1])) {R = nrow(DF)+1}
        
        DF[R,] = c(MoI[m], "P", pol, P.mean, P.sd, NA, NA, P.med, P.min, P.max)
      }
    } # closing pol
    # SaveAsFile(DF, paste("BasicStats1", Active.Type, sep = "_"), "csv", TRUE)
    SaveAsFile(INput = DF, Filename = paste("BasicStats1", Active.Type, sep = "_"),
               Format = "csv", OverwriteLayer = TRUE)
  }
  print(DF)
  
  for (pol in pollutants)
  {
    # pol = pollutants[1]
    print(paste0(toupper(pol), ":" ))
    p = which(pollutants %in% pol)
    
    DF.xy = data.frame(cbind(matrix(0, ncol = length(MoI), nrow = length(MoI)), MoI))
    DF.p = DF.xy
    
    for (r in seq_along(MoI))
    {
      for (c in seq_along(MoI))
      {
        class(DF.xy[,c]) = "numeric"
        class(DF.p[,c]) = "numeric"
        
        DF.xy[r,c] = -1*(as.numeric(DF$Mean[DF$MoI == MoI[r] & DF$Pollutant == pol])[1] - 
                           as.numeric(DF$Mean[DF$MoI == MoI[c] & DF$Pollutant == pol])[1])
        
        DF.p[r,c] = (DF.xy[r,c] / as.numeric(DF$Mean[DF$MoI == MoI[r] & DF$Pollutant == pol])[1]) * 100
      }
    }
    print("Difference:")
    DF.xy = cbind(DF.xy[,length(DF.xy)], DF.xy[,1:(length(DF.xy)-1)])
    colnames(DF.xy) = c("MoI", MoI)
    print(DF.xy)
    SaveAsFile(INput = DF.xy, Filename = paste("BasicStats2", Active.Type, pol,
                                               "Difference", sep = "_"), Format = "csv", OverwriteLayer = TRUE)
    
    print("Percentage:")
    DF.p = cbind(DF.p[,length(DF.p)], DF.p[,1:(length(DF.p)-1)])
    colnames(DF.p) = c("MoI", MoI)
    print(DF.p)
    SaveAsFile(INput = DF.p, Filename = paste("BasicStats2", Active.Type, pol,
                                              "Percentage", sep = "_"), Format = "csv", OverwriteLayer = TRUE)
  }
  
}

BasicStatsCalculator2 <- function(Active.Type, HR_ALL, BusinesDates, ...)
{
  # Active.Type = "01.OW"
  OneHour = 60**2
  
  SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
                                                        ResidentialProfiles$Type == Active.Type]
  
  Type.Dynamics = ResidentialProfiles$Dynamics[ResidentialProfiles$Type == Active.Type][1]
  
  if (Type.Dynamics == "dynamic") { MoI = c("PST1", "P1", "P7")} else
  {MoI = c("P1", "P7")}
  
  SubProfilesFullPeriod_MoI = SubProfilesFullPeriod[SubProfilesFullPeriod %in%
                                                      paste0(Active.Type, "_", MoI)]
  
  DF = data.frame("MoI" = NA, "Phase" = NA, "Pollutant" = NA, "Mean" = NA, "SD" = NA, "pPST" = NA, "pwPST" = NA,
                  "Median" = NA, "Min" = NA, "Max" = NA)
  
  for (M in SubProfilesFullPeriod_MoI)
  {
    # M = SubProfilesFullPeriod_MoI[1]
    print(paste0(M, ":" ))
    m = which(SubProfilesFullPeriod_MoI %in% M)
    
    HR_ALL = read.dbf(file = file.path(output.dir, M, paste0("HR_", "ALL", ".dbf")))
    class(HR_ALL$TIME) = class(Time)
    
    if (ResidentialProfiles$Dynamics[ResidentialProfiles$Subtype == M] == "dynamic")
    {
      # Leave.P.raw = as.numeric(ResidentialProfiles$TimeLeavingPrimary[ResidentialProfiles$Subtype == M]) / 100
      # Leave.P.Minutes = (Leave.P.raw %% 1) * 100 / 60
      # Leave.P.StartTime = Leave.P.raw - (Leave.P.raw %% 1) + Leave.P.Minutes
      # Leave.S.raw = as.numeric(ResidentialProfiles$TimeLeavingSecondary[ResidentialProfiles$Subtype == M]) / 100
      # Leave.S.Minutes = (Leave.S.raw %% 1) * 100 / 60
      # Leave.S.StartTime = Leave.S.raw - (Leave.S.raw %% 1) + Leave.S.Minutes
      # 
      # T1 = which(HR_ALL$TIME %in% (BusinesDates + (ceiling(Leave.P.StartTime+1))*OneHour))
      # T2 = which(HR_ALL$TIME %in% (BusinesDates + (ceiling(Leave.S.StartTime+1))*OneHour))
      # 
      # Sseq = seq(ceiling(Leave.P.StartTime)+2, Leave.S.StartTime)
      # S.List = list()
      # for (s in Sseq)
      # {
      #   S.List[[s]] = BusinesDates + s*OneHour
      # }
      # Sdates = unlist(S.List)
      # S = which(HR_ALL$TIME %in% Sdates)
      # 
      # P = which(seq_along(HR_ALL$TIME) %ni% c(T1, T2, S))
      # 
      # length(S)+length(P)+length(T1)+length(T2) == nrow(HR_ALL)
      
      P = which(HR_ALL$Pweight != 0)
      S = which(HR_ALL$Sweight != 0)
      T1 = which(HR_ALL$T1weight != 0)
      T2 = which(HR_ALL$T2weight != 0)
      # T12 = which(HR_ALL$T1weight != 0 | HR_ALL$T2weight != 0)
      
    } else # when static
    {
      
    }
    
    for (pol in pollutants)
    {
      # pol = pollutants[1]
      print(paste0(toupper(pol), ":" ))
      
      p = which(pollutants %in% pol)
      
      if (p == 1) {RowExtra = -1}
      if (p == 2) {RowExtra = 0}
      
      # CoI = paste0(MoI[m], "_", toupper(pol))
      CoI = paste0("EXP_", toupper(pol))
      
      # CoI2 = which(colnames(HR_ALL) %in% CoI)
      
      if (ResidentialProfiles$Dynamics[ResidentialProfiles$Subtype == M] == "dynamic")
      {
        PST.mean = mean(HR_ALL[,CoI])
        PST.sd = sd(HR_ALL[,CoI])
        PST.med = median(HR_ALL[,CoI])
        PST.min = min(HR_ALL[,CoI])
        PST.max = max(HR_ALL[,CoI])
        
        P.mean = mean(HR_ALL[P,CoI] * HR_ALL$Pweight[P])
        P.sd = sd(HR_ALL[P,CoI] * HR_ALL$Pweight[P])
        P.med = median(HR_ALL[P,CoI] * HR_ALL$Pweight[P])
        P.min = min(HR_ALL[P,CoI] * HR_ALL$Pweight[P])
        P.max = max(HR_ALL[P,CoI] * HR_ALL$Pweight[P])
        
        S.mean = mean(HR_ALL[S,CoI] * HR_ALL$Sweight[S])
        S.sd = sd(HR_ALL[S,CoI] * HR_ALL$Sweight[S])
        S.med = median(HR_ALL[S,CoI] * HR_ALL$Sweight[S])
        S.min = min(HR_ALL[S,CoI] * HR_ALL$Sweight[S])
        S.max = max(HR_ALL[S,CoI] * HR_ALL$Sweight[S])
        
        T1.mean = mean(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1])
        T1.sd = sd(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1])
        T1.med = median(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1])
        T1.min = min(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1])
        T1.max = max(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1])
        
        T2.mean = mean(HR_ALL[T2,CoI] * HR_ALL$T2weight[T2])
        T2.sd = sd(HR_ALL[T2,CoI] * HR_ALL$T2weight[T2])
        T2.med = median(HR_ALL[T2,CoI] * HR_ALL$T2weight[T2])
        T2.min = min(HR_ALL[T2,CoI] * HR_ALL$T2weight[T2])
        T2.max = max(HR_ALL[T2,CoI] * HR_ALL$T2weight[T2])
        
        T.mean = mean(c(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1], HR_ALL[T2,CoI] * HR_ALL$T2weight[T2]))
        T.sd = sd(c(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1], HR_ALL[T2,CoI] * HR_ALL$T2weight[T2]))
        T.med = median(c(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1], HR_ALL[T2,CoI] * HR_ALL$T2weight[T2]))
        T.min = min(c(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1], HR_ALL[T2,CoI] * HR_ALL$T2weight[T2]))
        T.max = max(c(HR_ALL[T1,CoI] * HR_ALL$T1weight[T1], HR_ALL[T2,CoI] * HR_ALL$T2weight[T2]))
        
        PST_PST = PST.mean / PST.mean * 100
        P_PST = P.mean / PST.mean * 100
        S_PST = S.mean / PST.mean * 100
        T1_PST = T1.mean / PST.mean * 100
        T2_PST = T2.mean / PST.mean * 100
        T_PST = T.mean / PST.mean * 100
        
        lPST = length(S)+length(P)+length(T1)+length(T2)
        
        wPST_PST = PST.mean*(lPST/lPST) / PST.mean*(lPST/lPST) * 100
        wP_PST = P.mean*(length(P)/lPST) / PST.mean*(lPST/lPST) * 100
        wS_PST = S.mean*(length(S)/lPST) / PST.mean*(lPST/lPST) * 100
        wT1_PST = T1.mean*(length(T1)/lPST) / PST.mean*(lPST/lPST) * 100
        wT2_PST = T2.mean*(length(T2)/lPST) / PST.mean*(lPST/lPST) * 100
        wT_PST = T.mean*(length(c(T1,T2))/lPST) / PST.mean*(lPST/lPST) * 100
        
        as.character(sum(wP_PST, wS_PST, wT_PST)) == as.character(100)
        
        print(c(P.mean, P.sd, P_PST, wP_PST, P.med, P.min, P.max))
        print(c(S.mean, S.sd, S_PST, wS_PST, S.med, S.min, S.max))
        print(c(T1.mean, T1.sd, T1_PST, wT1_PST, T1.med, T1.min, T1.max))
        print(c(T2.mean, T2.sd, T2_PST, wT2_PST, T2.med, T2.min, T2.max))
        print(c(T.mean, T.sd, T_PST, wT_PST, T.med, T.min, T.max))
        
        # if (m == 1) {RowBase = 0}
        # if (m == 2) {RowBase = 12}
        # if (m == 3) {RowBase = 14}
        
        if (is.na(DF[1,1])) {R = 1}
        if (!is.na(DF[1,1])) {R = nrow(DF)+1}
        
        DF[R,] = c(MoI[m], "PST", pol, PST.mean, PST.sd, PST_PST, wPST_PST, PST.med, PST.min, PST.max)
        DF[nrow(DF)+1,] = c(MoI[m], "P",pol, P.mean, P.sd, P_PST, wP_PST, P.med, P.min, P.max)
        DF[nrow(DF)+1,] = c(MoI[m], "S",pol, S.mean, S.sd, S_PST, wS_PST, S.med, S.min, S.max)
        DF[nrow(DF)+1,] = c(MoI[m], "T1",pol, T1.mean, T1.sd, T1_PST, wT1_PST, T1.med, T1.min, T1.max)
        DF[nrow(DF)+1,] = c(MoI[m], "T2",pol, T2.mean, T2.sd, T2_PST, wT2_PST, T2.med, T2.min, T2.max)
        DF[nrow(DF)+1,] = c(MoI[m], "T",pol, T.mean, T.sd, T_PST, wT_PST, T.med, T.min, T.max)
        
      } else # if static
      {
        P.mean = mean(HR_ALL[,CoI])
        P.sd = sd(HR_ALL[,CoI])
        P.med = median(HR_ALL[,CoI])
        P.min = min(HR_ALL[,CoI])
        P.max = max(HR_ALL[,CoI])
        
        print(c(P.mean, P.sd, P.med, P.min, P.max))
        
        # if (m == 1) {RowBase = 0}
        # if (m == 2) {RowBase = 2}
        
        if (is.na(DF[1,1])) {R = 1}
        if (!is.na(DF[1,1])) {R = nrow(DF)+1}
        
        DF[R,] = c(MoI[m], "P", pol, P.mean, P.sd, NA, NA, P.med, P.min, P.max)
      }
    } # closing pol
    # SaveAsFile(DF, paste("BasicStats1", Active.Type, sep = "_"), "csv", TRUE)
    SaveAsFile(INput = DF, Filename = paste("BasicStats1", Active.Type, sep = "_"),
               Format = "csv", OverwriteLayer = TRUE)
  }
  print(DF)
  
  for (pol in pollutants)
  {
    # pol = pollutants[1]
    print(paste0(toupper(pol), ":" ))
    p = which(pollutants %in% pol)
    
    DF.xy = data.frame(cbind(matrix(0, ncol = length(MoI), nrow = length(MoI)), MoI))
    DF.p = DF.xy
    
    for (r in seq_along(MoI))
    {
      for (c in seq_along(MoI))
      {
        class(DF.xy[,c]) = "numeric"
        class(DF.p[,c]) = "numeric"
        
        DF.xy[r,c] = as.numeric(DF$Mean[DF$MoI == MoI[r] & DF$Pollutant == pol])[1] - 
          as.numeric(DF$Mean[DF$MoI == MoI[c] & DF$Pollutant == pol])[1]
        
        DF.p[r,c] = (DF.xy[r,c] / as.numeric(DF$Mean[DF$MoI == MoI[r] & DF$Pollutant == pol])[1]) * 100
      }
    }
    print("Difference:")
    DF.xy = cbind(DF.xy[,length(DF.xy)], DF.xy[,1:(length(DF.xy)-1)])
    colnames(DF.xy) = c("MoI", MoI)
    print(DF.xy)
    SaveAsFile(INput = DF.xy, Filename = paste("BasicStats2", Active.Type, pol,
                                               "Difference", sep = "_"), Format = "csv", OverwriteLayer = TRUE)
    
    print("Percentage:")
    DF.p = cbind(DF.p[,length(DF.p)], DF.p[,1:(length(DF.p)-1)])
    colnames(DF.p) = c("MoI", MoI)
    print(DF.p)
    SaveAsFile(INput = DF.p, Filename = paste("BasicStats2", Active.Type, pol,
                                              "Percentage", sep = "_"), Format = "csv", OverwriteLayer = TRUE)
  }
  
}

# plotting the Basic statistics bar chart
for (Active.Type in Types)
{
  # Active.Type = "01.OW"
  
  if (!all(file.exists(file.path(output.dir, paste0("BasicStats2", "_", Active.Type, "_", pollutants, "_", "Percentage", ".csv")))))
  {
    HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL", ".dbf")))
    class(HR_ALL$TIME) = class(Time)
    
    BasicStatsCalculator(Active.Type, HR_ALL, BusinesDates)
  }
  DF.BasicStats1 = read.csv(file.path(output.dir, paste0("BasicStats1", "_", Active.Type, ".csv")))
  
  Profiles <- c("Office workers", "Homeworkers","School pupils")
  pro = which(Types %in% Active.Type)
  print(Profiles[pro])
  SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
                                                        ResidentialProfiles$Type == Active.Type]
  
  Type.Dynamics = ResidentialProfiles$Dynamics[ResidentialProfiles$Type == Active.Type][1]
  if (Type.Dynamics == "dynamic") { MoI = c("PST1", "P1", "P7")} else
  {MoI = c("P1", "P7")}
  
  p.Li = list()
  for (pol in pollutants)
  {
    # pol = pollutants[2]
    polnr = which(pollutants %in% pol)
    DF.BasicStats2_Diff = read.csv(file.path(output.dir, paste0("BasicStats2", "_", Active.Type, "_", pol, "_", "Difference", ".csv")))
    DF.BasicStats2_Perc = read.csv(file.path(output.dir, paste0("BasicStats2", "_", Active.Type, "_", pol, "_", "Percentage", ".csv")))
    
    # diagram
    data = rbind(DF.BasicStats1[DF.BasicStats1$MoI == "PST1" & DF.BasicStats1$Pollutant == pol,][1,], 
                 DF.BasicStats1[DF.BasicStats1$MoI != "PST1" & DF.BasicStats1$Pollutant == pol,])
    data = data[!is.na(data[,1]),]
    
    diffr = NA
    Diff = NA
    Perc = NA
    counter = 0
    for (cm1 in data$MoI)
    {
      for (cm2 in data$MoI)
      {
        if (cm1 == cm2) {next}
        counter = counter+1
        diffr[counter] = paste(cm1, cm2, sep = "~")
        
        Diff[counter] = DF.BasicStats2_Diff[DF.BasicStats2_Diff$MoI == cm1,cm2]
        Perc[counter] = DF.BasicStats2_Perc[DF.BasicStats2_Perc$MoI == cm1,cm2]
      }
    }
    rm(counter)
    x = c(MoI, diffr)
    means = c(data$Mean, rep(0, each = length(Perc)))
    SDs = c(data$SD, rep(0, each = length(Perc)))
    base = c(rep(0, each = length(MoI)), rep(data$Mean, each = length(MoI)-1))
    diff = c(rep(0, each = length(MoI)), Diff)
    text1 = c(rep('', each = length(MoI)), round(diff[(length(MoI)+1):length(diff)], 2))
    text2 = c(paste(round(data$Mean,2), "±", paste(round(data$SD,2))), paste(round(Perc, 2), "%"))
    
    
    FillCol = diff
    FillCol[diff == 0] = 'rgba(55, 128, 191, 0.7)'
    FillCol[diff < 0] = 'rgba(219, 64, 82, 0.7)'
    FillCol[diff > 0] = 'rgba(35, 139, 69, 0.7)'
    LineCol = diff
    LineCol[diff == 0] = 'rgba(55, 128, 191, 1)'
    LineCol[diff < 0] = 'rgba(219, 64, 82, 1)'
    LineCol[diff > 0] = 'rgba(35, 139, 69, 1)'
    
    ErrorBars = FALSE
    if (ErrorBars) {ErCol = 'black'} else {ErCol = 'rgba(55, 128, 191, 0)'}
    
    p <- plot_ly(x = x, y = base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)')) %>%
      add_trace(y = means, text = text1 , textposition = 'auto', error_y = list(array = SDs, color = ErCol),
                marker = list(color = 'rgba(55, 128, 191, 0.7)',
                              line = list(color = 'rgba(55, 128, 191, 0.7)', width = 2))) %>%
      add_trace(y = diff, text = text2, textposition = 'auto',
                marker = list(color = FillCol, line = list(color = LineCol, width = 2))) %>%
      layout(title = paste0('Mean ', toupper(pol), ' concentrations and differences for ', Profiles[pro]),
             xaxis = list(title = "Calculation methods"),
             yaxis = list(title = "??g/m³", range = c(min(data$Mean)-2,max(data$Mean)+2)),
             barmode = 'stack',
             # paper_bgcolor = 'rgba(245, 246, 249, 1)',
             # plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
    
    print(c(min(data$Mean)-2,max(data$Mean)+2))
    print(pol)
    p.Li[[polnr]] = p
    
    CurrentDateAndTime = Sys.time() + 2*60**2
    NamePlotType = "GISCAPE_MeanConcentrations"
    NameForInteractivePlot = paste(NamePlotType, Active.Type, toupper(pol),
                                   paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                          format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                   sep = "_")
    
    widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
    htmlwidgets::saveWidget(p, widget_out)

    Sys.sleep(5)
    
    rm(p)
  } # closing pol
  p.Li[[1]]
  p.Li[[2]]
  
} # closing Active.Type



# Box plot
dataBox = head(HR_ALL, GroupSize*24*2)

p <- plot_ly(y = HR_ALL$P1_NO2, type = "box") %>%
  add_trace(y = ~rnorm(50, 1))

p <- plot_ly(data = dataBox, x = "test_x", y = ~PST1_NO2, type = "box") %>%
  layout(boxmode = "group")
p

p <- plot_ly(data = HR_ALL, x = "test_x", y = ~PST1_NO2, type = "box") %>%
  layout(boxmode = "group")

Profiles <- c("Office workers", "Homeworkers","School pupils")
pol = pollutants[1]

CurrentDateAndTime = Sys.time() + 1*60**2
NameForInteractivePlot = paste("GISCAPE_SummaryStats", Profiles[1], toupper(pol),
                               paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                      format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                               sep = "_")

widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
htmlwidgets::saveWidget(p, widget_out)

# regular box plot
boxplot(data = dataBox, x = dataBox$PST1_NO2, y = ~PST1_NO2, main = "SummaryStats",
        xlab = "test_x", ylab = "??g/m^3")

box1 = boxplot(data = HR_ALL, x = HR_ALL$PST1_NO2, y = ~PST1_NO2, main = "SummaryStats",
               xlab = "test_x", ylab = "??g/m^3")
points(max(HR_ALL$PST1_NO2), col = "red", pch = 2)
points(min(HR_ALL$PST1_NO2), col = "red", pch = 6)
points(mean(HR_ALL$PST1_NO2), colour = "green", pch = 151)

# efficient box plot (without crashes on large data)
statsWS = c(quantile(HR_ALL$PST1_NO2), mean(HR_ALL$PST1_NO2), sd(HR_ALL$PST1_NO2))
names(statsWS)[6] = "Mean"
names(statsWS)[7] = "SD"


# violin plot
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/violin_data.csv")
df = dataBox

p <- df %>%
  plot_ly(x = "test_x", y = ~PST1_NO2, type = 'violin', #split = ~day
          box = list(visible = T), meanline = list(visible = T)) %>% 
  layout(xaxis = list(title = "Day"),
         yaxis = list(title = "Total Bill", zeroline = F)
  )
p

p = plot_ly(data = df, x = "test_x", y = ~PST1_NO2, type = 'violin', #split = ~day
            box = list(visible = T), meanline = list(visible = T)) %>% 
  
  
  unloadNamespace("sqldf")

gg.box = ggplot(dataBox[3], aes(x = colnames(dataBox[3]), y = PST1_NO2)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  #geom_jitter(position = position_jitter(width = .1, height = 0), alpha=I(0.01)) +
  geom_point(y = max(dataBox[3]), colour = "red", pch = 2) +
  geom_point(y = min(dataBox[3]), colour = "red", pch = 6) +
  geom_point(y = mean(dataBox[3]), colour = "green", pch = 151) +
  ggtitle(paste0('Summarized ', toupper(pol), ' concentrations and differences for ', Profiles[pro]))
gg.box

gg.box = ggplot(HR_ALL[3], aes(x = colnames(HR_ALL[3]), y = PST1_NO2)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  #geom_jitter(position = position_jitter(width = .1, height = 0), alpha=I(0.01)) +
  geom_point(y = max(HR_ALL[3]), colour = "red", pch = 2) +
  geom_point(y = min(HR_ALL[3]), colour = "red", pch = 6) +
  geom_point(y = mean(HR_ALL[3]), colour = "green", pch = 151) +
  ggtitle(paste0('Summarized ', toupper(pol), ' concentrations and differences for ', Profiles[pro]))
gg.box



# pie charts for yearly EXC

p = Plot.EXCpie("01.OW",EXC_OW_1Y_NO2_PST1.perc, EXC_OW_1Y_PM25_PST1.perc, "Online")
p
p = Plot.EXCpie("03.SP", EXC_SP_1Y_NO2_PST1.perc, EXC_SP_1Y_PM25_PST1.perc, "Online")
p
p = Plot.EXCpie("02.HO", EXC_OW_1Y_NO2_P1.perc, EXC_OW_1Y_PM25_P1.perc, "Online")
p

# Individual hourly EXC percentages
Profiles2 = c("Office workers", "School pupils", "Homeworkers")
Col.Profiles = c("#2c7bb6", "#fdae61", "#b8e186")

p = Plot.EXCpercentages(EXC_OW_1H_NO2, EXC_SP_1H_NO2, EXC_HO_1H_NO2,
                        Profiles2, Col.Profiles, pollutants[1], GroupSize, "Online")
p
p = Plot.EXCpercentages(EXC_OW_1H_PM25, EXC_SP_1H_PM25, EXC_HO_1H_PM25,
                        Profiles2, Col.Profiles, pollutants[2], GroupSize, "Online")
p




p = Plot.EXCpercentages2(EXC_OW_1H_PM25_PST1, EXC_OW_1H_PM25_P1, EXC_OW_1H_PM25_P7,
                         EXC_SP_1H_PM25_PST1, EXC_SP_1H_PM25_P1, EXC_SP_1H_PM25_P7,
                         EXC_HO_1H_PM25_P1, EXC_HO_1H_PM25_P7,
                         Profiles2, Col.Profiles, pollutants[2], GroupSize)
p

# Individual hourly EXC frequncies

p = Plot.EXCfrequences(EXC_OW_1H_NO2, EXC_SP_1H_NO2, EXC_HO_1H_NO2,
                         Profiles2, Col.Profiles, pollutants[1], Location = "Online")
p

p = Plot.EXCfrequences(EXC_OW_1H_PM25, EXC_SP_1H_PM25, EXC_HO_1H_PM25,
                       Profiles2, Col.Profiles, pollutants[2], Location = "Online")
p


## Cumsum (for nice graph, with AQ standards)

for (Active.Type in Types)
{
  # Active.Type = Types[2]
  HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_ALL", ".dbf")))
  
  class(HR_ALL$TIME) = class(Time)
  
  pol = pollutants[2]
  
  # Calculation method and pollutant of interest
  CMoI_PoI = colnames(HR_ALL)[grepl(MoI[1], colnames(HR_ALL)) & grepl(toupper(pol), colnames(HR_ALL))]
  
  cm = which(colnames(HR_ALL) %in% CMoI_PoI)
  DF.CumSum = cbind(HR_ALL[, c("TIME", "IND")][order(HR_ALL[, c("TIME", "IND")]$IND),],
                    unlist(tapply(HR_ALL[,cm], HR_ALL[,"IND"], cumsum)))
  colnames(DF.CumSum)[3] = "cum"
  class(DF.CumSum$TIME) = class(Time)
  
  DF.CumSum[,4] = as.integer(DF.CumSum[,3])
  colnames(DF.CumSum)[4] = "cumInt"
  
  # 
  # for (cm in 3:length(HR_ALL))
  # {
  #   # CumSum = tapply(HR_ALL[,cm], HR_ALL[,"IND"], cumsum)
  #   # cum = unlist(CumSum)
  #   # BaseHR = HR_ALL[, c("TIME", "IND")]
  #   # BaseHR = BaseHR[order(BaseHR$IND),]
  #   # DF.CumSum = cbind(BaseHR, cum)
  #   
  #   
  #   # Plot.CumExposureGraph2(DF.CumSum[DF.CumSum$IND == 1:GroupSize,], pol)
  #   # Plot.CumExposureGraph2(DF.CumSum, pol)
  #   #Plot.CumExposureGraph2(DF.CumSum[DF.CumSum$IND == 1:250,], pol)
  #   
  #   # DF.CumSum2 = DF.CumSum[DF.CumSum$TIME == tail(Time,1),]
  #   # DF.CumSum3 = DF.CumSum2[order(DF.CumSum2$cum, decreasing = TRUE),]
  #   # 
  #   # head(DF.CumSum3, 10)
  # }
  # 
  # # Query, Pie chart / donut plot
  # for (cm in 3:length(HR_ALL))
  # {
  #   print(HR_ALL[HR_ALL[, cm] > 150, c(1,2,cm)])
  # }
  # HR_ALL[HR_ALL[, (3:length(HR_ALL))] > 150,]
  
  # How many individuals above 150?
  HR_ALL[HR_ALL$PST1_NO2 > 150,]
  
  # How many individuals of 1000?
  nrow(HR_ALL[HR_ALL$PST1_NO2 > 100,])
  nrow(HR_ALL[HR_ALL$PST1_PM25 > 100,])
  
  # CUM EXP and EXC hourly
  p = Plot.CumExposureGraph4(Active.Type, Time, DF.CumSum, pol, EXC_OW_1H_PM25_PST1, "Local", 2)
  
  
} # closing Active.Type





p = Plot.CumExposureGraph3(Active.Type, DF.CumSum, pol, EXC_OW_1H_NO2_PST1)
p

p <- plot_ly(x = EXC_OW_1H_PM25, type = "histogram") %>%
  layout(title="Office Workers that exceeded WHO health standard of 60 µg/m³ for PM2.5")
p

p = plot_ly(alpha = 0.3, xbins = list(start=0, end = EXCrange[2], size = 1)) %>%
  add_histogram(x = EXC_OW_1H_PM25) %>%
  add_histogram(x = EXC_SP_1H_PM25) %>%
  add_histogram(x = EXC_HO_1H_PM25) %>%
  layout(barmode = "stack", title="Office Workers that exceeded WHO health standard of 60 µg/m³ for PM2.5")
# barmode = "stack" / "overlay"
p

## Exceedence Histograms
hist(EXC_OW_1H_PM25)

d1 = density(EXC_OW_1H_PM25, bw = 1)
d2 = density(EXC_SP_1H_PM25, bw = 1)
d3 = density(EXC_HO_1H_PM25, bw = 1)

EXCrange = range(c(EXC_OW_1H_PM25, EXC_SP_1H_PM25, EXC_HO_1H_PM25))

f.test = hist(EXC_OW_1H_PM25, xlim = EXCrange, ylim = NULL, breaks = diff(EXCrange))
length(which(EXC_OW_1H_PM25 == 20))


d1[[1]][d1[[1]] < 0] = NA
d1[[2]][d1[[1]] < 0] = NA

d2[[1]][d2[[1]] < 0] = NA
d2[[2]][d2[[1]] < 0] = NA

d3[[1]][d3[[1]] < 0] = NA
d3[[2]][d3[[1]] < 0] = NA

f1 = d1[1:2]
f1[[2]] = f1[[2]] * GroupSize*10

f2 = d2[1:2]
f2[[2]] = f2[[2]] * GroupSize*10

f3 = d3[1:2]
f3[[2]] = f3[[2]] * GroupSize*10


Col.Profiles = c("#2c7bb6", "#fdae61", "#b8e186")
Col.Profiles2 = c("red", "blue", "green")

plot(f1, col= Col.Profiles[1], type = "l", lwd=2, main = paste0("Exceedances frequencies for ", toupper(pol)),
     xlab = "Amount of exceedances", ylab = "Frequency", 
     xlim = c(0,max(c(EXC_OW_1H_PM25, EXC_SP_1H_PM25, EXC_HO_1H_PM25))),
     ylim = c(0,max(c(f1[[2]], f2[[2]], f3[[2]])) + 25))
lines(f2, col = Col.Profiles[2], lwd=2)
lines(f3, col = Col.Profiles[3], lwd=2)

legend("topleft", c("Office workers", "School pupils", "Office @home"), xpd = FALSE, horiz = TRUE, inset = c(0,0),
       bty = "n", bg = "grey", cex = 1, pch = 22, pt.bg = Col.Profiles)






p = plot_ly(x = ~rnorm(50), type = "histogram", breaks = 50)
p

price_hist <- function(method = "FD") {
  # h <- hist(EXC, breaks = method, plot = FALSE)
  # plot_ly(x = h$mids, y = h$counts) %>% add_bars(name = method)
  # plot_ly(x = h$mids, y = h$counts) %>% add_bars(name = method)
  
  y = NA
  x = seq(0, max(EXC), 1)
  for (e in x)
  {
    y[e+1] = length(which(EXC == e))
  }
  # df.Li[[pr]] = data.frame(x,y)
  
  plot_ly(x = x, y = y) %>% add_bars(name = method)
}

test = price_hist()
test

length(which(EXC == 0))


## Make some sample data
x <- sample(0:30, 200, replace=T, prob=15 - abs(15 - 0:30))

## Calculate and plot the two histograms
hcum <- h <- hist(x, plot=FALSE, breaks = diff(EXCrange))
hcum$counts <- cumsum(hcum$counts)
plot(hcum, main="")
plot(h, add=T, col="grey")

## Plot the density and cumulative density
d <- density(x)
lines(x = d$x, y = d$y * length(x) * diff(h$breaks)[1], lwd = 2)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(x), lwd = 2)

hcum <- h <- hist(df.Li[[1]], plot=TRUE, breaks = diff(EXCrange))
plot(df.Li[[1]])
points(, col = "red")


myhist <- hist(x, plot = FALSE)
dens <- density(x)
# axis(side=1, at=seq(0,100, 20), labels=seq(0,100,20))

freqs.y = dens$y*(1/sum(myhist$density))*length(x)
plot(dens$x,freqs.y)




p = plot_ly() %>%
  add_trace(p, data = d, x = d$x, y = d$y * length(x) * diff(h$breaks)[1], name = Profiles2[1], type="scatter", mode="lines",
            color = I(Col.Profiles)[1], alpha = 0.5, sizes = 0.75)
p

# x <- 1:10
# y <- c(2,4,6,8,7,12,14,16,18,20)
# lo <- loess(y~x)
# plot(x,y)
# lines(predict(lo), col='red', lwd=2)
# 
# smoothingSpline = smooth.spline(x, y, spar=0.1)
# plot(x,y)
# lines(smoothingSpline)
# 
# plot(x,y)
# xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
# lines(xl, predict(lo,xl), col='red', lwd=2)

# % plot exccedances





y1 = NA
x1 = seq(0, max(EXC_OW_1H_PM25), 1)
for (e in x1)
{
  y1[e+1] = length(which(EXC_OW_1H_PM25 > e))
}
y1 = y1 / GroupSize * 100
df1 = data.frame(x1,y1)

y2 = NA
x2 = seq(0, max(EXC_SP_1H_PM25), 1)
for (e in x2)
{
  y2[e+1] = length(which(EXC_SP_1H_PM25 > e))
}
y2 = y2 / GroupSize * 100
df2 = data.frame(x2,y2)

y3 = NA
x3 = seq(0, max(EXC_HO_1H_PM25), 1)
for (e in x3)
{
  y3[e+1] = length(which(EXC_HO_1H_PM25 > e))
}
y3 = y3 / GroupSize * 100
df3 = data.frame(x3,y3)


plot(df1, col= Col.Profiles[1], type = "l", lwd=2, main = paste0("% of exceedances for ", toupper(pol)),
     xlab = "Amount of exceedances", ylab = "%", 
     xlim = c(0,max(c(EXC_OW_1H_PM25, EXC_SP_1H_PM25, EXC_HO_1H_PM25))),
     ylim = c(0, 110))
lines(df2, col = Col.Profiles[2], lwd=2)
lines(df3, col = Col.Profiles[3], lwd=2)

Profiles2 = c("Office workers", "School pupils", "Office @home")

legend("topright", Profiles2, xpd = FALSE, horiz = TRUE, inset = c(0,0),
       bty = "n", bg = "grey", cex = 1, pch = 22, pt.bg = Col.Profiles)


# in plotly
p = plot_ly(df1, x = ~x1, y = ~y1, name = Profiles2[1],
            type="scatter", mode="markers", color = I(Col.Profiles[1]), alpha = 0.5, sizes = 0.75) %>%
  layout(
    images = list(
      list(source = "https://raw.githubusercontent.com/wschuc002/ThesisWS/master/backend/img/GISCAPE_150px_2.png?raw=true",
           xref = "paper",
           yref = "paper",
           x= 0,
           y= 1,
           sizex = 0.2,
           sizey = 0.2,
           opacity = 1
      )
    )
  )

p = add_trace(p, data = df2, x = ~x2, y = ~y2, name = Profiles2[2], type="scatter", mode="markers",
              color = I(Col.Profiles)[2], alpha = 0.5, sizes = 0.75)
p = add_trace(p, data = df3, x = ~x3, y = ~y3, name = Profiles2[3], type="scatter", mode="markers",
              color = I(Col.Profiles)[3], alpha = 0.5, sizes = 0.75)
p

p



## 

EXC_NO2_ = c(length(which(EXC_OW_1H_NO2 > 0)), length(which(EXC_HO_1H_NO2 > 0)), length(which(EXC_SP_1H_NO2 > 0)))
EXC_NO2 = EXC_NO2*GroupSize*100
EXC_PM25 = c(length(which(EXC_OW_1H_NO2 > 0)), length(which(EXC_HO_1H_NO2 > 0)), length(which(EXC_SP_1H_NO2 > 0)))
EXC_PM25 = EXC_NO2*GroupSize*100
data <- data.frame(Types, EXC_NO2, EXC_PM25)

p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
  add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

x = c("Office Worker", "School Pupil", "Office @ Home")
y = c(length(which(EXC_OW_1H_PM25 > 0)), length(which(EXC_SP_1H_PM25 > 0)), length(which(EXC_HO_1H_PM25 > 0)))
y = y/GroupSize*100
y2 = c(length(which(EXC_OW_1H_PM25 > 20)), length(which(EXC_SP_1H_PM25 > 20)), length(which(EXC_HO_1H_PM25 > 20)))
y2 = y2/GroupSize*100
text <- c('27% market share', '24% market share', '19% market share')
data <- data.frame(x, y, y2, text)

p <- data %>% 
  plot_ly() %>%
  add_trace(x = ~x, y = ~y, type = 'bar', name='1 or more', 
            text = y, textposition = 'auto',
            marker = list(color = '#fdbb84',
                          line = list(color = 'rgb(50,50,50)', width = 1))) %>%
  add_trace(x = ~x, y = ~y2, type = 'bar', name='20+', 
            text = y2, textposition = 'auto',
            marker = list(color = '#e34a33',
                          line = list(color = 'rgb(50,50,50)', width = 1))) %>%
  layout(title = "Exceedeces WHO health standard of 60 µg/m³ for PM2.5",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

### END OF USED CODE
Sys.setenv(TZ = OriginalTimezone) # changed timezone to its original state






### Biweekly subset
for (Active.Type in Types)
{
  SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
                                                        ResidentialProfiles$Type == Active.Type][c(1,2,5)]
  # SubProfilesBiweeklyPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 1 &
  #                                                       ResidentialProfiles$Type == Active.Type][c(1,2,5)]
  
  HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_ALL", ".dbf")))
  
  HR_ALL_BiWeekly = HR_ALL[HR_ALL$TIME > BIWEEKLY[[1]][1] &
                             HR_ALL$TIME <= tail(BIWEEKLY[[1]], 1) + 24*60**2 |
                             HR_ALL$TIME > BIWEEKLY[[2]][1] &
                             HR_ALL$TIME <= tail(BIWEEKLY[[2]], 1) + 24*60**2 |
                             HR_ALL$TIME > BIWEEKLY[[3]][1] &
                             HR_ALL$TIME <= tail(BIWEEKLY[[3]], 1) + 24*60**2 ,]
  
  nrow(HR_ALL_BiWeekly) == 24*14*3*GroupSize
  
  StrSPlt1 = strsplit(colnames(HR_ALL_BiWeekly)[3:length(HR_ALL_BiWeekly)], "_")
  EvenNr = seq(2, length(unlist(StrSPlt1)), 2)
  PolOfInterest = unlist(StrSPlt1)[EvenNr]
  
  UnevenNr = seq(1, length(unlist(StrSPlt1)), 2)
  MethodsOfInterest = unlist(StrSPlt1)[UnevenNr]
  
  StrSPlt2 = strsplit(MethodsOfInterest, "[A-Z]*")
  NumbersOfInterest = as.numeric(unlist(StrSPlt2)[EvenNr])
  
  ReplacementNumbers = NumbersOfInterest+1
  
  StrSPlt3 = strsplit(MethodsOfInterest, "[0-9]")
  
  ReplacementColumns = paste0(unlist(StrSPlt3), ReplacementNumbers, "_", PolOfInterest)
  
  colnames(HR_ALL_BiWeekly)[3:length(HR_ALL_BiWeekly)] = ReplacementColumns
  
  SaveAsDBF(HR_ALL_BiWeekly, "HR", "HR", Active.Type, FALSE, "ALL_BiWeekly", 0)                       
  
} # closing Active.Type

### Time-based mean FullPeriod
for (Active.Type in Types[1])
{
  if (!file.exists(file.path(output.dir, Active.Type, paste0("HR_", "ALL_MeanPopulation", ".dbf"))))
  {
    # Use same structure
    HourBasedMeanPopulation = HR_ALL[1,]
    HourBasedMeanPopulation[,] = NA
    
    cat("\n")
    
    for (date in Time)
    {
      d = which(Time %in% date)
      cat(paste(paste0(round(d/length(Time)*100, 3), " % ")," "))
      
      HourBasedMeanPopulation[d, "TIME"] = date
      HourBasedMeanPopulation[d, "IND"] = "ALL"
      
      for (col in colnames(HourBasedMeanPopulation)[3:length(HourBasedMeanPopulation)])
      {
        #col = colnames(HourBasedMeanPopulation)[3:length(HourBasedMeanPopulation)][1]
        HourBasedMeanPopulation[d, col] = mean((HR_ALL[HR_ALL$TIME == date, col]))
      } #closing col
    } # closing date
    
    SaveAsDBF(HourBasedMeanPopulation, "HR", "HR", Active.Type, FALSE, "ALL_MeanPopulation", 0)
  } else
  {
    HourBasedMeanPopulation = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_", "ALL_MeanPopulation", ".dbf")))
  }
  
  ## Time-based mean BiWeeklyPeriod
  HourBasedMeanPopulation_BiWeekly = HourBasedMeanPopulation[HourBasedMeanPopulation$TIME > BIWEEKLY[[1]][1] &
                                                               HourBasedMeanPopulation$TIME <= tail(BIWEEKLY[[1]], 1) + 24*60**2 |
                                                               HourBasedMeanPopulation$TIME > BIWEEKLY[[2]][1] &
                                                               HourBasedMeanPopulation$TIME <= tail(BIWEEKLY[[2]], 1) + 24*60**2 |
                                                               HourBasedMeanPopulation$TIME > BIWEEKLY[[3]][1] &
                                                               HourBasedMeanPopulation$TIME <= tail(BIWEEKLY[[3]], 1) + 24*60**2 ,]
  
  colnames(HourBasedMeanPopulation_BiWeekly) = colnames(HR_ALL_BiWeekly)
  
  SaveAsDBF(HourBasedMeanPopulation_BiWeekly, "HR", "HR", Active.Type, FALSE, "ALL_Biweekly_MeanPopulation", 0)
  
} # closing Active.Type

### Individual-based mean
for (Active.Type in Types)
{
  HR_ALL = read.dbf(file = file.path(output.dir, Active.Type, paste0("HR_ALL", ".dbf")))
  
  # Use same structure
  IndividualBasedMean = HR_ALL[1,]
  IndividualBasedMean[,] = NA
  
  for (i in unique(HR_ALL$IND))
  {
    for (col in colnames(IndividualBasedMean)[3:length(IndividualBasedMean)])
    {
      IndividualBasedMean[i, col] = mean(HR_ALL[HR_ALL$IND == i, col])
    } # closing col
    IndividualBasedMean[i, "IND"] = i
  } # closing i
  
  IndividualBasedMean$TIME = year.active
  
  SaveAsDBF(IndividualBasedMean, "HR", "HR", Active.Type, FALSE, "ALL_IndividualMean", 0)
  
} # closing Active.Type

### Individual-based mean BiWeekly
for (Active.Type in Types)
{
  # Use same structure
  IndividualBasedMean_BiWeekly = HR_ALL_BiWeekly[1,]
  rownames(IndividualBasedMean_BiWeekly) = 1
  IndividualBasedMean_BiWeekly[,] = NA
  
  for (i in unique(HR_ALL_BiWeekly$IND))
  {
    for (col in colnames(IndividualBasedMean_BiWeekly)[3:length(IndividualBasedMean_BiWeekly)])
    {
      IndividualBasedMean_BiWeekly[i, col] = mean(HR_ALL_BiWeekly[HR_ALL_BiWeekly$IND == i, col])
    } # closing col
    IndividualBasedMean_BiWeekly[i, "IND"] = i
  } # closing i
  
  IndividualBasedMean_BiWeekly$TIME = year.active
  
  SaveAsDBF(IndividualBasedMean_BiWeekly, "HR", "HR", Active.Type, FALSE, "ALL_Biweekly_IndividualMean", 0)
  
} # closing Active.Type


## Collecting the EXP (HR) values [method 2]

# create HR base
ST.DF.HR.Base.Li = list()
for (i in 1:GroupSize)
{
  ST.DF.HR.Base.Li[[i]] = data.frame(Time, i)
}
ST.DF.HR.Base = do.call(rbind, ST.DF.HR.Base.Li)
rm(ST.DF.HR.Base.Li)
gc()
colnames(ST.DF.HR.Base) = c("TIME", "IND")

ST.DF.HR.Collector = ST.DF.HR.Base

Active.Type = Types[3]
# SubProfilesFullPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
#                                                       ResidentialProfiles$Type == Active.Type]

Subtypes = ResidentialProfiles$Subtype[ResidentialProfiles$Type == Active.Type]

#for (Active.Subtype in Subtypes[1:8])
for (Active.Subtype in SubProfilesFullPeriod[1:2])
{
  #Active.Subtype = SubProfilesFullPeriod[1]
  Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]
  Fragments = 1:20
  
  for (pol in pollutants)
  {
    ## Merging the fragments for HR
    ST.DF.HR.Li = list()
    for (f in Fragments)
    {
      ST.DF.HR.Li[[f]] = read.dbf(file.path(output.dir, paste0(Active.Subtype, "_", f), paste0("HR_", toupper(pol),".dbf")))
    }
    ST.DF.HR = do.call(rbind, ST.DF.HR.Li)
    rm(ST.DF.HR.Li)
    gc()
    
    class(ST.DF.HR$TIME) = class(YearDates)
    #colnames(ST.DF.HR) = c("TIME", "IND", paste(Active.Subtype, "EXP", toupper(pol), sep = "_"))
    
    
    ST.DF.HR.Collector = cbind(ST.DF.HR.Collector, ST.DF.HR$'EXP')
    head(ST.DF.HR.Collector)
    #rename last/newest column name
    colnames(ST.DF.HR.Collector)[ncol(ST.DF.HR.Collector)] = paste(Active.Subtype, "EXP", toupper(pol), sep = "_")
    head(ST.DF.HR.Collector)
  } # closing pol
  
} # closing Active.Subtype


if (!file.exists(file.path(output.dir, Active.Type)))
{
  dir.create(file.path(output.dir, Active.Type))
}




ST.DF.HR.Collector_BiWeekly = ST.DF.HR.Collector[ST.DF.HR.Collector$TIME >= BIWEEKLY[[1]][1] &
                                                   ST.DF.HR.Collector$TIME <= tail(BIWEEKLY[[1]], 1) |
                                                   ST.DF.HR.Collector$TIME >= BIWEEKLY[[2]][1] &
                                                   ST.DF.HR.Collector$TIME <= tail(BIWEEKLY[[2]], 1) |
                                                   ST.DF.HR.Collector$TIME >= BIWEEKLY[[3]][1] &
                                                   ST.DF.HR.Collector$TIME <= tail(BIWEEKLY[[3]], 1) ,]

colnames(ST.DF.HR.Collector_BiWeekly)[3:ncol(ST.DF.HR.Collector_BiWeekly)] = c(paste(SubProfilesBiweeklyPeriod[1], "EXP", toupper(pollutants), sep = "_"),
                                                                               paste(SubProfilesBiweeklyPeriod[2], "EXP", toupper(pollutants), sep = "_"))



## Individual based calculations
INDstats.DF = ST.DF.HR.Collector[1:GroupSize,]
colnames(INDstats.DF)[1] = "METHOD"
INDstats.DF$METHOD = "INDmean"
INDstats.DF$IND = 1:GroupSize
INDstats.DF[,3:ncol(INDstats.DF)] = NA

# Mean calculations
for (c in 3:ncol(ST.DF.HR.Collector))
{
  for (i in INDstats.DF$IND)
  {
    INDstats.DF[INDstats.DF$METHOD == "INDmean" & INDstats.DF$IND == i,c] =
      mean(ST.DF.HR.Collector[ST.DF.HR.Collector$IND == i,][,c], na.rm = TRUE)
  } # closing i
  
} # closing c

CorPlotGraphAndSave(Active.Type, INDstats.DF$`03.SP_PST1_EXP_NO2`, INDstats.DF$`03.SP_PST3_EXP_NO2`,
                    "PST1", "PST3", Width = 720, Height = 480, pol = toupper(pollutants[1]), GroupSize)

for (x in 3:ncol(ST.DF.HR.Collector))
{
  CorPlotGraphAndSave(Active.Type, INDstats.DF[,4], INDstats.DF[,6],
                      "PST1", "PST3", Width = 720, Height = 480, pol = toupper(pollutants[2]), GroupSize)
}

CorPlotTableAndSave(Active.Type, INDstats.DF[,3], INDstats.DF[,5], OW_C1.INDmean, OW_C2.INDmean, Width = 720, Height = 480, pol = toupper(pol))





ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 0 &
                              ResidentialProfiles$Type == Active.Type]

SubProfilesBiweeklyPeriod = ResidentialProfiles$Subtype[ResidentialProfiles$'T-gaps' == 1 &
                                                          ResidentialProfiles$Type == Active.Type]

for (Active.Subtype in SubProfilesBiweeklyPeriod[1:2])
{
  #Active.Subtype = SubProfilesFullPeriod[1]
  Active.Subprofile = ResidentialProfiles[ResidentialProfiles$Subtype == Active.Subtype,]
  
  
  
  c("TIME", "IND",
    "EXP_PST1_NO2", "EXP_PST1_PM25", "EXP_PST3_NO2","EXP_PST3_PM25", "EXP_PST5_NO2","EXP_PST5_PM25", "EXP_PST7_NO2","EXP_PST7_PM25",
    "EXP_P1_NO2", "EXP_P1_PM25", "EXP_P3_NO2","EXP_P3_PM25", "EXP_P5_NO2","EXP_P5_PM25", "EXP_P7_NO2","EXP_P7_PM25"
  )
  
  # biweekly
  if (Active.Subprofile$`T-gaps` == 1)
  {
    # Subset biweekly
    ST.DF.HR.BiWe = ST.DF.HR[ST.DF.HR$TIME >= BIWEEKLY[[1]][1] & ST.DF.HR$TIME <= BIWEEKLY[[1]][14] |
                               ST.DF.HR$TIME >= BIWEEKLY[[2]][1] & ST.DF.HR$TIME <= BIWEEKLY[[2]][14] |
                               ST.DF.HR$TIME >= BIWEEKLY[[3]][1] & ST.DF.HR$TIME <= BIWEEKLY[[3]][14],]
  }
  
  c("TIME", "IND",
    "EXP_PST2_NO2", "EXP_PST2_PM25", "EXP_PST4_NO2","EXP_PST4_PM25", "EXP_PST6_NO2","EXP_PST6_PM25", "EXP_PST8_NO2","EXP_PST8_PM25",
    "EXP_P2_NO2", "EXP_P2_PM25", "EXP_P4_NO2","EXP_P4_PM25", "EXP_P6_NO2","EXP_P6_PM25", "EXP_P8_NO2","EXP_P8_PM25"
  )
  
  OW = FALSE
  SaveAsDBF(ST.DF.HR_Type, "HR", "HR", Active.Type, OW, pol, 0)
  
  
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
  
  
  
  # Check if folder with fragments already exist
  TimeExp.lst = list.files(path = output.dir, pattern = paste0(Active.Subtype, "_", "[0-9]*"))
  # remove _ and backups in the lst
  NChar = NA
  for (c in seq_along(TimeExp.lst))
  {
    NChar[c] = nchar(TimeExp.lst[c])
  }
  TimeExp.lst = TimeExp.lst[NChar == nchar(Active.Subtype)+2 | NChar == nchar(Active.Subtype)+3]
  TimeExp.lst = mixedsort(TimeExp.lst) # fixes string order 1 10 11 -> 1 2 3
  
  if (Active.Subprofile$Dynamics == "dynamic")
  {
    Frag.lst = gsub(x = TimeExp.lst, pattern = paste0(Active.Subtype, "_"), replacement = "")
    Fragments = as.numeric(Frag.lst)
    
    if (length(Fragments) == tail(Fragments,1))
    {
      DaySplit = length(YearDates)/length(Fragments)
      SeqFragment = floor(seq(0, length(YearDates), DaySplit))
    } else
    {
      stop(paste("Fragments are not correct."))
    }
    
  } else # if static
  {
    if (is.na(TimeExp.lst))
    {
      Fragments = 1
      SeqFragment = 0
    }
  }
  
  
  ## reversed
  Fragments = 1:20
  DaySplit = length(YearDates)/length(Fragments)
  SeqFragment = floor(seq(0, length(YearDates), DaySplit))
  
  pol = pollutants[2]
  
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
    
    # Hours of the year
    HOURS.P_F = HourOfTheYear7(year.active, TIME.P_F, 0)
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      HOURS.S_F = HourOfTheYear7(year.active, TIME.S_F, 0)
      HOURS.T1_F = HourOfTheYear7(year.active, TIME.T1_F, 0)
      HOURS.T2_F = HourOfTheYear7(year.active, TIME.T2_F, 0)
    }
    
    ExposureValue.P_F = DBFreader("Exposure", "Primary", PPH.P, YearDates.Sub, FolderName, pol)
    ExposureValue.S_F = DBFreader("Exposure", "Secondary", PPH.P, YearDates.Sub, FolderName, pol)
    ExposureValue.T1_F = DBFreader("Exposure", "T1", PPH.P, YearDates.Sub, FolderName, pol)
    ExposureValue.T2_F = DBFreader("Exposure", "T2", PPH.P, YearDates.Sub, FolderName, pol)
    
    
    
    ST.DF.P_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_P_", toupper(pol), ".dbf")))
    ST.DF.S_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_S_", toupper(pol), ".dbf")))
    ST.DF.T1_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T1_", toupper(pol), ".dbf")))
    ST.DF.T2_F = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T2_", toupper(pol), ".dbf")))
    
    ExposureValue.P_F = list()
    ExposureValue.S_F = list()
    ExposureValue.T1_F = list()
    ExposureValue.T2_F = list()
    
    for (i in seq_along(PPH.P))
      #for (i in 250:1000)
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
    
    
    # Check if the lengths of TIME and EXP match
    if (length(unlist(TIME.P_F)) != length(unlist(ExposureValue.P_F)) |
        length(unlist(TIME.S_F)) != length(unlist(ExposureValue.S_F)) |
        length(unlist(TIME.T1_F)) != length(unlist(ExposureValue.T1_F)) |
        length(unlist(TIME.T2_F)) != length(unlist(ExposureValue.T2_F)))
    {
      stop(paste("Lengths of TIME and EXP do not match."))
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
  
  
  
  #! if TimeExp already exists.. detect fragments
  
  
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
      
      # ST.DF.P_F = DF.Structure2(PPH.P, TIME.P_F, TIME.P_F, ExposureValue.P_F) #, rm.na = TRUE
      # ST.DF.S_F = DF.Structure2(PPH.P, TIME.P_F, TIME.S_F, ExposureValue.S_F)
      # ST.DF.T1_F = DF.Structure2(PPH.P, TIME.P_F, TIME.T1_F, ExposureValue.T1_F)
      # ST.DF.T2_F = DF.Structure2(PPH.P, TIME.P_F, TIME.T2_F, ExposureValue.T2_F)
      # 
      # # Save DF structure
      # OW = TRUE
      # SaveAsDBF(ST.DF.P_F, "DF", "Primary", paste0(Active.Subtype, "_", f), OW, pol, 0)
      # SaveAsDBF(ST.DF.S_F, "DF", "Secondary", paste0(Active.Subtype, "_", f), OW, pol, 0)
      # SaveAsDBF(ST.DF.T1_F, "DF", "T1", paste0(Active.Subtype, "_", f), OW, pol, 0)
      # SaveAsDBF(ST.DF.T2_F, "DF", "T2", paste0(Active.Subtype, "_", f), OW, pol, 0)
      
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





# Add fit lines
abline(lm(WS1.INDmean~WS2.INDmean), col="red") # regression line (y~x)
lines(lowess(WS2.INDmean,WS1.INDmean), col="blue") # lowess line (x,y) 

plot(WS1.INDmean, WS2.INDmean)
str(summary(M.lm))

# Plotting results
Ind = 25
Plot.PersonalExposureGraph(Ind, 2, 5) # (Individual, Start(working)Day, Amount of days)

Plot.Group2(Active.Type, 1, 7, 25, TRUE)


## Raster Municipality
RasterMunicipality(pollutants[2], aq.dir, 10, "Antwerpen", 1+(24*5), 1+(24*5)) # BE_crs, output.dir, year.active, 

# # Calculate a raster from RIO-IFDM points with the Triangulation method
# res = 100
# AoI2.Raster = PointsToRasterTIN(SPDF = Points.Municipality, value = "value",
#                                 AoI = Municipality,
#                                 dmax = 20, mpp = res, dup = "error")
# plot(AoI2.Raster)
# lines(AoI2, col = "red")
# points(Points.AoI2.NoDup)
# SaveAsFile(AoI2.Raster, paste("Antwerpen", paste0(res,"x",res), "CON20150101_19_NO2", sep = "_"), "GeoTIFF", TRUE)
# 
# # Compare with Antwerpen NO2 hour19 raster
# raster_in = file.path("..", "output", "Raster_10x10_Antwerpen.tif")
# Raster.Antwerpen = raster(raster_in, layer = "Raster_10x10_Antwerpen.tif")
# projection(Raster.Antwerpen) = BE_crs
# plot(Raster.Antwerpen)
# 
# DiffRaster = Raster.Antwerpen - AoI2.Raster
# values(DiffRaster)
# plot(DiffRaster)
# SaveAsFile(DiffRaster, paste("DiffRaster", paste0(res,"x",res), sep = "_"), "GeoTIFF", TRUE)
# 
# Gemeente.Raster.Cut = gIntersects(Gemeente.Raster, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",])
# 
# SaveAsFile(Gemeente.Raster, paste("Raster", "Antwerpen", sep = "_"), "Shapefile", TRUE)


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