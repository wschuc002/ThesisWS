# Module for determining routes from two locations per individual
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

## Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("data.table","sp","rgdal","foreign","rgeos","osrm","SearchTrees")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(data.table)
library(sp)
library(rgdal)
library(foreign)
library(rgeos)
library(osrm)

library(SearchTrees)

DetermineAddressGoals_FL <- function(FL.Gemeente, Method.nr, ... )
{
  ## Read the input data
  zip_in = file.path("..", "data", "BE_FL", "CRAB_Adressenlijst.zip")
  shp_in = file.path("..", "data", "BE_FL", "CrabAdr.shp")
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(shp_in))
  {
    stop(paste("CRAB addresses not found (.shp)"))
  }
  if (!file.exists(shp_in))
  {
    unzip(zip_in, exdir= file.path("..", "data", "BE_FL"))
  }
  
  CRAB = readOGR(shp_in, layer = "CrabAdr") #! Make subset with Gemeente(n) in this stage?
  CRAB@proj4string = BE_crs
  CRAB_backup = CRAB
  
  # Drop the attributes that are not being used
  drops = c("STRAATNMID", "APPTNR", "BUSNR", "HNRLABEL", "NISCODE", "HERKOMST")
  CRAB_filtered = CRAB[ , !(names(CRAB) %in% drops)]
  
  
  ## Make a subset of CRAB addresses, depending on first argument empty or not
  if (is.null(FL.Gemeente))
  {
    CRAB_filtered_sub = CRAB_filtered
  } else
  {
    #CRAB_filtered_sub = subset(CRAB_filtered, GEMEENTE==FL.Gemeente[1])
    CRAB_filtered_sub = CRAB_filtered[CRAB_filtered$GEMEENTE %in% FL.Gemeente,]
  }
  
  
  # Use Company terrain datasets for CRAB CLASSIFICATION (partial)
  zip_in = file.path("..", "data", "BE_FL", "Bedrijventerreinen_Toestand_01_06_2016.zip")
  shp_in = file.path("..", "data", "BE_FL", "Gebrprc.shp")
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(shp_in))
  {
    stop(paste("CRAB addresses not found (.shp)"))
  }
  if (!file.exists(shp_in))
  {
    unzip(zip_in, exdir= file.path("..", "data", "BE_FL"))
  }
  
  Gebrprc = readOGR(shp_in, layer = "Gebrprc")
  Gebrprc@proj4string = BE_crs
  
  # Keep the attributes that will be used
  keeps = c("FUNCT")
  Gebrprc_filtered = Gebrprc[ , (names(Gebrprc) %in% keeps)]
  
  # Make the over(lay)
  o = over(CRAB_filtered_sub, Gebrprc_filtered)
  
  # Copy SpatialPointsDataFrame and add the attribute values from FUNCT
  CRAB_Doel = CRAB_filtered_sub
  CRAB_Doel@proj4string = BE_crs
  
  # Create the attribute "DOEL" in filtered CRAB with default NA
  CRAB_filtered_sub@data["DOEL"] = NA
  
  o$FUNCT.ch = o$FUNCT
  o$FUNCT.ch = as.character(o$FUNCT)
  
  #head(o,500)
  #tail(o,250)
  
  CRAB_Doel@data$DOEL = o$FUNCT.ch
  #class(CRAB_Doel@data$DOEL) = "character"
  
  #CRAB_Doel[CRAB_Doel@data$DOEL %in% "Agrarische functie",]
  
  ## Companies
  zip_in = file.path("..", "data", "BE_FL", "KboOpenData_0036_2017_01_Full.zip")
  csv.companies_in = file.path("..", "data", "BE_FL", "address.csv")
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(csv.companies_in))
  {
    stop(paste("CRAB addresses not found (.shp)"))
  }
  if (!file.exists(csv.companies_in))
  {
    unzip(zip_in, "address.csv", exdir= file.path("..", "data", "BE_FL")) #! Select only addresses.csv instead of unpacking all csv files
  }
  
  Companies = fread(csv.companies_in, sep="auto", header=TRUE, #integer64 = "numeric",
                    select = c("Zipcode", "MunicipalityFR", "StreetFR", "HouseNumber")) # 64-bit only.  This will take about x minutes.
  
  CRAB_Doel@data$DOEL[tolower(paste0(CRAB_Doel@data$POSTCODE, CRAB_Doel@data$STRAATNM, CRAB_Doel@data$HUISNR)) %in% 
                        tolower(paste0(Companies$Zipcode, Companies$StreetFR, Companies$HouseNumber))] = "Company"
  
  CRAB_Doel@data[CRAB_Doel@data$DOEL %in% "Company",] # VIEW
  
  
  csv.onderwijs.basis_in = file.path("..", "data", "BE_FL", "FL_onderwijs_basis.csv")
  Onderwijs.Basis = fread(csv.onderwijs.basis_in, sep="auto", header=TRUE, #integer64 = "numeric",
                          select = c("crab-code", "straat", "crab-huisnr", "crab-busnr", "gemeente", "Lx", "Ly", "postcode")) # 64-bit only.  This will take about x minutes.
  
  csv.onderwijs.secundair_in = file.path("..", "data", "BE_FL", "FL_onderwijs_secundair_gewoon.csv")
  Onderwijs.Secundair = fread(csv.onderwijs.secundair_in, sep="auto", header=TRUE, #integer64 = "numeric",
                              select = c("crab-code", "straat", "crab-huisnr", "crab-busnr", "gemeente", "Lx", "Ly", "postcode")) # 64-bit only.  This will take about x minutes.
  
  if (Method.nr == 1)
  {
    # METHOD 1: join on CRAB ID (+200000000)
    
    # create (empty) DOEL object
    #CRAB_Doel@data$DOEL = NA
    
    ## Primary schools
    
    CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]]),]
    CRAB_Doel@data$DOEL[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]])] = "Basisonderwijs"
    CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]]),]
    
    CRAB_Doel@data$STRAATJE = NA
    CRAB_Doel@data$STRAATJE[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]])] = Onderwijs.Basis[[2]]
    CRAB_Doel@data$NUMMERTJE = NA  
    CRAB_Doel@data$NUMMERTJE[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]])] = Onderwijs.Basis[[3]]
    
    # check on numbers
    length(Onderwijs.Basis[[1]]) == length(CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]]),])
    delta = (length(Onderwijs.Basis[[1]])-(length(CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Basis[[1]]),])))
    paste("Missing",delta, "features.")
    
    ## Secondary schools
    
    CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]]),]
    CRAB_Doel@data$DOEL[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]])] = "Secundair onderwijs"
    CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]]),]
    
    CRAB_Doel@data$STRAATJE = NA
    CRAB_Doel@data$STRAATJE[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]])] = Onderwijs.Secundair[[2]]
    CRAB_Doel@data$NUMMERTJE = NA  
    CRAB_Doel@data$NUMMERTJE[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]])] = Onderwijs.Secundair[[3]]
    
    #     # check on numbers
    #     length(Onderwijs.Secundair[[1]]) == length(CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]]),])
    #     delta = (length(Onderwijs.Secundair[[1]])-(length(CRAB_Doel[CRAB_Doel@data$ID %in% (2000000000+Onderwijs.Secundair[[1]]),])))
    #     paste("Missing",delta, "features.")
  }
  
  if (Method.nr == 2)
  {
    # METHOD 2: join on "municipality+street+number" combination
    
    ## Primary schools
    Onderwijs.Basis$NUMMERTJE = NA
    Onderwijs.Basis$NUMMERTJE = paste0(Onderwijs.Basis[["crab-huisnr"]],Onderwijs.Basis[["crab-busnr"]])
    head(Onderwijs.Basis$NUMMERTJE, 100)
    
    sub.Basis = tolower(paste0(CRAB_Doel@data$GEMEENTE, CRAB_Doel@data$STRAATNM, CRAB_Doel@data$HUISNR)) %in%
      tolower(paste0(Onderwijs.Basis$gemeente, Onderwijs.Basis$straat, Onderwijs.Basis$NUMMERTJE))
    summary(sub.Basis)
    
    CRAB_Doel@data$DOEL[sub.Basis] = "Basisonderwijs"
    CRAB_Doel@data[CRAB_Doel@data$DOEL %in% "Basisonderwijs",] # VIEW
    
    # check on numbers (total Flanders only)
    length(Onderwijs.Basis[[1]]) == length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    delta = length(Onderwijs.Basis[[1]]) - length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    paste("Missing",delta, "features.")
    
    # for subset
    nrow(Onderwijs.Basis[Onderwijs.Basis$gemeente == toupper(FL.Gemeente)])
    nrow(CRAB_Doel@data[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    
    
    ## Secondary schools
    
    Onderwijs.Secundair$NUMMERTJE = NA
    Onderwijs.Secundair$NUMMERTJE = paste0(Onderwijs.Secundair[["crab-huisnr"]],Onderwijs.Secundair[["crab-busnr"]])
    head(Onderwijs.Secundair$NUMMERTJE, 100)
    
    sub.Secundair = tolower(paste0(CRAB_Doel@data$GEMEENTE, CRAB_Doel@data$STRAATNM, CRAB_Doel@data$HUISNR)) %in%
      tolower(paste0(Onderwijs.Secundair$gemeente, Onderwijs.Secundair$straat, Onderwijs.Secundair$NUMMERTJE))
    summary(sub.Secundair)
    
    CRAB_Doel@data$DOEL[sub.Secundair] = "Secundair onderwijs"
    CRAB_Doel@data[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",] # VIEW
    
    
    CRAB_Doel@data[CRAB_Doel@data$DOEL %in% c("Basisonderwijs", "Secundair onderwijs"),] # VIEW Basis and Secundair
    
    #     # check on numbers
    #     length(Onderwijs.Secundair[[1]]) == length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",])
    #     delta = length(Onderwijs.Secundair[[1]]) - length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",])
    #     paste("Missing",delta, "features.")
    
  }
  
  if (Method.nr == 3)
  {
    # METHOD 3: Spatial join, based on closest point
    
    ## Primary schools
    
    # make NA coordinates -99,-99
    Onderwijs.Basis$Lx[is.na(Onderwijs.Basis$Lx)] = -99
    Onderwijs.Basis$Ly[is.na(Onderwijs.Basis$Ly)] = -99
    coordinates(Onderwijs.Basis) <- cbind(Onderwijs.Basis$Lx , Onderwijs.Basis$Ly)
    
    Onderwijs.Basis@proj4string = BE_crs
    
    
    # Find indices of the nearest (k) points in CRAB_Doel to each of the points in Onderwijs.Basis.
    tree = createTree(coordinates(CRAB_Doel))
    inds = knnLookup(tree, newdat=coordinates(Onderwijs.Basis), k = 1) # gives the matrix
    #inds = as.integer(inds)
    Basis.ID = inds
    
    #Onderwijs.Basis[Onderwijs.Basis@data$gemeente %in% "DILBEEK",]
    #CRAB_Doel[CRAB_Doel@data$GEMEENTE %in% "Dilbeek" & CRAB_Doel@data$DOEL %in% "Basisonderwijs",]
    
    CRAB_Doel[Basis.ID[,1],]
    CRAB_Doel@data$DOEL[Basis.ID[,1]] = "Basisonderwijs"
    
    #     CRAB_Doel@data$straatje = NA
    #     CRAB_Doel@data$straatje[Basis.ID] = Onderwijs.Basis@data$straat
    #     CRAB_Doel@data$nummertje = NA
    #     CRAB_Doel@data$nummertje[Basis.ID] = Onderwijs.Basis@data$'crab-huisnr'
    
    # check on numbers
    length(Onderwijs.Basis[[1]]) == length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    delta = length(Onderwijs.Basis[[1]]) - length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    paste("Missing",delta, "features.")
    
    ## Secondary schools
    
    # make NA coordinates -99,-99
    Onderwijs.Secundair$Lx[is.na(Onderwijs.Secundair$Lx)] = -99
    Onderwijs.Secundair$Ly[is.na(Onderwijs.Secundair$Ly)] = -99
    coordinates(Onderwijs.Secundair) <- cbind(Onderwijs.Secundair$Lx , Onderwijs.Secundair$Ly)
    
    Onderwijs.Secundair@proj4string = BE_crs
    
    
    # Find indices of the nearest (k) points in CRAB_Doel to each of the points in Onderwijs.Secundair.
    tree = createTree(coordinates(CRAB_Doel))
    inds = knnLookup(tree, newdat=coordinates(Onderwijs.Secundair), k = 1) # gives the matrix
    #inds = as.integer(inds)
    Secundair.ID = inds
    
    Onderwijs.Secundair[Onderwijs.Secundair@data$gemeente %in% "DILBEEK",]
    CRAB_Doel[CRAB_Doel@data$GEMEENTE %in% "Dilbeek" & CRAB_Doel@data$DOEL %in% "Secundair onderwijs",]
    
    CRAB_Doel[Secundair.ID[,1],]
    CRAB_Doel@data$DOEL[Secundair.ID[,1]] = "Secundair onderwijs"
    
    #     CRAB_Doel@data$straatje = NA
    #     CRAB_Doel@data$straatje[Secundair.ID] = Onderwijs.Secundair@data$straat
    #     CRAB_Doel@data$nummertje = NA
    #     CRAB_Doel@data$nummertje[Secundair.ID] = Onderwijs.Secundair@data$'crab-huisnr'
    
    # check on numbers
    length(Onderwijs.Secundair[[1]]) == length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",])
    delta = length(Onderwijs.Secundair[[1]]) - length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",])
    paste("Missing",delta, "features.")
  }
  
  #   tail(CRAB_Doel, 3000)
  #   head(CRAB_Doel, 2000)
  #   summary(CRAB_Doel)
  #   CRAB_Doel[5000:7000,]
  
  
  # Replace all NA with "Woonfunctie" (assuming all other addresses are Residence)
  CRAB_Doel@data$DOEL[is.na(CRAB_Doel@data$DOEL)] = "Woonfunctie"
  
  # Remove duplicates
  CRAB.unique = CRAB_Doel[!duplicated(CRAB_Doel@data[ , 2:6 ]), ]
  CRAB.unique@data[CRAB.unique@data$DOEL %in% "Basisonderwijs",] # VIEW
  
  CRAB.unique@proj4string = BE_crs
  
  return(CRAB.unique)
}



# Names.sub = Names
# FL.primary = 1000
# OSRM.level = "full"
# Prob.Method = "detailed" # "simplified"
# Plot = TRUE
# SaveResults = TRUE

DeterminePPH_FL <- function(CRAB_Doel, Names.sub, FL.primary, OSRM.Level, Active.Type,
                            Prob.Method, Plot, SaveResults, ...)
{
  # Create the attribute "object_id" (verplaatsen naar andere functie)
  CRAB_Doel@data["object_id"] = seq.int(nrow(CRAB_Doel@data))
  
  if (Active.Type == "04.FA")
  {
    Primary = subset(CRAB_Doel, CRAB_Doel@data$DOEL == "Agrarische functie")
  } else{
    ## Subset: only Primary
    Primary = subset(CRAB_Doel, CRAB_Doel@data$DOEL == "Woonfunctie")
  }
  PrimaryAmount = nrow(Primary)
  
  if (PrimaryAmount < FL.primary)
  {
    FL.primary = PrimaryAmount
    print(paste("Cannot take a sample larger than the population. Sample size is set to population size", PrimaryAmount))
  }
  
  ## Pick x random redidence object_id and make a subset
  RandObj_Re = sample(Primary@data$object_id, FL.primary)
  keeps_RS_Re = RandObj_Re
  Primary_KEEPS = Primary@data$object_id %in% keeps_RS_Re
  Primary_random = subset(Primary, Primary_KEEPS)
  
  if (Active.Profile$Dynamics == "static")
  {
    if (is.null(Subset.Gemeente))
    {
      SaveAsFile(Primary_random, paste(Active.Type, "Primary", sep = "_"), "GeoJSON", TRUE)
    } else
    {
      SaveAsFile(Primary_random, paste0(Active.Type, "_Primary_", Names.sub), "GeoJSON", TRUE)
    }
  }
  
  # For simple calculation method, based on assumption on normal distribution
#   Mean.distance = as.numeric(Active.Profile$MeanDistance)
#   SD.distance = as.numeric(Active.Profile$SDdistance)
  
  #OSRM profile
  if (Active.Type == "01.OW")
  {
    options(osrm.profile = "driving")
  }
  if (Active.Type == "03.SP")
  {
    options(osrm.profile = "biking")
  }
  
  
  if (Active.Profile$Dynamics == "dynamic")
  {
    #Subset: only Offices
    if (Active.Type == "01.OW")
    {
      Secondary = CRAB_Doel[CRAB_Doel@data$DOEL %in% "Economische functie" | CRAB_Doel@data$DOEL %in% "Company" ,]
      #Secondary@proj4string = BE_crs
      
      start.time = Sys.time()
      SecondaryPaired = CommutingDistancePairer(PPH.P, Secondary, MaxLinKM = 60,
                                                SEC.SampleSize = 100, Plot = FALSE)
      end.time = Sys.time()
      time.taken = end.time - start.time  
      time.taken
    }
    #Subset: only schools
    if (Active.Type == "03.SP")
    {
      #Secondary = subset(CRAB_Doel, CRAB_Doel@data$DOEL == "Basisonderwijs")
      Secondary = CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",] # CRAB_Doel@data$DOEL %in% "Secundair onderwijs" 
      
      SecondaryPaired = "place a "
      
    }
    
    #! Remove the individuals who did not pair
    if (length(PRI) != length(SecondaryPaired))
    {
      head(SecondaryPaired)
      head(PRI)
    }
    
    #! Remove the individuals who drive outside Flanders +250m buffer
    
    
    
    # Save files to hard drive
    if (is.null(Subset.Gemeente))
    {
      SaveAsFile(Primary_random, paste(Active.Type, "Primary", sep = "_"), "GeoJSON", TRUE)
      SaveAsFile(SecondaryPaired, paste(Active.Type, "Secondary", sep = "_"), "GeoJSON", TRUE)
    } else
    {
      SaveAsFile(Primary_random, paste0(Active.Type, "_Primary_", Names.sub), "GeoJSON", TRUE)
      SaveAsFile(SecondaryPaired, paste0(Active.Type, "_Secondary_", Names.sub), "GeoJSON", TRUE)
    }
  
    
    #gjson_in = file.path("..", "output", "01.OW_Primary.geojson")
    #SecondaryPaired = readOGR(gjson_in, layer = 'OGRGeoJSON')
    #SecondaryPaired@proj4string = BE_crs
    
    start.time = Sys.time()
    PPH.T = Router(PRI, SecondaryPaired, "full")
    end.time = Sys.time()
    time.taken = end.time - start.time  
    time.taken
    
    PPH.T1 = PPH.T[[1]] #Outwards
    PPH.T2 = PPH.T[[2]] #Inwards
    mean(PPH.T1@data$distance)
    
    
    
    if (Plot == TRUE)
    {
      lines(PPH.T1)
    }
    
    if (SaveResults == TRUE)
    {
      if (is.null(Subset.Gemeente))
      {
        SaveAsFile(PPH.T1, paste(Active.Type, paste0("TransportOutwards", Names.sub),substr(OSRM.Level, 1, 1), sep = "_"), "GeoJSON", TRUE)
        SaveAsFile(PPH.T2, paste(Active.Type, paste0("TransportInwards", Names.sub),substr(OSRM.Level, 1, 1), sep = "_"), "GeoJSON", TRUE)
      } else
      {
        SaveAsFile(PPH.T1, paste0(Active.Type,"_TransportOutwards_", Names.sub, "_", substr(OSRM.Level, 1, 1)), "GeoJSON", TRUE)
        SaveAsFile(PPH.T2, paste0(Active.Type,"_TransportInwards_", Names.sub, "_", substr(OSRM.Level, 1, 1)), "GeoJSON", TRUE)
      }
    }


    
    #Secondary_Selected = IsoChroneSampler(Primary_random[1,], Secondary, "driving duration", TRUE)


    
    

  }
}


#gjson_in = file.path("..", "output", "01.OW_Primary.geojson")
#PRI = readOGR(gjson_in, layer = 'OGRGeoJSON')
#PRI@proj4string = BE_crs

OSRM.Level = "full"

Router <- function(PRI, SecondaryPaired, OSRM.Level, ...)
{
  # Transform RD new -> WGS84 for the route calculation (OSRM)
  WGS84 = "+init=epsg:4326"
  src1 = spTransform(PRI, WGS84)
  dst2 = src1
  
  dst1 = spTransform(SecondaryPaired, WGS84)
  dst1@proj4string = CRS(WGS84)
  src2 = dst1
  
  PPH.T1.Li = list()
  PPH.T2.Li = list()
  for (i in seq_along(SecondaryPaired))
  #for (i in 1:10)
  {
    PPH.T1.Li[[i]] = osrmRoute.WS(src = src1[i,], dst = dst1[i,], overview = OSRM.Level, sp = TRUE)
    PPH.T1.Li[[i]] = spTransform(PPH.T1.Li[[i]], BE_crs)
    
    PPH.T2.Li[[i]] = osrmRoute.WS(src = src2[i,], dst = dst2[i,], overview = OSRM.Level, sp = TRUE)
    PPH.T2.Li[[i]] = spTransform(PPH.T2.Li[[i]], BE_crs)
    
    PPH.T1.Li[[i]]@data$src_CRAB = NA
    PPH.T1.Li[[i]]@data$dst_CRAB = NA
    PPH.T1.Li[[i]]@data$src_CRAB = src1[i,]@data$object_id
    PPH.T1.Li[[i]]@data$dst_CRAB = dst1[i,]@data$object_id
    
    PPH.T2.Li[[i]]@data$src_CRAB = PPH.T1.Li[[i]]@data$dst_CRAB
    PPH.T2.Li[[i]]@data$dst_CRAB = PPH.T1.Li[[i]]@data$src_CRAB
  }
  PPH.T1 = do.call(rbind, PPH.T1.Li)
  PPH.T2 = do.call(rbind, PPH.T2.Li)
  
  return(list(PPH.T1, PPH.T2))
}

# PRI = PPH.P
# SEC = Secondary 
# MaxLinKM = 100
# Plot = TRUE
# SEC.SampleSize = 5
CommutingDistancePairer <- function(PRI, SEC, MaxLinKM, SEC.SampleSize, Plot, ...)
{
  OSRM.level = "simplified" 
  # remove duplicates
  SEC.NoDup = SEC[!duplicated(SEC@coords), ]
  
  # Transform RD new -> WGS84 for the route calculation (OSRM)
  WGS84 = "+init=epsg:4326"
  src <- spTransform(PRI, WGS84)

  SEC.Pared.Li = list()

  if (Plot == TRUE){plot(Flanders)}
  
  for (p in seq_along(PRI))
  #for (p in 1:10)  # p = 1
  {
    success = FALSE
    while(!success)
    {
      print(paste("Starting with ", p, "of", length(PRI)))
      
      #SEC.SampleSize = 100
      SEC.ids = sample(SEC.NoDup@data$object_id, size = SEC.SampleSize)
      SEC.rs = SEC.NoDup[SEC.NoDup@data$object_id %in% SEC.ids,]
      
      SEC.rsSP = SpatialPoints(SEC.rs@coords, proj4string = BE_crs)
      
      dst = spTransform(SEC.rsSP, WGS84)
      
      DrivingDistance = NA
      DistanceLinear = NA
      Probabilities = NA
      #StraatKoppel = list()
      
      for (s in seq_along(SEC.rs))
        #for (s in 1:20) # s = 1
      {
        DistanceLinear[s] = gDistance(PRI[p,],SEC.rs[s,])
        if (DistanceLinear[s] < MaxLinKM*1000)
        {
          # build the query
          req <- paste(getOption("osrm.server"),
                       "route/v1/", getOption("osrm.profile"), "/", 
                       src@coords[p,1], ",", src@coords[p,2],";",
                       dst@coords[s,1],",",dst@coords[s,2], 
                       "?alternatives=false&geometries=polyline&steps=false&overview=",
                       tolower(OSRM.level),
                       sep="")
          
          # Sending the query
          resRaw <- RCurl::getURL(utils::URLencode(req), useragent = "'osrm' R package")
          # Deal with \\u stuff
          vres <- jsonlite::validate(resRaw)[1]
          if(!vres){
            resRaw <- gsub(pattern = "[\\]", replacement = "zorglub", x = resRaw)
          }
          # Parse the results
          res <- try(jsonlite::fromJSON(resRaw))
          
          # Error handling
          #e <- simpleError(res$message)
          if(vres)
          {
            if (res$code == "Ok")
            {
              DrivingDistance[s] = res$routes$legs[[1]]$distance/1000
              Probabilities[s] = Commuting$percentage[DrivingDistance[s] > Commuting$km_min &
                                                        DrivingDistance[s] < Commuting$km_max]
              #StraatKoppel[[s]] = res$waypoints$name
            } else
            {
              DrivingDistance[s] = NA
              Probabilities[s] = 0
            }
          } else
          {
            DrivingDistance[s] = NA
            Probabilities[s] = 0
          }
        } else
        {
          DrivingDistance[s] = NA
          Probabilities[s] = 0
        }
      }
      
      # Repeat 'p' when there are only Probabilities of 0 or NA returned. | test with low SEC.SampleSize
      if(length(which(TRUE %in% 0 == Probabilities | is.na(Probabilities))) == length(Probabilities)) # When all values are 0
      {
        print(paste("Resetting p."))
        success = FALSE
      } else
      {
        print(paste("Pair", p, "finished."))
        success = TRUE
      }
    }
    
    Probabilities[is.na(Probabilities)] = 0
    
    SEC.sel = sample(DrivingDistance, size = 1, prob = Probabilities)
    SEC.Pared = SEC.rsSP[DrivingDistance %in% SEC.sel,]
    SEC.id = SEC.ids[DrivingDistance %in% SEC.sel]
    
    if (Plot == TRUE)
    {
      points(PRI[p,], col = "green")
      points(SEC.Pared[1,], col = "orange")
      
      #overlap = which(coordinates(SEC.NoDup) %in% coordinates(SEC.Pared[1,]))
      overlap = which(as.logical(gIntersects(SEC.NoDup, SEC.Pared[1,], byid = TRUE) == TRUE))
      points(SEC.NoDup[overlap,], col = "red", pch = "+")
      
      
#       inter = gIntersection(SEC.NoDup, SEC.Pared[1,], byid = TRUE)
#       length(inter)
    }
    SEC.Pared.Li[[p]] = SEC.Pared[1,]
  }
  
  #   #use the coordinated to return the SPDF
  SEC.ParedWS = do.call(rbind, SEC.Pared.Li)
  
  if (length(SEC.ParedWS) == length(PRI))
  {
    overlap = which(!is.na(over(SEC.NoDup, SEC.ParedWS)) == TRUE)
    SEC.ParedSPDF = SEC.NoDup[overlap,]
    if (Plot == TRUE)
    {
      points(SEC.ParedSPDF, col = "red", pch = "O")
    }
    return(SEC.ParedSPDF)
  } else
  {
    warning(paste("Output secondaries did not match input primaries. Incomplete features returned instead. No data frame included."))
    return(SEC.ParedWS)
  }
}


Tester <- function(...)
{
  Num1 = 1:20
  Num2 = 21:40
  Num3 = 1:15
  
  if(length(Num1) == length(Num3))
  {
    CHECK = "goed"
    return(CHECK)
  } else 
  {
    CHECK = "slecht"
    warning(paste("Output secondaries did not match input primaries. Incomplete list returned instead."))
    return(CHECK)
  }
}
UITKOMST = Tester()
rm(UITKOMST)

IsoChroneSampler <- function(Primary_random, Secondary, Method, Plot, ...)
{
  if (Method == "driving distance") # needs right values
  {
    ComTime.breaks = c(1,10,20,30,40,50,100,200,250)
    ComTime.percentage = c(5.49,8.52,7.1,5.06,3.39,8.72,3.82,0.81,0)
  }
  if (Method == "driving duration")
  {
    ComTime.breaks = c(1,10,20,30,40,50,100,200,250)
    ComTime.percentage = c(5.49,8.52,7.1,5.06,3.39,8.72,3.82,0.81,0)
  }
  
  isolist = list()
  SecondarySample.Li = list()
  for (i in seq_along(Primary_random))
  #for (i in (1:5)) # i = 1
  {
    Sample.Prob = sample(ComTime.percentage, prob = ComTime.percentage, size = 1)
    
    isolist[[i]] = osrmIsochrone.WS(Primary_random[i,],
                    breaks = c(ComTime.breaks[which(ComTime.percentage == Sample.Prob)],
                               ComTime.breaks[which(ComTime.percentage == Sample.Prob)+1]),
                    res = 50)
    
    #isolist[[i]] = osrmIsochrone.WS(Primary[i,], breaks = ComTime.breaks, res = 100)
    #isolist[[1]]@data$prob = rev(ComTime.percentage[2:length(ComTime.percentage)])
    
    #isolist[[i]] = rgeos::gUnaryUnion(isolist[[i]], id = NULL)
    #plot(isolist[[i]], col = "red")
    
    #plot(isolist[[i]][1,], col = "blue")
    #points(Primary_random[i,], col = "green")
    TF = as.logical(gIntersects(Secondary, isolist[[i]][1,], byid = TRUE))
    
    Secondary2 = Secondary[TF,]
    SecondarySample.Li[[i]] = Secondary2[sample(1:length(Secondary2), size = 1),]
    #points(SecondaySample[[i]], col = "red")
    
  }
  SecondarySample = do.call(rbind, SecondarySample.Li)
  
  if (Plot == TRUE)
  {
      plot(Flanders)
      points(Primary_random[1:5,], col = "green")
      points(SecondarySample, col = "orange")
    #   
    #   plot(SecondarySample, col = "orange")
    #   
    #   plot(isolist[[1]])
    #   points(Primary_random_kopp[1,], col = "green")
    #   points(Secondary_random_NR, col = "orange")
    #   spplot(isolist[[1]], "prob")
  }
  
  return(SecondarySample)
}


RouteCorrection <- function(...)
{
  # Check difference in route distance and linear distance to perform a category specific distance correction
  CommutingRoutes1_SLDF@data$distance.linear = NA
  CommutingRoutes1_SLDF@data$distance_diff = NA
  CommutingRoutes1_SLDF@data$distance_diff.ra = NA
  
  for (i in seq_along(CommutingRoutes1_SLDF))
  {
    CommutingRoutes1_SLDF@data$distance.linear[i] = gDistance(Primary_random_kopp[i,],
                                                              Secondary_random_NR[Secondary_random_NR@data$NR == Primary_random_kopp@data$koppeling[i],]) / 1000
    
    CommutingRoutes1_SLDF@data$distance_diff[i] = CommutingRoutes1_SLDF@data$distance[i] - CommutingRoutes1_SLDF@data$distance.linear[i]
    CommutingRoutes1_SLDF@data$distance_diff.ra[i] = CommutingRoutes1_SLDF@data$distance.linear[i] / CommutingRoutes1_SLDF@data$distance[i]
  }
  mean(CommutingRoutes1_SLDF@data$distance_diff) # 15.5805 #14.7509
  mean(CommutingRoutes1_SLDF@data$distance_diff.ra) # 15.5805 #14.7509
  
  # classify in same groups as Commuting (csv) and give group specific correction
  Commuting$km_min.corr = NA
  Commuting$km_max.corr = NA
  Commuting$km_min.corr = Commuting$km_min * mean(CommutingRoutes1_SLDF@data$distance_diff.ra)
  Commuting$km_max.corr = Commuting$km_max * mean(CommutingRoutes1_SLDF@data$distance_diff.ra)
  
  plot((Commuting$km_min+Commuting$km_max)/ 2, Commuting$percentage, col = "red")
  points((Commuting$km_min.corr+Commuting$km_max.corr)/ 2, Commuting$percentage, col = "green")
  
}


CalcIsochrones <- function(...)
{
  #Isochrones
  
  data("com")
  
  # Map
  if(require("cartography")){
    osm <- getTiles(spdf = iso, crop = TRUE, type = "osmgrayscale")
    tilesLayer(osm)
    breaks <- sort(c(unique(iso$min), max(iso$max)))
    cartography::choroLayer(spdf = iso, df = iso@data,
                            var = "center", breaks = breaks,
                            border = NA,
                            legend.pos = "topleft",legend.frame = TRUE, 
                            legend.title.txt = "Isochrones\n(min)", 
                            add = TRUE)
  }
  
  # Get isochones with a SpatialPointsDataFrame, custom breaks
  iso2 <- osrmIsochrone(loc = src[7,], breaks = seq(from = 0,to = 30, by = 5))
  
  plot(iso2)
  spplot(iso2, "center")
  
  isoWS = osrmIsochrone(Primary_random_kopp[1:2,], breaks = Commuting$km_min)
  plot(isoWS)
  points(Secondary_random_NR)
  
  
  ComTime.breaks = c(1,10,20,30,40,50,100,200,250)
  ComTime.percentage = c(5.49,8.52,7.1,5.06,3.39,8.72,3.82,0.81,0)
  
  isolist = list()
  #for (i in seq_along(Primary_random_kopp))
  for (i in (1))
  {
    isolist[[i]] = osrmIsochrone.WS(Primary_random_kopp[i,], breaks = ComTime.breaks, res = 100)
    isolist[[1]]@data$prob = rev(ComTime.percentage[2:length(ComTime.percentage)])
  }
  plot(isolist[[1]])
  points(Primary_random_kopp[1,], col = "green")
  points(Secondary_random_NR, col = "orange")
  spplot(isolist[[1]], "prob")
  
}


#     ## Pick x random Secondary object_id and make a subset
#     if (nrow(Primary_random) < 500)
#     {
#       Secondary.SampleSize = round(nrow(Secondary) / 1000, digits = 0)
#     } else
#       {
#         Secondary.SampleSize = nrow(Secondary)
#       }
#       
#     RandObj_Se = sample(Secondary@data$object_id, Secondary.SampleSize)
#     keeps_RS_Se = RandObj_Se
#     Secondary_KEEPS = Secondary@data$object_id %in% keeps_RS_Se
#     Secondary_random = subset(Secondary, Secondary_KEEPS)
#     
#     ## Remove duplicates in object_id
#     Secondary_random = Secondary_random[!duplicated(Secondary_random@data$object_id),]
#     
# 
#     ## Create distances matrix
#     if (Prob.Method == "simplified")
#     {
#       DIST_GEO = matrix(data=NA, nrow=length(Primary_random), ncol=length(Secondary_random)) # empty/NA matrix
#       for (z in seq(1, length(Primary_random), by=1))
#       {
#         for (w in seq(1, length(Secondary_random), by=1))
#         {
#           DIST_GEO[z,w] = gDistance(Primary_random[z,],Secondary_random[w,]) #[Primary,Secondary]
#         }
#       }
#     } else
#     {
#       #DIST_GEO = Matrix.distance
#       MATRICES = CommutingDistanceMatrixer(Primary_random, Secondary_random, 75)
#       DIST_GEO = MATRICES[[1]]
#     }
#     
# 
#     
#     ## Select pair, based on prob distribution of commuting distance (CD)
#     ResWor = 0
#     
#     if (Prob.Method == "simplified") # based on simplified/aggregated statistics (more assumptions)
#     {
#       # prob distribution of Commuting Distances (CD)
#       #prob_CD = abs(rnorm(FL.primary, mean = Mean.distance, sd = SD.distance))
#       #prob_CD = prob_CD[prob_CD < 2000000] # remove the large distances
#       #hist(prob_CD, probability=TRUE)
#       #CD = seq(min(prob_CD), max(prob_CD), length=28)
#       #lines(CD, dnorm(CD, mean=Mean.distance, sd=SD.distance))
#       
#       prob_CD = rnorm(FL.primary, mean = Mean.distance, sd = SD.distance)
#       
#       # Extract the pair numbers Primary~Secondary (ex: 1~24, 2~35) (test group can be collegues)
#       for (z in seq_along(Primary_random))
#       {
#         RV = sample(prob_CD, size=1)
#         ResWor[z] = which.min(abs(RV-DIST_GEO[z,]))
#       }
#     }
#     
#     if (Prob.Method == "detailed") # based on detailed statistics (less assumptions)
#     {
#       for (z in seq_along(Primary_random))
#       #for (z in 1:20)
#       {
#         Dist = DIST_GEO[z,]
#         Prob = Dist # use same structure
#         for (l in (1:nrow(Commuting)))
#         {
#           Prob[Dist > Commuting$km_min[l]*1000 & Dist < Commuting$km_max[l]*1000] = Commuting$percentage[l]/100
#         }
# 
#         Selected = matrix(sample(na.omit(as.numeric(Dist)), prob = na.omit(as.numeric(Prob)), size = 1))
#         ResWor[z] = which(Dist %in% Selected)
#       }
#     }
#     


#     # Giving the pairs to the datasets
#     Primary_random_kopp = Primary_random
#     Primary_random_kopp@data$koppeling = ResWor
#     
#     # Give the WSResID
#     Primary_random_kopp@data$WSResID = 1:length(Primary_random_kopp)
#     
#     # Delete the Workplaces in SPDF that are not paires (these residuals are not used)
#     Secondary_random@data$NR = seq(length(Secondary_random))
#     
#     keeps_WP = Primary_random_kopp@data$koppeling
#     Secondary_KEEPS2 = Secondary_random@data$NR %in% keeps_WP
#     Secondary_random_NR = subset(Secondary_random, Secondary_KEEPS2)
#     
#     Primary_random_kopp@proj4string = BE_crs
#     Secondary_random_NR@proj4string = BE_crs
#     
#     if (is.null(Subset.Gemeente))
#     {
#       SaveAsFile(Primary_random, paste(Active.Type, "Primary", sep = "_"), "GeoJSON", TRUE)
#       SaveAsFile(SecondaryPaired, paste(Active.Type, "Secondary", sep = "_"), "GeoJSON", TRUE)
#     } else
#       {
#         SaveAsFile(Primary_random, paste0(Active.Type, "_Primary_", Names.sub), "GeoJSON", TRUE)
#         SaveAsFile(SecondaryPaired, paste0(Active.Type, "_Secondary_", Names.sub), "GeoJSON", TRUE)
#       }
#     
#     # Transform RD new -> WGS84 for the route calculation (OSRM)
#     WGS84 = "+init=epsg:4326"
#     Primary_random_kopp_WGS84 = Primary_random_kopp
#     Primary_random_kopp_WGS84_T <- spTransform(Primary_random_kopp, WGS84)
#     
#     Secondary_random_NR_WGS84 = Secondary_random_NR
#     Secondary_random_NR_WGS84_T <- spTransform(Secondary_random_NR, WGS84)
#     
#     # Copy @data and add the coordinates to the new dataframe (...2)
#     Primary_random_kopp_WGS84_T2 = Primary_random_kopp_WGS84_T@data
#     Primary_random_kopp_WGS84_T2$lon1 = Primary_random_kopp_WGS84_T@coords[,1]
#     Primary_random_kopp_WGS84_T2$lat1 = Primary_random_kopp_WGS84_T@coords[,2]
#     
#     Secondary_random_NR_WGS84_T2 = Secondary_random_NR_WGS84_T@data
#     Secondary_random_NR_WGS84_T2$lon2 = Secondary_random_NR_WGS84_T@coords[,1]
#     Secondary_random_NR_WGS84_T2$lat2 = Secondary_random_NR_WGS84_T@coords[,2]
#     
#     ## Merge Primary with Workplace and order on Primary (WSResID)
#     RESWOR = merge(Primary_random_kopp_WGS84_T2,Secondary_random_NR_WGS84_T2, by.x= "koppeling", by.y="NR")
#     RESWOR = setorder(RESWOR, cols=WSResID)
#     
#     ## Generate Commuting Routes (CR)
#     # Outwards (Primary -> Secondary)
#     CommutingRoutes1 = list()
#     CommutingRoutes2 = list()
#     for (i in seq_along(Primary_random_kopp_WGS84_T))
#     {
#       RES = c(paste("RES",i,sep="_"), RESWOR["lon1"][i,], RESWOR["lat1"][i,])
#       WOR = c(paste("WOR",RESWOR["koppeling"][i,],sep="_"), RESWOR["lon2"][i,], RESWOR["lat2"][i,])
#       
#       CommutingRoutes1[i] = osrmRoute(src= RES, dst = WOR, overview = OSRM.level, # "full"/"simplified"
#                                       sp = TRUE)
#       CommutingRoutes1[[i]] = spTransform(CommutingRoutes1[[i]], BE_crs)
#       CommutingRoutes1[[i]]@lines[[1]]@ID = paste(CommutingRoutes1[[i]]@lines[[1]]@ID,i)
#       row.names(CommutingRoutes1[[i]]) = as.character(i)
#       
#       CommutingRoutes2[i] = osrmRoute(src= WOR, dst = RES, overview = OSRM.level, # "full"/"simplified"
#                                       sp = TRUE)
#       CommutingRoutes2[[i]] = spTransform(CommutingRoutes2[[i]], BE_crs)
#       CommutingRoutes2[[i]]@lines[[1]]@ID = paste(CommutingRoutes2[[i]]@lines[[1]]@ID,i)
#       row.names(CommutingRoutes2[[i]]) = as.character(i)
#     }
#     
#     # Convert large list to a SpatialLinesDataFrame with all the Commuting Routes
#     CommutingRoutes1_SLDF = CommutingRoutes1[[1]]
#     CommutingRoutes2_SLDF = CommutingRoutes2[[1]]
#     for (i in seq(2, length(Primary_random_kopp_WGS84_T), by=1))
#     {
#       CommutingRoutes1_SLDF = rbind(CommutingRoutes1_SLDF, CommutingRoutes1[[i]])
#       CommutingRoutes2_SLDF = rbind(CommutingRoutes2_SLDF, CommutingRoutes2[[i]])
#     }
#     
#     CommutingRoutes1_SLDF@data["PersonID"] = seq.int(CommutingRoutes1_SLDF)
#     CommutingRoutes2_SLDF@data["PersonID"] = seq.int(CommutingRoutes2_SLDF)
#     
#     if (is.null(Subset.Gemeente))
#     {
#       SaveAsFile(CommutingRoutes1_SLDF, paste(Active.Type, paste0("TransportOutwards", Names.sub),substr(OSRM.level, 1, 1), sep = "_"), "GeoJSON", TRUE)
#       SaveAsFile(CommutingRoutes2_SLDF, paste(Active.Type, paste0("TransportInwards", Names.sub),substr(OSRM.level, 1, 1), sep = "_"), "GeoJSON", TRUE)
#     } else
#     {
#       SaveAsFile(CommutingRoutes1_SLDF, paste0(Active.Type,"_TransportOutwards_", Names.sub, "_", substr(OSRM.level, 1, 1)), "GeoJSON", TRUE)
#       SaveAsFile(CommutingRoutes2_SLDF, paste0(Active.Type,"_TransportInwards_", Names.sub, "_", substr(OSRM.level, 1, 1)), "GeoJSON", TRUE)
#       
#     }




# DetermineRoutesNL <- function(NL.Provincie, NL.offices, NL.workplaces, ... )
#   {
# 
#     ## Read the input data
#     zip_in = file.path("..", "data", "NL", "bag-adressen-laatst.csv.zip")
#     csv_in = file.path("..", "data", "NL", "bagadres.csv")
#     
#     # Check if input data is available
#     if (!file.exists(zip_in) & !file.exists(csv_in))
#     {
#       stop(paste("BAG addresses not found (.csv)"))
#     }
#     if (!file.exists(csv_in))
#     {
#       unzip(zip_in, exdir= file.path("..", "data", "NL"))
#     }
#     
#     dbf_in = file.path("..", "data", "NL", "verblijfsobjectgebruiksdoel.dbf")
#     
#     BAG_Addresses = fread(csv_in, sep=";", header=TRUE, integer64 = "numeric",
#                           drop = c("huisletter", "object_type", "lon", "lat", "nevenadres")) # 64-bit only.  This will take about 2 minutes.
#     #file.remove(csv_in)
#     
#     ## Make a subset of BAG addresses, depending on first argument empty or not
#     if (is.null(NL.Provincie))
#     {
#       BAG_Addresses_sub = BAG_Addresses
#     } else
#       {
#         BAG_Addresses_sub = subset(BAG_Addresses, provincie==NL.Provincie)
#     }
#     
#     # Drop the attributes that are not being used
#     drops = c("gemeente","provincie","huisnummertoevoeging") #,"lon","lat","nevenadres","object_type"
#     SUB1 = BAG_Addresses_sub[ , !(names(BAG_Addresses_sub) %in% drops)]
#     BAG_Addresses_subbed = subset(BAG_Addresses_sub, select = SUB1)
#     
#     ## Remove intermediate results from environment
#     rm(BAG_Addresses,BAG_Addresses_sub)
#     
#     ## Read the database file with "Gebruiksdoelen" Goal of Use
#     BAG_Doel = read.dbf(dbf_in) #as.is=FALSE       # this will take about 10 minutes.
#     
#     ## Keep useful attributes
#     keeps_dbf = c("IDENTIFICA","GEBRUIKSDO")
#     BAG_Doel_sub = subset(BAG_Doel, select=keeps_dbf)
#     
#     ## Rename column name Object ID from "Gebruiksdoelen", to match with "Addresses"
#     colnames(BAG_Doel_sub)[1] <- "object_id"
#     
#     ## Merge "Gebruiksdoelen" and "Addresses"
#     BAG_AddrDoel = merge(BAG_Doel_sub, BAG_Addresses_subbed, by="object_id", duplicateGeoms=TRUE)
#     
#     ## Copy Doelbestanden and delete duplicates. This will create unique obs.
#     BAG_AddrDoel_uni = BAG_AddrDoel
#     BAG_AddrDoel_uni = BAG_AddrDoel[!duplicated(BAG_AddrDoel),]
#     
#     ## Make spatial
#     BAG_AddrDoel_SPDF = BAG_AddrDoel_uni
#     coordinates(BAG_AddrDoel_SPDF)<-~x+y
#     
#     ## Remove intermediate results from environment
#     rm(BAG_Addresses_subbed,BAG_Doel,BAG_Doel_sub,BAG_AddrDoel,BAG_AddrDoel_uni)
#     
#     ## Set Coordinate Reference System (CRS)
#     #RD_new = "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
#     RD_new = "+init=epsg:28992"
#     BAG_AddrDoel_SPDF@proj4string = CRS(RD_new)
#     
#     ## Subset: only Residence
#     Residence = subset(BAG_AddrDoel_SPDF, BAG_AddrDoel_SPDF@data$GEBRUIKSDO == "woonfunctie")
#     
#     ## Pick x random redidence object_id and make a subset
#     RandObj_Re = sample(Residence@data$object_id, NL.offices)
#     keeps_RS_Re = RandObj_Re
#     Residence_KEEPS = Residence@data$object_id %in% keeps_RS_Re
#     Residence_random = subset(Residence, Residence_KEEPS)
#     
#     ## Subset: only Workplaces (offices)
#     Workplace = subset(BAG_AddrDoel_SPDF, BAG_AddrDoel_SPDF@data$GEBRUIKSDO == "kantoorfunctie")
#     
#     ## Pick x random offices object_id and make a subset
#     RandObj_Of = sample(Workplace@data$object_id, NL.workplaces)
#     keeps_RS_Of = RandObj_Of
#     Workplace_KEEPS = Workplace@data$object_id %in% keeps_RS_Of
#     Workplace_random = subset(Workplace, Workplace_KEEPS)
#     
#     ## Remove duplicates in object_id
#     Workplace_random = Workplace_random[!duplicated(Workplace_random@data$object_id),]
#     
#     ## Create distance matrix
#     DIST_GEO = matrix(data=NA, nrow=length(Residence_random), ncol=length(Workplace_random)) # empty/NA matrix
#     for (z in seq(1, length(Residence_random), by=1))
#     {
#       for (w in seq(1, length(Workplace_random), by=1))
#       {
#         DIST_GEO[z,w] = gDistance(Residence_random[z,],Workplace_random[w,]) #[Residence,Workplace]
#       }
#     }
#     
#     ## Select pair, based on prob distribution of commuting distance (CD)
#     
#     # prob distribution of CD
#     prob_CD = rnorm(1000, mean=28000, sd=10000)
#     prob_CD = prob_CD[prob_CD > 200]
#     #hist(prob_CD, probability=TRUE)
#     #CD = seq(min(prob_CD), max(prob_CD), length=28)
#     #lines(CD, dnorm(CD, mean=28000, sd=10000))
#     
#     # Extract the pair numbers Residence~Workplace (ex: 1~24, 2~35) (test group can be collegues)
#     ResWor=0
#     for (z in seq(1, length(Residence_random), by=1))
#     {
#       RV = sample(prob_CD, size=1)
#       ResWor[z] = which.min(abs(RV-DIST_GEO[z,]))
#     }
#     
#     # Giving the pairs to the datasets
#     Residence_random_kopp = Residence_random
#     Residence_random_kopp@data$koppeling = ResWor
#     
#     # Give the WSResID
#     Residence_random_kopp@data$WSResID = 1:length(Residence_random_kopp)
#     
#     # Delete the Workplaces in SPDF that are not paires (these residuals are not used)
#     Workplace_random@data$NR = seq(length(Workplace_random))
#     
#     keeps_WP = Residence_random_kopp@data$koppeling
#     Workplace_KEEPS2 = Workplace_random@data$NR %in% keeps_WP
#     Workplace_random_NR = subset(Workplace_random, Workplace_KEEPS2)
#     
#     #Write to geojson
#     Gjson_out_name = "Residence"
#     Gjson_out_ext = ".geojson"
#     Gjson_out = file.path("..", "output", Gjson_out_name)
#     writeOGR(Residence_random_kopp, Gjson_out , Gjson_out_name, driver='GeoJSON')
#     file.rename(Gjson_out, file.path("..", "output", paste(Gjson_out_name,Gjson_out_ext,sep="")))
#     
#     #Write to geojson
#     Gjson_out_name = "Workplace"
#     Gjson_out_ext = ".geojson"
#     Gjson_out = file.path("..", "output", Gjson_out_name)
#     writeOGR(Workplace_random_NR, Gjson_out , Gjson_out_name, driver='GeoJSON')
#     file.rename(Gjson_out, file.path("..", "output", paste(Gjson_out_name,Gjson_out_ext,sep="")))
#     
#     
#     # Transform RD new -> WGS84 for the route calculation (OSRM)
#     WGS84 = "+init=epsg:4326"
#     Residence_random_kopp_WGS84 = Residence_random_kopp
#     Residence_random_kopp_WGS84_T <- spTransform(Residence_random_kopp, WGS84)
#     
#     Workplace_random_NR_WGS84 = Workplace_random_NR
#     Workplace_random_NR_WGS84_T <- spTransform(Workplace_random_NR, WGS84)
#     
#     # Copy @data and add the coordinates to the new dataframe (...2)
#     Residence_random_kopp_WGS84_T2 = Residence_random_kopp_WGS84_T@data
#     Residence_random_kopp_WGS84_T2$lon1 = Residence_random_kopp_WGS84_T@coords[,1]
#     Residence_random_kopp_WGS84_T2$lat1 = Residence_random_kopp_WGS84_T@coords[,2]
#     
#     Workplace_random_NR_WGS84_T2 = Workplace_random_NR_WGS84_T@data
#     Workplace_random_NR_WGS84_T2$lon2 = Workplace_random_NR_WGS84_T@coords[,1]
#     Workplace_random_NR_WGS84_T2$lat2 = Workplace_random_NR_WGS84_T@coords[,2]
#     
#     ## Merge Residence with Workplace and order on Residence (WSResID)
#     RESWOR = merge(Residence_random_kopp_WGS84_T2,Workplace_random_NR_WGS84_T2, by.x= "koppeling", by.y="NR")
#     RESWOR = setorder(RESWOR, cols=WSResID)
#     
#     ## Generate Commuting Routes (CR)
#     CommutingRoutes = list()
#     for (i in seq(1, length(Residence_random_kopp_WGS84_T), by=1))
#     {
#       CommutingRoutes[i] = osrmRoute(src=c(paste("RES",i,sep="_"), RESWOR["lon1"][i,], RESWOR["lat1"][i,]),
#                                      dst = c(paste("WOR",RESWOR["koppeling"][i,],sep="_"), RESWOR["lon2"][i,], RESWOR["lat2"][i,]),
#                                      overview = "full", #"simplified"
#                                      sp = TRUE)
#       CommutingRoutes[[i]] = spTransform(CommutingRoutes[[i]], RD_new)
#       CommutingRoutes[[i]]@lines[[1]]@ID = paste(CommutingRoutes[[i]]@lines[[1]]@ID,i)
#       row.names(CommutingRoutes[[i]]) = as.character(i)
#     }
#     
#     # Convert large list to a SpatialLinesDataFrame with all the Commuting Routes
#     CommutingRoutes_SLDF = CommutingRoutes[[1]]
#     for (i in seq(2, length(Residence_random_kopp_WGS84_T), by=1))
#     {
#       CommutingRoutes_SLDF = rbind(CommutingRoutes_SLDF, CommutingRoutes[[i]])
#     }
#     
#     # Write to GeoJSON
#     Gjson_out_name = "Commuting Routes"
#     Gjson_out_ext = ".geojson"
#     Gjson_out = file.path("..", "output", Gjson_out_name)
#     writeOGR(CommutingRoutes_SLDF, Gjson_out , Gjson_out_name, driver='GeoJSON')
#     file.rename(Gjson_out, file.path("..", "output", paste(Gjson_out_name,Gjson_out_ext,sep="")))
# }

