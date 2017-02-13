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
list.of.packages <- c("data.table","sp","rgdal","foreign","rgeos","osrm")
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
  
  BE_crs = CRS("+init=epsg:31370")
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
  
  
  # OLD METHOD FOR CRAB CLASSIFICATION  
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
    # METHOD 2: join on "street+number+zipcode" combination
    
    ## Primary schools
    
    # create (empty) DOEL object
    #CRAB_Doel@data$DOEL = NA
    
    Onderwijs.Basis$NUMMERTJE = NA
    Onderwijs.Basis$NUMMERTJE = paste0(Onderwijs.Basis[["crab-huisnr"]],Onderwijs.Basis[["crab-busnr"]])
    head(Onderwijs.Basis$NUMMERTJE, 100)
    
    CRAB_Doel@data$DOEL[tolower(paste0(CRAB_Doel@data$POSTCODE, CRAB_Doel@data$STRAATNM, CRAB_Doel@data$HUISNR)) %in% 
                          tolower(paste0(Onderwijs.Basis$postcode,Onderwijs.Basis$straat, Onderwijs.Basis$NUMMERTJE))] = "Basisonderwijs"
    
    CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",] # VIEW
    
    # check on numbers (total Flanders only)
    length(Onderwijs.Basis[[1]]) == length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    delta = length(Onderwijs.Basis[[1]]) - length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs",])
    paste("Missing",delta, "features.")
    
    ## Secondary schools
    
    Onderwijs.Secundair$NUMMERTJE = NA
    Onderwijs.Secundair$NUMMERTJE = paste0(Onderwijs.Secundair[["crab-huisnr"]],Onderwijs.Secundair[["crab-busnr"]])
    head(Onderwijs.Secundair$NUMMERTJE, 100)
    
    CRAB_Doel@data$DOEL[tolower(paste0(CRAB_Doel@data$POSTCODE, CRAB_Doel@data$STRAATNM, CRAB_Doel@data$HUISNR)) %in% 
                          tolower(paste0(Onderwijs.Secundair$postcode,Onderwijs.Secundair$straat, Onderwijs.Secundair$NUMMERTJE))] = "Secundair onderwijs"
    
    CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",] # VIEW
    
    #     # check on numbers
    #     length(Onderwijs.Secundair[[1]]) == length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",])
    #     delta = length(Onderwijs.Secundair[[1]]) - length(CRAB_Doel[CRAB_Doel@data$DOEL %in% "Secundair onderwijs",])
    #     paste("Missing",delta, "features.")
    
  }
  
  if (Method.nr == 3)
  {
    # METHOD 3: Spatial join, based on closest point
    
    ## Primary schools
    
    # create (empty) DOEL object
    #CRAB_Doel@data$DOEL = NA
    
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
  
  
  # Companies
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
  
  CRAB_Doel[CRAB_Doel@data$DOEL %in% "Company",] # VIEW
  
  
  # Replace all NA with "Woonfunctie" (assuming all other addresses are Residence)
  CRAB_Doel@data$DOEL[is.na(CRAB_Doel@data$DOEL)] = "Woonfunctie"
  
  # Remove duplicates
  #tail(CRAB_Doel@data[!duplicated(CRAB_Doel@data[ , 2:4 ]), ])
  #tail(CRAB_Doel)
  
  CRAB.unique = CRAB_Doel[!duplicated(CRAB_Doel@data[ , 2:4 ]), ]
  #tail(CRAB.unique)
  
  return(CRAB.unique)
}


#Names.sub = Names
#FL.residence = 10
#FL.secondary = 100
#OSRM.level = OSRM.Level

DeterminePPH_FL <- function(CRAB_Doel, Names.sub, FL.residence, FL.secondary, OSRM.level, Active.Type, ... )
{
  
  # Create the attribute "object_id" (verplaatsen naar andere functie)
  CRAB_Doel@data["object_id"] = seq.int(nrow(CRAB_Doel@data))
  
  ## Subset: only Residence
  Residence = subset(CRAB_Doel, CRAB_Doel@data$DOEL == "Woonfunctie")
  
  ## Pick x random redidence object_id and make a subset
  RandObj_Re = sample(Residence@data$object_id, FL.residence)
  keeps_RS_Re = RandObj_Re
  Residence_KEEPS = Residence@data$object_id %in% keeps_RS_Re
  Residence_random = subset(Residence, Residence_KEEPS)
  
  if (Active.Type == "02.HO")
  {
    SaveAsFile(Residence_random, paste0(Active.Type,"_Residence_", Names.sub), "GeoJSON", TRUE)
  }
  
  #Subset: only Offices
  if (Active.Type == "01.OW")
  {
    #Secondary = subset(CRAB_Doel, CRAB_Doel@data$DOEL == "Economische functie")
    Secondary = CRAB_Doel[CRAB_Doel@data$DOEL %in% "Economische functie" | CRAB_Doel@data$DOEL %in% "Company" ,]
    Mean.distance = 46210
    SD.distance = 10000
  }
  
  #Subset: only schools
  if (Active.Type == "03.SP")
  {
    #Secondary = subset(CRAB_Doel, CRAB_Doel@data$DOEL == "Basisonderwijs")
    Secondary = CRAB_Doel[CRAB_Doel@data$DOEL %in% "Basisonderwijs" | CRAB_Doel@data$DOEL %in% "Secundair onderwijs" ,]
    Mean.distance = 0.001 #closest school
    SD.distance = 10
  }
  
  if (Active.Type != "02.HO")
  {
    ## Pick x random Secondary object_id and make a subset
    RandObj_Se = sample(Secondary@data$object_id, FL.secondary)
    keeps_RS_Se = RandObj_Se
    Secondary_KEEPS = Secondary@data$object_id %in% keeps_RS_Se
    Secondary_random = subset(Secondary, Secondary_KEEPS)
    
    ## Remove duplicates in object_id
    Secondary_random = Secondary_random[!duplicated(Secondary_random@data$object_id),]
    
    ## Create distance matrix
    DIST_GEO = matrix(data=NA, nrow=length(Residence_random), ncol=length(Secondary_random)) # empty/NA matrix
    for (z in seq(1, length(Residence_random), by=1))
    {
      for (w in seq(1, length(Secondary_random), by=1))
      {
        DIST_GEO[z,w] = gDistance(Residence_random[z,],Secondary_random[w,]) #[Residence,Secondary]
      }
    }
    
    ## Select pair, based on prob distribution of commuting distance (CD)
    
    # prob distribution of CD
    prob_CD = rnorm(1000, mean = Mean.distance, sd = SD.distance) 
    prob_CD = prob_CD[prob_CD > 200] # remove the large distances
    #hist(prob_CD, probability=TRUE)
    #CD = seq(min(prob_CD), max(prob_CD), length=28)
    #lines(CD, dnorm(CD, mean=28000, sd=10000))
    
    # Extract the pair numbers Residence~Secondary (ex: 1~24, 2~35) (test group can be collegues)
    ResWor=0
    for (z in seq(1, length(Residence_random), by=1))
    {
      RV = sample(prob_CD, size=1)
      ResWor[z] = which.min(abs(RV-DIST_GEO[z,]))
    }
    
    # Giving the pairs to the datasets
    Residence_random_kopp = Residence_random
    Residence_random_kopp@data$koppeling = ResWor
    
    # Give the WSResID
    Residence_random_kopp@data$WSResID = 1:length(Residence_random_kopp)
    
    # Delete the Workplaces in SPDF that are not paires (these residuals are not used)
    Secondary_random@data$NR = seq(length(Secondary_random))
    
    keeps_WP = Residence_random_kopp@data$koppeling
    Secondary_KEEPS2 = Secondary_random@data$NR %in% keeps_WP
    Secondary_random_NR = subset(Secondary_random, Secondary_KEEPS2)
    
    SaveAsFile(Residence_random_kopp, paste0(Active.Type,"_Residence_", Names.sub), "GeoJSON", TRUE)
    SaveAsFile(Secondary_random_NR, paste0(Active.Type,"_Secondary_", Names.sub), "GeoJSON", TRUE)
    
    # Transform RD new -> WGS84 for the route calculation (OSRM)
    WGS84 = "+init=epsg:4326"
    Residence_random_kopp_WGS84 = Residence_random_kopp
    Residence_random_kopp_WGS84_T <- spTransform(Residence_random_kopp, WGS84)
    
    Secondary_random_NR_WGS84 = Secondary_random_NR
    Secondary_random_NR_WGS84_T <- spTransform(Secondary_random_NR, WGS84)
    
    # Copy @data and add the coordinates to the new dataframe (...2)
    Residence_random_kopp_WGS84_T2 = Residence_random_kopp_WGS84_T@data
    Residence_random_kopp_WGS84_T2$lon1 = Residence_random_kopp_WGS84_T@coords[,1]
    Residence_random_kopp_WGS84_T2$lat1 = Residence_random_kopp_WGS84_T@coords[,2]
    
    Secondary_random_NR_WGS84_T2 = Secondary_random_NR_WGS84_T@data
    Secondary_random_NR_WGS84_T2$lon2 = Secondary_random_NR_WGS84_T@coords[,1]
    Secondary_random_NR_WGS84_T2$lat2 = Secondary_random_NR_WGS84_T@coords[,2]
    
    ## Merge Residence with Workplace and order on Residence (WSResID)
    RESWOR = merge(Residence_random_kopp_WGS84_T2,Secondary_random_NR_WGS84_T2, by.x= "koppeling", by.y="NR")
    RESWOR = setorder(RESWOR, cols=WSResID)
    
    BE_crs = "+init=epsg:31370"
    
    ## Generate Commuting Routes (CR)
    # Outwards (Residence -> Secondary)
    CommutingRoutes1 = list()
    CommutingRoutes2 = list()
    for (i in seq_along(Residence_random_kopp_WGS84_T))
    {
      RES = c(paste("RES",i,sep="_"), RESWOR["lon1"][i,], RESWOR["lat1"][i,])
      WOR = c(paste("WOR",RESWOR["koppeling"][i,],sep="_"), RESWOR["lon2"][i,], RESWOR["lat2"][i,])
      
      CommutingRoutes1[i] = osrmRoute(src= RES, dst = WOR, overview = OSRM.level, # "full"/"simplified"
                                      sp = TRUE)
      CommutingRoutes1[[i]] = spTransform(CommutingRoutes1[[i]], BE_crs)
      CommutingRoutes1[[i]]@lines[[1]]@ID = paste(CommutingRoutes1[[i]]@lines[[1]]@ID,i)
      row.names(CommutingRoutes1[[i]]) = as.character(i)
      
      CommutingRoutes2[i] = osrmRoute(src= WOR, dst = RES, overview = OSRM.level, # "full"/"simplified"
                                      sp = TRUE)
      CommutingRoutes2[[i]] = spTransform(CommutingRoutes2[[i]], BE_crs)
      CommutingRoutes2[[i]]@lines[[1]]@ID = paste(CommutingRoutes2[[i]]@lines[[1]]@ID,i)
      row.names(CommutingRoutes2[[i]]) = as.character(i)
    }
    
    # Convert large list to a SpatialLinesDataFrame with all the Commuting Routes
    CommutingRoutes1_SLDF = CommutingRoutes1[[1]]
    CommutingRoutes2_SLDF = CommutingRoutes2[[1]]
    for (i in seq(2, length(Residence_random_kopp_WGS84_T), by=1))
    {
      CommutingRoutes1_SLDF = rbind(CommutingRoutes1_SLDF, CommutingRoutes1[[i]])
      CommutingRoutes2_SLDF = rbind(CommutingRoutes2_SLDF, CommutingRoutes2[[i]])
    }
    
    CommutingRoutes1_SLDF@data["PersonID"] = seq.int(CommutingRoutes1_SLDF)
    CommutingRoutes2_SLDF@data["PersonID"] = seq.int(CommutingRoutes2_SLDF)
    
    SaveAsFile(CommutingRoutes1_SLDF, paste0(Active.Type,"_CommutingRoutesOutwards_", Names.sub, "_", substr(OSRM.level, 1, 1)), "GeoJSON", TRUE)
    SaveAsFile(CommutingRoutes2_SLDF, paste0(Active.Type,"_CommutingRoutesInwards_", Names.sub, "_", substr(OSRM.level, 1, 1)), "GeoJSON", TRUE)
    
  }
  
}


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

