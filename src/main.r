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

## TESTED ON WINDOWS 7 (64-bit)

## TODO:  - Make separate modules.
##        - Download input data from server.
##        - Read Air Pollution data (PM10 & NO2)
##          - Convert image bands to values with legend information. (temporary method)
##        - ...

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

## Clear the workspace
#rm(list = ls()) 

#getwd()

## Read the input data
zip_in = file.path("data", "bag-adressen-laatst.csv.zip")
csv_in = file.path("data", "bagadres.csv")

# Check if input data is available
if (!file.exists(zip_in) & !file.exists(csv_in))
{
  stop(paste("BAG addresses not found (.csv)"))
}
if (!file.exists(csv_in))
  {
    unzip(zip_in, exdir= file.path("data"))
  }

dbf_in = file.path("data", "verblijfsobjectgebruiksdoel.dbf")

BAG_Addresses = fread(csv_in, sep=";", header=TRUE, integer64 = "numeric",
            drop = c("huisletter", "object_type", "lon", "lat", "nevenadres")) # 64-bit only.  This will take about 2 minutes.
#file.remove(csv_in)

## Make a subset of BAG addresses
BAG_Addresses_sub = subset(BAG_Addresses, provincie=="Noord-Holland")

# Drop the attributes that are not being used
drops = c("gemeente","provincie","huisnummertoevoeging") #,"lon","lat","nevenadres","object_type"
SUB1 = BAG_Addresses_sub[ , !(names(BAG_Addresses_sub) %in% drops)]
BAG_Addresses_subbed = subset(BAG_Addresses_sub, select = SUB1)

## Remove intermediate results from environment
rm(BAG_Addresses,BAG_Addresses_sub)

## Read the database file with "Gebruiksdoelen" Goal of Use
BAG_Doel = read.dbf(dbf_in) #as.is=FALSE       # this will take about 10 minutes.

## Keep useful attributes
keeps_dbf = c("IDENTIFICA","GEBRUIKSDO")
BAG_Doel_sub = subset(BAG_Doel, select=keeps_dbf)

## Rename column name Object ID from "Gebruiksdoelen", to match with "Addresses"
colnames(BAG_Doel_sub)[1] <- "object_id"

## Merge "Gebruiksdoelen" and "Addresses"
BAG_AddrDoel = merge(BAG_Doel_sub, BAG_Addresses_subbed, by="object_id", duplicateGeoms=TRUE)

## Copy Doelbestanden and delete duplicates. This will create unique obs.
BAG_AddrDoel_uni = BAG_AddrDoel
BAG_AddrDoel_uni = BAG_AddrDoel[!duplicated(BAG_AddrDoel),]

## Make spatial
BAG_AddrDoel_SPDF = BAG_AddrDoel_uni
coordinates(BAG_AddrDoel_SPDF)<-~x+y

## Remove intermediate results from environment
rm(BAG_Addresses_subbed,BAG_Doel,BAG_Doel_sub,BAG_AddrDoel,BAG_AddrDoel_uni)

## Set Coordinate Reference System (CRS)
#RD_new = "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
RD_new = "+init=epsg:28992"
BAG_AddrDoel_SPDF@proj4string = CRS(RD_new)

## Subset: only Residence
Residence = subset(BAG_AddrDoel_SPDF, BAG_AddrDoel_SPDF@data$GEBRUIKSDO == "woonfunctie")

## Pick 50 random redidence object_id and make a subset
RandObj_Re = sample(Residence@data$object_id, 50)
keeps_RS_Re = RandObj_Re
Residence_KEEPS = Residence@data$object_id %in% keeps_RS_Re
Residence_random = subset(Residence, Residence_KEEPS)

## Subset: only Workplaces (offices)
Workplace = subset(BAG_AddrDoel_SPDF, BAG_AddrDoel_SPDF@data$GEBRUIKSDO == "kantoorfunctie")

## Pick 1000 random offices object_id and make a subset
RandObj_Of = sample(Workplace@data$object_id, 1000)
keeps_RS_Of = RandObj_Of
Workplace_KEEPS = Workplace@data$object_id %in% keeps_RS_Of
Workplace_random = subset(Workplace, Workplace_KEEPS)

## Remove duplicates in object_id
Workplace_random = Workplace_random[!duplicated(Workplace_random@data$object_id),]

## Visualisation & point highlighting
plot(Residence_random, col = "green")
plot(Workplace_random, col = "orange", add = TRUE)
points(Residence_random[4,], col="blue" ) # highlight Residence 4
points(Workplace_random[12,], col="red" ) # highlight workplace 12

## Create distance matrix
DIST_GEO = matrix(data=NA, nrow=length(Residence_random), ncol=length(Workplace_random)) # empty/NA matrix
for (z in seq(1, length(Residence_random), by=1))
  {
    for (w in seq(1, length(Workplace_random), by=1))
    {
      DIST_GEO[z,w] = gDistance(Residence_random[z,],Workplace_random[w,]) #[Residence,Workplace]
    }
  }

## Select pair, based on prob distribution of commuting distance (CD)

# prob distribution of CD
prob_CD = rnorm(1000, mean=28000, sd=10000)
prob_CD = prob_CD[prob_CD > 200]
hist(prob_CD, probability=TRUE)
#CD = seq(min(prob_CD), max(prob_CD), length=28)
#lines(CD, dnorm(CD, mean=28000, sd=10000))

# Extract the pair numbers Residence~Workplace (ex: 1~24, 2~35) (test group can be collegues)
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
Workplace_random@data$NR = seq(length(Workplace_random))

keeps_WP = Residence_random_kopp@data$koppeling
Workplace_KEEPS2 = Workplace_random@data$NR %in% keeps_WP
Workplace_random_NR = subset(Workplace_random, Workplace_KEEPS2)

#Write to geojson
Gjson_out_name = "Residence"
Gjson_out_ext = ".geojson"
Gjson_out = file.path("output", Gjson_out_name)
writeOGR(Residence_random_kopp, Gjson_out , Gjson_out_name, driver='GeoJSON')
file.rename(Gjson_out, file.path("output", paste(Gjson_out_name,Gjson_out_ext,sep="")))

#Write to geojson
Gjson_out_name = "Workplace"
Gjson_out_ext = ".geojson"
Gjson_out = file.path("output", Gjson_out_name)
writeOGR(Workplace_random_NR, Gjson_out , Gjson_out_name, driver='GeoJSON')
file.rename(Gjson_out, file.path("output", paste(Gjson_out_name,Gjson_out_ext,sep="")))


# Transform RD new -> WGS84 for the route calculation (OSRM)
WGS84 = "+init=epsg:4326"
Residence_random_kopp_WGS84 = Residence_random_kopp
#Residence_random_kopp_WGS84@proj4string = CRS(WGS84)
#Residence_random_kopp_WGS84@data$WSResID = 1:length(Residence_random_kopp_WGS84)
Residence_random_kopp_WGS84_T <- spTransform(Residence_random_kopp, WGS84)

Workplace_random_NR_WGS84 = Workplace_random_NR
#Workplace_random_NR_WGS84@proj4string = CRS(WGS84)
#Workplace_random_NR_WGS84@data$WSWorID = 1:length(Workplace_random_NR_WGS84)
Workplace_random_NR_WGS84_T <- spTransform(Workplace_random_NR, WGS84)

# Copy @data and add the coordinates to the new dataframe (...2)
Residence_random_kopp_WGS84_T2 = Residence_random_kopp_WGS84_T@data
Residence_random_kopp_WGS84_T2$lon1 = Residence_random_kopp_WGS84_T@coords[,1]
Residence_random_kopp_WGS84_T2$lat1 = Residence_random_kopp_WGS84_T@coords[,2]

Workplace_random_NR_WGS84_T2 = Workplace_random_NR_WGS84_T@data
Workplace_random_NR_WGS84_T2$lon2 = Workplace_random_NR_WGS84_T@coords[,1]
Workplace_random_NR_WGS84_T2$lat2 = Workplace_random_NR_WGS84_T@coords[,2]

## Merge Residence with Workplace and order on Residence (WSResID)
RESWOR = merge(Residence_random_kopp_WGS84_T2,Workplace_random_NR_WGS84_T2, by.x= "koppeling", by.y="NR")
RESWOR = setorder(RESWOR, cols=WSResID)

## Generate Commuting Routes (CR)
CommutingRoutes = list()
for (i in seq(1, length(Residence_random_kopp_WGS84_T), by=1))
  {
    CommutingRoutes[i] = osrmRoute(src=c(paste("RES",i,sep="_"), RESWOR["lon1"][i,], RESWOR["lat1"][i,]),
                           dst = c(paste("WOR",RESWOR["koppeling"][i,],sep="_"), RESWOR["lon2"][i,], RESWOR["lat2"][i,]),
                             overview = "full", #"simplified"
                             sp = TRUE)
    #CommutingRoutes[[i]]@proj4string = CRS(RD_new)
    CommutingRoutes[[i]] = spTransform(CommutingRoutes[[i]], RD_new)
    CommutingRoutes[[i]]@lines[[1]]@ID = paste(CommutingRoutes[[i]]@lines[[1]]@ID,i)
    
    row.names(CommutingRoutes[[i]]) = as.character(i)
}

# Convert large list to a SpatialLinesDataFrame with all the Commuting Routes
CommutingRoutes_SLDF = CommutingRoutes[[1]]
for (i in seq(2, length(Residence_random_kopp_WGS84_T), by=1))
  {
    CommutingRoutes_SLDF = rbind(CommutingRoutes_SLDF, CommutingRoutes[[i]])
  }

# Write to GeoJSON
Gjson_out_name = "Commuting Routes"
Gjson_out_ext = ".geojson"
Gjson_out = file.path("output", Gjson_out_name)
writeOGR(CommutingRoutes_SLDF, Gjson_out , Gjson_out_name, driver='GeoJSON')
file.rename(Gjson_out, file.path("output", paste(Gjson_out_name,Gjson_out_ext,sep="")))



# Write subset
#csv_out = file.path("output", "bagadres_ProvincieUtrecht.csv")

#system.time(OUT =  write.csv2(WS2, file = csv_out) ) # Europe , sep
#system.time(OUT =  write.csv(WS2, file = csv_out) ) # USA/UK . sep
