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
##        - Download input data from server (Google Drive).
##        - Systematically convert RGB to SingleBand
##        - ...

## Note 1: This script only brings different modules in modules/ together.
## No code belonging to one of the modules should go here.

## Note 2: It is expected that the working directory is set to backend/src/

#### Import modules ####

source("modules/input.r")
source("modules/RGBtoSingleBand.r")
source("modules/DetermineRoutes.r")
source("modules/CumulativeExposure.r")

download.AQNL("20161108_pm10_no2.7z") # bug in downloading files from Google Drive
unzip.AQNL("20161108_pm10_no2.zip")

#RGBtoSingleBand("20161108_vandaag_no2_03.tiff")
RGB.list = list.files(file.path("..", "data", "RIVM2"), pattern = ".tiff" )
for (i in RGB.list)
{
  RGBtoSingleBand(i)
}

DetermineRoutesNL("Limburg")




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

## Read the input data
zip_in = file.path("data", "BAG", "bag-adressen-laatst.csv.zip")
csv_in = file.path("data", "BAG", "bagadres.csv")

zip_in = file.path("data", "BE_FL", "CRAB_Adressenlijst.zip")
shp_in = file.path("data", "BE_FL", "CrabAdr.shp")

# Check if input data is available
if (!file.exists(zip_in) & !file.exists(csv_in))
{
  stop(paste("BAG addresses not found (.csv)"))
}
if (!file.exists(csv_in))
{
  unzip(zip_in, exdir= file.path("data", "BAG"))
}

dbf_in = file.path("data", "BAG", "verblijfsobjectgebruiksdoel.dbf")

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


## Read Air Quality data (PM10)
pm10_in = file.path("data", "RIVM", "20160926_vandaag_pm10_09.tiff")
PM10_b1 = raster(pm10_in, band=1)
PM10_b2 = raster(pm10_in, band=2)
PM10_b3 = raster(pm10_in, band=3)
PM10_b1_agg = aggregate(PM10_b1, fun=modal, fact=20)
PM10_b2_agg = aggregate(PM10_b2, fun=modal, fact=20)
PM10_b3_agg = aggregate(PM10_b3, fun=modal, fact=20)
#PM10_RGB = brick(PM10_b1_agg, PM10_b2_agg, PM10_b3_agg) # brick aggregated
PM10_RGB = brick(PM10_b1, PM10_b2, PM10_b3) # brick un-aggregated
plot(PM10_RGB)

# Remove all white (255,255,255) values | Remove all 255 values in 2nd and 3rd band
DROPS = values(PM10_RGB[[1]]) == 255 & values(PM10_RGB[[2]]) == 255 & values(PM10_RGB[[3]]) == 255
DROPS2 = values(PM10_RGB[[2]]) == 255 | values(PM10_RGB[[3]]) == 255

PM10_RGB_filtered = PM10_RGB
values(PM10_RGB_filtered)[DROPS == TRUE | DROPS2 == TRUE] <- NA
plot(PM10_RGB_filtered)



## Read legend files with their colour bands (On 2016-09-27 PM10 and NO2 have the same legend)
no2_pix.lst = list.files(path = file.path("data", "RIVM", "legends"), pattern = "no2_[0-9]*_[0-9]*.png|no2_[0-9]*[+].png")
no2_pix.dr = file.path("data", "RIVM", "legends", no2_pix.lst)

library(png)
no2_pix_png = list()
for (i in seq(1, length(no2_pix.lst), by=1))
{
  no2_pix_png[[i]] = 255*readPNG(no2_pix.dr[i], native = FALSE, info = FALSE)
}

no2_pix_png_WS = list()#no2_pix_png
for (i in seq(1, length(no2_pix.lst), by=1))
{
  no2_pix_png_WS[paste(no2_pix.lst[i])] = no2_pix_png[i][1]
}
no2_pix_png_WS_M = as.matrix(no2_pix_png_WS)

# Read values from legend names (RegEx)
legend.split = strsplit(no2_pix.lst, "(_)|(!?.png)|(!?[+])")

# Assign AVR (Average value from legend)
for (i in seq(1, length(legend.split), by=1))
{
  legend.split[[i]][4] = (as.integer(legend.split[[i]][2])+as.integer(legend.split[[i]][3]))/2
  #Fix 200+
  if (is.na(legend.split[[i]][4]))
  {
    legend.split[[i]][4] = 220
  }
}
# Assign AVR to list
for (i in seq_along(legend.split))
{
  no2_pix_png_WS[[i]][4] = legend.split[[i]][4]
}
# Calculate RGBSUM in table
for (i in seq_along(no2_pix_png_WS))
{
  no2_pix_png_WS[[i]][5] = as.numeric(no2_pix_png_WS[[i]][1])+as.numeric(no2_pix_png_WS[[i]][2])+as.numeric(no2_pix_png_WS[[i]][3])
}

# Calculate RGBSUM in raster
SUMbrick_filtered = PM10_RGB_filtered[[1]]+PM10_RGB_filtered[[2]]+PM10_RGB_filtered[[3]]

values(SUMbrick_filtered)
plot(SUMbrick_filtered)

## Convert RGB to PM10 value

# Create data table
DT = as.data.table(no2_pix_png_WS)
DT = data.table(R=as.numeric(DT[1]), G=as.numeric(DT[2]), B=as.numeric(DT[3]), AVR = as.numeric(DT[4]), RGBSUM = as.numeric(DT[5]))

# # Check
# unique(SUMbrick_filtered) %in% unique(DT$RGBSUM)
# unique(PM10_RGB_filtered[[1]]) %in% unique(DT$R)
# unique(PM10_RGB_filtered[[2]]) %in% unique(DT$G)
# unique(PM10_RGB_filtered[[3]]) %in% unique(DT$B)
# 
# unique(PM10_b1) %in% unique(DT$R)
# unique(PM10_b2) %in% unique(DT$G)
# unique(PM10_b3) %in% unique(DT$B)

# Convert / conditional replace
Converted_filtered = SUMbrick_filtered
for (m in seq_along(legend.split))
{
  values(Converted_filtered)[values(Converted_filtered) == DT$RGBSUM[m]] <- DT$AVR[m]
}

#DROPS = DT$RGBSUM %in% values(SUMbrick_filtered)
plot(Converted_filtered)

## Filter when the values are too high. Via aggr errors
#values(Converted_filtered)[values(Converted_filtered) > 300] <- 0
#plot(Converted_filtered)


# Check if RGB sums are unique
if (length(unique(DT$RGBSUM)) != length(legend.split))
{
  stop(paste("RGB sums are not unique. Find another method for bands..."))
}


# Write to GeoTIFF
GTiff_out_name = "pm10_200160926_09_WS"
#GTiff_out_ext = ".tiff"
GTiff_out = file.path("output", GTiff_out_name)
writeRaster(Converted_filtered, GTiff_out, "GTiff")
#writeGDAL(Converted_filtered, GTiff_out , GTiff_out_name, drivername = "GTiff", type = "Float32")
#file.rename(Gjson_out, file.path("output", paste(Gjson_out_name,Gjson_out_ext,sep="")))




# toepassen op ongeaggr. raster

# seq_along(legend.split) ipv seq(1, length(legend.split), by =1)

# Get Legend Graphic
#http://acceptatie.geodata.rivm.nl:80/geoserver/ows?service=WMS&request=GetLegendGraphic&format=image/png&width=20&height=20&layer=vandaag_no2_15

# Write subset
#csv_out = file.path("output", "bagadres_ProvincieUtrecht.csv")

#system.time(OUT =  write.csv2(WS2, file = csv_out) ) # Europe , sep
#system.time(OUT =  write.csv(WS2, file = csv_out) ) # USA/UK . sep
