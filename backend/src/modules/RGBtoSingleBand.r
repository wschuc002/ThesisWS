# Module for converting RGB GeoTiffs to single band GeoTiffs
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
list.of.packages = c("raster", "png", "data.table")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# For packages not available in CRAN, but are on Github:
#if(length(new.packages)) devtools::install_github("duncantl/RGoogleDocs")


## Load the package(s)
library(raster)
library(png)
library(data.table)


RGBtoSingleBand <- function(RGB.airNL.in, ... )
  {

    ## Read Air Quality data (PM10)
    pm10_in = file.path("..", "data", "RIVM2", RGB.airNL.in)
    PM10_b1 = raster(pm10_in, band=1)
    PM10_b2 = raster(pm10_in, band=2)
    PM10_b3 = raster(pm10_in, band=3)
    PM10_RGB = brick(PM10_b1, PM10_b2, PM10_b3) # brick un-aggregated
    #plot(PM10_RGB)
    
    # Remove all white (255,255,255) values | Remove all 255 values in 2nd and 3rd band
    DROPS = values(PM10_RGB[[1]]) == 255 & values(PM10_RGB[[2]]) == 255 & values(PM10_RGB[[3]]) == 255
    DROPS2 = values(PM10_RGB[[2]]) == 255 | values(PM10_RGB[[3]]) == 255
    
    PM10_RGB_filtered = PM10_RGB
    values(PM10_RGB_filtered)[DROPS == TRUE | DROPS2 == TRUE] <- NA
    #plot(PM10_RGB_filtered)
    
    ## Read legend files with their colour bands (On 2016-09-27 PM10 and NO2 have the same legend)
    no2_pix.lst = list.files(path = file.path("..", "data", "RIVM", "legends"), pattern = "no2_[0-9]*_[0-9]*.png|no2_[0-9]*[+].png")
    no2_pix.dr = file.path("..", "data", "RIVM", "legends", no2_pix.lst)
    
    #library(png)
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
    
    #values(SUMbrick_filtered)
    #plot(SUMbrick_filtered)
    
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
    #plot(Converted_filtered)
    
    ## Filter when the values are too high. Via aggr errors
    #values(Converted_filtered)[values(Converted_filtered) > 300] <- 0
    #plot(Converted_filtered)
    
    
    # Check if RGB sums are unique
    if (length(unique(DT$RGBSUM)) != length(legend.split))
    {
      stop(paste("RGB sums are not unique. Find another method for bands..."))
    }
    
    
    ## Write to GeoTIFF
    # Remove "vandaag" in filename
    out.split = regmatches(x = RGB.airNL.in, gregexpr('[_]|[.]',RGB.airNL.in),invert=TRUE)
    
    GTiff_out_name = paste(out.split[1][[1]][1],out.split[1][[1]][4],out.split[1][[1]][3], "WS", sep = "_")
    GTiff_out = file.path("..", "output", GTiff_out_name)
    writeRaster(Converted_filtered, GTiff_out, "GTiff")
    
  }