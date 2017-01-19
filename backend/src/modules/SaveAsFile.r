# Module for saving spatial files to hard drive as GeoJSON or Shapefile.
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
# list.of.packages <- c("rhdf5", "raster")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# #if(length(new.packages)) install.packages(new.packages)
# if(length(new.packages)) source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

## Load the packages
library(rgdal)

SaveAsFile <- function(INput, Filename, Format, OverwriteLayer, ...)
{
  if (Format == "Shapefile")
  {
    # Write to Shapefile in output folder
    Shape_out = file.path("..", "output")
    writeOGR(INput, Shape_out, Filename, driver="ESRI Shapefile", overwrite_layer = OverwriteLayer)
    
  }
  if (Format == "GeoJSON")
  {
    # Write to GeoJSON in output folder
    GeoJSON_out = file.path("..", "output", Filename)
    writeOGR(INput, GeoJSON_out, Filename, driver="GeoJSON", overwrite_layer = overwrite_layer)
    GeoJSON_ext = ".geojson"
    file.rename(GeoJSON_out, file.path("..", "output", paste0(Filename, GeoJSON_ext)))
  }
}