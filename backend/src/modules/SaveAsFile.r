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
list.of.packages <- c("rgdal", "raster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(rgdal)
library(raster)

SaveAsFile <- function(INput, Filename, Format, Active.Type, OverwriteLayer, ...)
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
  
  if (Format == "csv")
  {
    csv_out = file.path("..", "output", paste(Active.Type, Filename, "Residence.csv", sep = "_"))
    write.csv(INput, csv_out, eol = "\n")
  }
  
  if (Format == "GeoTIFF")
  {
    GTiff_out = file.path("..", "output", Filename)
    writeRaster(INput, GTiff_out, "GTiff", overwrite = OverwriteLayer)
  }
  
}

SaveAsDBF <- function(INput, FileType, PhaseType, Filename, Active.Type, OverwriteLayer, pol, ...)
{
  if (PhaseType == "Primary") {PhaseLetter = "P"}
  if (PhaseType == "Secondary") {PhaseLetter = "S"}
  if (PhaseType == "T1") {PhaseLetter = "T1"}
  if (PhaseType == "T2") {PhaseLetter = "T2"}
  
  if (FileType == "Exposure") {FileString = "EXP_"}
  if (FileType == "Time") {FileString = "TIME_"}
  
  if (class(INput) == "list")
  {
    Folder = file.path("..", "output", Active.Type)
    if (!dir.exists(Folder))
    {
      dir.create(file.path("..", "output", Active.Type))
    }
    
    for (i in seq_along(INput))
      #for (i in seq(1,5,1))  
    {
      DF = data.frame(transpose(INput[[i]]))
      
      COLNAMES = c(1:length(DF))
      colnames(DF) = paste0("C",COLNAMES)
      
      if (FileType == "Exposure")
      {
        #dbf_out = file.path("..", "output", Active.Type, paste(Filename, i, toupper(pol), sep = "_"))
        dbf_out = file.path("..", "output", Active.Subtype, paste0(FileString, PhaseLetter, "_" , i, "_", toupper(pol), ".dbf"))
      }
      if (FileType == "Time")
      {
        dbf_out = file.path("..", "output", Active.Subtype, paste0(FileString, PhaseLetter, "_" , i, ".dbf"))
      }
      
      # write
      if (file.exists(dbf_out))
      {
        if (OverwriteLayer)
        {
          write.dbf(DF, dbf_out, factor2char = TRUE, max_nchar = 254)
        }
      } else
      {
        write.dbf(DF, dbf_out, factor2char = TRUE, max_nchar = 254)
      }
    }
  }
  
  if (class(INput) == "data.frame")
  {
    Temp_dir = file.path("..", "output", "temp")
    if (!dir.exists(Temp_dir)) 
    {
      dir.create(Temp_dir)
    }
    
    DF = INput
    
    COLNAMES = c(1:length(DF))
    colnames(DF) = paste0("C",COLNAMES)
    
    dbf_out = file.path(Temp_dir, Filename)
    write.dbf(DF, dbf_out, factor2char = TRUE, max_nchar = 254, over)
  }
}