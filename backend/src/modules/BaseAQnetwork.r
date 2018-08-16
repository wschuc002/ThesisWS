# Module for reading the base of the RIO-IFDM network
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

## Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("sp", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)
library(data.table)

BaseAQnetwork <- function(pol, aq.dir, ...)
{
  PolDir = file.path(aq.dir, toupper(pol))
  
  # Check if the TXT base already is present, otherwise look for compressed bz2 file
  BaseFile = paste0(year.active, "0101_1_", toupper(pol), ".txt")
  txt.Points = file.path(PolDir, BaseFile)
  
  if (!file.exists(txt.Points))
  {
    # unzip
    base.txt.dr = ExtractBZ2(pol, PolDir, 1, 1)
  }
  
  # if (base.txt.dr != txt.Points)
  # {
  #   stop(paste("Base file", BaseFile, "is probably missing."))
  # }
  
  Points.NoVal = fread(txt.Points, sep=";", header=TRUE)
  coordinates(Points.NoVal) = ~x+y
  colnames(Points.NoVal@data) = NA
  Points.NoVal@data[,1] = NA
  Points.NoVal@proj4string = BE_crs
  
  return(Points.NoVal)
}