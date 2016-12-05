# Module for reading IDF5-files
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
library(rhdf5)
library(raster)
library(stringr)
library(rgdal)
library(rgeos)

#pol = "no2"
#locationId.BE = 68083

ReadHDF5 <- function(pol, locationId.BE, ...)
{
  ## Open the IDF5-file
  pollutant = pol
  polFile = paste0(pol, "-gzip.hdf5")
  h5f_in = file.path("..", "data", "BE", "ATMOSYS", polFile)
  h5f = H5Fopen(h5f_in)
  #h5f.D = H5Dopen(h5f, h5loc = h5f$"0")
  
  activeDataset = locationId.BE / 10000
  activeLocation = locationId.BE %% 10000
  activeName = activeDataset - (activeLocation/10000)
  
  h5f.active = h5read(h5f_in, as.character(activeName)) # h5read(h5f_in, 6)
  #h5f.active.df = data.frame(h5f.active)
  
  #h5f.active = h5read(h5f_in, "6", index=list(2,201))
  h5f.active$data[5,activeLocation] #[hour of the year,activeLocation]
  
  #ActiveH5FLocation = list(h5f.active.df, activeLocation)
  
  return(ActiveH5FLocation)
  
}

h5ls(h5f_in)
WSWSWS = h5read(h5f_in,"0/data")
WSWSWS2 = h5f$0$data

h5f_WS$"6"$data[4,6500]

activeDataset = locationId.BE / 10000
activeLocation = locationId.BE %% 10000
activeName = activeDataset - (activeLocation/10000)

h5f$paste0(activeName)$data[4,3000]

h5f$"9"$data[4,6500]
h5f[[as.character(activeName)]]



(h5read(h5f_in, as.character(activeName)))[4,3000]

h5f&"7"[4,6000]

HDF5_dir = file.path("..", "data", "BE", "ATMOSYS", "no2-gzip_WS.hdf5")
UnzipHDF5 <- function(HDF5_dir, ...)
{
  h5createFile(HDF5_dir)
  h5ls(HDF5_dir)
  
  h5createGroup(HDF5_dir, as.character(1))
  h5ls(HDF5_dir)
  
  h5f_WS = H5Fopen(HDF5_dir)
  H5close()
  
  A = matrix(1:10,nr=5,nc=2)
  h5write(A, HDF5_dir, as.character(2))
  
  h5write(h5f.active, HDF5_dir, as.character(6))
  
  
  
  H5Fclose(h5f_WS)
  H5Fclose(h5f)
}


ExtractExposureValue <- function(pol, locationId.BE, HourOfTheYear, ...)
{
  ## Open the IDF5-file
  pollutant = pol
  polFile = paste0(pol, "-gzip.hdf5")
  h5f_in = file.path("..", "data", "BE", "ATMOSYS", polFile)
  h5f = H5Fopen(h5f_in)
  
  activeDataset = locationId.BE / 10000
  activeLocation = locationId.BE %% 10000
  activeName = activeDataset - (activeLocation/10000)
  
  # find better solution than repeating 42 times
  if (activeName == 0)
  {
    ExposureValue = h5f$"0"$data[HourOfTheYear+1, activeLocation]
    #ExposureValue = h5f$"0"$data[4,6500]
  }
  
  if (activeName == 1)
  {
    ExposureValue = h5f$"1"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 2)
  {
    ExposureValue = h5f$"2"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 3)
  {
    ExposureValue = h5f$"3"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 4)
  {
    ExposureValue = h5f$"4"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 5)
  {
    ExposureValue = h5f$"5"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 6)
  {
    ExposureValue = h5f$"6"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 7)
  {
    ExposureValue = h5f$"7"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 8)
  {
    ExposureValue = h5f$"8"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 9)
  {
    ExposureValue = h5f$"9"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 10)
  {
    ExposureValue = h5f$"10"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 11)
  {
    ExposureValue = h5f$"11"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 12)
  {
    ExposureValue = h5f$"12"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 13)
  {
    ExposureValue = h5f$"13"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 14)
  {
    ExposureValue = h5f$"14"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 15)
  {
    ExposureValue = h5f$"15"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 16)
  {
    ExposureValue = h5f$"16"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 17)
  {
    ExposureValue = h5f$"17"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 18)
  {
    ExposureValue = h5f$"18"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 19)
  {
    ExposureValue = h5f$"19"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 20)
  {
    ExposureValue = h5f$"20"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 21)
  {
    ExposureValue = h5f$"21"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 22)
  {
    ExposureValue = h5f$"22"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 23)
  {
    ExposureValue = h5f$"23"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 24)
  {
    ExposureValue = h5f$"24"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 25)
  {
    ExposureValue = h5f$"25"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 26)
  {
    ExposureValue = h5f$"26"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 27)
  {
    ExposureValue = h5f$"27"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 28)
  {
    ExposureValue = h5f$"28"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 29)
  {
    ExposureValue = h5f$"29"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 30)
  {
    ExposureValue = h5f$"30"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 31)
  {
    ExposureValue = h5f$"31"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 32)
  {
    ExposureValue = h5f$"32"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 33)
  {
    ExposureValue = h5f$"33"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 34)
  {
    ExposureValue = h5f$"34"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 35)
  {
    ExposureValue = h5f$"35"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 36)
  {
    ExposureValue = h5f$"36"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 37)
  {
    ExposureValue = h5f$"37"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 38)
  {
    ExposureValue = h5f$"38"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 39)
  {
    ExposureValue = h5f$"39"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 40)
  {
    ExposureValue = h5f$"40"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 41)
  {
    ExposureValue = h5f$"41"$data[HourOfTheYear+1, activeLocation]
  }
  if (activeName == 42)
  {
    ExposureValue = h5f$"42"$data[HourOfTheYear+1, activeLocation]
  }
  
  
  H5Fclose(h5f)
  return (ExposureValue)
}

ExtractExposureValue_ <- function(ActiveH5FLocation, HourOfTheYear, ...)
{
  #locationId = locationId.BE #68083 # LEFT OF . IS "name" | RIGHT OF . IS "otype dclass dim"
  #activeCode  = as.character(CT$location[68083])
  
  ExposureValue = ActiveH5FLocation[[1]][[paste0("data.", ActiveH5FLocation[[2]])]][HourOfTheYear+1]
  
  return (ExposureValue)
}


ExtractExposureValueResidence <- function(ActiveH5FLocation, HourOfTheYear, ...)
{
  
  
  ExposureValueResidence = ActiveH5FLocation[[1]][[paste0("data.", ActiveH5FLocation[[2]])]][HourOfTheYear+1]
  return (ExposureValueResidence )
}

CalculateResolution <- function(CT, ...)
{
  ## calculate resolution
  BE_surface = 30528 #Belgian surface in km2
  data.obs = nrow(CT)
  #obs.per.km2 = data.obs/BE_surface # obs per km2
  
  obs.per.km2 = data.obs/BE_surface # observations per km2
  ws = sqrt(obs.per.km2)
  res = 1/ws*1000
  paste0("The average resolution is: ", res, "m ", "x ", res, "m.")
}
