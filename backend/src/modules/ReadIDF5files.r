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

# Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("rhdf5", "raster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
if(length(new.packages))
  {
  source("http://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
  }

## Load the packages
library(rhdf5)
library(raster)
library(stringr)
library(rgdal)
library(rgeos)


#HDF5_dir = file.path("..", "data", "BE", "ATMOSYS", "no2-gzip_WS.hdf5")
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
  
  h5write(h5f.active, HDF5_dir, as.character(7))
  
  h5createDataset(HDF5_dir, as.character(7), dims = C(1,1))
  H5close()
  
  h5f_WS$"7"$data[[1]]
  
  h5read(HDF5_dir, as.character(7))
  h5readAttributes(HDF5_dir, as.character(7))
  
  H5Fclose(h5f_WS)
  H5Fclose(h5f)
}



#pol = "no2"
#locationId.BE = LocationIDs.C1
#HOURS = HOURS.C1
#rm(pol, locationId.BE, HOURS)
ExtractExposureValue1 <- function(pol, locationId.BE, HOURS, ...) # Method 1: read from compressed file
{
  ## Open the IDF5-file
  polFile = paste0(pol, "-gzip.hdf5")
  h5f_in = file.path("..", "data", "BE", "ATMOSYS", polFile)
  
  EXPWS = list(list())
  
  if (class(locationId.BE)== "integer") #R & W
  {
    for (i in seq_along(locationId.BE)) # per individual
      #for (i in seq(1,2))
    {
      activeDataset = locationId.BE[i] / 10000
      activeLocation = locationId.BE[i] %% 10000
      activeName = activeDataset - (activeLocation/10000)
      
      h5f.active = h5read(h5f_in, as.character(activeName))
      
      EXP = HOURS[[i]] # use same structure
      
      for (h in seq_along(HOURS[[i]])) # per day
        #for (h in seq(1,3))
      {
        for (f in seq_along(HOURS[[i]][[h]])) # per hour
        {
          EXP[[h]][f] = h5f.active$data[HOURS[[i]][[h]][f]+1, activeLocation] #[hour of the year,activeLocation]
        }
      }
      EXPWS[[i]] = EXP
    }
  }
  
  if (class(locationId.BE)== "list") #C1 & C2
  {
    #for (i in seq_along(locationId.BE)) # per individual
    for (i in seq(1,2))
    {
      for (v in locationId.BE[i])
      {
        
        activeDataset = locationId.BE[[i]][v] / 10000
        activeLocation = locationId.BE[i] %% 10000
        activeName = activeDataset - (activeLocation/10000)
        
        h5f.active = h5read(h5f_in, as.character(activeName))
        
        EXP = HOURS[[i]] # use same structure
        
        #for (h in seq_along(HOURS[[i]])) # per day
        for (h in seq(1,3))
        {
          for (f in seq_along(HOURS[[i]][[h]])) # per hour
          {
            EXP[[h]][f] = h5f.active$data[HOURS[[i]][[h]][f]+1, activeLocation] #[hour of the year,activeLocation]
          }
        }
        EXPWS[[i]] = EXP
        
        
        
        
      }
      
      
      
    }
  }
  
  return (EXPWS)
}

ExtractExposureValue2 <- function(pol, locationId.BE, HOURS, ...) # Method 2: read from uncompressed (larger) file
{
  ## Open the IDF5-file
  polFile = paste0(pol, "-gzip.hdf5")
  h5f_in = file.path("..", "data", "BE", "ATMOSYS", polFile)
  
  activeDataset_WS = list(list())
  activeLocation_WS = list(list())
  activeName_WS = list(list())
  
  #Bool_00 = list(list())
  
  for (i in seq_along(locationId.BE))
  {
    activeDataset_WS[[i]] = locationId.BE[[i]] / 10000
    activeLocation_WS[[i]] = locationId.BE[[i]] %% 10000
    activeName_WS[[i]] = activeDataset_WS[[i]] - (activeLocation_WS[[i]]/10000)
  }
  
#   I.nr = NA
#   for (i in seq_along(locationId.BE)) # per individual
#   {
#     I.nr[i] = paste0("I_", sprintf("%04d", (i) ))
#   }
#   
#   Bool.nr = NA
#   for (n in seq(0,43)) # per slot (0-42)
#   {
#     Bool.nr[n] = paste0("Bool_", sprintf("%03d", n-1))
#   }
#   
#   Bools = list()
#   
#   for (i in seq_along(I.nr)) # create Boolian for active dataset
#   {
#     Bools[[i]] = data.frame(matrix(NA, nrow = length(activeName_WS[[i]]), ncol = 43))
#     colnames(Bools[[i]]) = Bool.nr
#     
#     for (n in seq_along(Bools[[i]])) # per slot (0-42)
#     {
#       Bools[[i]][n] = activeName_WS[[i]] == n-1
#     }
#   }
#   names(Bools) = I.nr
  

  ## copy Bools so that the exposure values can be fillen in
  ## every non-filled in slot will still be a Boolian (TRUE or FALSE)
  #EXP = Bools
  
  #h5f.active_WS = h5read(h5f_in, as.character(0)) # deze verwijderen | staat hier vanwege snelheid tests
  
  #EXPWS = list(list())
  
#   for (i in seq_along(activeName_WS)) # per individual
#   {
#     EXP = HOURS[[i]] # use same structure to fill in one pollutant value per hour
#     
#   }
  
  EXP = HOURS # use same structure to fill in one pollutant value per hour
  # filling values with NA
  for (i in seq_along(activeName_WS))
  {
    for (d in seq_along(HOURS[[i]])) # per vector
    {
      for (v in seq_along(activeName_WS[[i]])) # per day
      {
        EXP[[i]][[d]][v] = NA
      }
    }
  }
  
  #for (a in seq(0,42)) # per slot (0-42) [active location]
  for (a in seq(4,8))
  #for (a in 16)
  {
    h5f.active_WS = h5read(h5f_in, as.character(a))
    
    for (i in seq_along(activeName_WS)) # per individual
    #for (i in seq_along(1))
    {
      for (v in seq_along(activeName_WS[[i]])) # per vector
      {
        if (activeName_WS[[i]][v] == a)
        {
          for (d in seq_along(HOURS[[i]])) # per day
          #for (d in seq(1,3))
          {
            for (h in seq_along(HOURS[[i]][[d]])) # per hour
            {
              EXP[[i]][[d]][v] = h5f.active_WS$data[HOURS[[i]][[d]][h]+1, activeLocation_WS[[i]][v]] #[hour of the year,activeLocation]
            }
          }
          
        }
      }
      #EXPWS[[i]] = EXP
    }
  }
  
  return (EXP) 
}