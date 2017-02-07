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
list.of.packages <- c("rhdf5", "raster", "stringr", "rgdal", "rgeos")
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


#locationId.BE = LocationIDs.C1
#HOURS = HOURS.C1
#rm(pol, locationId.BE, HOURS)


ExtractExposureValue.Dynamic2 <- function(h5f_dir, locationId.BE, HOURS, ...) # Method 2: read from compressed file
{
  ## Open the IDF5-file
  h5f_in = h5f_dir
  
  
  activeDataset_WS = list(list())
  activeLocation_WS = list(list())
  activeName_WS = list(list())
  
  for (i in seq_along(locationId.BE))
  {
    activeDataset_WS[[i]] = locationId.BE[[i]] / 10000
    activeLocation_WS[[i]] = locationId.BE[[i]] %% 10000
    activeName_WS[[i]] = activeDataset_WS[[i]] - (activeLocation_WS[[i]]/10000)
  }
  
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
  
  for (a in seq(0,42)) # per slot (0-42) [active location]
  #for (a in seq(4,8))
  #for (a in 16)
  {
    h5f.active_WS = h5read(h5f_in, as.character(a))
    
    for (i in seq_along(activeName_WS)) # per individual
    #for (i in seq_along(1))
    {
      for (v in seq_along(activeName_WS[[i]])) # per vector
      {
        #if (activeName_WS[[i]][v] == a) # some bugs
        if (as.character(activeName_WS[[i]][v]) == as.character(a))
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


ExtractExposureValue.Dynamic3 <- function(h5f_dir, locationId.BE1, locationId.BE2, HOURS1, HOURS2, ...) # Method 3: read from compressed file, with Inwards and Outwards combined
{
  ## Open the IDF5-file
  h5f_in = h5f_dir
  
  activeDataset_WS1 = list(list())
  activeLocation_WS1 = list(list())
  activeName_WS1 = list(list())
  activeDataset_WS2 = list(list())
  activeLocation_WS2 = list(list())
  activeName_WS2 = list(list())
  
  for (i in seq_along(locationId.BE1))
  {
    activeDataset_WS1[[i]] = locationId.BE1[[i]] / 10000
    activeLocation_WS1[[i]] = locationId.BE1[[i]] %% 10000
    activeName_WS1[[i]] = activeDataset_WS1[[i]] - (activeLocation_WS1[[i]]/10000)
  }
  
  for (i in seq_along(locationId.BE2))
  {
    activeDataset_WS2[[i]] = locationId.BE2[[i]] / 10000
    activeLocation_WS2[[i]] = locationId.BE2[[i]] %% 10000
    activeName_WS2[[i]] = activeDataset_WS2[[i]] - (activeLocation_WS2[[i]]/10000)
  }
  
  EXP1 = HOURS1 # use same structure to fill in one pollutant value per hour
  EXP2 = HOURS2 # use same structure to fill in one pollutant value per hour
  # filling values with NA
  for (i in seq_along(activeName_WS1)) # per individual
  {
    for (d in seq_along(HOURS1[[i]])) # per vector
    {
      for (v in seq_along(activeName_WS1[[i]])) # per day
      {
        EXP1[[i]][[d]][v] = NA
      }
    }
    
    for (d in seq_along(HOURS2[[i]])) # per vector
    {
      for (v in seq_along(activeName_WS2[[i]])) # per day
      {
        EXP2[[i]][[d]][v] = NA
      }
    }
    
  }
  

  
  for (a in seq(0,42)) # per slot (0-42) [active location]
  #for (a in seq(4,8))
  #for (a in 16)
  {
    h5f.active_WS = h5read(h5f_in, as.character(a))
    
    
    for (i in seq_along(activeName_WS1)) # per individual
      #for (i in seq_along(1))
    {
      
      for (v in seq_along(activeName_WS1[[i]])) # per vector
      {
        if (as.character(activeName_WS1[[i]][v]) == as.character(a))
        {
          for (d in seq_along(HOURS1[[i]])) # per day
            #for (d in seq(1,3))
          {
            for (h in seq_along(HOURS1[[i]][[d]])) # per hour (Outwards)
            {
              EXP1[[i]][[d]][v] = h5f.active_WS$data[HOURS1[[i]][[d]][h]+1, activeLocation_WS1[[i]][v]] #[hour of the year,activeLocation]
            }
          }
        }
      }
      
      
      for (v in seq_along(activeName_WS2[[i]])) # per vector
      {
        if (as.character(activeName_WS2[[i]][v]) == as.character(a))
        {
          for (d in seq_along(HOURS2[[i]])) # per day
            #for (d in seq(1,3))
          {
            for (h in seq_along(HOURS2[[i]][[d]])) # per hour (Outwards)
            {
              EXP2[[i]][[d]][v] = h5f.active_WS$data[HOURS2[[i]][[d]][h]+1, activeLocation_WS2[[i]][v]] #[hour of the year,activeLocation]
            }
          }
        }
      }
      
      
    }
  }
  return (list(EXP1,EXP2))
}

ExtractExposureValue.Integral <- function(h5f_dir, locationId.BE1, locationId.BE2, HOURS1, HOURS2, ...) # Method 3: read from compressed file, with Inwards and Outwards combined
{
  ## Open the IDF5-file
  h5f_in = h5f_dir
  
  activeDataset_WS1 = list(list())
  activeLocation_WS1 = list(list())
  activeName_WS1 = list(list())
  activeDataset_WS2 = list(list())
  activeLocation_WS2 = list(list())
  activeName_WS2 = list(list())
  
  for (i in seq_along(locationId.BE1))
  {
    activeDataset_WS1[[i]] = locationId.BE1[[i]] / 10000
    activeLocation_WS1[[i]] = locationId.BE1[[i]] %% 10000
    activeName_WS1[[i]] = activeDataset_WS1[[i]] - (activeLocation_WS1[[i]]/10000)
  }
  
  for (i in seq_along(locationId.BE2))
  {
    activeDataset_WS2[[i]] = locationId.BE2[[i]] / 10000
    activeLocation_WS2[[i]] = locationId.BE2[[i]] %% 10000
    activeName_WS2[[i]] = activeDataset_WS2[[i]] - (activeLocation_WS2[[i]]/10000)
  }
  
  EXP1 = HOURS1 # use same structure to fill in one pollutant value per hour
  EXP2 = HOURS2 # use same structure to fill in one pollutant value per hour
  # filling values with NA
  for (i in seq_along(activeName_WS1)) # per individual
  {
    for (d in seq_along(HOURS1[[i]])) # per vector
    {
      for (v in seq_along(activeName_WS1[[i]])) # per day
      {
        EXP1[[i]][[d]][v] = NA
      }
    }
    
    for (d in seq_along(HOURS2[[i]])) # per vector
    {
      for (v in seq_along(activeName_WS2[[i]])) # per day
      {
        EXP2[[i]][[d]][v] = NA
      }
    }
    
  }
  
  
  
  for (a in seq(0,42)) # per slot (0-42) [active location]
    #for (a in seq(4,8))
    #for (a in 16)
  {
    h5f.active_WS = h5read(h5f_in, as.character(a))
    
    
    for (i in seq_along(activeName_WS1)) # per individual
      #for (i in seq_along(1))
    {
      
      for (v in seq_along(activeName_WS1[[i]])) # per vector
      {
        if (as.character(activeName_WS1[[i]][v]) == as.character(a))
        {
          for (d in seq_along(HOURS1[[i]])) # per day
            #for (d in seq(1,3))
          {
            for (h in seq_along(HOURS1[[i]][[d]])) # per hour (Outwards)
            {
              EXP1[[i]][[d]][v] = h5f.active_WS$data[HOURS1[[i]][[d]][h]+1, activeLocation_WS1[[i]][v]] #[hour of the year,activeLocation]
            }
          }
        }
      }
      
      
      for (v in seq_along(activeName_WS2[[i]])) # per vector
      {
        if (as.character(activeName_WS2[[i]][v]) == as.character(a))
        {
          for (d in seq_along(HOURS2[[i]])) # per day
            #for (d in seq(1,3))
          {
            for (h in seq_along(HOURS2[[i]][[d]])) # per hour (Outwards)
            {
              EXP2[[i]][[d]][v] = h5f.active_WS$data[HOURS2[[i]][[d]][h]+1, activeLocation_WS2[[i]][v]] #[hour of the year,activeLocation]
            }
          }
        }
      }
      
      
    }
  }
  return (list(EXP1,EXP2))
}


ExtractExposureValue.Static <- function(h5f_dir, locationId.BE, HOURS, ...) # Method 1: read from compressed file
{

  EXPWS = list()
  
  if (class(locationId.BE)== "integer") #R & W
  {
    #for (i in seq_along(locationId.BE)) # per individual
    for (i in seq(1,2))
    {
      activeDataset = locationId.BE[i] / 10000
      activeLocation = locationId.BE[i] %% 10000
      activeName = activeDataset - (activeLocation/10000)
      
      h5f.active = h5read(h5f_dir, as.character(activeName))
      
      EXP = HOURS[[i]] # use same structure
      
      for (d in seq_along(HOURS[[i]])) # per day
        #for (d in seq(1,3))
      {
        for (h in seq_along(HOURS[[i]][[d]])) # per hour
        {
          EXP[[d]][h] = h5f.active$data[HOURS[[i]][[d]][h]+1, activeLocation] #[hour of the year,activeLocation]
        }
      }
      EXPWS[[i]] = EXP
    }
  }
  return (EXPWS)
}

ExtractExposureValue.Static2 <- function(h5f_dir, locationId.BE, HOURS, ...) # Method 1: read from compressed file
{
  start.time = Sys.time()
  
  activeDataset_WS = NA
  activeLocation_WS = NA
  activeName_WS = NA
  
  for (i in seq_along(locationId.BE))
  {
    activeDataset_WS[i] = locationId.BE[i] / 10000
    activeLocation_WS[i] = locationId.BE[i] %% 10000
    activeName_WS[i] = activeDataset_WS[i] - (activeLocation_WS[i]/10000)
  }
  
  slots = seq(0,42)
  usedNames = slots %in% activeName_WS
  usedSlots = slots[usedNames]
  
  EXPWS = list()
  
  Temp_dir = file.path("..", "output", "temp")
  if (!dir.exists(Temp_dir)) 
  {
    dir.create(Temp_dir)
  }

  for (a in usedSlots)
  #for (a in seq(1,3))
  #for (a in head(usedSlots,3))
  {
    print(paste("Starting with slot", a, "of", length(usedSlots)))
    for (i in seq_along(locationId.BE)) # per individual
    #for (i in seq(1,4)) 
    {
      if (activeName_WS[i] == as.character(a))
      {
        File = file.path(Temp_dir, paste0(paste("DF",activeName_WS[i], activeLocation_WS[i], sep = "-"), ".dbf"))
        if (file.exists(File))
        {
          print(paste0(File, " exists. reading it..."))
          DF = read.dbf(File)
        }else{
          print(paste(File, "does not exist. reading h5f slot", a, "..."))
          h5f.active = h5read(h5f_dir, as.character(a))
          
          DF = data.frame(h5f.active$data[,activeLocation_WS[i]])
          SaveAsDBF2(DF, paste("DF", activeName_WS[i], activeLocation_WS[i], sep = "-"))
        }
        
        EXP = HOURS[[i]] # use same structure
        
        for (d in seq_along(HOURS[[i]])) # per day
          #for (d in seq(1,3))
        {
          for (h in seq_along(HOURS[[i]][[d]])) # per hour
          {
            EXP[[d]][h] = DF[HOURS[[i]][[d]][h]+1,]
          }
        }
        EXPWS[[i]] = EXP 
      }
    }
  }
  H5close()
  
  end.time = Sys.time()
  time.taken.m = difftime(end.time, start.time, units = "mins")
  time.taken.h = difftime(end.time, start.time, units = "hours")
  time.taken.s = difftime(end.time, start.time, units = "sec")
  print(paste("Duration of calculation:", time.taken.s, "seconds", "=", time.taken.m, "minutes", "=",time.taken.h, "hours"))
  return (EXPWS)
}