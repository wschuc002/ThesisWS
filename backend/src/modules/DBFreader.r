# Module for reading intermediate DBF files.
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
# list.of.packages <- c("rhdf5", "raster")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# #if(length(new.packages)) install.packages(new.packages)
# if(length(new.packages)) source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

## Load the packages

DBFreader <- function(FileType, PhaseType, PPH.P, YearDates, Active.Subtype, ...)
{
  if (PhaseType == "Primary") {PhaseLetter = "P"}
  if (PhaseType == "Secondary") {PhaseLetter = "S"}
  if (PhaseType == "T1") {PhaseLetter = "T1"}
  if (PhaseType == "T2") {PhaseLetter = "T2"}
  
  if (FileType == "Exposure") {FileString = "EXP_"}
  if (FileType == "Time") {FileString = "TIME_"}
  
  NUM = rep(list(list()), length(PPH.P)) # use same structure
  for (i in seq_along(PPH.P)) # per individual
  #for (i in seq(1,2))
  {
    DF = read.dbf(file.path("..", "output", Active.Subtype, paste0(FileString, PhaseLetter, "_" , i, ".dbf")))
    
    for (d in seq_along(YearDates)) # per day
    {
      if (FileType == "Exposure")
      {
        NUM[[i]][[d]] = as.numeric(na.omit(as.numeric(DF[d,]))) # Exposure
      }else{
        NUM[[i]][[d]] = na.omit(as.POSIXct(as.numeric(DF[d,]), origin = "1970-01-01")) # TIME
      }
    }
    
    if (PhaseType != "Primary")
    {
      NUM[[i]][which(!(YearDates %in% BusinesDates))] = NA
    }
  } # closing i
 return(NUM)
}