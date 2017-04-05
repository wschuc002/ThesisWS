# Module for correcting TIME and ExposureValues for all days of the year
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

## Load the packages


IncludeWeekends <- function(PhaseType, INput, YearDates, BusinesDates, WeekendDates, ...)
{
  if (PhaseType == "Primary")
  {
    Wknd = AtPrimaryOrSecondary2("Primary", PHASES, WeekendDates, "Weekends") # list of lists
    AllDays = rep(list(list()), length(PPH.P)) # use same structure
  }else{
    Wknd = NA
    AllDays = rep(list(list()), length(PPH.P)) # use same structure
  }
  
  for (i in seq_along(PPH.P)) # per individual
  {
    if (PhaseType == "Primary")
    {
      AllDays[[i]][YearDates %in% WeekendDates] = Wknd[[i]]
      AllDays[[i]][YearDates %in% BusinesDates] = INput[[i]]
    }else{
      AllDays[[i]][YearDates %in% WeekendDates] = Wknd
      AllDays[[i]][YearDates %in% BusinesDates] = INput[[i]]
    }
  }
  return(AllDays)
}

NAWeekends <- function(INput, YearDates, BusinesDates, WeekendDates, ...)
{
  for (i in seq_along(INput)) # per individual
  {
    INput[[i]][YearDates %in% WeekendDates] = NA # change name to INput
  }
  return(INput)
}