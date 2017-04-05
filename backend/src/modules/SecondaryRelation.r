# Module for putting the Secondary ExposureValues in the right place in a
# 1:many (Primary:Secondary) relation.
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

SecondaryRelation <- function(PPH.P, PPH.S, ExposureValue.S, ...)
{
  # TOEVOEGEN: Koppeling S aan P, zodat lenght(W)=lenght(R) | When there is a many:1 relation
  PPH.P@data$PSlink = PPH.P@data$koppeling
  PPH.S@data$PSlink = PPH.S@data$NR
  PPH.S@data$SNR = seq_along(PPH.S)
  
  PS = NA
  for (k in seq_along(PPH.P))
  {
    for (f in seq_along(PPH.S))
    {
      if (PPH.S@data$PSlink[f] == PPH.P@data$PSlink[k])
      {
        PS[k] = PPH.S@data$SNR[f]
      }
    }
  }
  
  for (i in seq_along(PS))
  {
    ExposureValue.S[[i]] = ExposureValue.S[[PS[i]]]
  }
  return(ExposureValue.S)
}