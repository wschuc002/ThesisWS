# Module for temporal aggregation: all values to the whole hour.
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

# ## Check for required packages and install them (incl dependencies) if they are not installed yet.
# list.of.packages <- c("data.table")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
# ## Load the packages
# library(data.table)


WhichHourForWhichPoint <- function(PPH.P, Time, HOURS.P, HOURS.S, HOURS.T1, HOURS.T2,
                                   Print = FALSE, Active.Subprofile, ...)
{
  wP = list()
  wS = list()
  wT1 = list()
  wT2 = list()
  
  for (i in seq_along(PPH.P))
    #for (i in 1:2)  
  {
    P = list()
    S = list()
    T1 = list()
    T2 = list()
    for (h in seq_along(Time))
      #for (h in 1:(7*24))
    {
      day = ceiling(h/24)
      if (Print)
        {
          print(paste0("Year Hour ", h))
          print(paste0("Day ", day))
        }
      
      P[[h]] = which(h == HOURS.P[[i]][[day]])
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        S[[h]] = which(h == HOURS.S[[i]][[day]])
        T1[[h]] = which(h == HOURS.T1[[i]][[day]])
        T2[[h]] = which(h == HOURS.T2[[i]][[day]])
      }
    }
    wP[[i]] = P
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      wS[[i]] = S
      wT1[[i]] = T1
      wT2[[i]] = T2
    }
  }
  
  return(list(wP, wS, wT1, wT2))
}

ToHourValues <- function(PPH.P, Time, ExposureValue.P, ExposureValue.S, ExposureValue.T1, ExposureValue.T2,
                         TIME.P, TIME.S, TIME.T1, TIME.T2, wP, wS, wT1, wT2, ... )
{
  ExposureValueCombined = list()
  for (i in seq_along(PPH.P))
  {
    ExposureValueCombined[[i]] = 1:(length(Time))
    ExposureValueCombined[[i]][ExposureValueCombined[[i]] > 0] = NA
  }
  
  for (h in 1:length(Time))
  #for (h in (4*24+1):(5*24))
  {
    #print(paste0("Series Hour ", h))
    day = ceiling(h/24)

    for (i in seq_along(PPH.P))
    #for (i in 14)
    {
      #print(paste0("Individual ", i))
      
      
      if (length(wP[[i]][[h]]) > 0 & !length(wT1[[i]][[h]]) > 0 & !length(wT2[[i]][[h]]) > 0) # Only P, no hour overlap
      {
        ExposureValueCombined[[i]][h] = ExposureValue.P[[i]][[day]][wP[[i]][[h]]]
      }
      
      if (length(wS[[i]][[h]]) > 0 & !length(wT1[[i]][[h]]) > 0 & !length(wT2[[i]][[h]]) > 0) # Only S, no hour overlap
      {
        ExposureValueCombined[[i]][h] = ExposureValue.S[[i]][[day]][wS[[i]][[h]]]
      }
      
      if (length(wT1[[i]][[h]]) > 0 & length(wS[[i]][[h]]) == 0) # Only T1, no hour overlap
      {
        Mean.T1 = mean(ExposureValue.T1[[i]][[day]][wT1[[i]][[h]]])
        ExposureValueCombined[[i]][h] = Mean.T1
        rm(Mean.T1)
    }
      
      if (length(wT2[[i]][[h]]) > 0 & length(wP[[i]][[h]]) == 0) # Only T2, no hour overlap
      {
        Mean.T2 = mean(ExposureValue.T2[[i]][[day]][wT2[[i]][[h]]])
        ExposureValueCombined[[i]][h] = Mean.T2
        rm(Mean.T2)
      }
      
      
      if (length(wT1[[i]][[h]]) > 0 & length(wS[[i]][[h]]) == 1) # Hour overlap T1 & S wP[[i]][[h]]
      {
        EXP.part.T1_1 = ExposureValue.T1[[i]][[day]][wT1[[i]][[h]]][1]
        EXP.part.T1_ = ExposureValue.T1[[i]][[day]][wT1[[i]][[h]]][-(1:1)]
        EXP.part.T1 = c(EXP.part.T1_1,EXP.part.T1_)
        
        Weight.T1_1 = as.numeric(difftime(TIME.T1[[i]][[day]][wT1[[i]][[h]]][1],
                               floor_date(tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), 'hours'), units = "hours"))
        
        Weight.T1_ = NA
        for (e in 1:(length(TIME.T1[[i]][[day]][wT1[[i]][[h]]])-1))
        {
          Weight.T1_[e] = difftime(TIME.T1[[i]][[day]][wT1[[i]][[h]]][e+1], TIME.T1[[i]][[day]][wT1[[i]][[h]]][e], units = "hours")
        }
        Weight.T1 = c(Weight.T1_1, Weight.T1_)
        
        EXP.part.S = ExposureValue.S[[i]][[day]][wS[[i]][[h]]]
        Weight.S = as.numeric(difftime(TIME.S[[i]][[day]][1] , tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), units = 'hours')) / 60
        
        if (sum(Weight.T1, Weight.S) == 1) # should be 1
        {
          ExposureValueCombined[[i]][h] = sum(EXP.part.T1 * Weight.T1, EXP.part.S * Weight.S)
          
          rm(EXP.part.T1_1, EXP.part.T1_, EXP.part.T1, Weight.T1_1, Weight.T1_, Weight.T1, EXP.part.S, Weight.S)
        } else
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i))
        }
      }

      
      if (length(wT2[[i]][[h]]) > 0 & length(wP[[i]][[h]]) > 0) # Hour overlap T2 & P | TIME.P[[i]][[day]][wP[[i]][[h]]]
      {
        EXP.part.T2_1 = ExposureValue.T2[[i]][[day]][wT2[[i]][[h]]][1]
        EXP.part.T2_ = ExposureValue.T2[[i]][[day]][wT2[[i]][[h]]][-(1:1)]
        EXP.part.T2 = c(EXP.part.T2_1,EXP.part.T2_)
        
        Weight.T2_1 = as.numeric(difftime(TIME.T2[[i]][[day]][wT2[[i]][[h]]][1],
                                          floor_date(tail(TIME.T2[[i]][[day]][wT2[[i]][[h]]],1), 'hours'), units = "hours"))
        
        Weight.T2_ = NA
        for (e in 1:(length(TIME.T2[[i]][[day]][wT2[[i]][[h]]])-1))
        {
          Weight.T2_[e] = difftime(TIME.T2[[i]][[day]][wT2[[i]][[h]]][e+1], TIME.T2[[i]][[day]][wT2[[i]][[h]]][e], units = "hours")
        }
        Weight.T2 = c(Weight.T2_1, Weight.T2_)
        #sum(Weight.T2)
        
        EXP.part.P = ExposureValue.P[[i]][[day]][wP[[i]][[h]]]
        Weight.P_1 = as.numeric(difftime(TIME.P[[i]][[day]][wP[[i]][[h]]][1],
                                         tail(TIME.T2[[i]][[day]][wT2[[i]][[h]]],1), units = 'hours'))
        
        Weight.P_ = NA
        for (e in 1:(length(TIME.P[[i]][[day]][wP[[i]][[h]]])-1))
        {
          Weight.P_[e] = difftime(TIME.P[[i]][[day]][wP[[i]][[h]]][e+1], TIME.P[[i]][[day]][wP[[i]][[h]]][e], units = "hours")
        }
        Weight.P = c(Weight.P_1, Weight.P_)
        
        if (is.na(Weight.P_))
        {
          Weight.P = as.numeric(difftime(floor_date(TIME.P[[i]][[day]][wP[[i]][[h]]]+1*60**2, unit = 'hours'),
                                         TIME.P[[i]][[day]][wP[[i]][[h]]]))
        }
        
        if (sum(Weight.T2, Weight.P) == 1) # should be 1
        {
          ExposureValueCombined[[i]][h] = sum(EXP.part.T2 * Weight.T2, EXP.part.P * Weight.P)
          
          rm(EXP.part.T2_1, EXP.part.T2_, EXP.part.T2, Weight.T2_1, Weight.T2_, Weight.T2, EXP.part.P, Weight.P_1, Weight.P_, Weight.P)
        } else
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i))
        }
        
        
        #length(wT2[[i]][[h]]) > 0 & length(wP[[i]][[h]]) == 1
#         Mean.T2 = mean(ExposureValue.T2[[i]][[day]][wT2[[i]][[h]]])
#         Weight.T2 = as.numeric(tail(TIME.T2[[i]][[day]][wT2[[i]][[h]]],1) -
#                                  floor_date(tail(TIME.T2[[i]][[day]][wT2[[i]][[h]]],1), 'hours')) / 60
#         
#         Weight.P = as.numeric(TIME.P[[i]][[day]][wP[[i]][[h]]] - tail(TIME.T2[[i]][[day]][wT2[[i]][[h]]],1)) / 60
#         #Weight.T2 + Weight.P # should be 1
#         
#         ExposureValueCombined[[i]][h] = Mean.T2 * Weight.T2 + ExposureValue.P[[i]][[day]][wP[[i]][[h]]] * Weight.P
      }
      

      #! Hour overlap P and T1 and S
      if (length(wP[[i]][[h]]) == 1 & length(wT1[[i]][[h]]) > 0 & length(wS[[i]][[h]]) > 0) # Hour overlap T1 & P & S
      {
        EXP.part.P = ExposureValue.P[[i]][[day]][wP[[i]][[h]]]
        Weight.P = as.numeric(difftime(TIME.P[[i]][[day]][wP[[i]][[h]]],
                                       floor_date(tail(TIME.P[[i]][[day]][wP[[i]][[h]]],1), 'hours'), units = 'hours'))
        
        EXP.part.T1_1 = ExposureValue.T1[[i]][[day]][wT1[[i]][[h]]][1]
        EXP.part.T1_ = ExposureValue.T1[[i]][[day]][wT1[[i]][[h]]][-(1:1)]
        EXP.part.T1 = c(EXP.part.T1_1,EXP.part.T1_)
        
        Weight.T1_1 = as.numeric(abs(difftime(TIME.P[[i]][[day]][wP[[i]][[h]]],
                                          TIME.T1[[i]][[day]][wT1[[i]][[h]]][1], units = "hours")))
        
        Weight.T1_ = NA
        for (e in 1:(length(TIME.T1[[i]][[day]][wT1[[i]][[h]]])-1))
        {
          Weight.T1_[e] = difftime(TIME.T1[[i]][[day]][wT1[[i]][[h]]][e+1], TIME.T1[[i]][[day]][wT1[[i]][[h]]][e], units = "hours")
        }
        Weight.T1 = c(Weight.T1_1, Weight.T1_)
        
        EXP.part.S = ExposureValue.S[[i]][[day]][wS[[i]][[h]]]
        Weight.S_1 = as.numeric(abs(difftime(TIME.S[[i]][[day]][wS[[i]][[h]]][1],
                                             tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), units = "hours")))
        Weight.S_ = NA
        for (e in 1:(length(TIME.S[[i]][[day]][wS[[i]][[h]]])-1))
        {
          Weight.S_[e] = difftime(TIME.S[[i]][[day]][wS[[i]][[h]]][e+1], TIME.S[[i]][[day]][wS[[i]][[h]]][e], units = "hours")
        }
        Weight.S = c(Weight.S_1, Weight.S_)
        
        if (as.character(sum(Weight.P, Weight.T1, Weight.S)) == as.character(1)) # should be 1 # buggy with: (sum(Weight.P, Weight.T1, Weight.S) == 1)
        {
          ExposureValueCombined[[i]][h] = sum(EXP.part.P * Weight.P, EXP.part.T1 * Weight.T1, EXP.part.S * Weight.S)
          
          rm(EXP.part.P, Weight.P, EXP.part.T1_1, EXP.part.T1_, EXP.part.T1, Weight.T1_1, Weight.T1_, Weight.T1,
             EXP.part.S, Weight.S_1, Weight.S_, Weight.S)
        } else
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i))
        }

      }
      

    } # closing i
    
  } # closing h
  
  return(ExposureValueCombined)
}