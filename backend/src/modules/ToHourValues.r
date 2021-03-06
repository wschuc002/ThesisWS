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

#DaySplit = SeqFragment[f]
#Time = Time.Sub

WhichHourForWhichPoint <- function(PPH.P, Time, HOURS.P, HOURS.S, HOURS.T1, HOURS.T2,
                                   Print = FALSE, Active.Subprofile, SeqFragment, f, ...)
{
  #if (!exists("DaySplit")) {DaySplit = 0}
  
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
    #for (h in seq_along(Time)+(SeqFragment[f]*24))
      #for (h in 1:(7*24))
    {
      dayS = ceiling(h/24)
      day = SeqFragment[f]+1
      hr = h+SeqFragment[f]*24
      if (Print)
        {
          print(paste0("Hour ", h))
          print(paste0("Day ", dayS))
          print(paste0("Year Hour ", hr))
          print(paste0("Year Day ", day))
        }
      
      P[[h]] = which(hr == HOURS.P[[i]][[dayS]])
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        S[[h]] = which(hr == HOURS.S[[i]][[dayS]])
        T1[[h]] = which(hr == HOURS.T1[[i]][[dayS]])
        T2[[h]] = which(hr == HOURS.T2[[i]][[dayS]])
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

# Time_ = Time
# Time = Time.Sub
# ExposureValue.P = ExposureValue.P_F
# ExposureValue.S = ExposureValue.S_F
# ExposureValue.T1 = ExposureValue.T1_F
# ExposureValue.T2 = ExposureValue.T2_F
# TIME.P = TIME.P_F
# TIME.S = TIME.S_F
# TIME.T1 = TIME.T1_F
# TIME.T2 = TIME.T2_F
# wP = HoP.P_F
# wS = HoP.S_F
# wT1 = HoP.T1_F
# wT2 = HoP.T2_F
# 
# Time = Time_

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
        # rm(Mean.T1)
      }
      
      if (length(wT2[[i]][[h]]) > 0 & length(wP[[i]][[h]]) == 0) # Only T2, no hour overlap
      {
        Mean.T2 = mean(ExposureValue.T2[[i]][[day]][wT2[[i]][[h]]])
        ExposureValueCombined[[i]][h] = Mean.T2
        # rm(Mean.T2)
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
          
          # if (exists("EXP.part.T1_1")) {rm(EXP.part.T1_1)}
          # if (exists("EXP.part.T1_")) {rm(EXP.part.T1_)}
          # if (exists("EXP.part.T1")) {rm(EXP.part.T1)}
          # if (exists("Weight.T1_1")) {rm(Weight.T1_1)}
          # if (exists("Weight.T1_")) {rm(Weight.T1_)}
          # if (exists("Weight.T1")) {rm(Weight.T1)}
          # if (exists("EXP.part.S")) {rm(EXP.part.S)}
          # if (exists("Weight.S")) {rm(Weight.S)}
          
        } else
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i))
        }
      }

      
      if (length(wT2[[i]][[h]]) > 0 & length(wP[[i]][[h]]) > 0) # Hour overlap T2 & P | 
      {
        if (length(wT2[[i]][[h]]) == 1)
        {
          EXP.part.T2 = ExposureValue.T2[[i]][[day]][wT2[[i]][[h]]]
          
          Weight.T2 = as.numeric(difftime(TIME.T2[[i]][[day]][wT2[[i]][[h]]][1],
                                            floor_date(tail(TIME.T2[[i]][[day]][wT2[[i]][[h]]],1), 'hours'), units = "hours"))
        } else
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
        }
        
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
        
        # write to vector
        ExposureValueCombined[[i]][h] = sum(EXP.part.T2 * Weight.T2, EXP.part.P * Weight.P)
        
        #is.wholenumber(sum(Weight.T2, Weight.P))
        #all.equal(sum(Weight.T2, Weight.P)+0.001, as.integer(1))
        
        #sum(Weight.T2, Weight.P) %in% 1
        #round(sum(Weight.T2, Weight.P), 5) != 1
        
        CHECK = as.character(sum(Weight.T2, Weight.P, na.rm = TRUE)) %in% as.character(1)
        if(!CHECK)
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i, "||",
                        "Weight.P:", Weight.P, "Weight.T2:", Weight.T2, "EXP.part.P:", EXP.part.P, "EXP.part.T2:", EXP.part.T2,
                        "SUM weights:", sum(Weight.T2, Weight.P), '\n'))
        }
        
        # remove intermediate results
        # if (exists("EXP.part.T2_1")) {rm(EXP.part.T2_1)}
        # if (exists("EXP.part.T2_")) {rm(EXP.part.T2_)}
        # if (exists("EXP.part.T2")) {rm(EXP.part.T2)}
        # if (exists("Weight.T2_1")) {rm(Weight.T2_1)}
        # if (exists("Weight.T2_")) {rm(Weight.T2_)}
        # if (exists("Weight.T2")) {rm(Weight.T2)}
        # if (exists("EXP.part.P")) {rm(EXP.part.P)}
        # if (exists("Weight.P_1")) {rm(Weight.P_1)}
        # if (exists("Weight.P_")) {rm(Weight.P_)}
        # if (exists("Weight.P")) {rm(Weight.P)}
      }
      

      # Hour overlap P and T1 and S
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
        

        if (as.character(sum(Weight.P, Weight.T1, Weight.S, na.rm = TRUE)) == as.character(1)) # should be 1 # buggy with: (sum(Weight.P, Weight.T1, Weight.S) == 1)
        {
          ExposureValueCombined[[i]][h] = sum(EXP.part.P * Weight.P, EXP.part.T1 * Weight.T1, EXP.part.S * Weight.S)
          
          # rm(EXP.part.P, Weight.P, EXP.part.T1_1, EXP.part.T1_, EXP.part.T1, Weight.T1_1, Weight.T1_, Weight.T1,
          #    EXP.part.S, Weight.S_1, Weight.S_, Weight.S)
        } else
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i))
        }

      }
      
      # Hour overlap T1 and S
      if (length(wT1[[i]][[h]]) > 0 & length(wS[[i]][[h]]) > 0 & length(wP[[i]][[h]]) == 0 & length(wT2[[i]][[h]]) == 0)
      {
        #T1
        EXP.part.T1 = ExposureValue.T1[[i]][[day]][wT1[[i]][[h]]]
        
        if (length(wT1[[i]][[h]]) == 1)
        {
          Weight.T1 = as.numeric(difftime(TIME.T1[[i]][[day]][wT1[[i]][[h]]][1],
                                          floor_date(tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), 'hours'), units = "hours"))
        } else
        {
          Weight.T1_1 = as.numeric(difftime(TIME.T1[[i]][[day]][wT1[[i]][[h]]][1],
                                            floor_date(tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), 'hours'), units = "hours"))
          
          Weight.T1_ = NA
          for (e in 1:(length(TIME.T1[[i]][[day]][wT1[[i]][[h]]])-1))
          {
            Weight.T1_[e] = difftime(TIME.T1[[i]][[day]][wT1[[i]][[h]]][e+1], TIME.T1[[i]][[day]][wT1[[i]][[h]]][e], units = "hours")
          }
          
          #Weight.T1_ = as.numeric(diff(TIME.T1[[i]][[day]][wT1[[i]][[h]]])) / 60**2
          Weight.T1 = c(Weight.T1_1, Weight.T1_)
          #sum(Weight.T1)
        }
        
        #S
        EXP.part.S = ExposureValue.S[[i]][[day]][wS[[i]][[h]]]
        
        if (length(wS[[i]][[h]]) == 1)
        {
          Weight.S = as.numeric(difftime(TIME.S[[i]][[day]][wS[[i]][[h]]],
                                           tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), units = 'hours'))
        } else
        {
          Weight.S_1 = as.numeric(difftime(TIME.S[[i]][[day]][wS[[i]][[h]]][1],
                                           tail(TIME.T1[[i]][[day]][wT1[[i]][[h]]],1), units = 'hours'))
          
          Weight.S_ = NA
          for (e in 1:(length(TIME.S[[i]][[day]][wS[[i]][[h]]])-1))
          {
            Weight.S_[e] = difftime(TIME.S[[i]][[day]][wS[[i]][[h]]][e+1], TIME.S[[i]][[day]][wS[[i]][[h]]][e], units = "hours")
          }
          #Diff = diff(TIME.S[[i]][[day]][wS[[i]][[h]]], units = "hours")
          #Weight.S_ = as.numeric(diff(TIME.S[[i]][[day]][wS[[i]][[h]]])) / 60**2 # unstable
          Weight.S = c(Weight.S_1, Weight.S_)
        }
        
        # write to vector
        ExposureValueCombined[[i]][h] = sum(EXP.part.T1 * Weight.T1, EXP.part.S * Weight.S)
        
        CHECK = as.character(sum(Weight.T1, Weight.S, na.rm = TRUE)) %in% as.character(1)
        if(!CHECK)
        {
          stop(paste("Weights do not add up to 1.", "Hour:", h, "| Individual:", i, "||",
                     "Weight.T1:", Weight.T1, "Weight.S:", Weight.S, "EXP.part.T1:", EXP.part.T1, "EXP.part.S:", EXP.part.S,
                     "SUM weights:", sum(Weight.T1, Weight.S), '\n'))
        }
        
        # # remove intermediate results
        # rm(EXP.part.T1_1, EXP.part.T1_, EXP.part.T1, Weight.T1_1, Weight.T1_, Weight.T1, EXP.part.S, Weight.S_1, Weight.S_, Weight.S)
        # 
      }

    } # closing i
    
    # # Check if ExposureValueCombined has the correct amount of values
    # if (length(unlist(ExposureValueCombined[[i]])) != length(Time))
    # {
    #   stop(paste())
    # }
    
    
  } # closing h
  
  return(ExposureValueCombined)
}

# Time_ = Time
# Time = Time.Sub
# ST.DF.P = ST.DF.P_F
# ST.DF.S = ST.DF.S_F
# ST.DF.T1 = ST.DF.T1_F
# ST.DF.T2 = ST.DF.T2_F
# ST.DF.P_2 = ST.DF.P_F_2
# ST.DF.S_2 = ST.DF.S_F_2
# ST.DF.T1_2 = ST.DF.T1_F_2
# ST.DF.T2_2 = ST.DF.T2_F_2
# TIME.P = TIME.P_F
# TIME.S = TIME.S_F
# TIME.T1 = TIME.T1_F
# TIME.T2 = TIME.T2_F
# wP = HoP.P_F
# wS = HoP.S_F
# wT1 = HoP.T1_F
# wT2 = HoP.T2_F
# 
# Time = Time_

ToHourValuesFromDF.Dynamic <- function(PPH.P, Time.Sub, output.dir, FolderName,
                               #ST.DF.P, ST.DF.S, ST.DF.T1, ST.DF.T2,
                               #ST.DF.P_2, ST.DF.S_2, ST.DF.T1_2, ST.DF.T2_2,
                         TIME.P, TIME.S, TIME.T1, TIME.T2, pollutants, ...)
{
  Columns = 6 + length(pollutants)
  testDF = transpose(data.frame(1:Columns, stringsAsFactors = FALSE))
  colnames(testDF) = c("TIME", "IND", paste0("EXP_", toupper(pollutants)) , "Pweight", "Sweight", "T1weight", "T2weight")
  testDF[1,] = NA
  
  class(testDF[,1]) = class(Time)
  class(testDF[,2]) = "numeric"
  class(testDF[,3:(2+length(pollutants))]) = "integer"
  class(testDF[,(3+length(pollutants)):Columns]) = "integer"
  
  HR_DF.Li = list()
  HR_DF.Li_2 = list()
  
  for (pol in pollutants)
  {
    print(paste(pol))
    cat("\n")
    
    PolNR = which(pollutants %in% pol)
    
    ST.DF.P = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_P_", toupper(pol), ".dbf")))
    ST.DF.S = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_S_", toupper(pol), ".dbf")))
    ST.DF.T1 = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T1_", toupper(pol), ".dbf")))
    ST.DF.T2 = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_T2_", toupper(pol), ".dbf")))
    
    # add location phase
    ST.DF.P$LocationPhase = "P"
    ST.DF.S$LocationPhase = "S"
    ST.DF.T1$LocationPhase = "T1"
    ST.DF.T2$LocationPhase = "T2"
    
    for (i in as.numeric(rownames(PPH.P@data)))
    {
      cat(paste(i," "))
      
      ST.DF.P[ST.DF.P$IND == i,]$TIME = unlist(TIME.P[[i]])
      ST.DF.S[ST.DF.S$IND == i,]$TIME = unlist(TIME.S[[i]])[!is.na(unlist(TIME.S[[i]]))]
      ST.DF.T1[ST.DF.T1$IND == i,]$TIME = unlist(TIME.T1[[i]])[!is.na(unlist(TIME.T1[[i]]))]
      ST.DF.T2[ST.DF.T2$IND == i,]$TIME = unlist(TIME.T2[[i]])[!is.na(unlist(TIME.T2[[i]]))]
      
      # collect per individual
      ST.DF = rbind(ST.DF.P[i == ST.DF.P$IND,], ST.DF.S[i == ST.DF.S$IND,], ST.DF.T1[i == ST.DF.T1$IND,], ST.DF.T2[i == ST.DF.T2$IND,])
      ST.DF = ST.DF[order(ST.DF$TIME),]
      
      # Time class
      class(ST.DF$TIME) = class(YearDates)
      
      if (PolNR == 1)
      {
        # fix P S T1 S T2 P order
        for (dup in which(duplicated(ST.DF$TIME)))
        {
          DUP1 = ST.DF[dup-1,]
          DUP2 = ST.DF[dup,]
          
          ST.DF[dup,] = DUP1
          ST.DF[dup-1,] = DUP2
        }
         
        ST.DF$Weight = NA
        ST.DF[1,]$Weight = 1
        for (t in (2:nrow(ST.DF)))
        {
          ST.DF[t,]$Weight = as.numeric(difftime(ST.DF[t,]$TIME, ST.DF[t-1,]$TIME, units = "hours"))
        }
        
        for (h in 1:length(Time.Sub))
        {
          PeriodOfInterestDF = ST.DF[ST.DF$TIME > Time.Sub[h]-(1*60**2) & ST.DF$TIME <= Time.Sub[h],]
          
          if (as.character(sum(PeriodOfInterestDF$Weight)) != 1)
          {
            if (all(as.character(sum(ST.DF[ST.DF$TIME > Time.Sub[h]-(1*60**2) & ST.DF$TIME <= Time.Sub[h+2],]$Weight)) != 3,
                    (as.character(sum(ST.DF[ST.DF$TIME > Time.Sub[h]-(2*60**2) & ST.DF$TIME <= Time.Sub[h+1],]$Weight)) != 3)))
            {
              stop(print("Inconsistent weights"))
            }
          }
          
          testDF[h,]$TIME = Time.Sub[h]
          testDF[h,3] = mean(PeriodOfInterestDF$EXP)
          
          testDF[h,]$Pweight = sum(PeriodOfInterestDF[PeriodOfInterestDF$LocationPhase == "P",]$Weight)
          testDF[h,]$Sweight = sum(PeriodOfInterestDF[PeriodOfInterestDF$LocationPhase == "S",]$Weight)
          testDF[h,]$T1weight = sum(PeriodOfInterestDF[PeriodOfInterestDF$LocationPhase == "T1",]$Weight)
          testDF[h,]$T2weight = sum(PeriodOfInterestDF[PeriodOfInterestDF$LocationPhase == "T2",]$Weight)
        } # closing h
        
        seqWS = seq_along(Time.Sub)+length(Time.Sub)*(i-1)
        rownames(testDF) = as.character(seqWS)
        
        testDF$IND = i
        
        HR_DF.Li[[i]] = testDF
      } # closing PolNR == 1
      
      if (PolNR != 1)
      {
        EXP2 = NA
        for (h in 1:length(Time.Sub))
        {
          EXP2[h] = mean(ST.DF[ST.DF$TIME > Time.Sub[h]-(1*60**2) & ST.DF$TIME <= Time.Sub[h],]$EXP)
        }
        HR_DF.Li_2[[i]] = EXP2
      }
      
    } # closing i
    cat("\n")
    
    if (PolNR != 1)
    {
      HR_DF = do.call(rbind, HR_DF.Li)
    }

    if (PolNR != 1)
    {
      HR_DF[,paste0("EXP_", toupper(pol))] = unlist(HR_DF.Li_2)
    }
    
  } # closing pol
  

  return(HR_DF)
}

ToHourValuesFromDF.Static <- function(PPH.P, output.dir, FolderName, TIME.P, pollutants, ...)
{
  Columns = 2 + length(pollutants)
  testDF = transpose(data.frame(1:Columns, stringsAsFactors = FALSE))
  colnames(testDF) = c("TIME", "IND", paste0("EXP_", toupper(pollutants)))
  testDF[1,] = NA
  
  class(testDF[,1]) = class(Time)
  class(testDF[,2]) = "numeric"
  class(testDF[,3:(2+length(pollutants))]) = "integer"
  
  HR_DF.Li = list()
  HR_DF.Li_2 = list()
  
  for (pol in pollutants)
  {
    print(paste(pol))
    cat("\n")
    
    PolNR = which(pollutants %in% pol)
    
    ST.DF.P = read.dbf(file = file.path(output.dir, FolderName, paste0("DF_P_", toupper(pol), ".dbf")))
    
    # add location phase
    ST.DF.P$LocationPhase = "P"
    
    colnames(ST.DF.P)[colnames(ST.DF.P) == "EXP"] = paste0("EXP_", toupper(pol))
    
    for (i in as.numeric(rownames(PPH.P@data)))
    {
      cat(paste(i," "))
      
      if (!all(ST.DF.P[ST.DF.P$IND == i,]$TIME == unlist(TIME.P[[i]])))
      {
        ST.DF.P[ST.DF.P$IND == i,]$TIME = unlist(TIME.P[[i]])
        
        # collect per individual
        ST.DF = ST.DF.P[i == ST.DF.P$IND,]
        ST.DF = ST.DF[order(ST.DF$TIME),]
      } else
      {
        ST.DF = ST.DF.P
      }
    
      # Time class
      class(ST.DF$TIME) = class(YearDates)
      
    } # closing i
    
    HR_DF.Li[[PolNR]] = ST.DF.P
    
  } # closing pol

  HR_DF = do.call(cbind, HR_DF.Li)
  
  if (all(unlist(HR_DF[,c(1,2,4)]) == unlist(HR_DF[,c(5,6,8)])))
  {
    HR_DF2 = HR_DF[, c("TIME", "IND", paste0("EXP_", toupper(pollutants)))]
  } else
  {
    stop(print("Inconsistent cbind"))
  }
  
  return(HR_DF2)
}