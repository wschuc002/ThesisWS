# Module for creating large data frame structure from list structure
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
list.of.packages <- c("sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
#library(sp)

#TIME = Time
#ExposureValue = ExposureValueCombined

DF.Structure2 <- function(PPH.P, TIME.P, TIME, ExposureValue, ...)
{
  ClassTime = class(TIME)
  ClassExp = class(ExposureValue[[1]])
  
  ExposureValue.ul = list()
  TIME.ul = list()
  
  if (ClassExp == "list")
  {
    for (i in seq_along(PPH.P))
    {
      TIME.ul[[i]] = na.omit(unlist(TIME[[i]]))
      ExposureValue.ul[[i]] = na.omit(unlist(ExposureValue[[i]]))
      
      class(TIME.ul[[i]])
      class(ExposureValue.ul[[i]])
      
      
      # unlist
      TIME.ul[[i]] = unlist(TIME[[i]])
      ExposureValue.ul[[i]] = unlist(ExposureValue[[i]])
      
      # remove NAs
      TIME.ul[[i]] = TIME.ul[[i]][!is.na(ExposureValue.ul[[i]])]
      ExposureValue.ul[[i]] = ExposureValue.ul[[i]][!is.na(ExposureValue.ul[[i]])]
      
      # correction when not the whole year is used
      #TIME.ul[[i]] = TIME.ul[[i]][!is.na(ExposureValue.ul[[i]])]
      
    }
  }
  
  #length(ExposureValue.ul[[i]])
  #length(TIME.ul[[i]])
  #length(ExposureValue.ul[[i]]) - length(TIME.ul[[i]]) == 0
  
  # for (d in seq_along(ExposureValue[[i]]))
  # {
  #   #print(length(ExposureValue[[i]][[d]]))
  #   print(length(TIME[[i]][[d]]))
  # }
  
  
  ST.DF = list()
  
  for (i in seq_along(PPH.P))
  {
    print(i)
    if (all(ClassTime == "list" & ClassExp == "list"))
    {
      ST.DF[[i]] = data.frame(TIME.ul[[i]], ExposureValue.ul[[i]], i)
    }
    
    if (all(ClassTime == class(TIME.P[[1]][[1]])) & ClassExp == "list")
    {
      ST.DF[[i]] = data.frame(TIME, ExposureValue.ul[[i]], i)
    }
    
    if (all(ClassTime == class(TIME.P[[1]][[1]])) & ClassExp == "numeric")
    {
      ST.DF[[i]] = data.frame(TIME, ExposureValue[[i]], i)
    }
    
    colnames(ST.DF[[i]]) = c("TIME", "EXP", "IND")
    class(ST.DF[[i]]$TIME) = class(TIME.P[[1]][[1]])
    ST.DF[[i]] = ST.DF[[i]][!is.na(ST.DF[[i]]$EXP),]
  }
  ST.DF = do.call(rbind, ST.DF)
  
  # Give numbered row names
  #rownames(ST.DF) = 1:nrow(ST.DF)

  return(ST.DF)
}

# TIME = TIME.T1
# Hr.amount = 7*24
# ExposureValue = ExposureValue.T1
# WHICH = wT1

DF.Structure <- function(TIME, ExposureValue, IND.amount, Hr.amount, WHICH, ...)
{
  ST.WS = list()
  
  #for (i in seq(1,IND.amount))
  #for (i in 1:2)
  for (h in 1:Hr.amount)  
  {
    # Build the DF base
    ST.DF = NULL
    ST.DF = data.frame(unlist(TIME[[1]])[h])
    ST.DF = cbind(ST.DF, data.frame(NA), 1)
    colnames(ST.DF) = c("TIME", "EXP", "IND")
    
    day = ceiling(h/24)
    
    #for (h in 1:Hr.amount)
    #for (h in 1:(7*24))
    for (i in seq(1,IND.amount))
    #for (i in 1:2)
    {

      if (length(WHICH[[i]][[h]]) == 1)
      {
        ST.DF[i,]$TIME = TIME[[i]][[day]][WHICH[[i]][[h]]]
        ST.DF[i,]$IND = i
        ST.DF[i,]$EXP = ExposureValue[[i]][[day]][WHICH[[i]][[h]]]
      }
      
      if (length(WHICH[[i]][[h]]) > 1)
      {
        ST.DF_ = ST.DF
        for (l in 1:length(WHICH[[i]][[h]]))
        {
          #rowNR = i+l-1
          
          ST.DF_[l,]$TIME = TIME[[i]][[day]][WHICH[[i]][[h]]][l]
          ST.DF_[l,]$IND = i
          ST.DF_[l,]$EXP = ExposureValue[[i]][[day]][WHICH[[i]][[h]]][l]
        }
        ST.DF = rbind(ST.DF, ST.DF_)
      }
      
      class(ST.DF$TIME) = class(TIME.P[[1]][[1]])
    }
    class(ST.DF$TIME) = class(TIME.P[[1]][[1]])
    ST.WS[[h]] = ST.DF
  }
  ST.DF.WS = data.table::rbindlist(ST.WS)
  ST.DF.WS = ST.DF.WS[!is.na(ST.DF.WS$TIME)]
  ST.DF.WS = ST.DF.WS[!is.na(ST.DF.WS$EXP)]
  
  return(ST.DF.WS)
#   with (ST.DF.WS, plot(TIME, EXP, pch = "-", cex=1, col = Col.T1, ylim=c(0, E.max+20),
#                       xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
#                       main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(ExposureValue.P), "individuals")))
#   
  
  
#   #for (h in 1:(length(YearDates)*24))
#   for (h in 1:Hr.amount)
#   {
#     day = ceiling(h/24)
#     
#     
#     
#     #for (i in seq(1,IND.amount))
#     for (i in 1)
#     {
#         ST.DF[i,]$TIME = TIME[[i]][[day]][WHICH[[i]][[h]]]
#         ST.DF[i,]$EXP = ExposureValue[[i]][[day]][WHICH[[i]][[h]]]
#         ST.DF[i,]$IND = i
#         
#         #ST.DF = rbind(ST.DF, ST.DF[1,])
#         #ST.DF$EXP[i] = ExposureValue[[i]][h]
#         #ST.DF$IND[i] = i
#         
#         #ST.DF$TIME = unlist(TIME[[i]])[h]
#     }
#     class(ST.DF$TIME) = class(TIME.P[[1]][[1]])
#     ST.WS[[h]] = ST.DF
#   }
#   ST.DF.WS = data.table::rbindlist(ST.WS)
#   return(ST.DF.WS)
  
#   ST.WS = list()
#   #for (h in 1:(length(YearDates)*24))
#   for (h in 1:Hr.amount)
#   {
#     for (i in seq(1,IND.amount))
#     {
#       if (i == 1) # creating the base
#       {
#         ST.DF = NULL
#         ST.DF = data.frame(unlist(TIME[[i]])[h])
#         ST.DF = cbind(ST.DF, data.frame(ExposureValue[[i]][h]), 1)
#         colnames(ST.DF) = c("TIME", "EXP", "IND")
#       } else {
#         ST.DF = rbind(ST.DF, ST.DF[1,])
#         ST.DF$EXP[i] = ExposureValue[[i]][h]
#         ST.DF$IND[i] = i
#       }
#     }
#     class(ST.DF$TIME) = class(TIME.P[[1]][[1]])
#     ST.WS[[h]] = ST.DF
#   }
#   ST.DF.WS = data.table::rbindlist(ST.WS)
#   return(ST.DF.WS)
#}
  
  
}

DF.Stats <- function(ST.DF.WS, ...)
{
  # calculating mean, min and max and standard deviation
  mean.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), mean)))
  min.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), min)))
  max.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), max)))
  sd.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), sd)))
  
  stats.EXP = cbind(mean.EXP, min.EXP, max.EXP, sd.EXP)
  names(stats.EXP) = c("meanEXP", "minEXP", "maxEXP", "sdEXP")
  stats.EXP$TIME = rownames(stats.EXP)
  
  #class(stats.EXP$TIME) = class(TIME.P[[1]][[1]])
  
  return(stats.EXP)
}