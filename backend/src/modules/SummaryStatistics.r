# Module for calculating various summary statistics.
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
list.of.packages <- c("corrplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(corrplot)


CorPlotTable <- function(GroupName, CorType, WS1, WS2, C1 = -9999, C2 = -9999, ...)
{
  if (all(C1 == -9999 & C2 == -9999))
  {
    collect = data.frame(cbind(WS1, WS2))
  } else
  {
    collect = data.frame(cbind(WS1, WS2, C1, C2))
  }
  
  C = cor(collect)
  C_squared = C**2
  
  if (CorType == "Ellipse")
  {
    corrplot(C, method="ellipse", type = "upper",
             mar = c(1, 1, 1, 1))
  }
  
  if (CorType == "Numbers")
  {
    corrplot(C_squared, method="number", number.digits = 5, title = paste(GroupName, "R²"), #type = "upper"
             umber.cex = 1, mar = c(1, 0, 1, 0), tl.srt = 90)
  }
}

#Profile = Active.Type
#IND.amount = length(PPH.P)
#PlotMinMax = FALSE
#DAY.start = 5
#DAYS = 1 #length(YearDates)-1

Plot.Group2 <- function(Profile, DAY.start, DAYS, IND.amount, PlotMinMax, ST.DF.P, ST.DF.S, ST.DF.T1, ST.DF.T2,
                        stats.EXP.P, stats.EXP.S, stats.EXP.T1, stats.EXP.T2, ...)
{
  if (IND.amount > length(ExposureValue.P))
  {
    print(paste0("Warning: The amount of individuals to be plotted is higher than available in the data set. This
                 amount is reduced to this number:", length(ExposureValue.P)))
    IND.amount = length(ExposureValue.P)
  }
  
  
  ST.DF.HR = OW_WS1.ST.DF.HR
  
  
  ST.DF.P.sub = ST.DF.P[ST.DF.P$TIME > YearDates[DAY.start] & ST.DF.P$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.S.sub = ST.DF.S[ST.DF.S$TIME > YearDates[DAY.start] & ST.DF.S$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.T1.sub = ST.DF.T1[ST.DF.T1$TIME > YearDates[DAY.start] & ST.DF.T1$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.T2.sub = ST.DF.T2[ST.DF.T2$TIME > YearDates[DAY.start] & ST.DF.T2$TIME <= YearDates[DAY.start+DAYS],]
  
  stats.EXP.P.sub = DF.Stats(ST.DF.P.sub)
  stats.EXP.S.sub = DF.Stats(ST.DF.S.sub)
  stats.EXP.T1.sub = DF.Stats(ST.DF.T1.sub)
  stats.EXP.T2.sub = DF.Stats(ST.DF.T2.sub)
  
  
  ST.DF.HR.sub = ST.DF.HR[ST.DF.HR$TIME > YearDates[DAY.start] & ST.DF.HR$TIME <= YearDates[DAY.start+DAYS],]
  stats.EXP.HR.sub = DF.Stats(ST.DF.HR.sub)
  
  #E.max = max(stats.EXP.P.sub$maxEXP, na.rm = T)
  E.max = max(c(stats.EXP.P.sub$maxEXP, stats.EXP.S.sub$maxEXP, stats.EXP.T1.sub$maxEXP, stats.EXP.T2.sub$maxEXP), na.rm = T)
  E.max = max(stats.EXP.HR.sub$maxEXP, na.rm = T)
  E.max = 100
  
  Col.P = rgb(red=0, green=0.5, blue=0.5, alpha=0.2)
  Col.S = rgb(red=1, green=0.2, blue=0.5, alpha=0.2)
  Col.T1 = rgb(red=0.5, green=0.2, blue=0.5, alpha=0.2)
  Col.T2 = rgb(red=1, green=0.2, blue=0.2, alpha=0.2)
  Col.HR = rgb(red=0.6, green=0.2, blue=0.2, alpha=0.2)
  
  # point plot with transparency in color
  with (ST.DF.P.sub, plot(TIME, EXP, pch = "-", cex=1, col = Col.P, ylim=c(0, E.max+20),
                        xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                        main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(ExposureValue.P), "individuals")))
  
  with (ST.DF.HR.sub, plot(TIME, EXP, pch = "-", cex=1, col = Col.P, ylim=c(0, E.max+20),
                      xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                      main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(PPH.P), "individuals")))
  
  with (OW_C2.ST.DF.HR, plot(TIME, EXP, pch = ".", cex=1, col = Col.HR, ylim=c(0, E.max+20),
                            xlab = "Time", ylab = paste(toupper(pol), "Biweekly concentration (µg/m³)"),
                            main = paste(Active.Subprofile$Type, ":", IND.amount, "out of", length(ExposureValue.P), "individuals")))
  
  
  with (ST.DF.S.sub, points(TIME, EXP, pch = "-", cex=1, col = Col.S))
  with (ST.DF.T1.sub, points(TIME, EXP, pch = ".", cex=1, col = Col.T1))
  with (ST.DF.T2.sub, points(TIME, EXP, pch = ".", cex=1, col = Col.T2))
  
  mtext(paste(head(ST.DF.P.sub$TIME,1), "-", tail(ST.DF.P.sub$TIME,1) + 0.001))
  mtext(paste(head(ST.DF.HR.sub$TIME,1), "-", tail(ST.DF.HR.sub$TIME,1) + 0.001))

  #add mean, min and max to plot
  points(as.POSIXct(stats.EXP.P$TIME), stats.EXP.P$meanEXP, col = "orange", pch = "-", cex = 1)
  points(as.POSIXct(stats.EXP.HR$TIME), stats.EXP.HR$meanEXP, col = "orange", pch = "-", cex = 1)
  
  if (PlotMinMax == TRUE)
  {
    points(as.POSIXct(stats.EXP.P$TIME), stats.EXP.P$minEXP, col = "white", pch = 24, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.75)
    points(as.POSIXct(stats.EXP.P$TIME), stats.EXP.P$maxEXP, col = "white", pch = 25, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.75)
  }
  
  legend("top", c("Individual exposure value","Mean"), xpd = FALSE, horiz = TRUE, inset = c(0,0),
         bty = "n", pch = c("-","-"), col = c(Col.P,"orange"), cex = 1)
  
  
  #abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01"), col = "orange") # start summertime / Daylight saving time (DST)
  #abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01"), col = "grey") # start wintertime / Standard time
  abline(v = as.POSIXct(unlist(PHASES[[1]]), origin = "1970-01-01"), col = Col.T2)
  
  #abline(v = Time, col = "grey") # hours
  abline(v = YearDates, col = "grey") # days
  
  abline(v = unlist(PHASES[[1]][YearDates %in% BusinesDates][2]), col = Col.T1) # T1
  

  Sel = which(YearDates %in% BusinesDates) < DAY.start+DAYS & which(YearDates %in% BusinesDates) >= DAY.start
  for (h in (PHASES[[1]][YearDates %in% BusinesDates][Sel]))
  {
    abline(v = h[2], col = "red") # Leave Primary
    abline(v = h[4], col = "blue") # Leave Secondary
  }
  
}

Weighted.Dynamic <- function(ExposureValue.C, WEIGHTS.C, CalcType, ...)
{
  EXP = list()
  Exp = NA
  
  for (i in seq_along(ExposureValue.C))
  {
    for (d in seq_along(ExposureValue.C[[i]]))
    {
      
      if (CalcType == "WeightedMean")
      {
        Exp[d] = sum(ExposureValue.C[[i]][[d]] * WEIGHTS.C[[i]])
      }
      
    }
    EXP[[i]] = Exp
  }
  return(EXP)
}

Weighted.Static <- function(ExposureValue, CalcType, ...)
{
  EXP = list()
  Exp = NA
  
  for (i in seq_along(ExposureValue))
  {
    for (d in seq_along(ExposureValue[[i]]))
    {
      
      if (CalcType == "WeightedMean.Day")
      {
        Exp[d] = mean(ExposureValue[[i]][[d]])
      }
      
      if (CalcType == "CumSum.Day")
      {
        Exp[d] = cumsum(ExposureValue[[i]][[d]])
      }
      
      if (CalcType == "WeightedMean.Ind")
      {
        Exp[i] = mean(ExposureValue[[i]][[d]])
      }
      
    }
    EXP[[i]] = Exp
  }
  return(EXP)
}

Plot.PersonalExposureGraph <- function(IND, DAY, DAYS, ...)
{
  if (Active.Profile$Dynamics == "static")
  {
    if (length(TIME.P[[IND]][[DAY]]) == length(ExposureValue.P[[IND]][[DAY]]))
    {
      R.T = as.POSIXct(unlist(TIME.P[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
      R.E = unlist(ExposureValue.P[[IND]][seq(DAY, DAY+DAYS, by = 1)])
      E.max = max(R.E, na.rm = TRUE)
      
      plot(main = paste(Active.Type, "Individual", IND), x = R.T, y = R.E, col = ifelse(R.E == 0,'red','darkgreen'),
           ylim=c(0, E.max+10), xlab = "Time", ylab = paste(pol, "concentration (µg/m³)"), pch = 19)
      
      mtext(paste(R.T[1]+0.01, "-", tail(R.T,1)+0.01)) # day: Mo.... Th...
      
      legend("topleft", "Primary (residence)", col="darkgreen", pch = 19, box.lwd = 0, box.col = "transparent", bg="transparent", inset = c(0.00,0.005))
      
      abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET"), col = "orange") # start summertime / Daylight saving time (DST)
      abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET"), col = "grey") # start wintertime / Standard time
      
    } else
    {
      stop(paste("The TIME and ExposureValue data do not match lengths: fix bug first."))
    }
  }
  
  if (Active.Profile$Dynamics == "dynamic")
  {
    # check for same length of corresponding inputs
    if (length(TIME.S[[IND]][[DAY]]) == length(ExposureValue.S[[IND]][[DAY]]) &
        length(TIMEVertex.T1[[IND]][[DAY]]) == length(ExposureValue.T1[[IND]][[DAY]]) &
        length(TIMEVertex.T2[[IND]][[DAY]]) == length(ExposureValue.T2[[IND]][[DAY]]))
    {
      
      R.T = as.POSIXct(unlist(TIME.P[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
      W.T = as.POSIXct(unlist(TIME.S[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
      C1.T = as.POSIXct(unlist(TIMEVertex.T1[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
      C2.T = as.POSIXct(unlist(TIMEVertex.T2[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
      
      R.E = unlist(ExposureValue.P[[IND]][seq(DAY, DAY+DAYS, by = 1)])
      W.E = unlist(ExposureValue.S[[IND]][seq(DAY, DAY+DAYS, by = 1)])
      C1.E = unlist(ExposureValue.T1[[IND]][seq(DAY, DAY+DAYS, by = 1)])
      C2.E = unlist(ExposureValue.T2[[IND]][seq(DAY, DAY+DAYS, by = 1)])
      
      E.max = max(c(R.E, W.E, C1.E, C2.E), na.rm = TRUE)
      
      plot(main = paste(Active.Type, "Individual", IND), x = R.T, y = R.E, col = ifelse(R.E == 0,'red','darkgreen'),
           ylim=c(0, E.max+10), xlab = "Time", ylab = paste(pol, "concentration (µg/m³)"), pch = 19)
      
      points(x = W.T, y = W.E, col = "orange", pch = 19)
      points(x = C1.T, y = C1.E, col = "darkgrey", pch = 1)
      points(x = C2.T, y = C2.E, col = "darkgrey", pch = 19)
      
      legend("topleft", "Secondary", col="orange", pch = 19, box.lwd = 0, box.col = "transparent",bg="transparent", inset = c(0.275,0.005))
      legend("topright", "Transport Outwards", col="darkgrey", pch = 1, box.lwd = 0, box.col = "transparent", bg="transparent", inset = c(0.25,0.005))
      legend("topright", "Transport Inwards", col="darkgrey", pch = 19, box.lwd = 0, box.col = "transparent", bg="transparent", inset = c(0.00,0.005))
      
      abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET"), col = "orange") # start summertime / Daylight saving time (DST)
      abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET"), col = "grey") # start wintertime / Standard time
      
    } else
    {
      stop(paste("The TIME and ExposureValue data do not match lengths: fix bug first."))
    }
  }
}

Plot.Group <- function(Profile, DAY.start, DAYS, IND.amount, PlotMinMax, ...)
{
  if (IND.amount > length(ExposureValue.P))
  {
    print(paste0("Warning: The amount of individuals to be plotted is higher than available in the data set. This
                 amount is reduced to this number:", length(ExposureValue.P)))
    IND.amount = length(ExposureValue.P)
  }
  
  ST.WSS = list()
  for (d in seq(DAY.start,DAYS))
  {
    ST.WS = list()
    for (h in seq_along(TIME.P[[1]][[d]]))
    {
      for (i in seq(1,IND.amount))
      {
        if (i == 1) # creating the base
        {
          ST.DF = NULL
          ST.DF = data.frame(TIME.P[[i]][[d]][h])
          ST.DF = cbind(ST.DF, data.frame(ExposureValue.P[[i]][[d]][h]), 1)
          colnames(ST.DF) = c("TIME", "EXP", "IND")
        } else {
          ST.DF = rbind(ST.DF, ST.DF[1,])
          ST.DF$EXP[i] = ExposureValue.P[[i]][[d]][h]
          ST.DF$IND[i] = i
        }
      }
      ST.WS[[h]] = ST.DF
    }
    ST.DF.WS = data.table::rbindlist(ST.WS)
    ST.WSS[[d]] = ST.DF.WS
  }
  ST.DF.WSS = data.table::rbindlist(ST.WSS)
  
  WS.col = rgb(red=0, green=0.5, blue=0.5, alpha=0.2)
  
  
  # calculating mean, min and max and standard deviation
  mean.EXP = data.frame (with (ST.DF.WSS, tapply(EXP, factor(TIME), mean)))
  min.EXP = data.frame (with (ST.DF.WSS, tapply(EXP, factor(TIME), min)))
  max.EXP = data.frame (with (ST.DF.WSS, tapply(EXP, factor(TIME), max)))
  sd.EXP = data.frame (with (ST.DF.WSS, tapply(EXP, factor(TIME), sd)))
  
  stats.EXP = cbind(mean.EXP, min.EXP, max.EXP, sd.EXP)
  names(stats.EXP) = c("meanEXP", "minEXP", "maxEXP", "sdEXP")
  stats.EXP$TIME = rownames(stats.EXP)
  
  #create a POSIXct for new TIME = TIME2
  stats.EXP$TIME2 = NA
  for (v in seq_along(stats.EXP$TIME))
  {
    stats.EXP$TIME2[v] = as.POSIXct(stats.EXP$TIME[v], origin = "1970-01-01", tz = "CET")
  }
  
  E.max = max(stats.EXP$maxEXP)
  
  # point plot with transparency in color
  with (ST.DF.WSS, plot(TIME, EXP, pch = "-", cex=1, col = WS.col, ylim=c(0, E.max+20),
                        xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                        main = paste(Active.Profile$FullName, ":", IND.amount, "out of", length(ExposureValue.P), "individuals")))
  
  mtext(paste(head(ST.DF.WSS$TIME,1)+0.01, "-", tail(ST.DF.WSS$TIME,1)+0.01)) # day: Mo.... Th...
  
  #legend("topleft", "Individual exporuse value", col = WS.col, pch = "-", cex=1, box.lwd = 0, box.col = "transparent", bg="transparent", inset = c(0.00,0.005))
  
  #add mean, min and max to plot
  points(stats.EXP$TIME2, stats.EXP$meanEXP, col = "orange", pch = "-", cex=1)
  
  if (PlotMinMax == TRUE)
  {
    points(stats.EXP$TIME2, stats.EXP$minEXP, col = "white", pch = 24, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.75)
    points(stats.EXP$TIME2, stats.EXP$maxEXP, col = "white", pch = 25, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.75)
  }
  
  #   legend("topleft", "Mean", col = "orange", pch = "-", cex=1, box.lwd = 0, box.col = "transparent",bg="transparent", inset = c(0.275,0.005))
  #   legend("topright", "Min", col = "white", pch = 24, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.5, box.lwd = 0, box.col = "transparent", inset = c(0.25,0.005))
  #   legend("topright", "Max", col = "white", pch = 25, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.5, box.lwd = 0, box.col = "transparent", inset = c(0.00,0.005))
  
  #   legend("topright", c("Individual exporuse value","Mean","Min","Max"), inset=c(-0.2,0), title="Legend",
  #          pch=c("-","-",24,25), cex = c(1,1,0.5,0.5), col = c(WS.col,"orange","white","white"),
  #          bg = c("transparent","transparent", rgb(red=0, green=0.5, blue=0.5),rgb(red=0, green=0.5, blue=0.5)),
  #          xpd = TRUE)
  
  legend("top", c("Individual exposure value","Mean"), xpd = FALSE, horiz = TRUE, inset = c(0,0),
         bty = "n", pch = c("-","-"), col = c(WS.col,"orange"), cex = 1)
  
  abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET"), col = "orange") # start summertime / Daylight saving time (DST)
  abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET"), col = "grey") # start wintertime / Standard time
  
  
  #   # ploting mean connected with lines
  #   points(stats.EXP$meanEXP, type = "b", col = "orange", pch = 19)
  #   plot(stats.EXP$meanEXP, type = "b", col = "orange", pch = "-")
  #   
  #   with (stats.EXP, points(TIME, meanEXP, pch = "-", cex=1.5, col = rgb(red=1, green=0, blue=0, alpha=0.10)))
  #   
  #   points(stats.EXP$meanEXP)
  #   plot(stats.EXP$meanEXP)
  #   
  #   
  #   
  #   #! nu nog met 1000 testen
  #   
  #   class(stats.EXP$TIME2) = class(ST.DF.WSS$TIME)
  #   
  #   library(ggplot2)
  #   qplot(TIME.P,ExposureValue.P, geom='smooth', span =0.5, ylim=c(0, 100))
  #   
  #   qplot(TIME.P[1:2][2:4],ExposureValue.P[1:2][2:4])
}


Plot.CumExposureGraph <- function(IND, TYPE, ...)
{
  R.T = as.POSIXct(TIME.unlisted.P[[IND[1]]], origin = "1970-01-01", tz = "CET")
  
  E.max = max(ExposureValueCumYear.P[IND])
  
  if (length(IND) > 1)
  {
    Plot.Main = paste("Cumulative hourly concentration", Active.Type, "Individuals", head(IND,1), "-", tail(IND,1))
  } else {
    Plot.Main = paste("Cumulative hourly concentration", Active.Type, "Individual", IND)
  }
  
  plot(main = Plot.Main,
       x = R.T, y = ExposureValueCum.P[[IND[1]]], col = ifelse(ExposureValueCum.P[[IND[1]]] == 0,'green','white'),
       xlim=c(head(R.T,1), tail(R.T,1)), ylim=c(0, E.max+10), xlab = "Time", ylab = paste(pol, "Cumulative hourly concentration (µg/m³)"), pch = 19)
  
  for (i in IND)
  {
    
    Selected.Standard.Bool =HealthStandards$'Averaging period' == "1 year" &
      HealthStandards$Pollutant == toupper(pol) &
      HealthStandards$Agency == "EU"
    
    #   lines(TIME.unlisted.P[[i]], ExposureValueCum.P[[i]],
    #         col = ifelse(ExposureValueCumYear.P[i] > HealthStandards$Concentration[Selected.Standard.Bool] * (24*365),
    #                      WS.red, WS.col))
    
    if (ExposureValueCumYear.P[i] > HealthStandards$Concentration[Selected.Standard.Bool] * (24*365))
    {
      WS.col = rgb(red=1, green=0.5, blue=0.5, alpha=0.5)
    }
    if (ExposureValueCumYear.P[i] > 20 * (24*365) &
        ExposureValueCumYear.P[i] < HealthStandards$Concentration[Selected.Standard.Bool] * (24*365))
    {
      WS.col = rgb(red=255/255, green=165/255, blue=0, alpha=0.2)
    }
    if (ExposureValueCumYear.P[i] < 20 * (24*365))
    {
      WS.col = rgb(red=0, green=0.5, blue=0.5, alpha=0.1)
    }
    
    lines(TIME.unlisted.P[[i]], ExposureValueCum.P[[i]], col = WS.col)
    
    
    points(tail(R.T,1), HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                                        HealthStandards$Pollutant == toupper(pol) &
                                                        HealthStandards$Agency == "EU"] * (24*365),
           col = "red", pch = 13)
    
    points(tail(R.T,1), 20 * (24*365), col = "orange", pch = 13)
    
    if (TYPE == "monthly")
    {
      for (i in IND)
      {
        # points(TIME.unlisted.P[[i]], ExposureValueCum.P[[i]], col = WS.col, cex = 0.05)
        points(TIME.unlisted.P[[i]][ExposureValueDiff[[i]]], ExposureValueCum.P[[i]][ExposureValueDiff[[i]]], col = rgb(red=1, green=0.5, blue=0.5, alpha=0.5), cex = 0.05)
        
      }
    }
    
    
    
  }
}



Plot.PersonalExposureGraph.P <- function(IND, DAY, DAYS, ...)
{
  R.T = as.POSIXct(unlist(TIME.P[[IND]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
  R.E = unlist(ExposureValue.P[[IND]][seq(DAY, DAY+DAYS, by = 1)])
  
  E.max = max(R.E)
  
  plot(main = paste(Active.Type, "Individual", IND), x = R.T, y = R.E, col = ifelse(R.E == 0,'red','darkgreen'), ylim=c(0, E.max+10),
       xlab = "Time", ylab = paste(pol, "concentration (µ/m³)"), pch = 19)
  
  legend("topleft", "Residence", col="darkgreen", pch = 19)
}

Plot.PersonalExposureGraph.R.summary <- function(DAY, DAYS, ...)
{
  #R.T = seq(TIME.R_[[DAY]][1], tail(TIME.R_[[DAYS]],1), by = 1*60**2)
  R.T = as.POSIXct(unlist(TIME.R[[1]][seq(DAY, DAY+DAYS, by = 1)]), origin = "1970-01-01", tz = "CET")
  R.E = unlist(ExposureValue.R100[seq(DAY, DAY+DAYS, by = 1)])
  
  E.max = max(R.E)
  
  plot(main = paste(Active.Type, "Mean all", length(ExposureValue.R),"individuals"), x = R.T, y = R.E,
       col = ifelse(R.E == 0,'red','darkgreen'), ylim=c(0, E.max+10),
       xlab = "Time", ylab = paste(pol, "concentration (µ/m³)"), pch = 19)
  
  abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01", tz = "CET"), col = "orange")
  abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01", tz = "CET"), col = "grey")
  
  legend("topleft", "Residence", col="darkgreen", pch = 19)
}


#NIET GEBRUIKTE CODE

# plt <- ggplot(Xv,aes(x=group,y=Y)) + stat_binhex() + scale_fill_gradientn(colours=c("yellow","red"),name = "Frequency",na.value=NA) + theme_bw()
# 
# # calculating mean
# out1 <- data.frame (with (Xv,  tapply( Y, factor(group), mean)))
# names(out1) <- c("meanY")
# out1$grp <- as.numeric (rownames (out1))
# 
# # ploting mean connected with lines
# plt1 <- plt + geom_point (aes(grp, meanY), data = out1, pch = 19, col = "blue", cex = 3) 
# 
# # connecting with line 
# plt1 +  geom_line (aes(grp, meanY), data = out1, col = "green1", lwd = 1)
# 



#   DF1 = data.frame(transpose(ExposureValue.P[[1]]))
#   COLNAMES = c(1:length(DF1))
#   colnames(DF1) = paste0("C",COLNAMES)
#   
#   DF.ind = data.frame(seq(1, 1, length.out = nrow(DF1)))
#   colnames(DF.ind) = paste0("ind")
#   DF1 = cbind(DF.ind,DF1)
# 
# 
#   DF2 = data.frame(transpose(ExposureValue.P[[2]]))
#   colnames(DF2) = paste0("C",COLNAMES)
#   DF2 = cbind(2,DF1)
# 
#   DF.list = list()
# #  DF.ind = list()
#   #for (i in (seq_along(ExposureValue.P)))
#   for (i in (seq(1,10)))
#   {
#     DF = data.frame(transpose(ExposureValue.P[[i]]))
#     COLNAMES = c(1:length(DF))
#     colnames(DF) = paste0("C",COLNAMES)
#     
#     DF.ind = data.frame(seq(i, i, length.out = nrow(DF)))
#     colnames(DF.ind) = paste0("ind")
#     DF.list[[i]] = cbind(DF.ind, DF)
#     
#     if (i == 1)
#     {
#       DF.WS = DF.list[[i]]
#     } else
#     {
#       DF.WS = rbind(DF.WS,DF.list[[i]])
#     }
#   }
#   
#   plt <- ggplot(DF.WS, aes(x=TIME.P[1][[1]],y=ExposureValue.P[1][[1]])) + stat_binhex() + scale_fill_gradientn(colours=c("yellow","red"),name = "Frequency",na.value=NA) + theme_bw()
#   plt
#   
#   ST.DF = NULL
#   # test for hour 1
#   for (i in (seq_along(ExposureValue.P))) # per individual
#   #for (i in (seq(1,100)))  
#   {
#     if (i == 1)
#     {
#       ST.DF = data.frame(TIME.P[[i]][[1]][2])
#       ST.DF = cbind(ST.DF, data.frame(ExposureValue.P[[i]][[1]][2]), 1)
#       colnames(ST.DF) = c("TIME", "EXP", "IND")
#     }
#     
#     if (i > 1)
#     {
#       ST.DF = rbind(ST.DF, ST.DF[1,])
#       ST.DF$EXP[i] = ExposureValue.P[[i]][[1]][2]
#       ST.DF$IND[i] = i
#     }
#   }
