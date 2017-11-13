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
list.of.packages <- c("corrplot", "lattice", "ggplot2", "plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(corrplot)
library(lattice)
library(ggplot2)

ScatterplotMatrixAndSave <- function(Type, Stats.HR_ALL_IndividualBased, StatsMethodWithMethodsOfInterestPol,
                                     PlotSave = FALSE, Width = 1208, Height = 720, pol, StatsMethod, ...)
{
  Plot_dir = file.path("..", "output", "plots")
  if (!dir.exists(Plot_dir) & PlotSave) 
  {
    dir.create(Plot_dir)
    #try(dev.off(), silent = T) # clear existing plots
  }
  
  if (PlotSave)
  {
    try(dev.off(), silent = T) # clear existing plots
    
    png(filename = file.path(Plot_dir, paste("SPM", Type, pol, paste0(StatsMethod, ".png"), sep = "_")),
        width = Width, height = Height, units = "px", pointsize = 12)
  }
  
  pairs(Stats.HR_ALL_IndividualBased[, StatsMethodWithMethodsOfInterestPol], # lower.panel = panel.smooth, 
        upper.panel = panel.cor, pch = 1, main = paste0("Individual-based scatterplot matrix and R² of ", pol))
  
  if (PlotSave) {dev.off()}
}
  
CorPlotGraphAndSave <- function(Type, Subtype1, Subtype2, Abbr1, Abbr2, Width = 1208, Height = 720, pol, GroupSize, ...)
{
  Plot_dir = file.path("..", "output", "plots")
  if (!dir.exists(Plot_dir)) 
  {
    dir.create(Plot_dir)
  }

  png(filename = file.path(Plot_dir, paste("CORPLOT", Type, paste(Abbr1, Abbr2 , sep = "~"), pol,
                                           paste0(GroupSize, ".png"), sep = "_")),
      width = Width, height = Height, units = "px", pointsize = 12)
  
  plot(Subtype1, Subtype2, main = paste(Type, Abbr1, "vs.", Abbr2, pol, "(µg/m³)", GroupSize, "individuals"),
       pch = "+", xlab = Abbr1, ylab = Abbr2)
  R.squared = cor(Subtype1, Subtype2)**2
  text(min(Subtype1), max(Subtype2)-1, pos = 1, labels = "R²:", font = 2)
  text(min(Subtype1)+5, max(Subtype2)-1, pos = 1, labels = R.squared)

  dev.off()
}

# In1 = paste(StatsMethod, MethodsOfInterest[1], toupper(pol), sep = "_")
# In2 = paste(StatsMethod, MethodsOfInterest[2], toupper(pol), sep = "_")

CorPlotGraph <- function(Type, In1, In2, Width = 1208, Height = 720, pol,
                         GroupSize, IndividualBasedMean, IndividualBasedMean_BiWeekly = NA, ...)
{
  CombinedValues = c(In1, In2)
  
  collectFull = colnames(IndividualBasedMean) %in% CombinedValues
  
  if (!is.na(IndividualBasedMean_BiWeekly))
  {
    collectBi = colnames(IndividualBasedMean_BiWeekly) %in% paste(CombinedValues, toupper(pol), sep = "_")
    collect = cbind(IndividualBasedMean[,collectFull], IndividualBasedMean_BiWeekly[,collectBi])
  } else
  {
    collect = IndividualBasedMean[,collectFull]
  }
  
  C = cor(collect)
  C_squared = C**2
  
  plot(collect[,1], collect[,2], main = paste(Type, colnames(collect)[1], "vs.", colnames(collect)[2], "(µg/m³)", GroupSize, "individuals"),
       pch = "+", xlab = colnames(collect)[1], ylab = colnames(collect)[2])
  R.squared = C_squared[1,2]
  text(min(collect[,1]), max(collect[,2])-1, pos = 1, labels = "R²:", font = 2)
  text(min(collect[,1])+0.75, max(collect[,2])-1, pos = 1, labels = round(R.squared, 8))
}

CorPlotTableAndSave <- function(Type, Subtype1, Subtype2, Subtype3 = -9999, Subtype4 = -9999,
                                Width = 1208, Height = 720, pol, ...)
{
  Plot_dir = file.path("..", "output", "plots")
  if (!dir.exists(Plot_dir)) 
  {
    dir.create(Plot_dir)
  }
  
  png(filename = file.path(Plot_dir, paste("CORPLOTTABLE", Type, pol, paste0(length(PPH.P), ".png"), sep = "_")),
      width = Width, height = Height, units = "px", pointsize = 12)
  
  CorPlotTable(Type, "Numbers", Subtype1, Subtype2, Subtype3, Subtype4, pol)
  
  dev.off()
}

# GroupName = Active.Type
# CorType = "Mixed"
# CombinedValues = MethodsOfInterest
# CombinedValues = StatsMethodWithMethodsOfInterest
# IndividualBasedMean = Stats.HR_ALL_IndividualBased

CorPlotTable2 <- function(GroupName, CorType, CombinedValues, pol, IndividualBasedMean, IndividualBasedMean_BiWeekly = NA, ...)
{
  collectFull = colnames(IndividualBasedMean) %in% paste(CombinedValues, toupper(pol), sep = "_")
  
  if (!is.na(IndividualBasedMean_BiWeekly))
  {
    collectBi = colnames(IndividualBasedMean_BiWeekly) %in% paste(CombinedValues, toupper(pol), sep = "_")
    collect = cbind(IndividualBasedMean[,collectFull], IndividualBasedMean_BiWeekly[,collectBi])
  } else
  {
    collect = IndividualBasedMean[,collectFull]
  }
  
  C = cor(collect)
  C_squared = C**2
  
  print(C_squared)
  
  if (CorType == "Ellipse")
  {
    corrplot(C, method="ellipse", type = "upper",
             mar = c(1, 1, 1, 1))
  }
  
  if (CorType == "Numbers")
  {
    corrplot(C_squared, method="number", number.digits = 5, title = paste(GroupName, "R²", pol), #type = "upper"
             mar = c(1, 0, 1, 0), tl.col = "black", tl.srt = 45, cl.lim = c(0,1))
  }
  
  if (CorType == "Mixed")
  {
    corrplot.mixed(C_squared, lower = "number", upper = "ellipse", title = paste(GroupName, "R²", pol), #type = "upper"
                   mar = c(2, 0, 2, 0), tl.col = "black", tl.srt = 45, number.digits = 4, cl.lim = c(0,1))#, cl.pos = "r") # cl.length = 5 
  }
}

# CombinedValues =StatsMethodWithMethodsOfInterest #c("PST1", "P1", "P7")
# IndividualBasedMean = Stats.HR_ALL_IndividualBased

ScatterPlotMatrix <- function(Active.Type, CombinedValues, Stats.HR_ALL_IndividualBased, pol, ...)
{
  # http://statmethods.net/graphs/scatterplot.html
  # Scatterplot Matrices from the lattice Package

  collectFull = colnames(Stats.HR_ALL_IndividualBased) %in% paste(CombinedValues, toupper(pol), sep = "_")
  collect = Stats.HR_ALL_IndividualBased[,collectFull]
  
  splom(collect, main = paste0("Scatter plot matrix of ", Active.Type, " for ", toupper(pol)))
  
  splom(collect, main = paste0("Scatter plot matrix of ", Active.Type, " for ", toupper(pol)),
        )
  
}

CorPlotTable <- function(GroupName, CorType, WS1, WS2, C1 = -9999, C2 = -9999, pol, ...)
{
  if (all(C1 == -9999 & C2 == -9999)) # when not given
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
    corrplot(C_squared, method="number", number.digits = 5, title = paste(GroupName, "R²", pol), #type = "upper"
             mar = c(1, 0, 1, 0), tl.col = "black", tl.srt = 45, cl.lim = c(0,1))
  }
}

# Profile = Active.Type
# DAY.start = 1
# DAYS = 100
# CombinedValues = c("mean_PST1", "mean_P1", "mean_P7")
# pol = "no2"
# HourBasedMeanPopulation = Stats.HR_ALL_HourBased

Plot.DeltaProposed <- function(Profile, DAY.start, DAYS, CombinedValues, pol, HourBasedMeanPopulation, ...)
{
  StartHr = DAY.start*24-23
  EndHr = (DAY.start + DAYS - 1)*24
  
  SubPeriodHours = StartHr:EndHr
  
  DeltaDF = HourBasedMeanPopulation[SubPeriodHours, paste(CombinedValues, toupper(pol), sep = "_")]
  DeltaDF = cbind(HourBasedMeanPopulation$TIME[SubPeriodHours], DeltaDF)
  colnames(DeltaDF)[1] = "TIME"

  for (CV in paste(CombinedValues, toupper(pol), sep = "_"))
  {
    #print(CV)
    DeltaDF[,CV] = DeltaDF[,CV] - HourBasedMeanPopulation[SubPeriodHours, paste(CombinedValues[1], toupper(pol), sep = "_")]
  }
  
  Ymin = min(DeltaDF[,2:length(DeltaDF)], na.rm = T)
  Ymax = max(DeltaDF[,2:length(DeltaDF)], na.rm = T)
  
  
  # plot (interactive)
  # With only one trace
  p <- plot_ly(x = HourBasedMeanPopulation$TIME[SubPeriodHours], y = DeltaDF[,2],
               type="scatter", mode="lines", fill = "tozeroy", color = I(Col.Comb[1]))
  p = add_trace(p, x = HourBasedMeanPopulation$TIME[SubPeriodHours], y = DeltaDF[,3],
                type="scatter", mode="lines", fill = "tonexty", color = I(Col.Comb[2]), alpha = 0.25)
  p = add_trace(p, x = HourBasedMeanPopulation$TIME[SubPeriodHours], y = DeltaDF[,4],
                type="scatter", mode="lines", fill = "tonexty", color = I(Col.Comb[3]), alpha = 0.25)
  p
  
  # Plot base
  plot(HourBasedMeanPopulation$TIME[SubPeriodHours], DeltaDF[,2], pch = "-", cex = 1, col = "white",
       ylim = c(Ymin, Ymax), xlab = "Time", ylab = paste("Delta", toupper(pol), "concentration (µg/m³)"),
       main = paste(Profile, ":", "Hour-based difference with ", CombinedValues[1]))
  
  Col.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=1), rgb(red=255/255, green=165/255, blue=0, alpha=1))
  ColFill.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=0.25), rgb(red=255/255, green=165/255, blue=0, alpha=0.25))
  
  for (cv in seq_along(CombinedValues))
  {
    lines(DeltaDF[,1], DeltaDF[,1+cv], pch = "-", cex = 1, col = Col.Comb[cv])
    polygon(c(DeltaDF[,1], rev(DeltaDF[,1])), c(as.numeric(DeltaDF[,1+cv]), as.numeric(rev(DeltaDF[,2]))),
            col = ColFill.Comb[cv], border = NA)
  }

  legend("bottomright", paste(tail(CombinedValues, 2), toupper(pol), sep = "_"), xpd = FALSE, horiz = TRUE, inset = c(0,0),
         bty = "n", bg = "grey", cex = 1, col = tail(Col.Comb, 2), pch = 22, pt.bg = tail(ColFill.Comb, 2))

  mtext(paste(head(HourBasedMeanPopulation$TIME[SubPeriodHours],1), "-", tail(HourBasedMeanPopulation$TIME[SubPeriodHours],1) + 0.001))
}

Plot.DeltaProposedAndSave <- function(Profile, DAY.start, DAYS, CombinedValues, pol, HourBasedMeanPopulation,
                                      PlotSave = FALSE, Width = 1208, Height = 720, PointSize = 12, StatsMethod, ...)
{
  Plot_dir = file.path("..", "output", "plots")
  if (!dir.exists(Plot_dir) & PlotSave) 
  {
    dir.create(Plot_dir)
  }
  
  if (PlotSave)
  {
    try(dev.off(), silent = T) # clear existing plots
    
    png(filename = file.path(Plot_dir, paste("DELTA", Profile, pol, paste0(StatsMethod, ".png"), sep = "_")),
        width = Width, height = Height, units = "px", pointsize = PointSize)
  }
  
  StartHr = DAY.start*24-23
  EndHr = (DAY.start + DAYS - 1)*24
  
  SubPeriodHours = StartHr:EndHr
  
  DeltaDF = HourBasedMeanPopulation[SubPeriodHours, paste(CombinedValues, toupper(pol), sep = "_")]
  DeltaDF = cbind(HourBasedMeanPopulation$TIME[SubPeriodHours], DeltaDF)
  colnames(DeltaDF)[1] = "TIME"
  
  for (CV in paste(CombinedValues, toupper(pol), sep = "_"))
  {
    #print(CV)
    DeltaDF[,CV] = DeltaDF[,CV] - HourBasedMeanPopulation[SubPeriodHours, paste(CombinedValues[1], toupper(pol), sep = "_")]
  }
  
  Ymin = min(DeltaDF[,2:length(DeltaDF)], na.rm = T)
  Ymax = max(DeltaDF[,2:length(DeltaDF)], na.rm = T)
  
  # Plot base
  plot(HourBasedMeanPopulation$TIME[SubPeriodHours], DeltaDF[,2], pch = "-", cex = 1, col = "white",
       ylim = c(Ymin, Ymax), xlab = "Time", ylab = paste("Delta", toupper(pol), "concentration (µg/m³)"),
       main = paste(Profile, ":", "Hour-based difference with ", CombinedValues[1]))
  
  Col.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=1), rgb(red=255/255, green=165/255, blue=0, alpha=1))
  ColFill.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=0.25), rgb(red=255/255, green=165/255, blue=0, alpha=0.25))
  
  # if (length(CombinedValues) == 2)
  # {
  #   Col.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=1), rgb(red=255/255, green=165/255, blue=0, alpha=1))
  #   ColFill.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=0.25), rgb(red=255/255, green=165/255, blue=0, alpha=0.25))
  # }
  
  for (cv in seq_along(CombinedValues))
  {
    ColNr = cv
    if (length(CombinedValues) == 2) {ColNr = cv + 1}
    
    lines(DeltaDF[,1], DeltaDF[,1+cv], pch = "-", cex = 1, col = Col.Comb[ColNr])
    polygon(c(DeltaDF[,1], rev(DeltaDF[,1])), c(as.numeric(DeltaDF[,1+cv]), as.numeric(rev(DeltaDF[,2]))),
            col = ColFill.Comb[ColNr], border = NA)
  }
  
  legend("bottomright", paste(tail(CombinedValues, 2), toupper(pol), sep = "_"), xpd = FALSE, horiz = TRUE, inset = c(0,0),
         bty = "n", bg = "grey", cex = 1, col = tail(Col.Comb, 2), pch = 22, pt.bg = tail(ColFill.Comb, 2))
  
  mtext(paste(head(HourBasedMeanPopulation$TIME[SubPeriodHours],1), "-", tail(HourBasedMeanPopulation$TIME[SubPeriodHours],1) + 0.001))

  if (PlotSave) {dev.off()}
}

# Profile = Active.Type
# PlotMinMax = FALSE
# DAY.start = 150
# DAYS = 21
# DFin = HR_ALL
# StatsDFinHourBased = Stats.HR_ALL_HourBased
# CalcMethod = MethodsOfInterest[1]

Plot.Group3 <- function(Profile, DAY.start, DAYS, DFin, StatsDFinHourBased, CalcMethod, PlotMinMax, pol, ...)
{
  CalcMethodPol = paste(CalcMethod, toupper(pol), sep = "_")
  
  IND.amount = length(unique(DFin[, "IND"]))
  
  ColnamesOfInterest = colnames(DFin)[colnames(DFin) != "TIME" & colnames(DFin) != "IND"]
  Transparency = 1/IND.amount*30
  Col.HR = rgb(red=0.6, green=0.2, blue=0.2, alpha = Transparency)
  Col.HR.max = rgb(red=0.6, green=0.6, blue=0.6, alpha=0.5)
  
  StartHr = DAY.start*24-23
  EndHr = (DAY.start + DAYS - 1)*24
  
  SubPeriodHours = StartHr:EndHr
  
  class(DFin$TIME) = class(Time)
  
  DF.sub = DFin[DFin$TIME %in% Time[StartHr:EndHr],]
  StatsDFinHourBased.sub = StatsDFinHourBased[StatsDFinHourBased$TIME %in% Time[StartHr:EndHr],]
  
  E.max = max(DF.sub[,CalcMethodPol], na.rm = T)
  #E.max = max(DF.sub[,ColnamesOfInterest], na.rm = T)
  
  PSH = "-"
  if (DAYS > 7) {PSH = "."}
  #CEX = 30 / DAYS
  CEX = 1
  
  plot(DF.sub[, "TIME"], DF.sub[, CalcMethodPol],
       pch = PSH, cex = CEX, col = Col.HR, ylim=c(0, E.max),
       xlab = "Time", ylab = paste(CalcMethodPol, "concentration (µg/m³)"),
       main = paste(Profile, ":", IND.amount, "out of", GroupSize, "individuals"))
  
  mtext(paste(head(DF.sub[, "TIME"],1), "-", tail(DF.sub[, "TIME"],1) + 0.001))

  if (PlotMinMax)
  {
    lines(StatsDFinHourBased.sub$TIME, StatsDFinHourBased.sub[, paste0("max_", CalcMethodPol)], col = Col.HR.max, pch=22, lty=2)
    lines(StatsDFinHourBased.sub$TIME, StatsDFinHourBased.sub[, paste0("mean_", CalcMethodPol)])
    
    # lines(StatsDFinHourBased.sub$TIME, StatsDFinHourBased.sub$CalcMethodPol)
    # 
    # points(StatsDFinHourBased.sub$TIME, StatsDFinHourBased.sub$"mean_PST1_NO2",
    #        pch = "-", cex = 1, col = Col.Diff2)
  }
  
}

Plot.CumExposureGraph2 <- function(DF.CumSum, pol, ...)
{
  if (pol == "pm25") {pol2 = "pm2.5"}
  if (pol == "no2") {pol2 = pol}
  
  class(DF.CumSum$TIME) = class(Time)
  E.max = max(DF.CumSum$cum)
  
  Plot.Main = paste("Cumulative hourly concentration", Active.Type, "Individuals", head(unique(DF.CumSum$IND),1), "-", tail(unique(DF.CumSum$IND),1))
  
  # plot blanc
  plot(main = Plot.Main,
       x = Time, y = 1:length(Time), col = 'white', xlim=c(head(Time,1), tail(Time,1)), ylim=c(0, E.max+100),
       xlab = "Time", ylab = paste(pol, "Cumulative hourly concentration (µg/m³)"), pch = 19)
  
  Selected.Standard.Bool =HealthStandards$'Averaging period' == "1 year" &
    HealthStandards$Pollutant == toupper(pol2) &
    HealthStandards$Agency == "EU"
  
  cat("\n")
  
  for (i in unique(DF.CumSum$IND))
  {
    cat(paste(i," "))
    
    if (tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) > HealthStandards$Concentration[Selected.Standard.Bool] * (24*365))
    {
      WS.col = rgb(red=1, green=0.5, blue=0.5, alpha=0.5)
    }
    if (tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) > 20 * (24*365) &
        tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) < HealthStandards$Concentration[Selected.Standard.Bool] * (24*365))
    {
      WS.col = rgb(red=255/255, green=165/255, blue=0, alpha=0.2)
    }
    if (tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) < 20 * (24*365))
    {
      WS.col = rgb(red=0, green=0.5, blue=0.5, alpha=0.1)
    }
    
    # lines(DF.CumSum[DF.CumSum$IND == i, "TIME"], DF.CumSum[DF.CumSum$IND == i, "cum"], col = WS.col)
    
    selcumdf = DF.CumSum[DF.CumSum$IND == i,]
    lines(selcumdf$TIME, selcumdf$cum, col = WS.col)
    
  
  } # closing i
  
  
  abline(h = HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                             HealthStandards$Pollutant == toupper(pol2) &
                                             HealthStandards$Agency == "EU"] * length(Time), lwd=2, col = "red")
  
  abline(h = HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                             HealthStandards$Pollutant == toupper(pol2) &
                                             HealthStandards$Agency == "EU"] * length(Time), lwd=2, col = "red")
  
  points(tail(Time,1),
         HealthStandards$Concentration[HealthStandards$'Averaging period' == "1 year" &
                                         HealthStandards$Pollutant == toupper(pol2) &
                                         HealthStandards$Agency == "EU"] * length(Time), col = "red", pch = 13)
  
  points(tail(Time,1), 20 * length(Time), col = "orange", pch = 13)
  
  # # Hourly exceedance
  # PM25.Exc = 
  
  # Daily exceedance etc
  
}



#Profile = Active.Type
#IND.amount = length(PPH.P)
#PlotMinMax = FALSE
#DAY.start = 5
#DAYS = length(YearDates)-1
#DAYS = 1 length(TIME.P[[1]]) #21 #length(YearDates)-1

Plot.Group2 <- function(Profile, DAY.start, DAYS, IND.amount, PlotMinMax, ST.DF.P, ST.DF.S, ST.DF.T1, ST.DF.T2,
                        stats.EXP.P, stats.EXP.S, stats.EXP.T1, stats.EXP.T2, ...)
{
  if (IND.amount > length(ExposureValue.P))
  {
    print(paste0("Warning: The amount of individuals to be plotted is higher than available in the data set. This
                 amount is reduced to this number:", length(ExposureValue.P)))
    IND.amount = length(ExposureValue.P)
  }
  IND.amount = GroupSize
  IND.amount = length(unique(ST.DF.HR_F$IND))
  
  ST.DF.HR = HO_WS1.ST.DF.HR
  
  
  ST.DF.P.sub = ST.DF.P[ST.DF.P$TIME > YearDates[DAY.start] & ST.DF.P$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.S.sub = ST.DF.S[ST.DF.S$TIME > YearDates[DAY.start] & ST.DF.S$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.T1.sub = ST.DF.T1[ST.DF.T1$TIME > YearDates[DAY.start] & ST.DF.T1$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.T2.sub = ST.DF.T2[ST.DF.T2$TIME > YearDates[DAY.start] & ST.DF.T2$TIME <= YearDates[DAY.start+DAYS],]
  
  class(ST.DF.P.sub$TIME) = class(Time)
  
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
  
  Transparency = 1/IND.amount*30
  
  Col.P = rgb(red=0, green=0.5, blue=0.5, alpha=Transparency)
  Col.S = rgb(red=1, green=0.2, blue=0.5, alpha=Transparency)
  Col.T1 = rgb(red=0.5, green=0.2, blue=0.5, alpha=Transparency)
  Col.T2 = rgb(red=1, green=0.2, blue=0.2, alpha=Transparency)
  Col.HR = rgb(red=0.6, green=0.2, blue=0.2, alpha=Transparency)
  Col.HRMean = rgb(red=0.6, green=0.2, blue=0.2, alpha=1)
  
  # point plot with transparency in color

  with (ST.DF.P.sub, plot(TIME, EXP, pch = "-", cex=1, col = Col.P, ylim=c(0, E.max+20),
                        xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                        main = paste("Office Worker", "|", "Method:", MoI[1], "|", "Multiple location phases")))
                        # main = paste("Office Worker", ":", IND.amount, "out of", IND.amount, "individuals")))
  
  with (ST.DF.HR.sub, plot(TIME, EXP, pch = ".", cex=1, col = Col.P, ylim=c(0, E.max+20),
                      xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                      main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(PPH.P), "individuals")))

  
  with (HourBasedMeanPopulation, plot(TIME[1:(30*24)], (PST1_NO2[1:(30*24)] - P1_NO2[1:(30*24)]), pch = "-", cex=1, col = Col.HRMean, ylim=c(-50, 50),
                           xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                           main = paste(Active.Subtype, ":", IND.amount, "out of", GroupSize, "individuals")))
  
  
  
  
  with (ST.DF.HR_F, plot(as.numeric(TIME), EXP, pch = ".", cex=1, col = Col.HR, ylim=c(0, E.max+20),
                           xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                           main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(PPH.P), "individuals")))
  
  axis.POSIXct(1, at = ST.DF.HR_F$TIME, labels = format(ST.DF.HR_F$TIME,"%b-%d"), las=2)
  
  plot(ST.DF.HR_F$TIME, ST.DF.HR_F$EXP)
  
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
  points(as.POSIXct(stats.EXP.HR$TIME), stats.EXP.HR$meanEXP, col = "orange", pch = ".", cex = 1) 
  points(as.POSIXct(stats.EXP.HR.sub$TIME), stats.EXP.HR.sub$meanEXP, col = "orange", pch = "-", cex = 1)
  
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


Plot.CumExposureGraph <- function(IND, TYPE = NA, ...)
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


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  r2 = r**2
  txt <- format(c(r2, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r2)
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
