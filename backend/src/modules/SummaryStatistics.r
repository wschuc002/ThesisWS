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
list.of.packages <- c("corrplot", "lattice", "ggplot2", "plyr", "plotly", "rmarkdown")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(corrplot)
library(lattice)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(RCurl)
library(rmarkdown)
library(rjson)
library(knitr)


Plot.EXCpie <- function(Profile, EXC_NO2.pc, EXC_PM25.pc, Location = "Local", ...)
{
  Profiles <- c("Office workers", "Homeworkers","School pupils")
  pro = which(Types %in% Profile)
  
  trace1 <- list(
    domain = list(x = c(0, 0.48)), 
    hole = 0.4, 
    hoverinfo = "label+percent+name", 
    labels = c("Exceeded", "Not exceeded"),
    name = "NO2 exceedance",
    textposition = "inside",
    type = "pie",
    marker = list( colors = I(c(rgb(red=1, green=0, blue=0, alpha=1),"green"))),
    values = c(EXC_NO2.pc, 100-EXC_NO2.pc)
  )
  trace2 <- list(
    domain = list(x = c(0.52, 1)), 
    hole = 0.4, 
    hoverinfo = "label+percent+name", 
    labels = c("Exceeded", "Not exceeded"), 
    name = "PM25 exceedance", 
    text = "PM2.5", 
    textposition = "inside", 
    type = "pie",
    marker = list( colors = I(c(rgb(red=1, green=0, blue=0, alpha=1),"green"))),
    values = c(EXC_PM25.pc, 100-EXC_PM25.pc)
  )
  data <- list(trace1, trace2)
  layout <- list(
    annotations = list(
      list(
        x = 0.185, 
        y = 0.5, 
        font = list(size = 20), 
        showarrow = FALSE, 
        text = "NO2"
      ), 
      list(
        x = 0.820, 
        y = 0.5, 
        font = list(size = 20), 
        showarrow = FALSE, 
        text = "PM2.5"
      )
    ),
    title = paste("Yearly WHO health standard exceedances for", Profiles[pro])
  )
  p <- plot_ly()
  p <- add_trace(p, domain=trace1$domain, hole=trace1$hole, hoverinfo=trace1$hoverinfo, labels=trace1$labels, name=trace1$name,
                 type=trace1$type, values=trace1$values, marker=trace1$marker, textposition=trace1$textposition)
  p <- add_trace(p, domain=trace2$domain, hole=trace2$hole, hoverinfo=trace2$hoverinfo, labels=trace2$labels, name=trace2$name,
                 text=trace2$text, type=trace2$type, values=trace2$values, marker=trace2$marker)
  p <- layout(p, annotations=layout$annotations, title=layout$title)
  
  
  CurrentDateAndTime = Sys.time() + 1*60**2
  NameForInteractivePlot = paste("GISCAPE_YearlyEXC", Profile,
                                 paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                        format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                 sep = "_")
  
  if (Location == "Local")
  {
    widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
    htmlwidgets::saveWidget(p, widget_out)
  }
  if (Location == "Online")
  {
    NameForInteractivePlot = paste("Figure 4-40: Individual-based mean exceedances", "for", Profiles[pro])
    api_create(p, filename = NameForInteractivePlot, sharing = "public")
  }
  
  return(p)
}

Plot.EXCfrequences <- function(EXC_OW, EXC_SP, EXC_HO, Profiles2, Col.Profiles, pol, Location = "Local", ...)
{
  EXCrange = range(c(EXC_OW, EXC_SP, EXC_HO))
  
  p = plot_ly() %>%
    layout(
      title = paste0("Individual exceedance frequency distribution for ", toupper(pol)),
      xaxis = list(title = "Amount of exceedances in group"),
      yaxis = list(title = "Frequency"),
      barmode = "overlay",
      images = list(
        list(source = "https://raw.githubusercontent.com/wschuc002/ThesisWS/master/backend/img/GISCAPE_150px_2.png?raw=true",
             xref = "paper",
             yref = "paper",
             x= 0,
             y= 1,
             sizex = 0.2,
             sizey = 0.2,
             opacity = 1
        )
      )
    )
  
  # df.Li = list()
  # smooth.Li = list()
  for (pr in 1:3)
  {
    if (pr == 1) {EXC = EXC_OW}
    if (pr == 2) {EXC = EXC_SP}
    if (pr == 3) {EXC = EXC_HO}
    
    y = NA
    x = seq(EXCrange[1], EXCrange[2], 1)
    for (e in x)
    {
      y[e+1] = length(which(EXC == e))
    }
    # df.Li[[pr]] = data.frame(x,y)
    
    
    
    # add bars
    # p = add_trace(p, x = x, y = y, name = Profiles2[pr], type = "bar",
    #               marker = list(color = "transparent",
    #                             line = list(color = Col.Profiles[pr],
    #                                         width = 1)))
    # add markers
    p = add_trace(p, x = x, y = y, name = Profiles2[pr], type = "scatter", mode = "markers",
                  marker = list(color = Col.Profiles[pr], symbol = 141, sizeref = 50))
    
    # p
    # plot_ly() %>% add_bars(x = x, y = y, name = "FD")
    
    
    
    myhist <- hist(EXC, plot = FALSE, breaks = diff(EXCrange))
    dens <- density(EXC, n = diff(EXCrange))
    # axis(side=1, at=seq(0,100, 20), labels=seq(0,100,20))
    
    freqs.y = dens$y*(1/sum(myhist$density))*length(EXC)
    
    freqs.y[dens$x < 0] = NA
    dens$x[dens$x < 0] = NA
    
    # plot(dens$x,freqs.y, type = "l", col = "green")
    
    # hist(EXC, plot = TRUE, breaks = diff(EXCrange))
    # lines(dens$x,freqs.y, type = "l", col = "green")
    
    p = add_trace(p, x = dens$x, y = freqs.y, name = Profiles2[pr], type = "scatter", mode = "lines", color = I(Col.Profiles[pr]))
    
    
    # y = NA
    # x = seq(0, max(EXC), 1)
    # for (e in x)
    # {
    #   y[e+1] = length(which(EXC == e))
    # }
    # df.Li[[pr]] = data.frame(x,y)
    # 
    # smooth <- loess(df.Li[[pr]]$y~df.Li[[pr]]$x)
    # # print(lo)
    # smooth.Li[[pr]] = smooth
    
    # d = density(EXC, bw = 1)
    # d[[1]][d[[1]] < 0] = NA
    # d[[2]][d[[1]] < 0] = NA
    # 
    # f = d[1:2]
    # f[[2]] = f[[2]] * GroupSize
    # df = data.frame(f[[1]], f[[2]])
    # colnames(df) = c("x", "y")
    # 
    # df.Li[[pr]] = df
  }
  # return(df.Li)
  
  
  # for (pr in (1:length(df.Li)))
  # {
  #   
  #   # # p = add_bars(p, x = df.Li[[pr]]$x, y = df.Li[[pr]]$y, name = Profiles2[pr], color = I(Col.Profiles)[pr])
  #   # 
  #   # # p = add_histogram(p, x = df.Li[[pr]]$x, y = df.Li[[pr]]$y, name = Profiles2[pr], color = I(Col.Profiles)[pr])
  #   # 
  #   # # p = add_trace(p, data = df.Li[[pr]], x = ~x, y = ~y, name = Profiles2[pr], type="scatter", mode="markers",
  #   # #               color = I(Col.Profiles)[pr], alpha = 0.5, sizes = 0.75)
  #   # # 
  #   # # p = add_trace(p, data = smooth.Li[[pr]], x = ~x, y = ~y, name = Profiles2[pr], type="scatter", mode="lines",
  #   # #               color = I(Col.Profiles)[pr], alpha = 0.5, sizes = 0.75)
  # }
  
  CurrentDateAndTime = Sys.time() + 2*60**2
  NamePlotType = "GISCAPE_EXCfreq"
  NameForInteractivePlot = paste(NamePlotType, toupper(pol),
                                 paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                        format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                 sep = "_")
  
  if (Location == "Local")
  {
    widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
    htmlwidgets::saveWidget(p, widget_out) # adaptive sizing in browser, but less stable creating
    # htmltools::save_html(p, widget_out) # static sizing in browser, but more stable creating
  }
  if (Location == "Online")
  {
    NameForInteractivePlot = paste("Figure 4-42: Frequency distribution of individual exceedances for", toupper(pol))
    api_create(p, filename = NameForInteractivePlot, sharing = "public")
  }
  
  return(p)
}


# EXC_OW_1H_PST1 = EXC_OW_1H_PM25_PST1
# EXC_OW_1H_P1 = EXC_OW_1H_PM25_P1
# EXC_OW_1H_P7 = EXC_OW_1H_PM25_P7
# EXC_SP_1H_PST1 = EXC_SP_1H_PM25_PST1
# EXC_SP_1H_P1 = EXC_SP_1H_PM25_P1
# EXC_SP_1H_P7 = EXC_SP_1H_PM25_P7
# EXC_HO_1H_P1 = EXC_HO_1H_PM25_P1
# EXC_HO_1H_P7 = EXC_HO_1H_PM25_P7

Plot.EXCpercentages2 <- function(EXC_OW_1H_PST1, EXC_OW_1H_P1, EXC_OW_1H_P7,
                                 EXC_SP_1H_PST1, EXC_SP_1H_P1, EXC_SP_1H_P7,
                                 EXC_HO_1H_P1, EXC_HO_1H_P7,
                                 Profiles2, Col.Profiles, pol, GroupSize, ...)
{
  p = plot_ly() %>%
    layout(
      title = paste0("% of exceedances for ", toupper(pol)),
      xaxis = list(title = "Yearly permitted exceedances"),
      yaxis = list(title = "% of group"),
      images = list(
        list(source = "https://raw.githubusercontent.com/wschuc002/ThesisWS/master/backend/img/GISCAPE_150px_2.png?raw=true",
             xref = "paper",
             yref = "paper",
             x= 0,
             y= 1,
             sizex = 0.2,
             sizey = 0.2,
             opacity = 1
        )
      )
    )
  
  # class(EXC_OW_1H_PST1$TIME) = class(Time)
  # class(EXC_OW_1H_P1$TIME) = class(Time)
  # class(EXC_OW_1H_P7$TIME) = class(Time)
  # class(EXC_SP_1H_PST1$TIME) = class(Time)
  # class(EXC_SP_1H_P1$TIME) = class(Time)
  # class(EXC_SP_1H_P7$TIME) = class(Time)
  # class(EXC_HO_1H_P1$TIME) = class(Time)
  # class(EXC_HO_1H_P7$TIME) = class(Time)
  
  # EXCfr_OW_1H_PST1 = NA
  # EXCfr_OW_1H_P1 = NA
  # EXCfr_OW_1H_P7 = NA
  
  for (i in (1:GroupSize))
  {
    EXCfr_OW_1H_PST1[i] = length(EXC_OW_1H_PST1[EXC_OW_1H_PST1$IND == i,]$IND)
    EXCfr_OW_1H_P1[i] = length(EXC_OW_1H_P1[EXC_OW_1H_P1$IND == i,]$IND)
    EXCfr_OW_1H_P7[i] = length(EXC_OW_1H_P7[EXC_OW_1H_P7$IND == i,]$IND)
  }
  
  for (pr in 1:3)
  {
    EXCfr_1H_PST1 = NA
    EXCfr_1H_P1 = NA
    EXCfr_1H_P7 = NA
    
    for (i in (1:GroupSize))
    {
      if (pr != 3) {EXCfr_1H_PST1[i] = length(EXC_OW_1H_PST1[EXC_OW_1H_PST1$IND == i,]$IND)}
      EXCfr_1H_P1[i] = length(EXC_OW_1H_P1[EXC_OW_1H_P1$IND == i,]$IND)
      EXCfr_1H_P7[i] = length(EXC_OW_1H_P7[EXC_OW_1H_P7$IND == i,]$IND)
    }
    
    if (pr == 1) {EXC = list(EXCfr_OW_1H_PST1, EXCfr_OW_1H_P1, EXCfr_OW_1H_P7)}
    if (pr == 2) {EXC = list(EXCfr_SP_1H_PST1, EXCfr_SP_1H_P1, EXCfr_SP_1H_P7)}
    if (pr == 3) {EXC = list(EXCfr_HO_1H_P1, EXCfr_HO_1H_P7)}
    
    df.Li = list()
    for (cm in seq_along(EXC))
    {
      y = NA
      x = seq(1, max(EXC[[cm]])+1, 1)
      for (e in x)
      {
        y[e] = length(which(EXC[[cm]] > e))
      }
      y = y / GroupSize * 100
      
      df.Li[[cm]] = data.frame(x,y)
      
      if (cm == 1) {LINE = list()}
      if (cm == 2) {LINE = list(dash = "dash")}
      if (cm == 3) {LINE = list(dash = "dot")}
      
      p = add_trace(p, data = df.Li[[cm]], x = ~x, y = ~y, name = Profiles2[pr], type="scatter", mode="markers+lines",
                    color = I(Col.Profiles)[pr], alpha = 0.5, sizes = 0.75, line = LINE)
    }
    p
    
  }
  
  for (pr in (1:length(df.Li)))
  {
    p = add_trace(p, data = df.Li[[pr]], x = ~x, y = ~y, name = Profiles2[pr], type="scatter", mode="markers+lines",
                  color = I(Col.Profiles)[pr], alpha = 0.5, sizes = 0.75, line = LINE)
  }
  
  return(p)
}

Plot.EXCpercentages <- function(EXC_OW, EXC_SP, EXC_HO, Profiles2, Col.Profiles, pol, GroupSize, Location = "Local",  ...)
{
  df.Li = list()
  for (pr in 1:3)
  {
    if (pr == 1) {EXC = EXC_OW}
    if (pr == 2) {EXC = EXC_SP}
    if (pr == 3) {EXC = EXC_HO}
    
    y = NA
    x = seq(1, max(EXC)+1, 1)
    for (e in x)
    {
      y[e] = length(which(EXC > e))
    }
    y = y / GroupSize * 100
    
    # df = data.frame(x,y)
    df.Li[[pr]] = data.frame(x,y)
  }
  # df = do.call(merge.data.frame, df.Li[[pr]])
  # df = unlist(df.Li[[pr]])
  
  p = plot_ly() %>%
    layout(
      title = paste("% of yearly permitted exceedances for", toupper(pol)),
      xaxis = list(title = "Yearly permitted exceedances"),
      yaxis = list(title = "% of group"),
      images = list(
        list(source = "https://raw.githubusercontent.com/wschuc002/ThesisWS/master/backend/img/GISCAPE_150px_2.png?raw=true",
             xref = "paper",
             yref = "paper",
             x= 0,
             y= 1,
             sizex = 0.2,
             sizey = 0.2,
             opacity = 1
        )
      )
    )
  
  for (pr in (1:length(df.Li)))
  {
    p = add_trace(p, data = df.Li[[pr]], x = ~x, y = ~y, name = Profiles2[pr], type="scatter", mode="markers+lines",
                  color = I(Col.Profiles)[pr], alpha = 0.5, sizes = 0.75)
  }
  
  CurrentDateAndTime = Sys.time() + 2*60**2
  NamePlotType = "GISCAPE_EXCperc"
  NameForInteractivePlot = paste(NamePlotType, toupper(pol),
                                 paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                        format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                 sep = "_")
  
  if (Location == "Local")
  {
    widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
    htmlwidgets::saveWidget(p, widget_out) # adaptive sizing in browser, but less stable creating
    # htmltools::save_html(p, widget_out) # static sizing in browser, but more stable creating
  }
  if (Location == "Online")
  {
    NameForInteractivePlot = paste("Figure 4-41: Yearly permitted exceedances for", toupper(pol))
    api_create(p, filename = NameForInteractivePlot, sharing = "public")
  }
  
  return(p)
}

# Profile = Active.Type
# pol = pollutants[2]
# knitr::opts_chunk$set(cache = FALSE)

# EXC = EXC_SP_1H_PM25_PST1


Plot.CumExposureGraph4 <- function(Profile, Time, DF.CumSum, pol, EXC, Location = "Local", Size, ...)
{
  Profiles <- c("Office workers", "Homeworkers", "School pupils")
  pro = which(Types %in% Profile)
  
  Col.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=1), rgb(red=255/255, green=165/255, blue=0, alpha=1))
  ColFill.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=0.25), rgb(red=255/255, green=165/255, blue=0, alpha=0.25))
  
  p = plot_ly(text = paste0("Individual: ", 1), name = "Cumulative exposure",
              type="scatter", mode="lines", color = I(Col.Comb)[2]) %>%
    layout(
      title = paste0("Cumulative exposure to ", toupper(pol), " for ", Size, " ", Profiles[pro]),
      yaxis = list(title = paste0(toupper(pol), " (??g/m3)")),
      showlegend = FALSE,
      images = list(
        list(source = "https://raw.githubusercontent.com/wschuc002/ThesisWS/master/backend/img/GISCAPE_150px_2.png?raw=true",
             xref = "paper",
             yref = "paper",
             x= 0,
             y= 1,
             sizex = 0.2,
             sizey = 0.2,
             opacity = 1
        )
      )
      
    )
  
  # Size = 250 # GroupSize
  
  # SEL.CUM = which(DF.CumSum$TIME %in% EXC$TIME &
  #               DF.CumSum$IND %in% EXC$IND[EXC$IND <= Size])
  # SEL.EXC = which(EXC$IND <= Size)
  
  cat("\n")
  for (i in 1:Size)
  {
    cat(paste(i," "))
    SEL.CUM = which(DF.CumSum$TIME %in% EXC$TIME &
                      DF.CumSum$IND %in% EXC$IND[EXC$IND == i])
    SEL.EXC = which(EXC$IND %in% i)
    
    SEL.CUM = which(DF.CumSum$TIME %in% EXC$TIME[SEL.EXC] & DF.CumSum$IND %in% i)
    
    #plot cum line
    p = add_trace(p, x = Time, y = DF.CumSum$cumInt[DF.CumSum$IND == i],
                  text = paste0("Individual: ", i),
                  alpha = 0.02, sizes = 0.05
                  # mode = "lines", color = I(Col.Comb)[2], name = "Cumulative exposure"
    )
    
    # EXC.sub = EXC[EXC$IND == i,]
    
    DF.CumSum[SEL.CUM,]
    
    #plot exceedance point
    p = add_trace(p, x = DF.CumSum$TIME[SEL.CUM], y = DF.CumSum$cumInt[SEL.CUM], mode="markers", color = I("#ca0020"),
                  name = "Exceeded values",
                  alpha = 0.05, sizes = 0.005,
                  text = paste('Individual: ', i,
                               '</br>Exposure: ', round(EXC$PST1_PM25[SEL.EXC],3))
    )
  }
  cat("\n")
  # p
  # rm(p)
  # gc()
  
  # p = add_trace(p, x = DF.CumSum[DF.CumSum$IND <= Size,]$TIME, y = DF.CumSum[DF.CumSum$IND <= Size,]$cum,
  #               text = paste0("Individual: ", DF.CumSum[DF.CumSum$IND <= Size,]$IND),
  #               alpha = 0.2, sizes = 0.05
  #               # mode = "lines", color = I(Col.Comb)[2], name = "Cumulative exposure"
  #               )
  # 
  # p = add_trace(p, x = DF.CumSum$TIME[SEL.CUM], y = DF.CumSum$cum[SEL.CUM], mode="markers", color = I("#ca0020"),
  #               name = "Exceeded values",
  #               alpha = 0.2, sizes = 0.05,
  #               text = paste('Individual: ', DF.CumSum[DF.CumSum$IND <= Size,]$IND[SEL.CUM],
  #                            '</br> Exposure: ', round(EXC$PST1_PM25[SEL.EXC],3))
  #               )
  # p
  
  CurrentDateAndTime = Sys.time() + 1*60**2
  NameForInteractivePlot = paste("GISCAPE_CumExc", Profile, toupper(pol),
                                 paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                        format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                 Size,
                                 sep = "_")
  
  if (Location == "Local")
  { 
    widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
    # htmlwidgets::saveWidget(p, widget_out) # adaptive sizing in browser, but less stable creating
    htmltools::save_html(p, widget_out) # static sizing in browser, but more stable creating
  }
  if (Location == "Online")
  {
    NameForInteractivePlot = paste("Figure 4-43: Cumulative exposure with hourly", toupper(pol), "exceedances",
                                   "for", Size, Profiles[pro])
    api_create(p, filename = NameForInteractivePlot, sharing = "public")
  }
  
  return(p)
  
  # # read the html file
  # resRaw = RCurl::getURL(utils::URLencode(paste0("C:/git/ThesisWS/frontend/html/", "GISCAPE_CumExc_01OW_PM25_20180129_1228", ".html")))
  # 
  # doc.html = htmlTreeParse(paste0("C:/git/ThesisWS/frontend/html/", "GISCAPE_CumExc_01OW_PM25_20180129_1228", ".html"),
  #                          useInternal = TRUE)
  # doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  # doc.text = gsub('\\n', ' ', doc.text)
  # # replace the data
  # 
  # 
  # library(xml2)
  # xmlhtml = read_xml(doc.html)
  
}


Plot.CumExposureGraph3 <- function(Profile, DF.CumSum, pol, EXC, Location = "Local", ...)
{
  p = plot_ly(x = DF.CumSum$TIME[DF.CumSum$IND == 1], y = DF.CumSum$cum[DF.CumSum$IND == 1],
              type="scatter", mode="lines", color = I(Col.Comb[2]), alpha = 0.1, sizes = 0.05)
  
  for (i in (2:GroupSize)) # GroupSize
  {
    p = add_lines(p, x = DF.CumSum$TIME[DF.CumSum$IND == i], y = DF.CumSum$cum[DF.CumSum$IND == i],
                  type="scatter", mode="lines", color = I(Col.Comb[2]), alpha = 0.1)
  }
  
  HR_S = DF.CumSum[DF.CumSum$TIME %in% EXC$TIME & DF.CumSum$IND %in% EXC$IND,]
  
  for (i in (1:nrow(HR_S)))
  {
    p = add_trace(p, x = HR_S$TIME[i], y = HR_S$cum[i],
                  type="scatter", mode="markers", color = I(Col.Comb[3]), alpha = 0.75)
  }
  
  CurrentDateAndTime = Sys.time() + 1*60**2
  NameForInteractivePlot = paste("GISCAPE_CumExc", Profile, toupper(pol),
                                 paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                        format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                 sep = "_")
  
  if (Location == "Local")
  {
    widget_out = gsub(pattern = "src", replacement = paste0("output/plots/", NameForInteractivePlot, ".html"), x = getwd())
    htmlwidgets::saveWidget(p, widget_out)
  }
  if (Location == "Online")
  {
    api_create(p, filename = NameForInteractivePlot, sharing = "public")
  }
  
  return(p)
}

# Profile = Active.Type
# DAY.start = 68
# DAYS = 8
# CombinedValues = c("mean_PST1", "mean_P1", "mean_P7")
# pol = "no2"
# HourBasedMeanPopulation = Stats.HR_ALL_HourBased
# InteractivePlot = TRUE
# Location = "Local" # "Online"

Plot.DeltaProposed <- function(Profile, MethodsOfInterest, DAY.start, DAYS, CombinedValues, pol, HourBasedMeanPopulation, InteractivePlot = FALSE,
                               Location = "Local", ...)
{
  Profiles <- c("Office workers", "Homeworkers","School pupils")
  pro = which(Types %in% Profile)
  
  StartHr = DAY.start*24-23
  EndHr = (DAY.start + DAYS - 1)*24
  
  SubPeriodHours = StartHr:EndHr
  
  DeltaDF = HourBasedMeanPopulation[SubPeriodHours, paste(CombinedValues, toupper(pol), sep = "_")]
  DeltaDF = cbind(HourBasedMeanPopulation$TIME[SubPeriodHours], DeltaDF)
  colnames(DeltaDF)[1] = "TIME"
  
  CM = paste(CombinedValues, toupper(pol), sep = "_")
  for (cm in CM)
  {
    print(cm)
    DeltaDF[,cm] = HourBasedMeanPopulation[SubPeriodHours, cm] - HourBasedMeanPopulation[SubPeriodHours, CM[1]]
  }
  
  Ymin = min(DeltaDF[,2:length(DeltaDF)], na.rm = T)
  Ymax = max(DeltaDF[,2:length(DeltaDF)], na.rm = T)
  
  # plot (interactive)
  # With only one trace
  if(InteractivePlot)
  {
    Col.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=1), rgb(red=255/255, green=165/255, blue=0, alpha=1))
    ColFill.Comb = c("black", rgb(red=0, green=0.5, blue=0.5, alpha=0.25), rgb(red=255/255, green=165/255, blue=0, alpha=0.25))
    
    if (Profile == "02.HO")
    {
      Col.Comb = c(Col.Comb[1], Col.Comb[3])
      ColFill.Comb = c(ColFill.Comb[1], ColFill.Comb[3])
    }
    
    p = plot_ly(x = HourBasedMeanPopulation$TIME[SubPeriodHours], y = DeltaDF[,2],
                name = paste0(MethodsOfInterest[1], " - ", MethodsOfInterest[1]),
                type="scatter", mode="lines", fill = "tozeroy", color = I(Col.Comb[1])) %>%
      layout(
        title = paste0("Delta analysis: ", "Means of ", Profiles[pro], " for ", toupper(pol)),
        images = list(
          list(source = "https://raw.githubusercontent.com/wschuc002/ThesisWS/master/backend/img/GISCAPE_150px_2.png?raw=true",
               xref = "paper",
               yref = "paper",
               x= 0,
               y= 1,
               sizex = 0.2,
               sizey = 0.2,
               opacity = 1
          )
        )
      )
    
    for (cm in 3:length(DeltaDF))
    {
      p = add_trace(p, x = HourBasedMeanPopulation$TIME[SubPeriodHours], y = DeltaDF[,cm],
                    name = paste0(MethodsOfInterest[cm-1], " - ", MethodsOfInterest[1]),
                    type="scatter", mode="lines", fill = "tonexty", color = I(Col.Comb[cm-1]), alpha = 0.25)
    }
    p
    
    CurrentDateAndTime = Sys.time() + 1*60**2
    NameForInteractivePlot = paste("GISCAPE_Delta", Profile, toupper(pol),
                                   paste0(format(CurrentDateAndTime, "%Y"), format(CurrentDateAndTime, "%m"),
                                          format(CurrentDateAndTime, "%d")), paste0(format(CurrentDateAndTime, "%H%M")),
                                   DAY.start, DAYS,
                                   sep = "_")
    
    if (Location == "Local")
    {
      widget_out = gsub(pattern = "backend/src", replacement = paste0("frontend/html/", NameForInteractivePlot, ".html"), x = getwd())
      htmlwidgets::saveWidget(p, widget_out)
    }
    if (Location == "Online")
    {
      NameForInteractivePlot = paste("Figure 4-38: Delta analysis", toupper(pol), "exposure", Profiles[pro])
      api_create(p, filename = NameForInteractivePlot, sharing = "public")
    }
  }
  
  
  if(!InteractivePlot)
  {
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
  return(p)
}

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

Plot.HourExposure <- function(Profile, DAY.start, DAYS, DFin, StatsDFinHourBased, CalcMethod, PlotMinMax, pol, ...)
{
  CalcMethodPol = paste(CalcMethod, toupper(pol), sep = "_")
  
  IND.amount = length(unique(DFin[, "IND"]))
  
  Profiles <- c("Office workers", "Homeworkers","School pupils")
  pro = which(Types %in% Profile)
  
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
       main = paste("Hourly", toupper(pol), "concentrations for", IND.amount, Profiles[pro]))
  
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
  
  Selected.Standard.Bool = HealthStandards$'Averaging period' == "1 year" &
    HealthStandards$Pollutant == toupper(pol2) &
    HealthStandards$Agency == "EU"
  
  cat("\n")
  
  for (i in unique(DF.CumSum$IND))
  {
    cat(paste(i," "))
    
    if (tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) > HealthStandards$Concentration[Selected.Standard.Bool] * (24*length(YearDates)))
    {
      WS.col = rgb(red=1, green=0.5, blue=0.5, alpha=0.5)
    }
    if (tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) > 20 * (24*length(YearDates)) &
        tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) < HealthStandards$Concentration[Selected.Standard.Bool] * (24*length(YearDates)))
    {
      WS.col = rgb(red=255/255, green=165/255, blue=0, alpha=0.2)
    }
    if (tail(DF.CumSum[DF.CumSum$IND == i, "cum"],1) < 20 * (24*length(YearDates)))
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
#IND.amount = 1000 length(PPH.P)
#PlotMinMax = FALSE
#DAY.start = 1
#DAYS = length(YearDates)-1
#DAYS = 21 length(TIME.P[[1]]) #21 #length(YearDates)-1

Plot.RawExposure <- function(Profile, DAY.start, DAYS, IND.amount, PlotMinMax, ST.DF.P, ST.DF.S, ST.DF.T1, ST.DF.T2,
                             stats.EXP.P, stats.EXP.S, stats.EXP.T1, stats.EXP.T2, ...)
{
  Profiles <- c("Office workers", "Homeworkers","School pupils")
  pro = which(Types %in% Profile)
  
  # if (IND.amount > length(ExposureValue.P))
  # {
  #   print(paste0("Warning: The amount of individuals to be plotted is higher than available in the data set. This
  #                amount is reduced to this number:", length(ExposureValue.P)))
  #   IND.amount = length(ExposureValue.P)
  # }
  
  INDmin = min(length(unique(ST.DF.P$IND)), length(unique(ST.DF.S$IND)), length(unique(ST.DF.T1$IND)), length(unique(ST.DF.T2$IND)))
  
  if (IND.amount > INDmin)
  {
    print(paste0("Warning: The amount of individuals to be plotted is higher than available in the data set. This
                 amount is reduced to this number:", INDmin))
    IND.amount = INDmin
  }
  
  # ST.DF.HR = HO_WS1.ST.DF.HR
  
  ST.DF.P.sub = ST.DF.P[ST.DF.P$TIME > YearDates[DAY.start] & ST.DF.P$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.S.sub = ST.DF.S[ST.DF.S$TIME > YearDates[DAY.start] & ST.DF.S$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.T1.sub = ST.DF.T1[ST.DF.T1$TIME > YearDates[DAY.start] & ST.DF.T1$TIME <= YearDates[DAY.start+DAYS],]
  ST.DF.T2.sub = ST.DF.T2[ST.DF.T2$TIME > YearDates[DAY.start] & ST.DF.T2$TIME <= YearDates[DAY.start+DAYS],]
  
  class(ST.DF.P.sub$TIME) = class(Time)
  
  if (PlotMinMax)
  {
    stats.EXP.P.sub = DF.Stats(ST.DF.P.sub)
    stats.EXP.S.sub = DF.Stats(ST.DF.S.sub)
    stats.EXP.T1.sub = DF.Stats(ST.DF.T1.sub)
    stats.EXP.T2.sub = DF.Stats(ST.DF.T2.sub)
  }
  
  # ST.DF.HR.sub = ST.DF.HR[ST.DF.HR$TIME > YearDates[DAY.start] & ST.DF.HR$TIME <= YearDates[DAY.start+DAYS],]
  if (PlotMinMax) {stats.EXP.HR.sub = DF.Stats(ST.DF.HR.sub)}
  
  #E.max = max(stats.EXP.P.sub$maxEXP, na.rm = T)
  # E.max = max(c(stats.EXP.P.sub$maxEXP, stats.EXP.S.sub$maxEXP, stats.EXP.T1.sub$maxEXP, stats.EXP.T2.sub$maxEXP), na.rm = T)
  # E.max = max(stats.EXP.HR.sub$maxEXP, na.rm = T)
  E.max = 100
  
  Transparency = 1/IND.amount*30
  
  Col.P = rgb(red=0, green=0.5, blue=0.5, alpha=Transparency)
  Col.S = rgb(red=1, green=0.2, blue=0.5, alpha=Transparency)
  Col.T1 = rgb(red=0.5, green=0.2, blue=0.5, alpha=Transparency)
  Col.T2 = rgb(red=1, green=0.2, blue=0.2, alpha=Transparency)
  Col.HR = rgb(red=0.6, green=0.2, blue=0.2, alpha=Transparency)
  Col.HRMean = rgb(red=0.6, green=0.2, blue=0.2, alpha=1)
  
  # point plot with transparency in color
  
  with (ST.DF.P.sub, plot(TIME, EXP_NO2, pch = "-", cex=1, col = Col.P, ylim=c(0, E.max+20),
                          xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
                          main = paste("Raw", toupper(pol), "concentrations for", IND.amount, Profiles[pro])))
  # main = paste("Office Worker", ":", IND.amount, "out of", IND.amount, "individuals")))
  
  WhichCol = which(colnames(ST.DF.P.sub) %in% paste0("EXP_", toupper(pol)))
  
  plot(ST.DF.P.sub$TIME, ST.DF.P.sub[,WhichCol], pch = "-", cex=1, col = Col.P, ylim=c(0, E.max+20),
       xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
       main = paste("Raw", toupper(pol), "concentrations for", IND.amount, Profiles[pro]))
  
  
  # with (ST.DF.HR.sub, plot(TIME, EXP, pch = ".", cex=1, col = Col.P, ylim=c(0, E.max+20),
  #                     xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
  #                     main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(PPH.P), "individuals")))
  
  
  # with (HourBasedMeanPopulation, plot(TIME[1:(30*24)], (PST1_NO2[1:(30*24)] - P1_NO2[1:(30*24)]), pch = "-", cex=1, col = Col.HRMean, ylim=c(-50, 50),
  #                          xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
  #                          main = paste(Active.Subtype, ":", IND.amount, "out of", GroupSize, "individuals")))
  
  # with (ST.DF.HR_F, plot(as.numeric(TIME), EXP, pch = ".", cex=1, col = Col.HR, ylim=c(0, E.max+20),
  #                          xlab = "Time", ylab = paste(toupper(pol), "concentration (µg/m³)"),
  #                          main = paste(Active.Subprofile$FullName, ":", IND.amount, "out of", length(PPH.P), "individuals")))
  # 
  # axis.POSIXct(1, at = ST.DF.HR_F$TIME, labels = format(ST.DF.HR_F$TIME,"%b-%d"), las=2)
  # 
  # plot(ST.DF.HR_F$TIME, ST.DF.HR_F$EXP)
  # 
  # with (OW_C2.ST.DF.HR, plot(TIME, EXP, pch = ".", cex=1, col = Col.HR, ylim=c(0, E.max+20),
  #                           xlab = "Time", ylab = paste(toupper(pol), "Biweekly concentration (µg/m³)"),
  #                           main = paste(Active.Subprofile$Type, ":", IND.amount, "out of", length(ExposureValue.P), "individuals")))
  # 
  
  points(ST.DF.S.sub$TIME, ST.DF.S.sub[,WhichCol], pch = "-", cex=1, col = Col.S)
  points(ST.DF.T1.sub$TIME, ST.DF.T1.sub[,WhichCol], pch = ".", cex=1, col = Col.T1)
  points(ST.DF.T2.sub$TIME, ST.DF.T2.sub[,WhichCol], pch = ".", cex=1, col = Col.T2)
  
  mtext(paste(head(ST.DF.P.sub$TIME,1), "-", tail(ST.DF.P.sub$TIME,1) + 0.001))
  # mtext(paste(head(ST.DF.HR.sub$TIME,1), "-", tail(ST.DF.HR.sub$TIME,1) + 0.001))
  
  if (PlotMinMax)
  {
    #add mean, min and max to plot
    points(as.POSIXct(stats.EXP.P$TIME), stats.EXP.P$meanEXP, col = "orange", pch = "-", cex = 1)
    points(as.POSIXct(stats.EXP.HR$TIME), stats.EXP.HR$meanEXP, col = "orange", pch = ".", cex = 1) 
    points(as.POSIXct(stats.EXP.HR.sub$TIME), stats.EXP.HR.sub$meanEXP, col = "orange", pch = "-", cex = 1)
    
    points(as.POSIXct(stats.EXP.P$TIME), stats.EXP.P$minEXP, col = "white", pch = 24, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.75)
    points(as.POSIXct(stats.EXP.P$TIME), stats.EXP.P$maxEXP, col = "white", pch = 25, bg = rgb(red=0, green=0.5, blue=0.5), cex=0.75)
  }
  
  # legend("top", c("Individual exposure value","Mean"), xpd = FALSE, horiz = TRUE, inset = c(0,0),
  #        bty = "n", pch = c("-","-"), col = c(Col.P,"orange"), cex = 1)
  # 
  # 
  # #abline(v=as.POSIXct("2009-03-29 02:00:00", origin = "1970-01-01"), col = "orange") # start summertime / Daylight saving time (DST)
  # #abline(v=as.POSIXct("2009-10-25 02:00:00", origin = "1970-01-01"), col = "grey") # start wintertime / Standard time
  # abline(v = as.POSIXct(unlist(PHASES[[1]]), origin = "1970-01-01"), col = Col.T2)
  # 
  # #abline(v = Time, col = "grey") # hours
  # abline(v = YearDates, col = "grey") # days
  # 
  # abline(v = unlist(PHASES[[1]][YearDates %in% BusinesDates][2]), col = Col.T1) # T1
  # 
  # 
  # Sel = which(YearDates %in% BusinesDates) < DAY.start+DAYS & which(YearDates %in% BusinesDates) >= DAY.start
  # for (h in (PHASES[[1]][YearDates %in% BusinesDates][Sel]))
  # {
  #   abline(v = h[2], col = "red") # Leave Primary
  #   abline(v = h[4], col = "blue") # Leave Secondary
  # }
  # 
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
