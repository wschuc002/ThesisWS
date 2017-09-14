# Module for calculating the spatial and/or temporal mean for the specified municipality.
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
list.of.packages <- c("rgdal", "raster", "sp", "geoR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(rgdal)
library(raster)
library(sp)
library(geoR)

PointsPerMunicipality <- function(pol, Points.NoVal, PolDir, ...)
{
  # ## Read base
  # #pol = pollutants[1]
  # if (exists("DriveLetter"))
  # {
  #   BASEAQ = BaseAQnetwork(pol, ExternalDrive = TRUE, DriveLetter = DriveLetter)
  # } else
  # {
  #   BASEAQ = BaseAQnetwork(pol, ExternalDrive = FALSE)
  # }
  # 
  # Points.NoVal = BASEAQ[[1]]
  # PolDir = BASEAQ[[2]]
  
  # Create search trees
  tree = createTree(coordinates(Points.NoVal))
  
  # Check which RIO-IFDM points belong to each municipality
  
  # Read municipalities
  Municipalities = getData("GADM", country = "Belgium", level = 4, path = output.dir)
  Municipalities = spTransform(Municipalities, BE_crs)
  Municipalities@proj4string = BE_crs
  
  Municipalities.Name4only = Municipalities
  Municipalities.Name4onlyDF = subset(Municipalities@data, select = c("OBJECTID","NAME_4"))
  Municipalities.Name4only@data = Municipalities.Name4onlyDF
  
  RIOIFDM_Mun = Points.NoVal %over% Municipalities.Name4only
  
  #plot(Points.NoVal[RIOIFDM_Mun$NAME_4 %in% "Antwerpen",])
  #lines(Municipalities[Municipalities$NAME_4 %in% "Antwerpen",], col = "green")
  
  RIOIFDM_Mun.PointNumbers = which(RIOIFDM_Mun$NAME_4 %in% "Antwerpen")
  
  Municipality.RIO_IFDM.Li = list()
  for (m in seq_along(Municipalities.Name4only$NAME_4))
  {
    print(Municipalities.Name4only$NAME_4[m])
    Municipality.RIO_IFDM.Li[[m]] = which(RIOIFDM_Mun$NAME_4 %in% Municipalities.Name4only$NAME_4[m])
  }
  
  return(Municipality.RIO_IFDM.Li)
}


# wP = HoP.P_F
# wS = HoP.S_F
# wT1 = HoP.T1_F
# wT2 = HoP.T2_F
# seq = SeqFragment[f]
# POL = Points.NoVal
# HOURS.P = HOURS.P_F
# HOURS.S = HOURS.S_F
# HOURS.T1 = HOURS.T1_F
# HOURS.T2 = HOURS.T2_F
# PPH.T1.Pnt = PPH.T1.PNT.RS
# PPH.T2.Pnt = PPH.T2.PNT.RS

MeanMunicipality <- function(PPH.P, PPH.S, PPH.T1.Pnt, PPH.T2.Pnt, POL, PolDir, Plot,
                             pol, StartHour = 1, EndHour = length(YearDates)*24,
                             HOURS.P, HOURS.S, HOURS.T1, HOURS.T2,
                             wP, wS, wT1, wT2, Active.Subprofile, seq,
                             Municipalities, Municipality.RIO_IFDM.Li,
                             Include_P = TRUE, Include_S = TRUE,
                             Include_T1 = TRUE, Include_T2 = TRUE, ...)
{
  start.time = Sys.time()
  
  txt.dr = ExtractBZ2(pol, PolDir, StartHour, EndHour)
  
  # Create tree
  tree = createTree(coordinates(POL))
  
  EXP.P.Li = list()
  EXP.S.Li = list()
  EXP.T1.Li = list()
  EXP.T2.Li = list()
  
  # Prepare lists
  for (i in seq_along(PPH.P))
  {
    EXP.P.Li[[i]] = HOURS.P[[i]]
    if (Active.Subprofile$Dynamics == "dynamic")
    {
      EXP.S.Li[[i]] = HOURS.S[[i]]
      EXP.T1.Li[[i]] = HOURS.T1[[i]]
      EXP.T2.Li[[i]] = HOURS.T2[[i]]
    }
    
    for (d in seq_along(HOURS.P[[i]]))
    {
      EXP.P.Li[[i]][[d]][EXP.P.Li[[i]][[d]] > 0] = NA
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        EXP.S.Li[[i]][[d]][EXP.S.Li[[i]][[d]] > 0] = NA
        EXP.T1.Li[[i]][[d]][EXP.T1.Li[[i]][[d]] > 0] = NA
        EXP.T2.Li[[i]][[d]][EXP.T2.Li[[i]][[d]] > 0] = NA
      }
    }
  }
  
  for (h in seq_along(StartHour:EndHour))
    #for (h in unlist(HOURS.P[[1]])[unlist(HOURS.P[[1]]) >= StartHour &
    #                                 unlist(HOURS.P[[1]]) <= EndHour])
    #for (h in 1:1)
  {
    hr = h+StartHour-1
    day = ceiling(hr/24)
    dayS = day-(ceiling(StartHour/24)-1)
    dayS2 = day-seq
    
    cat("\n")
    print(paste0("Series Hour ", h))
    print(paste0("Year Hour ", hr))
    print(paste0("Year Day ", day))
    print(paste0("Series Day ", dayS))
    print(paste0("Series2 Day ", dayS2))
    print(txt.dr[h])
    print(paste0("Progress current fragment: ", h/length(StartHour:EndHour)*100, "%"))
    if (exists("f")) {print(paste0("Progress current subtype: ", hr/length(Time)*100, "%"))}
    
    POL.h = fread(txt.dr[h], sep=";", header=TRUE, select = "values")
    POL@data = POL.h
    
    # create empty temp DF per municipality per hour
    TempDF = data.frame(cbind(Municipalities[1,]@data$OBJECTID, Municipalities[1,]@data$NAME_4, POL.h[1,]))
    colnames(TempDF) = c("ObjectID","Municipality", "Value")
    TempDF[1:length(Municipalities),] = NA
    
    for (i in seq_along(PPH.P))
      #for (i in 42)
    {
      cat(paste(i," "))
      
      if (Include_P)
      {
        if (length(wP[[i]][[h]]) > 0)
        {
          # select municipality points
          MuniID = (PPH.P[i,] %over% Municipalities)$OBJECTID
          
          # if not inside municipaliy: use closest one
          if (is.na(MuniID))
          {
            MuniID = which.min(gDistance(Municipalities, PPH.P[i,], byid = TRUE))
          }
          
          # check if already exists
          if (is.na(TempDF[MuniID,1]))
          {
            POL.sel = POL[Municipality.RIO_IFDM.Li[[MuniID]],]
            
            # calculate mean of the municipality
            Exp.P = mean(POL[Municipality.RIO_IFDM.Li[[MuniID]],]@data$values, na.rm = TRUE)
            
            # put it in temp file
            TempDF[MuniID,1] = MuniID
            TempDF[MuniID,2] = Municipalities[MuniID,]@data$NAME_4
            TempDF[MuniID,3] = Exp.P
            
            if (Plot) {plot(POL.sel)}
            
          } else
          {
            Exp.P = TempDF[MuniID,3]
          }
          
          if (Plot)
          {
            points(PPH.P[i,], col = 11, font = 2, pch = 19)
            text(PPH.P[i,], labels = round(Exp.P,3), pos = 1, cex = 1, col = 11, font = 2)
            # spplot(POL[Municipality.RIO_IFDM.Li[[MuniID]],], "values")
          }
          
          EXP.P.Li[[i]][[dayS]][wP[[i]][[h]]] = Exp.P
        }
      } # closing Primary
      
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        if (Include_S)
        {
          if (length(wS[[i]][[h]]) > 0 & Include_S)
          {
            # select municipality points
            MuniID = (PPH.S[i,] %over% Municipalities)$OBJECTID
            
            # if not inside municipaliy: use closest one
            if (is.na(MuniID))
            {
              MuniID = which.min(gDistance(Municipalities, PPH.S[i,], byid = TRUE))
            }
            
            # check if already exists
            if (is.na(TempDF[MuniID,1]))
            {
              POL.sel = POL[Municipality.RIO_IFDM.Li[[MuniID]],]
              
              # calculate mean of the municipality
              Exp.S = mean(POL[Municipality.RIO_IFDM.Li[[MuniID]],]@data$values, na.rm = TRUE)
              
              # put it in temp file
              TempDF[MuniID,1] = MuniID
              TempDF[MuniID,2] = Municipalities[MuniID,]@data$NAME_4
              TempDF[MuniID,3] = Exp.S
              
              if (Plot) {plot(POL.sel)}
              
            } else
            {
              Exp.S = TempDF[MuniID,3]
            }
            
            if (Plot)
            {
              points(PPH.S[i,], col = 11, font = 2, pch = 19)
              text(PPH.S[i,], labels = round(Exp.S,3), pos = 1, cex = 1, col = 11, font = 2)
              # spplot(POL[Municipality.RIO_IFDM.Li[[MuniID]],], "values")
            }
            
            EXP.S.Li[[i]][[dayS]][wS[[i]][[h]]] = Exp.S
          } 
        } # closing Secondary
        
        if (Include_T1)
        {
          if (length(wT1[[i]][[h]]) > 0)
          {
            for (v in seq_along(wT1[[i]][[h]]))
            {
              # select municipality points
              CoordsOfInterest = PPH.T1.Pnt[[i]][[dayS2]][v,]
              MuniID = (CoordsOfInterest %over% Municipalities)$OBJECTID
              
              # if not inside municipaliy: use closest one
              if (is.na(MuniID))
              {
                MuniID = which.min(gDistance(Municipalities, CoordsOfInterest, byid = TRUE))
              }
              
              # check if already exists
              if (is.na(TempDF[MuniID,1]))
              {
                POL.sel = POL[Municipality.RIO_IFDM.Li[[MuniID]],]
                
                # calculate mean of the municipality
                Exp.T1 = mean(POL[Municipality.RIO_IFDM.Li[[MuniID]],]@data$values, na.rm = TRUE)
                
                # put it in temp file
                TempDF[MuniID,1] = MuniID
                TempDF[MuniID,2] = Municipalities[MuniID,]@data$NAME_4
                TempDF[MuniID,3] = Exp.T1
                
                if (Plot) {plot(POL.sel)}
                
              } else
              {
                Exp.T1 = TempDF[MuniID,3]
              }
              
              if (Plot)
              {
                points(CoordsOfInterest, col="purple", font = 2, pch = 19)
                text(CoordsOfInterest, labels = round(Exp.T1,3), pos = 1, cex = 1, font = 2, col = "purple")
              }
              
              EXP.T1.Li[[i]][[dayS]][wT1[[i]][[h]][v]] = Exp.T1
            }
            
          }
        } # closing T1
  
        if (Include_T2)
        {
          if (length(wT2[[i]][[h]]) > 0)
          {
            for (v in seq_along(wT2[[i]][[h]]))
            {
              # select municipality points
              CoordsOfInterest = PPH.T2.Pnt[[i]][[dayS2]][v,]
              MuniID = (CoordsOfInterest %over% Municipalities)$OBJECTID
              
              # if not inside municipaliy: use closest one
              if (is.na(MuniID))
              {
                MuniID = which.min(gDistance(Municipalities, CoordsOfInterest, byid = TRUE))
              }
              
              # check if already exists
              if (is.na(TempDF[MuniID,1]))
              {
                POL.sel = POL[Municipality.RIO_IFDM.Li[[MuniID]],]
                
                # calculate mean of the municipality
                Exp.T2 = mean(POL[Municipality.RIO_IFDM.Li[[MuniID]],]@data$values, na.rm = TRUE)
                
                # put it in temp file
                TempDF[MuniID,1] = MuniID
                TempDF[MuniID,2] = Municipalities[MuniID,]@data$NAME_4
                TempDF[MuniID,3] = Exp.T2
                
                if (Plot) {plot(POL.sel)}
                
              } else
              {
                Exp.T2 = TempDF[MuniID,3]
              }
              
              if (Plot)
              {
                points(CoordsOfInterest, col="purple", font = 2, pch = 19)
                text(CoordsOfInterest, labels = round(Exp.T2,3), pos = 1, cex = 1, font = 2, col = "purple")
              }
              
              EXP.T2.Li[[i]][[dayS]][wT2[[i]][[h]][v]] = Exp.T2
            }
            
          }
        } # closing T2
        
      } # closing dynamics
      
    } # closing i
  } # closing h
  
  if (exists("EXP.P.Li")) {EXP.P = EXP.P.Li} else {EXP.P = NA}
  if (exists("EXP.S.Li")) {EXP.S = EXP.S.Li} else {EXP.S = NA}
  if (exists("EXP.T1.Li")) {EXP.T1 = EXP.T1.Li} else {EXP.T1 = NA}
  if (exists("EXP.T2.Li")) {EXP.T2 = EXP.T2.Li} else {EXP.T2 = NA}
  
  end.time = Sys.time()
  Duration = difftime(end.time, start.time, units = 'hours')
  
  return(list(EXP.P, EXP.S, EXP.T1, EXP.T2, Duration))
}
  
MeanMunicipalityTest <- function(PPH.P, ...)
{
  ## Read base
  #pol = pollutants[1]
  if (exists("DriveLetter"))
  {
    BASEAQ = BaseAQnetwork(pol, ExternalDrive = TRUE, DriveLetter = DriveLetter)
  } else
  {
    BASEAQ = BaseAQnetwork(pol, ExternalDrive = FALSE)
  }
  
  Points.NoVal = BASEAQ[[1]]
  PolDir = BASEAQ[[2]]
  
  # Create search trees
  tree = createTree(coordinates(Points.NoVal))
  
  # Check which RIO-IFDM points belong to each municipality
  
  # Read municipalities
  Municipalities = getData("GADM", country = "Belgium", level = 4, path = output.dir)
  Municipalities = spTransform(Municipalities, BE_crs)
  Municipalities@proj4string = BE_crs
  

  
  # Check which municipalities intersect with PPH
  # PPH.P_Mun = PPH.P %over% Municipalities
  # PPH.S_Mun = PPH.S %over% Municipalities
  # PPH.T1_Mun = PPH.T1 %over% Municipalities
  
  Municipalities.Name4only = Municipalities
  Municipalities.Name4onlyDF = subset(Municipalities@data, select = c("OBJECTID","NAME_4"))
  Municipalities.Name4only@data = Municipalities.Name4onlyDF
  
  RIOIFDM_Mun = Points.NoVal %over% Municipalities.Name4only
  
  plot(Points.NoVal[RIOIFDM_Mun$NAME_4 %in% "Antwerpen",])
  lines(Municipalities[Municipalities$NAME_4 %in% "Antwerpen",], col = "green")

  RIOIFDM_Mun.PointNumbers = which(RIOIFDM_Mun$NAME_4 %in% "Antwerpen")
  
  Municipality.RIO_IFDM.Li2 = list()
  for (m in seq_along(Municipalities.Name4only$NAME_4))
  {
    print(Municipalities.Name4only$NAME_4[m])
    Municipality.RIO_IFDM.Li2[[m]] = which(RIOIFDM_Mun$NAME_4 %in% Municipalities.Name4only$NAME_4[m])
  }
  
  
  
  
  for (p in seq_along(PPH.P))
  {
    if (is.na(PPH.P_Mun$NAME_4[p]))
    {
      Closest = which.min(gDistance(Municipalities, PPH.P[p,], byid=TRUE))
      PPH.P_Mun[p,] = Municipalities@data[Closest,]
    }
    if (is.na(PPH.S_Mun$NAME_4[p]))
    {
      Closest = which.min(gDistance(Municipalities, PPH.S[p,], byid=TRUE))
      PPH.S_Mun[p,] = Municipalities@data[Closest,]
    }
    
  }
  
  
  plot(PPH.T1, col= "green")
  points(PPH.P[295,], col= "red")
  
  lines(Municipalities)
  lines(Municipalities[unique(PPH.T1_Mun$OBJECTID),], col = "red")
  
  #Municipality.RIO_IFDM.Li = list()
  #for (m in seq_along(Municipalities$NAME_4))
  for (m in (340:length(Municipalities$NAME_4)))
  {
    cat("\n")
    #Name.Municipality = "Oudergem"
    Name.Municipality = Municipalities$NAME_4[m]
    
    print(paste0(m, "/", length(Municipalities)))
    cat(paste(Name.Municipality," "))
    Municipality = Municipalities[Municipalities@data$NAME_4 == Name.Municipality,]
    Municipality = spTransform(Municipality, BE_crs)
    Municipality@proj4string = BE_crs
    #plot(Municipality)
    
    
    # # Subset Base RIO-IFDM on Municipality
    # points.sel = 1000
    # Points.Base.Samples = gIntersection(Points.NoVal[1:points.sel,], Municipality,  byid = TRUE)
    # 
    # while (is.null(Points.Base.Samples))
    # {
    #   points.sel = points.sel + 1000
    #   Points.Base.Samples = gIntersection(Points.NoVal[(points.sel-1000):points.sel,], Municipality,  byid = TRUE)
    # }
    # points(Points.Base.Samples)
    
    # find most central points
    # X.min = Points.Base.Samples@bbox[1,1]
    # X.max = Points.Base.Samples@bbox[1,2]
    # Y.min = Points.Base.Samples@bbox[2,1]
    # Y.max = Points.Base.Samples@bbox[2,2]
    # X.mean = (X.max + X.min) / 2
    # Y.mean = (Y.max + Y.min) / 2
    # Central.Point = SpatialPoints(coords = cbind(X.mean, Y.mean), proj4string = BE_crs)
    Central.Point2 = gCentroid(Municipality)
    # points(Central.Point, col = "red")
    #points(Central.Point2, col = "blue")
    
    # Radius = max(X.max-X.mean, Y.max-Y.mean)
    
    Radmax = SpatialPoints(data.frame(Municipality@bbox[1,2], Municipality@bbox[2,2]) , proj4string = BE_crs)
    Radmax@proj4string = BE_crs
    #points(Radmax)
    
    Rad = gDistance(Central.Point2, Radmax)
    
    radsearch = as.numeric(knnLookup(tree, newdat = coordinates(Central.Point2), k = 3*Rad))
    #plot(Points.NoVal[radsearch,])
    #lines(Municipality, col = "green")
    
    
    Points.Base.Municipality = gIntersects(Points.NoVal[radsearch,], Municipality,  byid = TRUE)
    #plot(Points.NoVal[rownames(Points.NoVal@data) %in% Points.Base.Municipality,])
    
    Municipality.RIO_IFDM.Li[[m]] = radsearch[Points.Base.Municipality]
    
    #plot(Points.NoVal[radsearch[Points.Base.Municipality],])
  }
  
  
  
  
  
  
  
  Municipality.RIO_IFDM = Points.NoVal[radsearch[Points.Base.Municipality],]
  
  plot(Municipality.RIO_IFDM)
  
  class(radsearch[Points.Base.Municipality])
  
  
  
  
  Municipality.RIO_IFDM.Li$Municipalities$NAME_4[1] = c(1,2,3,4)
  
  length(Points.NoVal[radsearch,])
  length(Points.Base.Municipality.Buffer)
  
  rownames(Points.NoVal[radsearch,]@data) %in% colnames(Points.Base.Municipality.Buffer)
  
  Points.Base.Municipality.Buffer.TRUE = NA
  for (f in seq_along(Points.Base.Municipality.Buffer))
  {
    if (Points.Base.Municipality.Buffer[f])
    {
      Points.Base.Municipality.Buffer.TRUE[f] = as.numeric(colnames(Points.Base.Municipality.Buffer)[f])
    }
  }
  Points.Base.Municipality.Buffer.TRUE = Points.Base.Municipality.Buffer.TRUE[!is.na(Points.Base.Municipality.Buffer.TRUE)]
  plot(Points.NoVal[Points.Base.Municipality.Buffer.TRUE,])
  Points.Municipality = Points.NoVal[Points.Base.Municipality.Buffer.TRUE,]
  
  
  
  txt.dr = ExtractBZ2(pol, PolDir, StartHour, EndHour)
  
  POL = Points.NoVal
  
  
  boxCoordX <- seq(from = round(bbox(Points.Municipality)[1,1], -2),
                   to = round(bbox(Points.Municipality)[1,2], -2),
                   by = mpp)
  boxCoordY <- seq(from = round(bbox(Points.Municipality)[2,1], -2),
                   to =  round(bbox(Points.Municipality)[2,2], -2),
                   by = mpp)
  GridPol.Li = list()
  
  for (p in 1:length(Municipality@polygons[[1]]@Polygons))
  {
    GridPol = polygrid(boxCoordX, boxCoordY, Municipality@polygons[[1]]@Polygons[[p]]@coords, vec.inout = FALSE)
    GridPol.Li[[p]] = GridPol
  }
  GridPolWS = do.call(rbind, GridPol.Li)
  GridPol.SP = GridPolWS
  coordinates(GridPol.SP) = ~x+y
  GridPol.SP@proj4string = BE_crs
  plot(GridPol.SP)
  
  GridPol.SPDF = SpatialPointsDataFrame(GridPol.SP, data = data.frame(1:(length(GridPol.SP))))
  SaveAsFile(GridPol.SPDF, paste(Name.Municipality, paste0(mpp,"x",mpp),sep = "_"), "GeoJSON", TRUE)
  
  
  for (h in seq_along(StartHour:EndHour))
  {
    hr = StartHour+h-1
    
    POL.h = fread(txt.dr[h], sep=";", header=TRUE, select = "values")
    
    POL.h.Sel = POL.h[Points.Base.Municipality.Buffer.TRUE,]
    # Points.Municipality@data$pol = NA
    # Points.Municipality@data$pol = POL.h$values
    
    if (nrow(POL.h.Sel) == nrow(Points.Municipality))
    {
      WS = NA
      for (p in seq_along(GridPol.SP))
      {
        pnt = as.numeric(knnLookup(tree, newdat = coordinates(GridPol.SP[p]), k = 50))
        POL.sel = POL[pnt,]
        POL.sel@data$values = POL.h$values[pnt]
        #spplot(POL.sel, "values")
        
        
        WS[p] = unlist(akima::interp(x = POL.sel@coords[,1], y = POL.sel@coords[,2], z = POL.sel@data$values,
                                     xo = GridPol.SP@coords[,1][p], yo = GridPol.SP@coords[,2][p], extrap = FALSE,
                                     duplicate = "strip", linear = TRUE))[3]
      }
      GridPol.SPDF@data$pol = NA
      GridPol.SPDF@data$pol = WS
      #spplot(GridPol.SPDF, "pol")
      
      sgrid = sp::SpatialPointsDataFrame(coords = coordinates(GridPol.SPDF),
                                         data = GridPol.SPDF@data[2],
                                         proj4string = BE_crs)
      sp::gridded(sgrid) = TRUE
      
      r = raster::raster(sgrid)
      r = raster::rasterize(sgrid, r, field = sgrid@data$pol, na.rm = TRUE, fun = mean)
      plot(r)
      
      ma = raster::mask(r, Municipality)
      plot(ma)
      
      SaveAsFile(ma, paste(Name.Municipality, paste0(mpp,"x",mpp), pol, hr, sep = "_"), "GeoTIFF", TRUE)
    } # close test
    
  } # closing h
  
}
