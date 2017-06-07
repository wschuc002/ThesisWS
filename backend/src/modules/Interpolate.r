# Module for interpolating air quality maps
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
list.of.packages <- c("sp","akima", "SearchTrees")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)
library(akima)
library(SearchTrees)

#library(geoR)

#PPH = PPH.T2[42,]
#POL = Points.NoVal
#HOURS = HOURS.T2
#Plot = TRUE
#StartHour = (5-1)*24 + 1
#EndHour = 6*24
#NearestPoints = 50
#PPH.T1.Pnt = PPH.T1.PNT.RS
#PPH.T2.Pnt = PPH.T2.PNT.RS

PPH.TIN.InterpolationWS <- function(PPH.P, PPH.S, PPH.T1.Pnt, PPH.T2.Pnt, POL, PolDir, Plot,
                                    pol, StartHour = 1, EndHour = length(YearDates)*24,
                                    HOURS.P, HOURS.S, HOURS.T1, HOURS.T2, NearestPoints,
                                    wP, wS, wT1, wT2, ...)
{
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
  #for (h in 1:1)
  {
    hr = h+StartHour-1
    day = ceiling(hr/24)
    print(paste0("Series Hour ", h))
    print(paste0("Year Hour ", hr))
    print(paste0("Day ", day))
    print(txt.dr[h])

    POL.h = fread(txt.dr[h], sep=";", header=TRUE, select = "values")
    POL@data = POL.h
    
    for (i in seq_along(PPH.P))
    #for (i in 42)
    {
      cat(paste(i," "))

      if (length(wP[[i]][[hr]]) > 0)
      {
        # select proximity coordinates
        inds = knnLookup(tree, newdat = coordinates(PPH.P[i,]), k = NearestPoints) # gives the matrix
        inds = as.vector(inds)
        
        POL.sel = POL[inds,]
        
        # do the TIN interpolation
        Exp.P = unlist(akima::interp(x = POL.sel@coords[,1], y = POL.sel@coords[,2], z = unlist(POL.sel@data[,1]),
                                           xo = PPH.P[i,]@coords[,1], yo = PPH.P[i,]@coords[,2], extrap = FALSE, duplicate = "strip",
                                     linear = TRUE))[3]
        if (Plot == TRUE)
        {
          plot(POL.sel)
          text(POL.sel, labels = round( unlist(POL.sel@data[,1]),3), pos = 3, cex = 0.6)
          points(PPH.P[i,], col = 11, font = 2, pch = 19)
          text(PPH.P[i,], labels = round(Exp.P,3), pos = 1, cex = 1, col = 11, font = 2)
        }
        
        EXP.P.Li[[i]][[day]][wP[[i]][[hr]]] = Exp.P
      }
      
      if (Active.Subprofile$Dynamics == "dynamic")
      {
        if (length(wS[[i]][[hr]]) > 0)
        {
          # select proximity coordinates
          inds = knnLookup(tree, newdat = coordinates(PPH.S[i,]), k = NearestPoints) # gives the matrix
          inds = as.vector(inds)
          
          POL.sel = POL[inds,]
          
          # do the TIN interpolation
          Exp.S = unlist(akima::interp(x = POL.sel@coords[,1], y = POL.sel@coords[,2], z = unlist(POL.sel@data[,1]),
                                       xo = PPH.S[i,]@coords[,1], yo = PPH.S[i,]@coords[,2], extrap = FALSE, duplicate = "strip",
                                       linear = TRUE))[3]
          if (Plot == TRUE)
          {
            plot(POL.sel)
            text(POL.sel, labels = round( unlist(POL.sel@data[,1]),3), pos = 3, cex = 0.6)
            points(PPH.S[i,], col = "orange", font = 2, pch = 19)
            text(PPH.S[i,], labels = round(Exp.S,3), pos = 1, cex = 1, col = "orange", font = 2)
          }
          
          EXP.S.Li[[i]][[day]][wS[[i]][[hr]]] = Exp.S
        }
        
        if (length(wT1[[i]][[hr]]) > 0)
        {
          day = ceiling(hr/24)
          if (Plot == TRUE){ plot(POL[as.vector(knnLookup(tree, newdat = coordinates(PPH.T1.Pnt[[i]][[day]]), k = NearestPoints)),]) }
          
          for (v in seq_along(wT1[[i]][[hr]]))
          {
            # select proximity coordinates
            CoordsOfInterest = PPH.T1.Pnt[[i]][[day]][v,]@coords
            inds = knnLookup(tree, newdat = CoordsOfInterest, k = NearestPoints)
            inds = as.vector(inds)
            
            POL.sel = POL[inds,]
            
            # do the TIN interpolation
            Exp.T1 = unlist(akima::interp(x = POL.sel@coords[,1], y = POL.sel@coords[,2], z = unlist(POL.sel@data[,1]),
                                          xo = CoordsOfInterest[1], yo = CoordsOfInterest[2], extrap = FALSE, duplicate = "strip",
                                          linear = TRUE))[3]
            
            if (Plot == TRUE)
            {
              #plot(POL.sel)
              #               
              #               text(POL.sel, labels = round( unlist(POL.sel@data[,1]),3), pos = 3, cex = 0.6)
              points(CoordsOfInterest, col="purple", font = 2, pch = 19)
              text(CoordsOfInterest, labels = round(Exp.T1,3), pos = 1, cex = 1, font = 2, col = "purple")
            }
            
            EXP.T1.Li[[i]][[day]][wT1[[i]][[hr]][v]] = Exp.T1
            
            
          }
          
        }
        
        if (length(wT2[[i]][[hr]]) > 0)
        {
          day = ceiling(hr/24)
          if (Plot == TRUE){ plot(POL[as.vector(knnLookup(tree, newdat = coordinates(PPH.T2.Pnt[[i]][[day]]), k = NearestPoints)),]) }
          
          for (v in seq_along(wT2[[i]][[hr]]))
          {
            # select proximity coordinates
            CoordsOfInterest = PPH.T2.Pnt[[i]][[day]][v,]@coords
            inds = knnLookup(tree, newdat = CoordsOfInterest, k = NearestPoints)
            inds = as.vector(inds)
            
            POL.sel = POL[inds,]
            
            # do the TIN interpolation
            Exp.T2 = unlist(akima::interp(x = POL.sel@coords[,1], y = POL.sel@coords[,2], z = unlist(POL.sel@data[,1]),
                                          xo = CoordsOfInterest[1], yo = CoordsOfInterest[2], extrap = FALSE, duplicate = "strip",
                                          linear = TRUE))[3]
            
            if (Plot == TRUE)
            {
              #             text(POL.sel, labels = round( unlist(POL.sel@data[,1]),3), pos=3, cex = 0.4)
              points(CoordsOfInterest, col = "blue", font = 2, pch = 19)
              text(CoordsOfInterest, labels = round(Exp.T2,3), pos = 1, cex = 1, font = 2, col = "blue")
            }
            
            EXP.T2.Li[[i]][[day]][wT2[[i]][[hr]][v]] = Exp.T2
          }
          
        }
      } # closing dynamics
      
    } # closing i
  } # closing h
  
  return(list(EXP.P.Li, EXP.S.Li, EXP.T1.Li, EXP.T2.Li))
}

#PPH = PPH.P
#POL = Points.AoI.NoDup
#POL = Points.AoI_test
#value = "CON_20150101_19_NO2"
#Plot = TRUE

PPH.TIN.Interpolation <- function(PPH, POL, Plot, ...)
{
  #if (Plot == TRUE){plot(Flanders)}
  EXP.Li = list()
  for (i in seq_along(PPH))
    #for (i in 1:10)  
  {
    # select proximity coordinates
    tree = createTree(coordinates(POL))
    inds = knnLookup(tree, newdat=coordinates(PPH[i,]), k = 50) # gives the matrix
    inds = as.vector(inds)
    
    POL.p = POL[inds,]
    
    if (Plot == TRUE)
    {
      plot(POL.p)
      points(PPH[i,], col="green")
    }
    
    # do the TIN interpolation
    EXP.Li[[i]] = unlist(interp(x = POL.p@coords[,1], y = POL.p@coords[,2], z = POL.p@data[,2],
                                xo = PPH[i,]@coords[,1], yo = PPH[i,]@coords[,2], extrap = F, duplicate = "strip"))[3]
    
  }
  EXP = do.call(rbind, EXP.Li)
  
  return(EXP)
}

# SPDF = Points.AoI.NoDup
# AoI = AoI
# value = "CON20150101_19_NO2"
# dmax = 100
# mpp = 10
# dup = "error"
PointsToRasterTIN <- function(SPDF, value, AoI, dmax, mpp, dup, ...)
{
  WS = interp(x = SPDF@coords[,1], y = SPDF@coords[,2], z = SPDF@data[,colnames(SPDF@data) == value], #SPDF@data$values
              xo = PPH.S@coords[,1], yo = PPH.S@coords[,2], duplicate = dup, extrap = FALSE)
  
  WS$z
  
  
  
  boxCoordX <- seq(from = round(bbox(SPDF)[1,1], -2) - dmax,
                   to = round(bbox(SPDF)[1,2], -2) + dmax,
                   by = mpp)
  boxCoordY <- seq(from = round(bbox(SPDF)[2,1], -2) - dmax,
                   to =  round(bbox(SPDF)[2,2], -2) + dmax,
                   by = mpp)
  
  GridPol.Li = list()
  for (p in 1:length(AoI@polygons[[1]]@Polygons))
  {
    GridPol = polygrid(boxCoordX, boxCoordY, AoI@polygons[[1]]@Polygons[[p]]@coords, vec.inout = FALSE)
    GridPol.Li[[p]] = GridPol
  }
  GridPolWS = do.call(rbind, GridPol.Li)
  GridPol.SP = GridPolWS
  coordinates(GridPol.SP) = ~x+y
  GridPol.SP@proj4string = BE_crs
  plot(GridPol.SP)
  
  GridPol.SPDF = SpatialPointsDataFrame(GridPol.SP, data = data.frame(1:(length(GridPol.SP))))
  SaveAsFile(GridPol.SPDF, paste("GridPol.SP", paste0(mpp,"x",mpp),sep = "_"), "GeoJSON", TRUE)
  
  
  #sgridDS.SPDF = GridDownScaler(SPDF, AoI, dmax, mpp)
  #SaveAsFile(sgridDS.SPDF, paste("sgridDS", paste0(mpp,"x",mpp),sep = "_"), "GeoJSON", TRUE)
  
  WS = interp(x = SPDF@coords[,1], y = SPDF@coords[,2], z = SPDF@data[,colnames(SPDF@data) == value], #SPDF@data$values
              xo = GridPol.SP@coords[,1], yo = GridPol.SP@coords[,2], duplicate = dup, extrap = FALSE)
  
  return(WS)
  
  sgrid.SPDF = GridPol.SPDF
  sgrid.SPDF@data = cbind(sgrid.SPDF@data, as.numeric(WS$z))
  colnames(sgrid.SPDF@data)[2] = value
  plot(sgrid.SPDF)
  spplot(sgrid.SPDF, colnames(sgrid.SPDF@data)[2])
  
  WS = interp(x = SPDF@coords[,1], y = SPDF@coords[,2], z = SPDF@data[,colnames(SPDF@data) == value], #SPDF@data$values
              xo = boxCoordX, yo = boxCoordY, duplicate = dup, extrap = FALSE)
  idSeq <- seq(1, nrow(sgrid), 1)
  sgrid <- data.frame(ID = idSeq,
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2],
                      NO2 = as.numeric(WS$z))
  sgrid <- sp::SpatialPointsDataFrame(coords = sgrid[ , c(2, 3)],
                                      data = sgrid,
                                      proj4string = BE_crs)
  #Sub = as.logical(gIntersects(sgrid, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",], byid = T))
  
  #   test1 = gIntersection(AoI_SPDF, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",])
  #   plot(terror)
  #   
  #   test2 = gIntersection(sgrid, terror)
  #   
  #   Sub2 = gIntersects(sgrid, terror, byid = T)
  
  #sgrid = sgrid[Sub,]
  #plot(sgrid)
  #plot(sgrid[Sub,])
  #return(sgrid)
  sp::gridded(sgrid) <- TRUE
  #sgrid = sgrid[Sub,]
  #plot(sgrid)
  r <- raster::raster(sgrid)
  r <- raster::rasterize(sgrid, r, field = sgrid@data$NO2, na.rm=TRUE, fun = mean)
  #plot(r)
  #lines(Municipalities[Municipalities@data$NAAM %in% "Antwerpen",], col = "red")
  #lines(AoI_SPDF, col = "blue")
  #points(SPDF)
  #return(r)
  
  #r = AoI2.Raster
  ma = mask(r, AoI)
  #   plot(ma)
  #   lines(AoI)
  return(ma)
  
}

GridDownScaler <- function(SPDF, AoI, dmax, mpp, ...)
{
  # start with larger grid (100m?) with larger dmax and then fill them in with the 10m grid.
  # then interpolate on the 10m AoI grid
  
  boxCoordX <- seq(from = round(bbox(SPDF)[1,1], -2) - dmax,
                   to = round(bbox(SPDF)[1,2], -2) + dmax,
                   by = mpp)
  boxCoordY <- seq(from = round(bbox(SPDF)[2,1], -2) - dmax,
                   to =  round(bbox(SPDF)[2,2], -2) + dmax,
                   by = mpp)
  
  GridPol = polygrid(boxCoordX, boxCoordY, AoI@polygons[[1]]@Polygons[[1]]@coords, vec.inout = FALSE)
  
  GridPol.SP = GridPol
  coordinates(GridPol.SP) = ~x+y
  GridPol.SP@proj4string = BE_crs
  plot(GridPol.SP)
  
  
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(sgrid), 1)
  sgrid.DF <- data.frame(ID = idSeq, COORDX = sgrid[, 1], COORDY = sgrid[, 2])
  sgrid.SPDF <- sp::SpatialPointsDataFrame(coords = sgrid.DF[ , c(2, 3)],
                                           data = sgrid.DF, proj4string = BE_crs)
  
  sgrid.TF = IntersectsBoolean(sgrid.SPDF, AoI)
  sgrid.SPDF.AoI = sgrid.SPDF[sgrid.TF,]
  plot(sgrid.SPDF.AoI)
  #SaveAsFile(sgrid.SPDF.AoI, paste("Grid_AoI", paste0(mpp*10,"x",mpp*10),sep = "_"), "GeoJSON", TRUE)
  
  sp::gridded(sgrid.SPDF.AoI) <- TRUE # convert to SpatialPixelsDataFrame
  
  sgrid2.Li = list()
  for (p in seq_along(sgrid.SPDF.AoI))
  {
    boxCoordX <- seq(from = bbox(sgrid.SPDF.AoI[p,])[1,1], to = bbox(sgrid.SPDF.AoI[p,])[1,2], by = mpp)
    boxCoordY <- seq(from = bbox(sgrid.SPDF.AoI[p,])[2,1], to = bbox(sgrid.SPDF.AoI[p,])[2,2], by = mpp)
    sgrid2 <- expand.grid(boxCoordX, boxCoordY)
    idSeq2 <- seq(1, nrow(sgrid2), 1)
    sgrid2.DF <- data.frame(ID = idSeq2, COORDX = sgrid2[, 1], COORDY = sgrid2[, 2])
    sgrid2.SPDF <- sp::SpatialPointsDataFrame(coords = sgrid2.DF[ , c(2, 3)],
                                              data = sgrid2.DF, proj4string = BE_crs)
    sgrid2.Li[[p]] = sgrid2.SPDF
  }
  sgrid2WS = do.call(rbind, sgrid2.Li)
  
  # Remove duplicates
  #sgrid2WS.Dups = sgrid2WS[duplicated(sgrid2WS@coords), ]
  sgrid2WS.NoDup = sgrid2WS[!duplicated(sgrid2WS@coords), ]
  
  
  #   # connect points distance == 100 and seq 10m point between them (Tree method?)
  #   tree = createTree(coordinates(sgrid.SPDF.AoI))
  #   inds = knnLookup(tree, newdat=coordinates(sgrid.SPDF.AoI), k = 2) # gives the matrix
  #   
  #   sgrid2.Li = list()
  #   p=1
  #   for (p in seq_along(sgrid.SPDF.AoI))
  #     #for (p in 1:2)
  #   {
  #     if (sgrid.SPDF.AoI@coords[inds[p,1],1] > sgrid.SPDF.AoI@coords[inds[p,2],1])
  #     {
  #       SeqX = seq(from = sgrid.SPDF.AoI@coords[inds[p,1],1], to = sgrid.SPDF.AoI@coords[inds[p,2],1], by = -mpp)
  #     } else {
  #       SeqX = seq(from = sgrid.SPDF.AoI@coords[inds[p,1],1], to = sgrid.SPDF.AoI@coords[inds[p,2],1], by = mpp)
  #     }
  #     
  #     if (sgrid.SPDF.AoI@coords[inds[p,1],2] > sgrid.SPDF.AoI@coords[inds[p,2],2])
  #     {
  #       SeqY = seq(from = sgrid.SPDF.AoI@coords[inds[p,1],2], to = sgrid.SPDF.AoI@coords[inds[p,2],2], by = -mpp)
  #     } else {
  #       SeqY = seq(from = sgrid.SPDF.AoI@coords[inds[p,1],2], to = sgrid.SPDF.AoI@coords[inds[p,2],2], by = mpp)
  #     }
  #     
  #     sgrid2 <- expand.grid(SeqX, SeqY)
  #     
  #     plot(sgrid2)
  #     
  #     idSeq2 <- seq(1, nrow(sgrid2), 1)
  #     sgrid2.DF <- data.frame(ID = idSeq2, COORDX = sgrid2[, 1], COORDY = sgrid2[, 2])
  #     sgrid2.SPDF <- sp::SpatialPointsDataFrame(coords = sgrid2.DF[ , c(2, 3)],
  #                                               data = sgrid2.DF, proj4string = BE_crs)
  #     sgrid2.Li[[p]] = sgrid2.SPDF
  #   }
  #   sgrid2WS = do.call(rbind, sgrid2.Li)
  #   plot(sgrid.SPDF.AoI)
  #   points(sgrid2WS, col = "red")
  #   
  #   # Remove duplicates
  #   sgrid2WS.Dups = sgrid2WS[duplicated(sgrid2WS@coords), ]
  #   sgrid2WS.NoDup = sgrid2WS[!duplicated(sgrid2WS@coords), ]
  
  return(sgrid2WS.NoDup)
}

GridMaker <- function(Region, AoI, dmax, mpp, ...)
{
  boxCoordX <- seq(from = bbox(Region)[1,1] - dmax,
                   to = bbox(Region)[1,2] + dmax,
                   by = mpp)
  boxCoordY <- seq(from = bbox(Region)[2,1] - dmax,
                   to = bbox(Region)[2,2] + dmax,
                   by = mpp)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(sgrid), 1)
  sgrid <- data.frame(ID = idSeq,
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- sp::SpatialPointsDataFrame(coords = sgrid[ , c(2, 3)],
                                      data = sgrid,
                                      proj4string = BE_crs)
  return(sgrid)
}


#   ## Source: https://dahtah.wordpress.com/2013/03/06/barycentric-interpolation-fast-interpolation-on-arbitrary-grids/
#   #2D barycentric interpolation at points Xi for a function with values f measured at locations X
#   #For N-D interpolation simply replace tsearch with tsearchn and modify the sparse matrix definition to have non-zero values in the right spots.
#   interp.barycentric <- function(X,f,Xi)
#   {
#     require(geometry)
#     require(Matrix)
#     dn <- delaunayn(X)
#     tri <- tsearch(X[,1],X[,2],dn,Xi[,1],Xi[,2],bary=T)
#     #For each line in Xi, defines which points in X contribute to the interpolation
#     active <- dn[tri$idx,]
#     #Define the interpolation as a sparse matrix operation. Faster than using apply, probably slower than a C implementation
#     M <- sparseMatrix(i=rep(1:nrow(Xi),each=3),j=as.numeric(t(active)),x=as.numeric(t(tri$p)),dims=c(nrow(Xi),length(f)))
#     as.numeric(M%*%f)
#   }

PointsToRaster <- function(CT.SPDF, ...)
{
  Temp_dir = file.path("..", "output", "temp")
  #Temp_dir = file.path("F:", "output", "temp")
  
  if (!dir.exists(Temp_dir)) 
  {
    dir.create(Temp_dir)
  }
  
  activeDataset = NA
  activeLocation = NA
  activeName = NA
  for (p in seq_along(CT.SPDF))
  {
    activeDataset[p] = CT.SPDF@data[p,1] / 10000
    activeLocation[p] = CT.SPDF@data[p,1] %% 10000
    activeName[p] = activeDataset[p] - (activeLocation[p]/10000)
  }
  
  slots = seq(0,42) # find nicer way (auto slot detection on IDF5-file)
  usedNames = slots %in% activeName
  usedSlots = slots[usedNames]
  
  EXP = CT.SPDF # copy ConversionTable to fill in the EXP values
  EXP@data$hour19 = NA
  
  for (a in usedSlots)
    #for (a in seq(0,20))
    #for (a in head(usedSlots,3))
  {
    print(paste("Starting with slot", a, "of", length(usedSlots)))
    
    for (p in seq_along(CT.SPDF))
    {
      if (activeName[p] == as.character(a))
      {
        File = file.path(Temp_dir, paste0(paste("DF",activeName[p], activeLocation[p], sep = "-"), ".dbf"))
        if (file.exists(File))
        {
          print(paste0(File, " exists. reading it..."))
          DF = read.dbf(File)
        }else{
          print(paste(File, "does not exist. reading h5f slot", a, "..."))
          h5f.active = h5read(h5f_dir, as.character(a))
          
          DF = data.frame(h5f.active$data[,activeLocation[p]])
          SaveAsDBF(DF, paste("DF", activeName[p], activeLocation[p], sep = "-"))
        }
        EXP@data$hour19[p] = DF[19,] #19 for hour 19
      }
    }
    H5close()
  }
  
  SaveAsFile(EXP, paste("Hour19test", Subset.Gemeente, sep = "_"), "Shapefile", TRUE)
  
  
  ## CT subset
  zip_in = file.path("..", "data", "BE_FL", "gismonitor2015.zip")
  shp_in = file.path("..", "data", "BE_FL", "monitor2015.shp")
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(shp_in))
  {
    stop(paste("File(s) not found (.shp)"))
  }
  if (!file.exists(shp_in))
  {
    unzip(zip_in, file = c("monitor2015.shp","monitor2015.prj", "monitor2015.dbf", "monitor2015.shx"),
          exdir= file.path("..", "data", "BE_FL"))
  }
  
  Municipalities = readOGR(shp_in, layer = "monitor2015")
  Municipalities = Municipalities[Municipalities@data$NAAM %in% Subset.Gemeente,] # filter columns
  
  Municipalities@proj4string = BE_crs
  
  ## IDW
  
  library(gstat) # Use gstat's idw routine
  library(sp)    # Used for the spsample function
  library(rgdal)
  library(tmap)
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(EXP, "regular", n=10000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(EXP)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  EXP.idw <- gstat::idw(hour19 ~ 1, EXP, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to Texas
  r       <- raster(EXP.idw)
  r.m     <- mask(r, Municipalities)
  
  # Plot
  spplot(r.m)
  plot(r.m)
  
  tm_shape(r.m) + tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
                            title = paste("Concentration", toupper(pol),  "\n(µg/m³)")) + 
    tm_shape(EXP) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  
  tm_shape(r.m) + tm_raster(n=5,palette = "RdBu", auto.palette.mapping = TRUE,
                            title = paste("Concentration", toupper(pol),  "\n(µg/m³)")) + 
    #tm_shape(EXP) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  
  rast_name = paste(toupper(pol), "raster", "hour19", sep = "_")
  SaveAsFile(r.m, rast_name, "GeoTIFF", Active.Type, TRUE)
  
  
  
  spplot(EXP, "hour19")    
  
  labelat = seq(0,100,by=10)
  labeltext = paste("Exposure values in",Names)
  spplot(EXP, "hour2", col.regions = rainbow(100, start = 4/6, end = 1),
         colorkey=list(width=0.3,     # works
                       space="right", # not honoured
                       tick.number=5, # not honoured, can be left out
                       labels=list( # so we must do it by hand
                         at=labelat,
                         labels=labeltext )))
  spplot(EXP, "hour2", col.regions = rainbow(100, start = 4/6, end = 1))
}

DigitalSurfaceModel <- function(...)
{
  Antwerpen = Municipalities[Municipalities@data$NAAM %in% "Antwerpen",]
  Antwerpen@proj4string = BE_crs
  
  # read height raster (still little sloppy)
  nummers = c("07", "15")
  raster_in = file.path("..", "data", "BE_FL", "DSM", paste0("DHMVIIDSMRAS1m_k", nummers, ".tif"))
  K07 = raster(raster_in[1], ext = Antwerpen)
  K15 = raster(raster_in[2], ext = Antwerpen)
  projection(K07) = BE_crs
  projection(K15) = BE_crs
  
  DSM.K07.cr.ma = mask(crop(K07, Antwerpen), Antwerpen)
  plot(DSM.K07.cr.ma)
  DSM.K15.cr.ma = mask(crop(K15, Antwerpen), Antwerpen)
  plot(DSM.K15.cr.ma)
  
  DSM.Antwerpen = raster::merge(DSM.K07.cr.ma, DSM.K15.cr.ma)
  plot(DSM.Antwerpen)
  
  rm(DSM.K07.cr.ma, DSM.K15.cr.ma)
  
  rast_name = paste("DSM", Subset.Gemeente, sep = "_")
  SaveAsFile(DSM.Antwerpen, rast_name, "GeoTIFF")
  
  
  raster_in = file.path("..", "output", paste0(rast_name, ".tif"))
  DSM.Antwerp = raster(raster_in)
  projection(DSM.Antwerp) = BE_crs
  plot(DSM.Antwerp)
  
  
}

TriangulationFromPoints <- function(...)
{
  
  
}

RegressionKriging <- function(...)
{
  # Use roads for Regression Kriging interpolation
  
  Naam_Wegen = "Wegenregister_SHAPE_20170323"
  Naam_Feature = "Wegsegment"
  
  zip_in = file.path("..", "data", "BE_FL", paste0(Naam_Wegen, ".zip"))
  shp_in = file.path("..", "data", "BE_FL", Naam_Wegen, "Shapefile", paste0(Naam_Feature, ".shp"))
  
  # Check if input data is available
  if (!file.exists(zip_in) & !file.exists(shp_in))
  {
    stop(paste("File(s) not found (.shp)"))
  }
  if (!file.exists(shp_in))
  {
    shp_name = c(Naam_Wegen, Naam_Feature, ".shp", ".prj", ".dbf", ".shx")
    shp_names = NA
    for (n in seq(1, length(shp_name)-2))
    {
      shp_names[n] = file.path(Naam_Wegen, "Shapefile", paste0(shp_name[2], shp_name[2+n]))
    }
    unzip(zip_in, file = shp_names, exdir= file.path("..", "data", "BE_FL"))
  }
  
  shp_in_sub = file.path("..", "data", "BE_FL", Naam_Wegen, "Shapefile", paste0("Wegsegment_AntwerpenEO3", ".shp"))
  Roads = readOGR(shp_in_sub, layer = "Wegsegment_AntwerpenEO3")
  #Roads = Municipalities[Municipalities@data$NAAM %in% Subset.Gemeente,] # filter columns
  Roads@proj4string = BE_crs
  Antwerpen = Municipalities[Municipalities@data$NAAM %in% "Antwerpen",]
  Antwerpen@proj4string = BE_crs
  
  lines(Antwerpen, col = "red")
  plot(Roads, col = "grey")
  #spplot(Roads, "LBLMORF")
  
  "https://downloadagiv.blob.core.windows.net/dhm-vlaanderen-ii-dsm-raster-1m/DHMVIIDSMRAS1m_k15.zip"
  
  #   Roads_Antwerpen = gIntersection(Roads, Antwerpen, byid = T)
  #   plot(Roads_Antwerpen, col = "grey")
  #   lines(Antwerpen, col = "red")
  
  o = over(Roads, Antwerpen)
  Roads_Antwerpen = Roads[o$NAAM == "Antwerpen",]
  
  spplot(Roads_Antwerpen, "LBLMORF")
  
  Roads_Antwerpen = Roads_Antwerpen[,colnames(Roads_Antwerpen@data) == c("LBLMORF", "WEGCAT")]
  Roads_Antwerpen@data$RoadPullutor = 1
  Roads_Antwerpen@data$RoadPullutor[Roads_Antwerpen@data$LBLMORF == "autosnelweg"] = 5
  
  spplot(Roads_Antwerpen, "RoadPullutor")
  
}




# source: http://r-sig-geo.2731867.n2.nabble.com/DEM-interpolation-with-Delaunay-triangulation-td7013856.html
maybetin <- function(X, nx, ny, x=NULL, y=NULL, na.v=0)
{
  require(spatstat)
  lltes<-delaunay(X)
  
  if(is.null(x)){
    gri <-  gridcentres(X$window, nx=nx, ny=ny)
    gri.ppp <- ppp(gri$x,gri$y, window=X$window,
                   marks=rep(na.v,length(gri$x)))
  }
  if(!is.null(x)){
    gri.ppp<- ppp(x=x, y=y, window=X$window,
                  marks=rep(na.v, length(x)))
  }
  
  cat("\n","number of triangles =",
      length(lltes[[3]]),"\n\n")
  for(i in 1:length(lltes[[3]])){
    progressreport(i, length(lltes[[3]]))
    
    #grid points within the triangulation
    xoyo <- unmark(gri.ppp[lltes[[3]][[i]]])
    
    # original points defining the triangle
    xyz <- X[lltes[[3]][[i]]]
    # z values of the three points
    z<-xyz$marks
    mtrend <-with(xyz, lm(marks~x+y))
    
    grim <- predict(mtrend,
                    newdata=data.frame(x = xoyo$x, y=xoyo$y))
    
    #assign interpolated values
    gri.ppp[lltes[[3]][[i]]]$marks <- grim
  }
  return(gri.ppp)
} 

TestCode <- function(...)
{
  
  require(gstat)
  loadMeuse()
  require(spatstat)
  spatstat.options(gpclib=TRUE)
  
  #data pre-processing
  library(maptools) # for the "as" method
  meuse.ppp <- as(meuse,"ppp")
  meusegrid.ppp <- as(meuse.grid,"ppp")
  
  #readjust the window for not missing points of meusegrid
  meuse.ppp$window <- meusegrid.ppp$window
  
  # use only log(zinc) as mark
  meuse.ppp$marks <-log( meuse@data$zinc)
  
  #compute (maybe) TIN
  xgrid <-maybetin(meuse.ppp,x=meusegrid.ppp$x,y=meusegrid.ppp$y)
  
  #A very brute force kind of representation
  meuse.grid@data$tin<-xgrid$marks
  
  spplot(meuse.grid["tin"],col.regions=bpy.colors(),
         sp.layout=list("sp.points", meuse))
  
  meuse.grid@data$tine<-exp(xgrid$marks)
  spplot(meuse.grid["tine"],col.regions=bpy.colors(),
         sp.layout=list("sp.points", meuse))
  
  
  Dens <- stats::density(WS.ppp, adjust = 0.2)  # create density object
  
  
  #data pre-processing
  library(maptools) # for the "as" method
  WS.ppp <- as(EXP,"ppp")
  WSgrid.ppp <- as(Dens,"ppp")
  
  #readjust the window for not missing points of meusegrid
  meuse.ppp$window <- meusegrid.ppp$window
  
  # use only log(zinc) as mark
  meuse.ppp$marks <-log( meuse@data$zinc)
  
  #compute (maybe) TIN
  xgrid <-maybetin(meuse.ppp,x=meusegrid.ppp$x,y=meusegrid.ppp$y)
  
  #A very brute force kind of representation
  meuse.grid@data$tin<-xgrid$marks
  
  spplot(meuse.grid["tin"],col.regions=bpy.colors(),
         sp.layout=list("sp.points", meuse))
  
  meuse.grid@data$tine<-exp(xgrid$marks)
  spplot(meuse.grid["tine"],col.regions=bpy.colors(),
         sp.layout=list("sp.points", meuse))
  
  
  
  
  require(gstat)
  loadMeuse()
  x = krige(log(zinc)~x+y, meuse, meuse.grid, nmax=3)
  spplot(x[1],col.regions=bpy.colors(),
         sp.layout=list("sp.points", meuse)) 
  
  
}