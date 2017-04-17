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
list.of.packages <- c("sp","rgdal", "tmap", "gstat", "akima", "geometry", "Matrix")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(sp)
library(akima)

# SPDF = Gemeente.Points.NoDup
# dmax = 10
# mpp = 100
PointsToRasterTIN <- function(SPDF, dmax, mpp, ...)
{
  boxCoordX <- seq(from = bbox(SPDF)[1,1] - dmax,
                   to = bbox(SPDF)[1,2] + dmax,
                   by = mpp)
  boxCoordY <- seq(from = bbox(SPDF)[2,1] - dmax,
                   to = bbox(SPDF)[2,2] + dmax,
                   by = mpp)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(sgrid), 1)
  WS = interp(x = SPDF@coords[,1], y = SPDF@coords[,2], z = SPDF@data$values,
              xo = boxCoordX, yo = boxCoordY)
  sgrid <- data.frame(ID = idSeq,
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2],
                      NO2 = as.numeric(WS$z))
  sgrid <- sp::SpatialPointsDataFrame(coords = sgrid[ , c(2, 3)],
                                      data = sgrid,
                                      proj4string = BE_crs)
  Sub = as.logical(gIntersects(sgrid, Municipalities[Municipalities@data$NAAM %in% "Antwerpen",], byid = T))
  #sgrid = sgrid[Sub,]
  #plot(sgrid)
  #plot(sgrid[Sub,])
  #return(sgrid)
  sp::gridded(sgrid) <- TRUE
  sgrid = sgrid[Sub,]
  #plot(sgrid)
  r <- raster::raster(sgrid)
  r <- raster::rasterize(sgrid, r, field = sgrid@data$NO2, na.rm=TRUE, fun = mean)
  #plot(r)
  return(r)
  
  #SaveAsFile(sgrid, paste("sgrid", "Antwerpen", paste0(mpp, "x", mpp), sep = "_"), "GeoJSON", TRUE)
  
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