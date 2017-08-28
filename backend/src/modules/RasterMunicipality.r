# Module for creating a raster for the specified municipality.
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
list.of.packages <- c("rgdal", "raster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(rgdal)
library(raster)
library(sp)

RasterMunicipality <- function(pol, DriveLetter, mpp, Name.Municipality, StartHour = 1+(24*5), EndHour = 24+(24*5), ...)
{
  ## Read base
  #pol = pollutants[1]
  BASEAQ = BaseAQnetwork(pol, ExternalDrive = TRUE, DriveLetter = DriveLetter)
  Points.NoVal = BASEAQ[[1]]
  PolDir = BASEAQ[[2]]
  
  # Read municipalities
  Municipalities = getData("GADM",country = "Belgium", level = 4, path = output.dir)
  #Name.Municipality = "Oudergem"
  Municipality = Municipalities[Municipalities@data$NAME_4 == Name.Municipality,]
  Municipality = spTransform(Municipality, BE_crs)
  Municipality@proj4string = BE_crs
  
  # Subset Base RIO-IFDM on Municipality
  points.sel = 1000
  Points.Base.Samples = gIntersection(Points.NoVal[1:points.sel,], Municipality,  byid = TRUE)
  
  while (is.null(Points.Base.Samples))
  {
    points.sel = points.sel + 1000
    Points.Base.Samples = gIntersection(Points.NoVal[(points.sel-1000):points.sel,], Municipality,  byid = TRUE)
  }
  plot(Points.Base.Samples)
  
  # find most central points
  X.min = Points.Base.Samples@bbox[1,1]
  X.max = Points.Base.Samples@bbox[1,2]
  Y.min = Points.Base.Samples@bbox[2,1]
  Y.max = Points.Base.Samples@bbox[2,2]
  X.mean = (X.max + X.min) / 2
  Y.mean = (Y.max + Y.min) / 2
  Central.Point = SpatialPoints(coords = cbind(X.mean, Y.mean), proj4string = BE_crs)
  points(Central.Point, col = "red")
  
  Radius = max(X.max-X.mean, Y.max-Y.mean)
  
  # Create search trees
  tree = createTree(coordinates(Points.NoVal))
  test = as.numeric(knnLookup(tree, newdat = coordinates(Central.Point), k = Radius * 4))
  plot(Points.NoVal[test,])
  lines(Municipality, col = "green")
  
  Municipality.Buffer = gBuffer(Municipality, byid = F, id = NULL, width = 1000)
  lines(Municipality.Buffer, col = "red")
  
  Points.Base.Municipality.Buffer = gIntersects(Points.NoVal[test,], Municipality.Buffer,  byid = TRUE)
  plot(Points.NoVal[rownames(Points.NoVal@data) %in% Points.Base.Municipality.Buffer,])
  
  length(Points.NoVal[test,])
  length(Points.Base.Municipality.Buffer)
  
  rownames(Points.NoVal[test,]@data) %in% colnames(Points.Base.Municipality.Buffer)
  
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
  library(geoR)
  
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