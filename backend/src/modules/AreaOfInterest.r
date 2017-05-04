# Create buffer of AoI (=Location history)

library(sp)
library(rgdal)
library(rgeos)

AreaOfInterest.AQ <- function(Points, ...)
{
  dat = coordinates(Points)
  
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon
  
  #plot(dat, pch=19)
  #lines(coords, col="red")
  
  AoI <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  AoI@proj4string = BE_crs
  
  return(AoI)
}


# T1 = PPH.T1
# T2 = PPH.T2
AreaOfInterest.TransportVertices <- function(T1, T2, MinuteInterval, ...)
{
  T1.SPDF.Li = list()
  T2.SPDF.Li = list()
  for (i in seq_along(T1))
  #for (i in 1:2)
  {
    T1.SPDF.Li[[i]] = as(T1[i,], "SpatialPointsDataFrame")
    T2.SPDF.Li[[i]] = as(T2[i,], "SpatialPointsDataFrame")
  }
  T1.SPDF = do.call(rbind, T1.SPDF.Li)
  T2.SPDF = do.call(rbind, T2.SPDF.Li)
  return(list(T1.SPDF, T2.SPDF))
}
# T_SPDF = AreaOfInterest.TransportVertices(T1, T2)
# T1.SPDF = T_SPDF[[1]]
# T2.SPDF = T_SPDF[[2]]
# plot(T1.SPDF[T1.SPDF@data$src==1,])

  
# PRI = PPH.P
# SEC = PPH.S
# T1 = PPH.T1
# T2 = PPH.T2
# m = 250

AreaOfInterest.PPH.2 <- function(PRI, SEC, T1, T2, m, Plot, ...)
{
  if (Plot == TRUE){plot(Flanders)}
  
  Aoi_T.simp = gSimplify(merge(T1,T2), 50)
  
  AoI_T.buff.Li = list()
  for (i in 1:length(PRI))
  {
    AoI_T.buff.Li[[i]] = gBuffer(Aoi_T.simp[i,], width = m)
    AoI_T.buff.Li[[i]]@polygons[[1]]@ID = paste0(AoI_T.buff.Li[[i]]@polygons[[1]]@ID, i)
    
    if (Plot == TRUE)
    {
      plot(AoI_T.buff.Li[[i]], add = TRUE)
    }
  }
  AoI_T.buff = do.call(rbind, AoI_T.buff.Li)
  
  AoI_PS.buff = gBuffer(merge(PRI,SEC), byid = F, id = NULL, width = m)
  
  AoI.simp = gSimplify(rbind(AoI_PS.buff, AoI_T.buff), 50)
  AoI.UU = rgeos::gUnaryUnion(AoI.simp, id = NULL)
  
  AoI.SPDF = SpatialPolygonsDataFrame(AoI.UU, data = data.frame(m), match.ID = T)
  
  return(AoI.SPDF)
}

AreaOfInterest.PPH <- function(PRI, SEC, T1, T2, m, ...)
{
  Aoi_T.simp = gSimplify(merge(T1,T2), 50, topologyPreserve = TRUE)
  
  AoI_PS.buff = gBuffer(merge(PRI,SEC), byid = F, id = NULL, width = m)
  
  AoI_T.buff.Li = list()
  for (i in 1:length(PRI))
  {
    AoI_T.buff.Li[[i]] = gBuffer(Aoi_T.simp[i,], width = m)
    plot(AoI_T.buff.Li[[i]], add = TRUE)
    AoI_T.buff.Li[[i]]@polygons[[1]]@ID = paste0(AoI_T.buff.Li[[i]]@polygons[[1]]@ID, i)
  }
  AoI_T.buff = do.call(rbind, AoI_T.buff.Li)
  
  AoI_PS = rgeos::gUnaryUnion(AoI_PS.buff, id = NULL)
  AoI.T = rgeos::gUnaryUnion(AoI_T.buff, id = NULL)
  
  AoI.U = union(AoI_PS, AoI.T)
  AoI = gUnaryUnion(AoI.U, id = NULL)
  
  AoI.SPDF = SpatialPolygonsDataFrame(AoI, data = data.frame(m), match.ID = T)
  
  return(AoI.SPDF)
}