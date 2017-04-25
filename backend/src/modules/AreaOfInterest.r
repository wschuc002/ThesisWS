# Create buffer of AoI (=Location history)

library("sp")
library("rgdal")
library("rgeos")

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

AreaOfInterest.PPH <- function(PRI, SEC, T1, T2, m, ...)
{
  AoI_buff_PS = gBuffer(merge(PRI,SEC), byid = F, id = NULL, width = m)
  AoI_buff_T = gBuffer(merge(T1,T2), byid = F, id = NULL, width = m)
  AoI = union(AoI_buff_PS, AoI_buff_T)
  AoI.UU = rgeos::gUnaryUnion(AoI, id = NULL)
  
  AoI.SPDF = SpatialPolygonsDataFrame(AoI.UU, data = data.frame(m), match.ID = T)
    
  return(AoI.SPDF)
}

#SaveAsFile(AoI.SPDF, paste("AreaOfInterest", paste0(m,"m"), sep = "_"), "GeoJSON", TRUE)