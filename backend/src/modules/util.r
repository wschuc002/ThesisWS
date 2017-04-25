#' @name osrmIsochrone
#' @title Get a SpatialPolygonsDataFrame of Isochrones
#' @description Based on \code{\link{osrmTable}}, this function buids a 
#' SpatialPolygonsDataFrame of isochrones. 
#' @param loc a numeric vector of longitude and latitude (WGS84) or a 
#' SpatialPointsDataFrame or a SpatialPolygonsDataFrame of the origine point.
#' @param breaks a numeric vector of isochrone values (in minutes).
#' @return A SpatialPolygonsDateFrame of isochrones is returned. 
#' The data frame of the output contains four fields: 
#' id (id of each polygon), min and max (minimum and maximum breaks of the polygon), 
#' center (central values of classes).
#' @seealso \link{osrmTable}
#' @import sp
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data("com")
#' 
#' # Get isochones with lon/lat coordinates, default breaks
#' iso <- osrmIsochrone(loc = c(5.936036, 49.24882))
#' plot(iso)
#' points(5.936036, 49.24882, pch = 20, col = "red")
#' 
#' # Map
#' if(require("cartography")){
#'   osm <- getTiles(spdf = iso, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(osm)
#'   breaks <- sort(c(unique(iso$min), max(iso$max)))
#'   cartography::choroLayer(spdf = iso, df = iso@data,
#'                           var = "center", breaks = breaks,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' 
#' # Get isochones with a SpatialPointsDataFrame, custom breaks
#' iso2 <- osrmIsochrone(loc = src[7,], breaks = seq(from = 0,to = 30, by = 5))
#' 
#' # Map
#' if(require("cartography")){
#'   osm2 <- getTiles(spdf = iso2, crop = TRUE, type = "osmgrayscale")
#'   tilesLayer(osm2)
#'   breaks2 <- sort(c(unique(iso2$min), max(iso2$max)))
#'   cartography::choroLayer(spdf = iso2, df = iso2@data,
#'                           var = "center", breaks = breaks2,
#'                           border = NA,
#'                           legend.pos = "topleft",legend.frame = TRUE, 
#'                           legend.title.txt = "Isochrones\n(min)", 
#'                           add = TRUE)
#' }
#' }
#' 
# loc = Primary_random[1,]
# breaks = ComTime.breaks
# res = 100
osrmIsochrone.WS <- function(loc, breaks, res, ...){
  oprj <- NA
  if(testSp(loc)){
    oprj <- sp::proj4string(loc)
    loc <- loc[1,]
    loc <- sp::spTransform(x = loc, CRSobj = "+init=epsg:3857")
  }else{
    loc <- data.frame(lon = loc[1], lat = loc[2])
    loc <- sp::SpatialPointsDataFrame(coords = loc[,1:2], 
                                      data = loc, 
                                      proj4string = sp::CRS("+init=epsg:4326"))
    loc <- sp::spTransform(x = loc, CRSobj = sp::CRS("+init=epsg:3857"))
  }
  
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  speed <- 140 * 1000/60
  dmax <- tmax * speed
  #res <- Res
  sgrid <- rgrid(loc = sp::coordinates(loc), dmax = dmax, res = res)
  
  row.names(loc) <- "0"
  
  if(getOption("osrm.server") != "http://router.project-osrm.org/"){
    dmat <- osrmTable(src = loc, dst = sgrid)
    durations <- dmat$durations
    destinations <- dmat$destinations
  }else{
    Fragments = 16
    Step = ((res**2)/Fragments)
    
    dmatl = list()
    for (f in (1))
    {
      dmatl[[f]] <- osrmTable(src = loc, dst = sgrid[1:Step,])
      Sys.sleep(1)
    }
    for (f in (2:(Fragments-1)))
    {
      dmatl[[f]] = osrmTable(src = loc, dst = sgrid[(Step*f+1):(Step*(f+1)),])
      Sys.sleep(1)
    }
    for (f in Fragments)
    {
      dmatl[[f]] = osrmTable(src = loc, dst = sgrid[(Step*(f-1)+1):(res**2),])
    }
    
#     test = unlist(dmatl)
#     test
#     test = Map(cbind, dmatl, "durations")
#     test
#     
#     durations = data.table::rbindlist(dmatl)
    
    durations <- cbind(dmatl[[1]]$durations, dmatl[[2]]$durations, dmatl[[3]]$durations, dmatl[[4]]$durations,
                       dmatl[[5]]$durations, dmatl[[6]]$durations, dmatl[[7]]$durations, dmatl[[8]]$durations,
                       dmatl[[9]]$durations, dmatl[[10]]$durations, dmatl[[11]]$durations, dmatl[[12]]$durations,
                       dmatl[[13]]$durations, dmatl[[14]]$durations, dmatl[[15]]$durations, dmatl[[16]]$durations)
    
    destinations <- rbind(dmatl[[1]]$destinations, dmatl[[2]]$destinations, dmatl[[3]]$destinations, dmatl[[4]]$destinations,
                       dmatl[[5]]$destinations, dmatl[[6]]$destinations, dmatl[[7]]$destinations, dmatl[[8]]$destinations,
                       dmatl[[9]]$destinations, dmatl[[10]]$destinations, dmatl[[11]]$destinations, dmatl[[12]]$destinations,
                       dmatl[[13]]$destinations, dmatl[[14]]$destinations, dmatl[[15]]$destinations, dmatl[[16]]$destinations)
    
    
    #rm(durations, destinations)
  }
  
  rpt <- sp::SpatialPointsDataFrame(coords = destinations[ , c(1, 2)],
                                    data = data.frame(destinations),
                                    proj4string = sp::CRS("+init=epsg:4326"))
  rpt <- sp::spTransform(rpt, sp::proj4string(loc))
  rpt$d <- as.vector(durations)
  rpt$d[is.na(rpt$d)] <- max(rpt$d, na.rm=TRUE)
  sp::gridded(sgrid) <- TRUE
  r <- raster::raster(sgrid)
  r <- raster::rasterize(rpt, r, field = 'd', fun = min, na.rm=TRUE,
                         background= max(rpt$d, na.rm=TRUE)+1)
  isolines <- rasterToContourPoly(r = r, breaks = breaks)
  # contour correction
  isolines <- isolines[-1,]
  isolines@data[nrow(isolines), "min"] <- 0
  isolines@data[nrow(isolines), "center"] <- (isolines@data[nrow(isolines), "max"] - 
                                                isolines@data[nrow(isolines), "min"]) / 2
  # reproj
  if (!is.na(oprj)){
    isolines <- sp::spTransform(x = isolines, CRSobj = oprj)
  }else{
    isolines <- sp::spTransform(x = isolines, CRSobj = "+init=epsg:4326")
  }
  return(isolines)
}

# src = Primary_random[1,] src = PRI[1,]
# dst = SecondarySample[1,] dst = SecondaryPaired
# src = sp::spTransform(Primary_random[1,], WGS84)
# dst = sp::spTransform(SecondarySample[1,], WGS84)
# overview = "full"
# sp = TRUE

osrmRoute.WS <- function(src, dst, overview = "simplified", sp = FALSE){
  tryCatch({
    
    # src = com[1, c("comm_id", "lon","lat")]
    # dst = com[2, c("comm_id", "lon","lat")]
    # sp=TRUE
    # overview = "simplified"
    
    oprj <- NA
    if(testSp(src)){
      oprj <- sp::proj4string(src)
      src <- src[1,]
      x <- spToDf(x = src)
      src <- c(x[1,1],x[1,2], x[1,3])
    }
    if(testSp(dst)){
      dst <- dst[1,]
      x <- spToDf(x = dst)
      dst <- c(x[1,1],x[1,2], x[1,3])
    }
    
    # build the query
    req <- paste(getOption("osrm.server"),
                 "route/v1/", getOption("osrm.profile"), "/", 
                 src[2], ",", src[3],
                 ";",
                 dst[2],",",dst[3], 
                 "?alternatives=false&geometries=polyline&steps=false&overview=",
                 tolower(overview),
                 sep="")
    
    # Sending the query
    resRaw <- RCurl::getURL(utils::URLencode(req), 
                            useragent = "'osrm' R package")
    # Deal with \\u stuff
    vres <- jsonlite::validate(resRaw)[1]
    if(!vres){
      resRaw <- gsub(pattern = "[\\]", replacement = "zorglub", x = resRaw)
    }
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$message)
    if(res$code != "Ok"){stop(e)}
    
    if (overview == FALSE){
      return(round(c(duration = res$routes$duration/60, 
                     distance = res$routes$distance/1000), 2))
    }
    if(!vres){
      res$routes$geometry <- gsub(pattern = "zorglub", replacement = "\\\\", 
                                  x = res$routes$geometry)
    }
    # Coordinates of the line
    geodf <- gepaf::decodePolyline(res$routes$geometry)[,c(2,1)]
    
    # Convert to SpatialLinesDataFrame
    if (sp == TRUE){
      routeLines <- sp::Lines(slinelist = sp::Line(geodf[,1:2]), 
                              ID = "x")
      routeSL <- sp::SpatialLines(LinesList = list(routeLines), 
                                  proj4string = sp::CRS("+init=epsg:4326"))
      df <- data.frame(src = src[1], dst = dst[1], 
                       duration = res$routes$legs[[1]]$duration/60,
                       distance = res$routes$legs[[1]]$distance/1000)
      geodf <- sp::SpatialLinesDataFrame(routeSL, data = df, match.ID = FALSE)   
      row.names(geodf) <- paste(src[1], dst[1],sep="_")
      if (!is.na(oprj)){
        geodf <- sp::spTransform(geodf, oprj)
      }
    }
    return(geodf)
  }, error=function(e) {message("osrmRoute function returns an error: \n", e)})
  return(NULL)
}

## All Functions Utils
testSp <- function(x){
  if (class(x) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")){
    if (is.na(sp::proj4string(x))){
      stop(
        paste(
          "Your input (", quote(x),
          ") does not have a valid coordinate reference system.", sep=""),
        call. = F)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

spToDf <- function(x){
  # transform to WGS84
  x <- sp::spTransform(x = x, CRSobj = "+init=epsg:4326")
  # this function takes a SpatialDataFrame and transforms it into a dataframe
  x <- data.frame(id = row.names(x), 
                  lon = round(sp::coordinates(x)[,1],6), 
                  lat = round(sp::coordinates(x)[,2],6), 
                  stringsAsFactors = FALSE)
  return(x)
}



## osrmIsochrone Utils
rasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL){
  
  rmin <- raster::cellStats(r, min, na.rm = TRUE)
  rmax <- raster::cellStats(r, max, na.rm = TRUE)
  
  # default breaks and nclass
  if(is.null(breaks)){
    breaks <- seq(from = rmin,
                  to = rmax,
                  length.out = (nclass+1))
  }else{
    breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
    breaks <- unique(breaks)
    breaks <- sort(breaks)
    # nclass <- length(breaks)-1
  }
  
  myres <- raster::res(r)[1]
  myproj <- sp::CRS(sp::proj4string(r))
  
  if (is.null(mask)){
    mask <- masker(r)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- raster::extend(r, maskbuff, value=-1)
  }else{
    mask <- rgeos::gUnaryUnion(mask)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- raster::mask(r, maskbuff, updatevalue = -1)
    if(rgeos::gWithin(masker(r), mask)){stop("mask should be smaller than r",
                                             call. = FALSE)}
  }
  
  rmin <- min(r[r!=-1])
  rmax <- max(r[r!=-1])
  breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
  breaks <- unique(breaks)
  breaks <- sort(breaks)
  finalBreaks <- breaks
  # zero level problem
  if(breaks[1] <= 0){
    zv <- TRUE
    breaks <- breaks + 1
    r <- r + 1
  }else{
    zv <- FALSE
  }
  
  nclass <- length(breaks)-1
  breaks <- breaks[-(nclass+1)]
  
  r[is.na(r)] <- 0
  
  # test breaks
  if(length(breaks)<2){stop("breaks values do not fit the raster values",
                            call. = FALSE)}
  # build the contour lines
  cl <- raster::rasterToContour(r, levels = breaks)
  cl$level <- as.numeric(as.character(cl$level))
  SPlist <- list()
  SPlevels <- character()
  for (i in cl$level){
    linex <- cl[cl@data$level == i,]
    linex <- linex@lines
    linex <- linex[[1]]
    linex <- linex@Lines
    Plist <- NULL
    Plist <- list()
    for (j in 1:length(linex)){
      x <- linex[[j]]@coords
      x <- sp::Polygon(coords =  x, hole = F)
      x <- sp::Polygons(srl = list(x), ID = j)
      Plist[[j]] <- x
    }
    x <- sp::SpatialPolygons(Srl = Plist)
    x <- rgeos::union(x = x)
    
    if (class(x) != "SpatialPolygonsDataFrame"){
      x <- sp::SpatialPolygonsDataFrame(Sr = x,
                                        data = data.frame(
                                          level = rep(i, length(x))))
    } else {
      x <- x[x@data$count < 2,]
      x@data <- data.frame(level = rep(i, dim(x)[1]))
    }
    SPlist <- c(SPlist , x@polygons  )
    SPlevels <- c(SPlevels,x@data$level)
  }
  for (i in 1:length(SPlist)){
    SPlist[[i]]@ID <- as.character(i)
  }
  x <- sp::SpatialPolygons(Srl = SPlist, proj4string = myproj)
  x <- sp::SpatialPolygonsDataFrame(Sr = x,
                                    data = data.frame(levels = SPlevels))
  
  bks <- data.frame(b =c(breaks, rmax), t = finalBreaks)
  
  # # manage attributes data of the contour spdf
  # breaks <- c(breaks, rmax)
  x@data <- data.frame(id = paste("id_",row.names(x),sep=""),
                       min = bks[match(x$levels, bks[,1]),2],
                       max = bks[match(x$levels, bks[,1])+1,2],
                       center = NA,
                       stringsAsFactors = FALSE)
  x$center <- (x$min+x$max) / 2
  row.names(x) <- x$id
  
  # clip the contour spdf with the mask
  final <- rgeos::gIntersection(spgeom1 = x, spgeom2 = mask, byid = TRUE,
                                id = row.names(x))
  
  df <- data.frame(id = sapply(methods::slot(final, "polygons"),
                               methods::slot, "ID"))
  row.names(df) <- df$id
  final <- sp::SpatialPolygonsDataFrame(Sr = final, data = df)
  final@data <- data.frame(id = final$id, x[match(final$id, x$id),2:4])
  final@plotOrder <- 1:nrow(final)
  
  # ring correction
  df <- unique(final@data[,2:4])
  df$id <- 1:nrow(df)
  df <- df[order(df$center, decreasing = T),]
  
  z <- rgeos::gIntersection(final[final$center==df[1,3],],
                            final[final$center==df[1,3],], byid = F,
                            id = as.character(df[1,4]))
  for(i in 2:nrow(df)){
    y <- rgeos::gDifference(final[final$center==df[i,3],],
                            final[final$center==df[i-1,3],], byid = F, 
                            id = as.character(df[i,4]))
    z <- rbind(z, y)
  }
  dfx <- data.frame(id = sapply(methods::slot(z, "polygons"), 
                                methods::slot, "ID"))
  row.names(dfx) <- dfx$id
  z <- sp::SpatialPolygonsDataFrame(z, dfx)
  z@data <- df[match(x=z@data$id, table = df$id),c(4,1:3)]
  return(z)
}


masker <- function(r){
  xy <- sp::coordinates(r)[which(!is.na(raster::values(r))),]
  i <- grDevices::chull(xy)
  b <- xy[c(i,i[1]),]
  mask <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(b,
                                                                 hole = FALSE)),
                                                ID = "1")),
                              proj4string = sp::CRS(sp::proj4string(r)))
  return(mask)
}

rgrid <- function(loc, dmax, res){
  boxCoordX <- seq(from = loc[1] - dmax,
                   to = loc[1] + dmax,
                   length.out = res)
  boxCoordY <- seq(from = loc[2] - dmax,
                   to = loc[2] + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(sgrid), 1)
  sgrid <- data.frame(ID = idSeq,
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- sp::SpatialPointsDataFrame(coords = sgrid[ , c(2, 3)],
                                      data = sgrid,
                                      proj4string = sp::CRS("+init=epsg:3857"))
  return(sgrid)
}

# SPDF = Gemeente.Points.NoDup
# dmax = 100
# mpp = 100

rgrid.WS <- function(SPDF, dmax, mpp){
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
  return(sgrid)
}


## osrmTable Utils
distTableFormat <- function(res, src, dst){
  # extract distance table
  mat <- res$durations
  # From sec to minutes
  mat <- round(mat/(60), 1)
  # NA management
  mat[mat == 357913.94] <- NA
  # col and row names management
  dimnames(mat) <- list(src$id, dst$id)
  return(mat)
}  

coordFormat <- function(res, src, dst){
  sources <- data.frame(matrix(unlist(res$sources$location, 
                                      use.names = T), 
                               ncol = 2, byrow = T, 
                               dimnames = list(src$id, c("lon", "lat"))))
  destinations <- data.frame(matrix(unlist(res$destinations$location, 
                                           use.names = T), 
                                    ncol = 2, byrow = T, 
                                    dimnames = list(dst$id, c("lon", "lat"))))
  return(list(sources = sources, destinations = destinations)
  )
}

tableLoc <- function(loc){
  # Query build
  tab <- paste(getOption("osrm.server"), "table/v1/", getOption("osrm.profile"), "/polyline(", sep = "")
  tab <- paste0(tab, gepaf::encodePolyline(loc[,c("lat","lon")]),")")
  return(tab)
}

osrmLimit <- function(nSrc, nDst){
  e <- simpleError("The public OSRM API does not allow results with 
  a number of durations higher than 10000")
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & (nSrc*nDst) > 10000){
    stop(e)
  }
}


# options(osrm.profile = "driving")
# options(osrm.profile = "biking")
# getOption("osrm.profile")

# data("com")
# 
#  loc = com[1:20, c(1,3,4)]
# overview = "simplified"

osrmTrip.WS <- function(loc, overview = "simplified"){
  tryCatch({

    # check if inpout is sp, transform and name columns
    oprj <- NA
    if (testSp(loc)) {
      oprj <- sp::proj4string(loc)
      loc <- spToDf(x = loc)
    }else{
      names(loc) <- c("id", "lon", "lat")
    }
    
    # Build the query
    req <- paste(getOption("osrm.server"),
                 "trip/v1/", getOption("osrm.profile"), "/polyline(", 
                 gepaf::encodePolyline(loc[,c("lat","lon")]),
                 ")?steps=false&geometries=geojson&overview=",
                 tolower(overview), sep = "")
    # Send the query
    ua <- "'osrm' R package"
    resRaw <- RCurl::getURL(utils::URLencode(req), useragent = ua)
    
    if (resRaw=="") {
      stop("OSRM returned an empty string.", call. = FALSE)
    }
    
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$message)
    if (res$code != "Ok") {stop(e)}
    
    # Get all the waypoints
    waypointsg <- data.frame(res$waypoints[,c(1,5)], 
                             matrix(unlist(res$waypoints$location), 
                                    byrow = T, ncol = 2), id = loc$id)
    
    # In case of island, multiple trips
    ntour <- dim(res$trips)[1]
    trips <- vector("list", ntour)
    
    for (nt in 1:ntour) {
      # Coordinates of the line
      geodf <- data.frame(res$trips[nt,]$geometry$coordinates)
      # In case of unfinnish trip
      if (geodf[nrow(geodf),1] != geodf[1,1]) {
        geodf <- rbind(geodf,geodf[1,])
      }
      geodf$ind <- 1:nrow(geodf)
      # Extract trip waypoints
      waypoints <- waypointsg[waypointsg$trips_index == (nt - 1),]
      
      # Get points order and indexes
      geodf <- merge(geodf, waypoints, 
                     by.x = c("X1", "X2"), by.y = c("X1","X2"), 
                     all.x = T)
      geodf <- geodf[order(geodf$ind, decreasing = F),]
      
      
      indexes2 <- geodf[!is.na(geodf$waypoint_index),"ind"]
      xx <- geodf[!is.na(geodf$waypoint_index),]
      indexes <- c(stats::aggregate(xx$ind, by  = list(xx$waypoint_index),
                                    min)[,2], 
                   nrow(geodf))
      # Build the polylines
      wktl <- rep(NA,nrow(waypoints))
      for (i in 1:(length(indexes) - 1)) {
        wktl[i] <- paste("LINESTRING(",
                         paste(geodf[indexes[i]:indexes[i + 1],1]," ",
                               geodf[indexes[i]:indexes[i + 1],2], 
                               sep = "", collapse = ",")
                         ,")",sep = "")
      }
      wkt <- paste("GEOMETRYCOLLECTION(", paste(wktl, collapse = ","),")", sep = "")
      sl <- rgeos::readWKT(wkt)
      sl@proj4string <- sp::CRS("+init=epsg:4326")
      start <- (waypoints[order(waypoints$waypoint_index, decreasing = F),"id"])
      end <- start[c(2:length(start),1)]
      df <- data.frame(start, end, 
                       duration = res$trips[nt,]$legs[[1]][,"duration"] / 60, 
                       distance = res$trips[nt,]$legs[[1]][,"distance"] / 1000)
      sldf <- sp::SpatialLinesDataFrame(sl = sl, data = df, match.ID = F)
      
      # Reproj
      if (!is.na(oprj)) {
        sldf <- sp::spTransform(sldf, oprj)
      }
      
      # Build tripSummary
      tripSummary <- list(duration = res$trips[nt,]$duration/60,
                          distance = res$trips[nt,]$distance/1000)   
      
      trips[[nt]] <- list(trip = sldf, summary = tripSummary)
    }
    return(trips)
  }, error = function(e) { message("osrmTrip function returns an error: \n", e)})
  return(NULL)
}