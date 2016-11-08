# Module for determining routes from two locations
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
list.of.packages <- c("xml2","osmar", "xml")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
#library(httr)
#library(xml2)
#library(igraph)
library(osmar)
library(XML)
library(rgdal)

c('SQLite', 'OSM') %in% ogrDrivers()$name

src <- osmsource_api()

get_osm(node(18961430), source = src)
get_osm(way(3810479), source = src)
get_osm(way(3810479), source = src, full = TRUE)

bb <- center_bbox(174.76778, -36.85056, 700, 700)
ua <- get_osm(bb, source = src)
ua

summary(ua$nodes)

# find all traffic signal nodes
ts_ids <- find(ua, node(tags(v == "traffic_signals")))
ts_ids

# find all busstop nodes
bs_ids <- find(ua, node(tags(v %agrep% "busstop")))
bs_ids

# find all highway nodes
hw_ids <- find(ua, way(tags(k == "highway")))
hw_ids <- find_down(ua, way(hw_ids))

ts <- subset(ua, node_ids = ts_ids)
ts

bs <- subset(ua, node_ids = bs_ids)
bs

hw <- subset(ua, ids = hw_ids)
hw

plot(ua)
plot_ways(hw, add = TRUE, col = "green")
plot_nodes(ts, add = TRUE, col = "red")
plot_nodes(bs, add = TRUE, col = "blue")

args(as_sp)


url <- "http://osmar.r-forge.r-project.org/"
file <- "muenchen.osm.gz"
download.file(sprintf("%s%s", url, file), file)
system("gzip -d muenchen.osm.gz")

muc_in = file.path("data", "muenchen.osm")

src <- osmsource_osmosis(file ="muenchen.osm")
src <- osmsource_osmosis(muc_in)
muc_bbox <- center_bbox(11.575278, 48.137222, 3000, 3000)
debug(osmar:::get_osm_data.osmosis)
muc <- get_osm(muc_bbox, src)
muc
undebug(osmar:::get_osm_data.osmosis)


destination <- file.path("data")
request <- osm_request(src, what, destination)
ret <- system(request, ...)
response <- readLines(destination)
unlink(destination)
response






muc <- get_osm(muc_bbox, osmsource_api(url = "http://api.openstreetmap.org/api/0.6/"))


OSM_in = file.path("data", "netherlands-latest.osm.pbf")
#OSMfile = unzip(OSMpbf_in)
ogrListLayers(OSM_in)
osm <- readOGR(OSM_in, "lines")

src = osmsource_osmosis(file = OSM_in)

ned_extent_in = file.path("data", "map")
xml_bbox = xmlToList(xmlParse(ned_extent_in))
center_lat = (as.numeric(xml_bbox$bounds[1])+as.numeric(xml_bbox$bounds[3]))/2
center_lon = (as.numeric(xml_bbox$bounds[2])+as.numeric(xml_bbox$bounds[4]))/2
ned_bbox = center_bbox(center_lon, center_lat, 3000, 3000)

ned = get_osm(ned_bbox, src)
ned = get_osm(osm, src)

plot(osm)

BBosm = clip(osm, ned_bbox)


php_dir = "C:/git/openrouteservice/php/"


file.path("data", "netherlands-latest.osm.pbf")

resp3 = POST(paste(php_dir,"GETRoute.php", sep=""), body=params, encode="form")

respWS = POST(paste(php_dir,"GETRoute.php", sep=""), body=params, encode="form")

g=graph.full(100)
class(g)

plot(g)

get.shortest.paths(ResWor_pairs[1,],ResWor_pairs[2,])

plot(ResWor_pairs[1,])

# Set parameters
params = list(api_key="e2017639f5e987e6dc1f5f69a66d049c",
              start="5.22,52.23",
              end="5.25,52.25",
              via="",
              lang="en",
              distunit="KM",
              routepref="Car",
              weighting="Fastest",
              avoidAreas="",
              useTMC="",
              noMotorways="false",
              noTollways="false",
              noFerries="false",
              maxspeed="100",
              instructions="true",
              `_`="")

params = list(
  start="5.22,52.23",
  end="5.25,52.25",
  via="",
  lang="en",
  distunit="KM",
  routepref="Car",
  weighting="Fastest",
  noMotorways="false",
  noTollways="false",
  noFerries="false",
  noSteps="false",
  noUnpavedroads="false",
  instructions="true",
  api_key="eb85f2a6a61aafaebe7e2f2a89b102f5",
  `_`="")

params = list(
  start="5.22,52.23",
  end="5.25,52.25",
  via="",
  lang="en",
  distunit="KM",
  routepref="Car",
  weighting="Fastest"
  )


# *start = longitude and latitude of the start position, e.g. '7.0892567,50.7265543'
# *via (optional) = longitudes and latitudes of the via positions separated by blank, e.g. '7.0920891,50.7295968 7.1044487,50.7247613'
# *end = longitude and latitude of the end position, e.g. '7.0986258,50.7323634'
# *routepref = the preference of the routing: 'Car', 'Pedestrian', 'Bicycle', 'HeavyVehicle
# *weighting = the preference of the routing method: 'Fastest', 'Shortest', 'Recommended'
# *distunit = distance unit of route calculation (default in kilometer 'KM', meters 'M', miles 'MI')
# *noMotorways = Avoid Motorways? e.g. 'noMotorways=false' OR 'noMotorways=true'
# *noTollways = Avoid Tollways? e.g. 'noTollways=false' OR 'noTollways=true'
# *noFerries = Avoid Ferrys? e.g. 'noFerries=false' OR 'noFerries=true'
# *noUnpavedroads (only for Bicycle profile) = Avoid unpaved Roads? e.g. 'noUnpavedroads=false' OR 'noUnpavedroads=true'
# *noSteps (only for Bicycle profile) = Avoid Steps? e.g. 'noSteps=false' OR 'noSteps=true'
# *maxspeed specify the maximum speed in km/h for the selected route profile e.g. 'maxspeed=10'
# *instructions = Route instructions 'instructions=true' or 'instructions=false'
# *lang = language of routeinstructions: 'de' (Deutsch), 'en' (English), etc.


url = "http://www.openrouteservice.org/"

params = list(
  pos="8.92,52.28",
  zoom="14",
  layer="B000",
  routeOpt="Car",
  wp="8.26,52.29,8.89,52.27",
  lang="en",
  routeLang="en",
  distUnit="m",
  routeWeight="Fastest"
)

respWS = POST(url, body=params, encode="json")

#respWS = POST("http://www.openrouteservice.org/?pos=8.92,52.28&zoom=14&layer=B000&routeOpt=Car&wp=8.26,52.29,8.89,52.27&lang=en&routeLang=en&distUnit=m&routeWeight=Shortest", encode="form")
content(respWS)
summary(respWS)

respWS$content


proxy_url = "http://www.openrouteservice.org/cgi-bin/proxy.cgi?url=http://openls.geog.uni-heidelberg.de/routing?api_key=eb85f2a6a61aafaebe7e2f2a89b102f5"
respPR = POST(paste(proxy_url,"?start=9.256506,49.240011&end=9.156506,49.230011&via=&lang=de&distunit=KM&routepref=Car&weighting=Fastest")
content(respPR)

#http://openls.geog.uni-heidelberg.de/route?start=9.256506,49.240011&end=9.156506,49.230011&via=&lang=de&distunit=KM&routepref=Car&weighting=Fastest&avoidAreas=&useTMC=false&noMotorways=false&noTollways=false&noUnpavedroads=false&noSteps=false&noFerries=false&instructions=false

SOME_URL = ""

url="http://openls.geog.uni-heidelberg.de/routing" #?api_key=e2017639f5e987e6dc1f5f69a66d049c"
url = "http://openls.geog.uni-heidelberg.de/osm/routing"


text = read_xml("./src/modules/testrouting.xml")

dt = read.csv(url,text, header={'Content-Type':'application/xml'})


resp = POST(url, body=params, encode="form")




resp2 = POST("http://openls.geog.uni-heidelberg.de/routing?api_key=e2017639f5e987e6dc1f5f69a66d049c", body=params, encode="form")



respp = POST("openls.geog.uni-heidelberg.de/osm/routing?api_key=eb85f2a6a61aafaebe7e2f2a89b102f5&start=9.256506,49.240011&end=9.156506,49.230011&distunit=KM&routepref=Car", encode = "form")

resp3 = POST("https://github.com/GIScience/openrouteservice/blob/master/php/GETGeocode.php", body = params, encode = "form")
resp4 = POST("http://www.openrouteservice.org/php/GETRoute.php", body = params, encode = "form")

test1 = POST("http://openls.geog.uni-heidelberg.de/route?start=9.256506,49.240011&end=9.156506,49.230011&via=&lang=de&distunit=KM&routepref=Car&weighting=Fastest&avoidAreas=&useTMC=false&noMotorways=false&noTollways=false&noUnpavedroads=false&noSteps=false&noFerries=false&instructions=false")
content(test1)

content(respp)
content(resp3)
plot(resp)
class(resp)

xml_out = file.path("output", "test")
write_xml(resp, file=xml_out)

