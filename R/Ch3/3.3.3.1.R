library(lubridate)
library(xts)
library(readr)
library(here)
library(sp)
library(ggmap)
library(tidyverse)

sp500 = xts(
 c(1102.94, 1104.49, 1115.71, 1118.31),
 ymd(
   c("2010-02-25", "2010-02-26", "2010-03-01", "2010-03-02"),
   tz = Sys.getenv("TZ")
 )
)
sp500

sp500["2010-02"]
sp500["2010-03"]
sp500["2010-02-26/2010-03-01"]
sp500["2010-02-23/2010-03-03"]
sp500["2010-02-27/"]

data(AirPassengers)
ap = as.xts(AirPassengers)
apRel = diff(ap) / ap[-length(ap)]

head(embed(ap, 4))

createEmbedDS <- function(s, emb=4) {
  d = dim(s)
  if (!is.null(d) && d[2] > 1) stop("Only applicable to uni-variate time series")
  if (emb < 2 || emb > length(s)) stop("Invalid embed size")
  e = embed(s, emb)
  colnames(e) = c("T", paste("T", 1:(emb - 1), sep = "_"))
  if (is.xts(s)) return(
    xts(e, index(s)[emb:length(s)])
  )
  else return(e)
}
dataSet <- createEmbedDS(ap, emb = 5)
head(dataSet)

ff = read_csv(here("Data", "forestFires.txt"))

spatialCoords = select(ff, long = x, lat = y) # the contextual data
firesData = select(ff, ano2000) # the behavioral data
coordRefSys = CRS("+proj=longlat +ellps=WGS84")
fires2000 = SpatialPointsDataFrame(
  spatialCoords,
  firesData,
  proj4string = coordRefSys
)

fires2000[1:3,]

bbox(fires2000)
coordinates(fires2000)[1:3,]
summary(fires2000)

mapPT = get_map("Portugal", zoom = 7)
d4plot = as_tibble(cbind(coordinates(fires2000), burnt = fires2000$ano2000))
# Google requires an API key now :(
ggmap(mapPT) +
  geom_point(
   data = filter(d4plot, burnt == 1),
   aes(x = long, y = lat),
   col = "orange"
  )
