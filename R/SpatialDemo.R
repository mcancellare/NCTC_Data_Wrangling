# This is for playing with spatial data
require(sf)
require(raster)
require(tidyverse)
require(mapview)

# Get some Starkey Data
bndy<- st_read(dsn = "C:/Users/mcancellare/Desktop/CSP1004/Spatial", layer = "StarkeyBoundary")
strm<- st_read(dsn = "C:/Users/mcancellare/Desktop/CSP1004/Spatial", layer = "StarkeyStreams")

# Get collared dataset
pts<- read_delim("C:/Users/mcancellare/Desktop/CSP1004/Data/271_E_2012_G_JuneSubset.txt", delim = "\t") %>% 
  select(AID, Pres, geometry) %>% 
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26911")

mapview(bndy) + mapview(strm, zcol = "Name") + mapview(pts)

# Add in a raster for NLCD 2011
nlcd<- raster("C:/Users/mcancellare/Desktop/CSP1004/Spatial/nlcdraw.tif")

plot(nlcd)
plot(pts$geometry, add = TRUE)

# First we need to reclassify NLCD to be Forest/NotForest
# What are current categories in NLCD
freq(nlcd)
# Create Reclass table, make forest = 1, the rest NA
rcls<- data.frame(is =c(21, 22, 42, 52, 71, 82, 95),
                  becomes = c(NA, NA, 1, NA, NA, NA, NA))
  
  
# Now reclass raster
forest<- reclassify(nlcd, rcl = as.matrix(rcls))
plot(forest)

# Create a distance raster of meters from forest edge
forDist<- distance(forest)
plot(forDist)

# Create a distance raster of meters to stream
strmDist<- rasterize(as(strm, "Spatial"), nlcd)
plot(strmDist)
strmDist<- distance(strmDist)

# Stack our rasters together
rs<- stack(nlcd, forDist, strmDist)
plot(rs)
# Fix our names (were originally named nlcdRaw, layer.1, and layer.2
names(rs)[2:3]<- c("ForestDist", "StreamDist")
rs
plot(rs)

# Create pseudo absence points using Starkey Boundary
rpts<- st_sample(bndy, size = nrow(pts) * 2) %>% 
  st_sf(data.frame(AID = "Random", Pres = 0)) %>% 
  mutate(geometry = st_as_text(.)) %>% 
  st_drop_geometry() %>% 
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26911")
# Fix the geometry column
#names(rpts)[3]<- "geometry"
#st_geometry(rpts)<- "geometry"

        
# view in mapview to ensure they are aligned within boundary
mapview(rpts) + mapview(bndy)

mySamp<- pts %>% 
  rbind(rpts)

plot(mySamp$geometry)

# Exctract raster values to points
e<- data.frame(raster::extract(rs, as(mySamp, "Spatial")))
# Add attributes back to points
mySamp<- mySamp %>% 
  bind_cols(e)

# COerce NLCD to be a factor
mySamp<- mySamp %>% 
  mutate(nlcdRaw = as.factor(nlcdRaw))

# Let's fit a logistic regression
mod<- glm(Pres ~ nlcdRaw + ForestDist + StreamDist, data = mySamp, family = binomial(link = "logit"))

# Look at the results
summary(mod)

# Predict back to the landscape
pred<- predict(rs, model = mod, type = "response")
plot(pred)

#If we want to model these data, we need to create pseudo absence points (need presence and absence data). Will create these back upstream 