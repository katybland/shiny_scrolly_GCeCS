

#earthdata

library("raster")
r <-  raster("data/20151115090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")

plot(r)  #takes a few minutes
# see if I can project onto a sphere (stereographic projection?)
# restrict the bounding box to coordinates of pnw
# do sst anomaly?

# proj4string(r)=CRS("+init=EPSG:4326")
