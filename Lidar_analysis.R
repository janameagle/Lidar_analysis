#library(rgdal)
#library(raster)
library(tmap)
#library(tmaptools)
library(lidR)
#library(RStoolbox)
library(ggplot2)
library(viridis)

# red the data, a las file
# area of a 3d point cloud object (computed in the same units as the coordinate reference system)
setwd <- getwd()
lidar = readLAS("data/ST2_A01_2016_laz_61.laz")
area(lidar)
crs(lidar)


# plot las data point cloud
plot(lidar)
plot(lidar, color = "Z", bg = "white", axis = TRUE, legend = TRUE)


# Outliers of intensity breaks the color range. Use the trim parameter.
#plot(lidar, color = "Intensity", colorPalette = heat.colors(50))
#plot(lidar, color = "Intensity", colorPalette = heat.colors(50), trim = 0.99)


################################################################################
#### Digital Surface Model######################################################
# This is not a CHM as the data was not normalized with Terrain yet
thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- grid_canopy(lidar, 1, pitfree(thr, edg))
my_colors <- height.colors(50)
plot(chm, col = my_colors)


################################################################################
#### Creating Digital Terrain Model ############################################
# using different interpolation methods

dtm = grid_terrain(lidar, algorithm =  knnidw(k = 6, p = 2))

plot(dtm, col = gray.colors(50, 0, 1))
plot_dtm3d(dtm)

tm_shape(dtm) +
  tm_raster(style= "cont") +
  tm_layout(legend.outside = TRUE)
crs(dtm1) <- "+proj=utm +zone=21 +datum=WGS84"

# hillshade
slope <- terrain(dtm, opt='slope')
aspect <- terrain(dtm, opt='aspect')
hs <- hillShade(slope, aspect, angle=45, direction=315)
tm_shape(hs)+
  tm_raster(style= "cont")+
  tm_layout(legend.outside = TRUE)




################################################################################
#### normalize DSM #############################################################
lidarnorm <- lasnormalize(lidar, dtm)
chm2 <- grid_canopy(lidarnorm, res = 2, pitfree(c(0,2,5,10,15), c(0, 1)))

tm_shape(chm2)+
  tm_raster(style= "quantile", n=7)+
  tm_layout(legend.outside = TRUE)


################################################################################
#### Tree segmentation #########################################################

# get the location of the trees
ttops <- find_trees(lidar, lmf(ws = 5))

#plot(chm2, col = height.colors(50))
#plot(ttops, add = TRUE)

x <- plot(lidar, bg = "white", size = 3)
add_treetops3d(x, ttops)

tree_seg <- segment_trees(lidar, dalponte2016(chm, ttops))
my_colors <- random.colors(200)
plot(tree_seg, color = "treeID", colorPalette = my_colors)

crowns <- delineate_crowns(tree_seg)
par(mar=rep(0,4))
plot(crowns)

################################################################################
#### Statistics ################################################################
# statistics will be applied to the first returns only

head(lidar)
first_returns <- lasfilterfirst(lidar)
metrics <- stdmetrics(lidar$X, lidar$Y, lidar$Z, lidar$Intensity, lidar$ReturnNumber, lidar$Classification, dz=10)
metrics <- grid_metrics(first_returns, ~stdmetrics(lidar$X, lidar$Y, lidar$Z, lidar$Intensity, lidar$ReturnNumber, lidar$Classification, dz=10))
metrics <- grid_metrics(first_returns, .stdmetrics, res=10)
names(metrics)

metric <- metrics$zmean

metric[metric<0]=0
tm_shape(metric)+
  tm_raster(style= "quantile", n=7)+
  tm_layout(legend.outside = TRUE)
