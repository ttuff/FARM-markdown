# library(rgdal)
# library(maptools)
# library(maps)
# library(FARM)
# library(rgeos)
#
#
# origins <- readOGR("/Users/bvilela/Box Sync/Bruno/WUSTL projects/culture diversity (now domestication)/Origins/Origins_updated.shp")
# map()
# plot(origins, add = T, col = "black")
#
# coords <- as.matrix(apply(language_centroids[, 3:4], 2, as.numeric)) #coords
# points(coords, col = adjustcolor("blue", .4))
# check <- numeric(nrow(coords))
# for (i in 1:nrow(coords)) {
#   coords.sp <- SpatialPoints(coords[i, , drop = F])
#   proj4string(coords.sp) <- proj4string(origins)
#   check[i] <- gWithin(coords.sp, origins)
# }
#
#
# points(coords[as.logical(check), ], col = adjustcolor("red", .4))
#
#
# language_centroids <- cbind(language_centroids, as.logical(check))
# colnames(language_centroids)[ncol(language_centroids)] <- "in_origin"
# devtools::use_data(language_centroids, overwrite = TRUE)
