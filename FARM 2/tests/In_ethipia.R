# library(maptools)
# library(FARM)
# library(rgeos)
# library(maps)
#
# data("wrld_simpl")
# Ethiopia <- wrld_simpl[wrld_simpl@data$NAME == 'Ethiopia', ]
#
# coords <- as.matrix(apply(language_centroids[, 3:4], 2, as.numeric)) #coords
# map()
# points(coords, col = adjustcolor("blue", .4))
# check <- numeric(nrow(coords))
#
# for (i in 1:nrow(coords)) {
#   coords.sp <- SpatialPoints(coords[i, , drop = F])
#   proj4string(coords.sp) <- proj4string(Ethiopia)
#   check[i] <- gWithin(coords.sp, Ethiopia)
# }
#
#
# points(coords[as.logical(check), ], col = adjustcolor("red", .4))
#
#
# language_centroids <- cbind(language_centroids, as.logical(check))
# colnames(language_centroids)[ncol(language_centroids)] <- "in_Ethiopia"
# devtools::use_data(language_centroids, overwrite = TRUE)
