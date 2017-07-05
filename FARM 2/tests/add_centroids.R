# load("others/rich.Rdata")
# load("~/Box Sync/Bruno/WUSTL projects/culture diversity (now domestication)/Analysis/mydata.RData")
# load('/Users/bvilela/Box Sync/Bruno/WUSTL projects/Simulations_humans/Real_phy/tree.Rdata')
#
# data <- read.csv('others/Merged_language_dataframe_FARM.csv', row.names = 'X')
#
# data$in_D_place_dataset[!data$ISO_merger %in% mydata$ISO_code] <- NA
#
# data <- data[!is.na(data$Glottolog_longitude), ]
# data <- data[!duplicated(data$ISO_merger), ]
# keep <- !is.na(data$in_D_place_dataset) & !is.na(data$in_D_place_tree)
#
# tree$tip.label[!tree$tip.label %in% data$ISO_merger[keep]]
#
#
# library(maps)
# map()
# points(data[!keep, c("Glottolog_longitude",
#                      "Glottolog_latitude")],
#        col = rgb(.5, .5, .5, .4), pch = 20)
# points(data[keep, c("Glottolog_longitude",
#                     "Glottolog_latitude")],
#        col = rgb(1, 0, 0, .4), pch = 20)
# points(data[keep, c("D_place_adj_longitude",
#                     "D_place_adj_latitude")],
#        col = rgb(0, 0, 01, .4), pch = 20)
#
#
# is_in_D_place <- ifelse(keep, "in_D_place", "not_in_D_place")
# ISO_label <- as.character(data$ISO_merger)
# longitude <- data$Glottolog_longitude
# longitude[keep] <- data$D_place_adj_longitude[keep]
# latitude <- data$Glottolog_latitude
# latitude[keep] <- data$D_place_adj_latitude[keep]
#
#
# language_centroids <- cbind(is_in_D_place, ISO_label, longitude, latitude)
# language_centroids <- language_centroids[!is.na(language_centroids[, 3]), ]
# language_centroids <- language_centroids[!is.na(language_centroids[, 4]), ]
#
# devtools::use_data(language_centroids, overwrite = TRUE)
#
#
#
#
# suitability <- extract(rich, cbind(mydata$Longitude, mydata$Latitude))
# mydata$farming_binary <- ifelse((mydata$agriculture + mydata$animal_husbandry) == 0, 0, 1)
#
#
# rem <- is.na(suitability)
# cut <- numeric(max(suitability[!rem], na.rm = T))
# for(i in 1:max(suitability[!rem], na.rm = T)) {
#   cut[i] <- summary(lm(mydata$farming_binary[!rem] ~
#                          as.factor(ifelse(suitability[!rem] > i, 1, 0))))[["r.squared"]]
# }
#
# r <- rich
# values(r) <- ifelse(values(r) > 10, 1, 0)
# plot(r)
# coords <- apply(language_centroids[, 3:4], 2, as.numeric)
# suitability2 <- extract(r, coords)
# suitability2[is.na(suitability2)] <- 0
# suitability2 <- as.matrix(suitability2)
# devtools::use_data(suitability2, overwrite = TRUE)
