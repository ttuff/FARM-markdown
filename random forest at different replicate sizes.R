library(randomForest)
setwd("~/Box Sync/colliding ranges/Simulations_humans/Results/available daily summaries")
details <- file.info(list.files())

trimmed_details <- details[which(list.files() %in% list.files(pattern = "Four_model_compare_results_extinct*")),]
ord <- order(trimmed_details$mtime, decreasing = TRUE)
rownames(trimmed_details[ord,])[1]
load(rownames(trimmed_details[ord,])[1])
extinct <- Concatenated_data

trimmed_details <- details[which(!(list.files() %in% list.files(pattern = "Four_model_compare_results_extinct*"))),]
ord <- order(trimmed_details$mtime, decreasing = TRUE)
rownames(trimmed_details[ord,])[1]
load(rownames(trimmed_details[ord,])[1])
extant <- Concatenated_data


#load('/Volumes/Tuff/Box Sync/Four model compare third run/Four_model_compare_results_26_Jun_2017_crop_to_745.Rdata')
extant <- Concatenated_data

one <- subset(extant, Model_type=="01" )
two <- subset(extant, Model_type=="02" )
three <- subset(extant, Model_type=="05" )
four <- subset(extant, Model_type=="06" )
length(one[,1])
length(two[,1])
length(three[,1])
length(four[,1])


crop_top <- 740
for(crop in seq(40, crop_top, by=100)){
	
one <- subset(extant, Model_type=="01" )
two <- subset(extant, Model_type=="02" )
three <- subset(extant, Model_type=="05" )
four <- subset(extant, Model_type=="06" )
length(one[,1])
length(two[,1])
length(three[,1])
length(four[,1])

one <- one[1:crop,]
two <- two[1:crop,]
three <- three[1:crop,]
four <- four[1:crop,]

Concatenated_data <- rbind(one, two, three, four)
dim(Concatenated_data)
save(Concatenated_data, file=paste0("~/Desktop/Evenly spaced dates RF outputs through time/Four_model_compare_results_", format(Sys.time(), format="%d_%b_%Y"),"_crop_to_",crop,"_.Rdata"))
print(crop)
}

#########
setwd("~/Desktop/Evenly spaced dates RF outputs through time")
list.files()
details <- file.info(list.files(pattern="Four_model_compare_results*"))
namers <- rownames(details)
namer_list <- strsplit(namers, "_")
days_files <- do.call(rbind, namer_list)
days_files <- cbind(days_files, namers)


ord <- order(as.numeric(days_files[,10]), decreasing = FALSE)
trimmed_standards <- days_files[ord,]

library(randomForest)
list_item <- 5 
###########
replacement_run <- function(list_item, trimmed_standards){
load(trimmed_standards[list_item,12])
load('~/Box Sync/colliding ranges/Simulations_humans/Available trees/real.analysis.RData')
load('~/Desktop/real.analysis.RData')
load('~/Desktop/Evenly spaced dates RF outputs through time/Four_model_compare_results_05_Jul_2017_crop_to_2916.Rdata')
length(Concatenated_data[,1])
names(Concatenated_data)
#Concatenated_data <- Concatenated_data[Concatenated_data[, 2] == "stats.no.bTO", ]
#Concatenated_data <- Concatenated_data[Concatenated_data[, 6] != "05", ]
# Concatenated_data[, 6] <- as.numeric(Concatenated_data[, 6])
# # Concatenated_data[original[, 2] == "background_takeover", 6] <-  Concatenated_data[original[, 2] == "background_takeover", 6] + 4
Concatenated_data[, 6] <- factor(Concatenated_data[, 6])
#head(Concatenated_data)
#names(Concatenated_data)

PCAdata <- Concatenated_data[, -(1:35)]
head(PCAdata)
length(as.numeric(PCAdata[,3]))

PCAdata <- PCAdata[, -12]
PCAdata <- apply(PCAdata, 2, as.numeric)
remove <- apply(is.na(PCAdata), 1, any)
length(PCAdata[,1])
PCAdata <- PCAdata[!remove, ]
length(PCAdata[,1])
# Predictions
load('~/Desktop/real.analysis.RData')

data.analysis.comp2 <- data.frame("Model" = as.factor(Concatenated_data[!remove, 6]),
                                  PCAdata)

head(data.analysis.comp2)



#load("Real_phy/real.analysis.RData")
a <- as.data.frame(real.analysis$results_summary_of_single_value_outputs)



head(data.analysis.comp2)
data.analysis.comp3 <- data.analysis.comp2[, -2]
head(data.analysis.comp3)
which(is.na(data.analysis.comp3))
dels <- which(is.na(data.analysis.comp3), arr.ind=TRUE)

if(length(dels) > 0){data.analysis.comp4 <- data.analysis.comp3[-dels,]
data.analysis.comp3 <- data.analysis.comp4
}
#data.analysis.comp3 <- data.analysis.comp3[data.analysis.comp3$Model %in% 1:4, ]
#data.analysis.comp3$Model <- factor(data.analysis.comp3$Model)
#sub <- unlist(lapply(as.list(c(1:4)), function(x, y) {
#  sample(which(y$Model == x), min(table(data.analysis.comp3$Model)))},
#  y = data.analysis.comp3))
# data.analysis.comp3 <- data.analysis.comp3[sub, ]
fun <- function(x, y, per = .33) {sample(which(y$Model == x), round(table(y$Model)[1]*per))}

sub.test <- unlist(lapply(as.list(paste0(0, c(1,2,5,6))), fun,
                          y = data.analysis.comp3))
test2 <- data.analysis.comp3[sub.test, 2:ncol(data.analysis.comp3)]
test1 <- data.analysis.comp3[sub.test, 1]
train <- data.analysis.comp3[-sub.test, ]

train[, -1] <- apply(train[, -1], 2, as.numeric)
test2 <- as.data.frame(apply(test2, 2, as.numeric))
infinites <- which(apply(train[, -1], 2, is.infinite), arr.ind=T)
if (nrow(infinites) > 0) {
train <- train[-infinites[, 1], ]
}
infinites2 <- which(apply(test2, 2, is.infinite), arr.ind=T)
if (nrow(infinites2) > 0) {
test2 <- test2[-infinites2[, 1], ]
test1 <- test1[-infinites2[, 1]]
}

i <- 1
for(i in 1:100){
fit <- randomForest(Model ~ ., data=train, xtest = test2, ytest = test1, 
                    importance=TRUE, ntree=2000, keep.forest = TRUE, replace=TRUE)

#predictions <- predict(fit, 
#                       a,
#                       type="prob")
#predictions

save(fit, file=paste0("~/Desktop/RandomForest outputs/RF_daily_output_", format(Sys.time(), format="%d_%b_%Y"), "_", trimmed_standards[list_item,10],"_",i, "_REPLACEMENT_.Rdata"))


(fit <- randomForest(Model ~ ., data=train, xtest = test2, ytest = test1, 
                    importance=TRUE, ntree=2000, keep.forest = TRUE, replace=FALSE))

#predictions <- predict(fit, 
#                       a,
#                       type="prob")
#predictions

save(fit, file=paste0("~/Desktop/RandomForest outputs/RF_daily_output_", format(Sys.time(), format="%d_%b_%Y"), "_", trimmed_standards[list_item,10],"_",i, "_NoREPLACEMENT_.Rdata"))

}
}

boxplot(predictions, ylim=c(0,1))

#replacement_run(1, trimmed_standards)


library(parallel) 

	NAI <- rev(1:8)

	ncores <- detectCores()
	cl <- makeCluster(5, type = "PSOCK", outfile="./error_report.txt")
	
	
		clusterEvalQ(cl, library(randomForest))
		
		clusterExport(cl, varlist=ls())

		clusterApplyLB(cl, fun = replacement_run, x = NAI, trimmed_standards= trimmed_standards)

		stopCluster(cl)



getwd()