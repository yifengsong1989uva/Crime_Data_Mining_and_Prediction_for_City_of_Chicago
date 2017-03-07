library(ks)
library(RColorBrewer)
library(geoR)
library(aqfig)
source("CrimeUtil.R")
#I made a small change to the plot.spatial.kde() function in the original
#"CrimeUtil.R" file, so I attached it to the Collab as well


# set prediction resolution
prediction.resolution.meters = 200

# read chicago boundary
city.boundary = read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/City_Boundary/City_Boundary", 
  "poly", "+init=epsg:3435", "+init=epsg:26971")



# get crime samples in each month, store them in a list
theft.all = sample.crime("2014_THEFT.csv", -1)
theft.monthly <- list()
for (i in 1:12) {
  theft.monthly[[i]] <- theft.all[theft.all$month==i,][,c("x","y")]
}
rm(theft.all)

# get negative observations (non crime points) within chicago
non.crime.points <- cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] <- "response"
# these grid points (with the city boudary) will also be used as the prediction points
prediction.points <- non.crime.points[,c("x","y")]

# get positive observations from each month within chicago, store them in a list
crime.points.monthly <- list()
for (i in 1:12) {
  crime.points = cbind(1, theft.monthly[[i]][,c("x","y")])
  names(crime.points)[1] = "response"
  crime.points.monthly[[i]]=crime.points
}



# calculate distance to nearest school from each non-crime training point (grid points)
# These values can also be used when using the grid points to make predictions
school.points <- read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/Schools/CPS_School_Locations_SY1415", 
  "points", "+init=epsg:3435", "+init=epsg:26971")@coords
#plot the school points within the city boundary
file_name <- paste("Images/ScatterPlots/Schools in Chicago.png",sep="")
png(file_name,height=720,width=670)
plot(school.points,pch=".",asp=1)
plot(city.boundary,add=T)
title_name <- "Schools in Chicago"
title(title_name)
dev.off()
# calculate minimum distance
school.min.distance <- get.min.distances(non.crime.points[,c("x","y")], school.points)

# calculate distance to nearest police station from each non-crime training point (grid points)
# These values can also be used when using the grid points to make predictions
police.points <- read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/PoliceStations/PoliceStationsDec2012", 
  "points", "+init=epsg:3435", "+init=epsg:26971")@coords
#plot the police station points within the city boundary
file_name <- paste("Images/ScatterPlots/Police Stations in Chicago.png",sep="")
png(file_name,height=720,width=670)
plot(police.points,pch=".",cex=4,asp=1,ylim=c(550000,600000))
plot(city.boundary,add=T)
title_name <- "police stations in Chicago"
title(title_name)
dev.off()
#calculate the minimum distance
police.min.distance <- get.min.distances(non.crime.points[,c("x","y")], police.points)

#For Parks shapefile, the type is "polygon". So first read in the shapefile
#as the polygons, and then extract the coordinates for each polygon. For convenience, just
#convert the polygons into points by calculating the mean x and y coordinates for each polygon, 
#and treat the (x,y) values as the new coordinate for each mall or plaza
park.polygons <- read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/Parks/Parks_Aug2012", 
  "poly", "+init=epsg:3435", "+init=epsg:26971")
park.polygons.list <- park.polygons@polygons
#write this function to calculate the center point of each polygon (each polygon stands for a park)
Center_Points <- function (polys) {
  poly_coords <- polys@Polygons[[1]]@coords
  n <- nrow(poly_coords)
  return(c(mean(poly_coords[1:n-1,1]),mean(poly_coords[1:n-1,2])))
}
park.points <- as.data.frame(t(sapply(park.polygons.list,Center_Points)))
colnames(park.points) <- c("x","y")
#plot the park points within the city boundary
file_name <- paste("Images/ScatterPlots/Parks in Chicago.png",sep="")
png(file_name,height=720,width=650)
plot(park.points$x,park.points$y,pch=".",cex=2,asp=1,ylim=c(550000,600000))
plot(city.boundary,add=T)
title_name <- "parks in Chicago"
title(title_name)
dev.off()
# calculate distance to nearest park from each non-crime training point (grid points)
# These values can also be used when using the grid points to make predictions
park.min.distance <- get.min.distances(non.crime.points[,c("x","y")], park.points)



# calculate the minimum distances from the actual crime points to the schools, police stations
# and malls & plazas, for each month from Feb. to Oct., store them in the lists. They will be
# used in training the data
school.min.distance.crime.points <- list()
police.min.distance.crime.points <- list()
park.min.distance.crime.points <- list()
for (i in 2:10) {
  school.min.distance.crime.points[[i]] <- get.min.distances(crime.points.monthly[[i]][,c("x","y")], 
                                                             school.points)
  police.min.distance.crime.points[[i]] <- get.min.distances(crime.points.monthly[[i]][,c("x","y")], 
                                                             police.points)
  park.min.distance.crime.points[[i]] <- get.min.distances(crime.points.monthly[[i]][,c("x","y")], 
                                                           park.points)
}



#grid.points.all are points in the rectangular area, including both the points within the city boundary
#and points not within the city boundary
grid.points.all = get.grid.points(city.boundary, prediction.resolution.meters,FALSE)

#find the index for each grid point that falls into the city boundary in the entire data frame
#of all of the grid points
sp.points <-  SpatialPoints(grid.points.all[,c("x", "y")], proj4string=city.boundary@proj4string)
points.in.boundary <-  !is.na(over(sp.points, city.boundary)$OBJECTID)
points.in.boundary.indices <- which(points.in.boundary==T)



#calculate the kde estimation value for each point in both the training set and the testing set
#(month 2-11)
kde.for.training <- list()
kde.for.testing <- list()
for (i in 2:11) {
  kde.for.testing[[i]] <- run.spatial.kde(theft.monthly[[i-1]],non.crime.points[,c("x","y")],1000)
  kde.for.crime.points <- run.spatial.kde(theft.monthly[[i-1]],theft.monthly[[i]],1000)
  kde.for.training[[i]] <- c(kde.for.testing[[i]],kde.for.crime.points)
}





#####
##### Make prediction for months 3-11 using the full model
##### (4 variables: kde, school, police station, park; run ten cross-validations)
#####

logistic_regression_coefficients <- data.frame()
theft.prediction.results <- list()
for (i in 2:10) {

# combine positive (1) and negative (0) points
training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])

# combine minimum distance points
school.min.distance.training <- c(school.min.distance, school.min.distance.crime.points[[i]])
police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[i]])
park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[i]])

# add predictor columns (theft density -- x1, school distances -- x2, police station distances -- x3,
# park distances -- x4) to the training data
training.data = cbind(training.data.response, kde.for.training[[i]], 
                      school.min.distance.training, police.min.distance.training,
                      park.min.distance.training)
colnames(training.data) <- c("response","x","y","x1","x2","x3","x4")

# fit GLM -- NOT include x- and y-coordinates as predictors.
glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
glm_summary <- summary(glm.fit)$coefficients
glm_coef <- glm_summary[c(2,17,3,18,4,19,5,20)]

logistic_regression_coefficients <- rbind(logistic_regression_coefficients,glm_coef)

# build dataframe to predict, with the kde based on the previous month's data
prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                      school.min.distance, police.min.distance,
                                      park.min.distance))
colnames(prediction.data) <- c("x","y","x1","x2","x3","x4")

# run prediction (theft.threats is the probability between 0 and 1)
theft.threats = predict(glm.fit, prediction.data, type="response")
theft.prediction.results[[i+1]] <- theft.threats

# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction = cbind(prediction.points, theft.threats)
names(theft.prediction) = c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from the month, using the surveillance plot
file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                   " by logistic regression.png",sep="")
png(file_name,height=458,width=717)
plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
title(sub=paste("Month ",as.character(i+1)," by logistic regression",sep=""))
dev.off()

}

colnames(logistic_regression_coefficients) <- c("x1","p_x1","x2","p_x2","x3","p_x3","x4","p_x4")





#####
##### predictions for month 3-11 only using last month's kde,
##### for comparison with logistic regression
#####
for (i in 3:11) {
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i),
                     " by KDE.png",sep="")
  png(file_name,height=458,width=717)
  theft.prediction <- cbind(prediction.points,kde.for.testing[[i]])
  colnames(theft.prediction) <- c("x","y","threat")
  plot.surveillance.curve(theft.prediction,
                          crime.points.monthly[[i]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i)," by KDE",sep=""))
  dev.off()
}





#####
##### Make prediction for months 3-11 using the partial model 1
##### (3 variables: kde, school, police station; run ten cross-validations)
#####

logistic_regression_coefficients.partial1 <- data.frame()
theft.prediction.results.partial1 <- list()
for (i in 2:10) {
  
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])
  
  # combine minimum distance points
  school.min.distance.training <- c(school.min.distance, school.min.distance.crime.points[[i]])
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[i]])
  
  # add predictor columns (theft density -- x1, school distances -- x2, police station distances -- x3)
  # to the training data
  training.data = cbind(training.data.response, kde.for.training[[i]], 
                        school.min.distance.training, police.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2","x3")
  
  # fit GLM -- NOT include x- and y-coordinates as predictors.
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(2,14,3,15,4,16)]
  
  logistic_regression_coefficients.partial1 <- rbind(logistic_regression_coefficients.partial1,glm_coef)
  
  # build dataframe to predict, with the kde based on the previous month's data
  prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                        school.min.distance, police.min.distance))
  colnames(prediction.data) <- c("x","y","x1","x2","x3")
  
  # run prediction (theft.threats is the probability between 0 and 1)
  theft.threats = predict(glm.fit, prediction.data, type="response")
  theft.prediction.results.partial1[[i+1]] <- theft.threats
  
  # build prediction dataframe for evaluation -- must have x, y, and threat columns
  theft.prediction = cbind(prediction.points, theft.threats)
  names(theft.prediction) = c("x", "y", "threat")
  
  # evaluate prediction on ground-truth crime records from the month, using the surveillance plot
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                     " by logistic regression - school and police.png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i+1)," by logistic regression (school and police)",sep=""))
  dev.off()
  
}

colnames(logistic_regression_coefficients.partial1) <- c("x1","p_x1","x2","p_x2","x3","p_x3")





#####
##### Make prediction for months 3-11 using the partial model 2
##### (3 variables: kde, school, park; run ten cross-validations)
#####

logistic_regression_coefficients.partial2 <- data.frame()
theft.prediction.results.partial2 <- list()
for (i in 2:10) {
  
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])
  
  # combine minimum distance points
  school.min.distance.training <- c(school.min.distance, school.min.distance.crime.points[[i]])
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[i]])
  
  # add predictor columns (theft density -- x1, school distances -- x2,
  # park distances -- x4) to the training data
  training.data = cbind(training.data.response, kde.for.training[[i]], 
                        school.min.distance.training, park.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2","x4")
  
  # fit GLM -- NOT include x- and y-coordinates as predictors.
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(2,14,3,15,4,16)]
  
  logistic_regression_coefficients.partial2 <- rbind(logistic_regression_coefficients.partial2,glm_coef)
  
  # build dataframe to predict, with the kde based on the previous month's data
  prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                        school.min.distance, park.min.distance))
  colnames(prediction.data) <- c("x","y","x1","x2","x4")
  
  # run prediction (theft.threats is the probability between 0 and 1)
  theft.threats = predict(glm.fit, prediction.data, type="response")
  theft.prediction.results.partial2[[i+1]] <- theft.threats
  
  # build prediction dataframe for evaluation -- must have x, y, and threat columns
  theft.prediction = cbind(prediction.points, theft.threats)
  names(theft.prediction) = c("x", "y", "threat")
  
  # evaluate prediction on ground-truth crime records from the month, using the surveillance plot
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                     " by logistic regression - school and park.png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i+1)," by logistic regression - school and park",sep=""))
  dev.off()
  
}

colnames(logistic_regression_coefficients.partial2) <- c("x1","p_x1","x2","p_x2","x4","p_x4")





#####
##### Make prediction for months 3-11 using the partial model 3
##### (3 variables: kde, police station, park; run ten cross-validations)
#####

logistic_regression_coefficients.partial3 <- data.frame()
theft.prediction.results.partial3 <- list()
for (i in 2:10) {
  
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])
  
  # combine minimum distance points
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[i]])
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[i]])
  
  # add predictor columns (theft density -- x1, police station distances -- x3,
  # park distances -- x4) to the training data
  training.data = cbind(training.data.response, kde.for.training[[i]], 
                        police.min.distance.training, park.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x3","x4")
  
  # fit GLM -- NOT include x- and y-coordinates as predictors.
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(2,14,3,15,4,16)]
  
  logistic_regression_coefficients.partial3 <- rbind(logistic_regression_coefficients.partial3,glm_coef)
  
  # build dataframe to predict, with the kde based on the previous month's data
  prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                        police.min.distance, park.min.distance))
  colnames(prediction.data) <- c("x","y","x1","x3","x4")
  
  # run prediction (theft.threats is the probability between 0 and 1)
  theft.threats = predict(glm.fit, prediction.data, type="response")
  theft.prediction.results.partial3[[i+1]] <- theft.threats
  
  # build prediction dataframe for evaluation -- must have x, y, and threat columns
  theft.prediction = cbind(prediction.points, theft.threats)
  names(theft.prediction) = c("x", "y", "threat")
  
  # evaluate prediction on ground-truth crime records from the month, using the surveillance plot
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                     " by logistic regression - police and park.png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i+1)," by logistic regression - police and park",sep=""))
  dev.off()
  
}

colnames(logistic_regression_coefficients.partial3) <- c("x1","p_x1","x3","p_x3","x4","p_x4")




#####
##### Make prediction for months 3-11 using the partial model 4
##### (2 variables: kde, school; run ten cross-validations)
#####

logistic_regression_coefficients.partial4 <- data.frame()
theft.prediction.results.partial4 <- list()
for (i in 2:10) {
  
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])
  
  # combine minimum distance points
  school.min.distance.training <- c(school.min.distance, school.min.distance.crime.points[[i]])
  
  # add predictor columns (theft density -- x1, school distances -- x2)
  # to the training data
  training.data = cbind(training.data.response, kde.for.training[[i]], 
                        school.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2")
  
  # fit GLM -- NOT include x- and y-coordinates as predictors.
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(2,11,3,12)]
  
  logistic_regression_coefficients.partial4 <- rbind(logistic_regression_coefficients.partial4,glm_coef)
  
  # build dataframe to predict, with the kde based on the previous month's data
  prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                        school.min.distance))
  colnames(prediction.data) <- c("x","y","x1","x2")
  
  # run prediction (theft.threats is the probability between 0 and 1)
  theft.threats = predict(glm.fit, prediction.data, type="response")
  theft.prediction.results.partial4[[i+1]] <- theft.threats
  
  # build prediction dataframe for evaluation -- must have x, y, and threat columns
  theft.prediction = cbind(prediction.points, theft.threats)
  names(theft.prediction) = c("x", "y", "threat")
  
  # evaluate prediction on ground-truth crime records from the month, using the surveillance plot
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                     " by logistic regression - school.png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i+1)," by logistic regression (school)",sep=""))
  dev.off()
  
}

colnames(logistic_regression_coefficients.partial4) <- c("x1","p_x1","x2","p_x2")





#####
##### Make prediction for months 3-11 using the partial model 5
##### (2 variables: kde, police station; run ten cross-validations)
#####

logistic_regression_coefficients.partial5 <- data.frame()
theft.prediction.results.partial5 <- list()
for (i in 2:10) {
  
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])
  
  # combine minimum distance points
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[i]])
  
  # add predictor columns (theft density -- x1, police station distances -- x3)
  # to the training data
  training.data = cbind(training.data.response, kde.for.training[[i]], 
                        police.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x3")
  
  # fit GLM -- NOT include x- and y-coordinates as predictors.
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(2,11,3,12)]
  
  logistic_regression_coefficients.partial5 <- rbind(logistic_regression_coefficients.partial5,glm_coef)
  
  # build dataframe to predict, with the kde based on the previous month's data
  prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                        police.min.distance))
  colnames(prediction.data) <- c("x","y","x1","x3")
  
  # run prediction (theft.threats is the probability between 0 and 1)
  theft.threats = predict(glm.fit, prediction.data, type="response")
  theft.prediction.results.partial5[[i+1]] <- theft.threats
  
  # build prediction dataframe for evaluation -- must have x, y, and threat columns
  theft.prediction = cbind(prediction.points, theft.threats)
  names(theft.prediction) = c("x", "y", "threat")
  
  # evaluate prediction on ground-truth crime records from the month, using the surveillance plot
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                     " by logistic regression - police.png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i+1)," by logistic regression (police)",sep=""))
  dev.off()
  
}

colnames(logistic_regression_coefficients.partial5) <- c("x1","p_x1","x3","p_x3")





#####
##### Make prediction for months 3-11 using the partial model 6
##### (2 variables: kde, park; run ten cross-validations)
#####

logistic_regression_coefficients.partial6 <- data.frame()
theft.prediction.results.partial6 <- list()
for (i in 2:10) {
  
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[i]])
  
  # combine minimum distance points
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[i]])
  
  # add predictor columns (theft density -- x1, park distances -- x4) to the training data
  training.data = cbind(training.data.response, kde.for.training[[i]], 
                        park.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x4")
  
  # fit GLM -- NOT include x- and y-coordinates as predictors.
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(2,11,3,12)]
  
  logistic_regression_coefficients.partial6 <- rbind(logistic_regression_coefficients.partial6,glm_coef)
  
  # build dataframe to predict, with the kde based on the previous month's data
  prediction.data = as.data.frame(cbind(prediction.points, kde.for.testing[[i+1]], 
                                        park.min.distance))
  colnames(prediction.data) <- c("x","y","x1","x4")
  
  # run prediction (theft.threats is the probability between 0 and 1)
  theft.threats = predict(glm.fit, prediction.data, type="response")
  theft.prediction.results.partial6[[i+1]] <- theft.threats
  
  # build prediction dataframe for evaluation -- must have x, y, and threat columns
  theft.prediction = cbind(prediction.points, theft.threats)
  names(theft.prediction) = c("x", "y", "threat")
  
  # evaluate prediction on ground-truth crime records from the month, using the surveillance plot
  file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(i+1),
                     " by logistic regression - park.png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(theft.prediction, crime.points.monthly[[i+1]], prediction.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(i+1)," by logistic regression - park",sep=""))
  dev.off()
  
}

colnames(logistic_regression_coefficients.partial6) <- c("x1","p_x1","x4","p_x4")









############################################################################################
#####
##### plot the heat map of the crime rates distribution predicted by logistic regression
#####
#using the March crime rates distribution predicted by logistic regression using the full model
prediction.march.all <- rep(0,nrow(grid.points.all))
for (i in 1:nrow(prediction.points)) {
  prediction.march.all[points.in.boundary.indices[i]] <- theft.prediction.results[[3]][i]
}
#plot the heat map by virtue of the plot.spatial.kde() function, save the image
file_name <- paste("Images/HeatMaps/Month 3 - by logistic regression.png",sep="")
png(file_name,height=720,width=580)
plot.spatial.kde(prediction.march.all,grid.points.all)
plot(city.boundary,add=T)
vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
title_name <- paste("Month 3 - by logistic regression",sep="")
title(title_name)
dev.off()

#plot the heat map predicted solely by KDE for comparison
prediction.march.all.kde <- rep(0,nrow(grid.points.all))
for (i in 1:nrow(prediction.points)) {
  prediction.march.all.kde[points.in.boundary.indices[i]] <- kde.for.testing[[3]][i]
}
#plot the KDE estimation for the crime rates, save the image
file_name <- paste("Images/HeatMaps/Month 3 - by KDE.png",sep="")
png(file_name,height=720,width=580)
plot.spatial.kde(prediction.march.all.kde,grid.points.all)
plot(city.boundary,add=T)
vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
title_name <- paste("Month 3 - by KDE",sep="")
title(title_name)
dev.off()



#define a new function which only keeps the crime prediction values in the hottest x% area
keep.hottest.areas <- function(AllPoints,area_percent,boundary) {
  sp.points <-  SpatialPoints(AllPoints[,c("x", "y")], proj4string=boundary@proj4string)
  points.in.boundary <-  !is.na(over(sp.points, boundary)$OBJECTID)
  #set all prediction values that are not within the range of the city boundary to be zero
  AllPoints$threat[which(points.in.boundary==F)] <- 0
  AllPoints_2 <-  AllPoints[points.in.boundary,]
  cut_off <- quantile(AllPoints_2$threat,probs=(1-area_percent))
  #set all prediction values that are smaller than the cut-off value to be zero
  indx <- which(AllPoints$threat<cut_off)
  AllPoints$threat[indx]<-0
  return(AllPoints)
}

#plot only the hottest 25% areas in terms of crime rate for march,
#based on the prediction from logistic regression (full model)
prediction.location.march.all <- cbind(prediction.march.all,grid.points.all)
colnames(prediction.location.march.all) <- c("threat","x","y")
hottest.points <- keep.hottest.areas(prediction.location.march.all,0.25,city.boundary)
file_name <- paste("Images/HeatMaps/top 25 percent area of Month 3 - by logistic regression.png",
                   sep="")
png(file_name,height=720,width=580)
plot.spatial.kde(hottest.points$threat,hottest.points[,c("x","y")])
plot(city.boundary,add=T)
vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
title_name <- paste("top 25% area of Month 3 - by logistic regression",sep="")
title(title_name)
dev.off()



# find out the percent of total crimes catched corresponding to different % area surveilled,
#compare the logistic regression with the KDE estimation
#for logistic regression
prediction.may.logistic <- cbind(prediction.points,theft.prediction.results[[5]])
colnames(prediction.may.logistic) <- c("x","y","threat")
surveillance.points.may.logistic <- get.surveillance.plot.points(prediction.may.logistic,
                                                                 crime.points.monthly[[5]],
                                                                 prediction.resolution.meters,
                                                                 city.boundary)
indx <- floor(nrow(surveillance.points.may.logistic)*seq(0.01,1,by=0.01))
crime.catched.may.logistic <- surveillance.points.may.logistic[indx,2]
#for KDE
prediction.may.kde <- cbind(prediction.points,kde.for.testing[[5]])
colnames(prediction.may.kde) <- c("x","y","threat")
surveillance.points.may.kde <- get.surveillance.plot.points(prediction.may.kde,
                                                                 crime.points.monthly[[5]],
                                                                 prediction.resolution.meters,
                                                                 city.boundary)
indx <- floor(nrow(surveillance.points.may.kde)*seq(0.01,1,by=0.01))
crime.catched.may.kde <- surveillance.points.may.kde[indx,2]

#compare the performance of the two models for predicting crime rates in may
crime.catched.may.logistic[c(5,10,15,20,25,30,35,40,45,50)]
crime.catched.may.kde[c(5,10,15,20,25,30,35,40,45,50)]