library(ks)
library(RColorBrewer)
library(geoR)
library(aqfig)
library(kernlab)

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





# calculate distance to nearest police station from each non-crime training point (grid points)
# These values can also be used when using the grid points to make predictions
police.points <- read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/PoliceStations/PoliceStationsDec2012", 
  "points", "+init=epsg:3435", "+init=epsg:26971")@coords
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
# calculate distance to nearest park from each non-crime training point (grid points)
# These values can also be used when using the grid points to make predictions
park.min.distance <- get.min.distances(non.crime.points[,c("x","y")], park.points)

#For malls & plazas shapefile, the type is also "polygon"; so follow the same steps as
#the parks shapefile to calculate the center of each polygon and set them as the coordinate
#of each mall (or plaza)
mall.polygons <- read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/Malls_and_Plazas/DATA_ADMIN_OPNSP_MALL_PLAZA", 
  "poly", "+init=epsg:3435", "+init=epsg:26971")
mall.polygons.list <- mall.polygons@polygons
mall.points <- as.data.frame(t(sapply(mall.polygons.list,Center_Points)))
colnames(mall.points) <- c("x","y")
#plot the malls & plazas points within the city boundary
file_name <- paste("Images/Scatter_Plots/Malls and Plazas in Chicago.png",sep="")
png(file_name,height=720,width=670)
plot(mall.points,pch=".",cex=4,asp=1,ylim=c(550000,600000))
plot(city.boundary,add=T)
title_name <- "malls and plazas in Chicago"
title(title_name)
dev.off()
# calculate distance to nearest mall (or plaza) from each non-crime training point (grid points)
# These values can also be used when using the grid points to make predictions
mall.min.distance <- get.min.distances(non.crime.points[,c("x","y")], mall.points)



# calculate the minimum distances from the actual crime points to the police stations, parks
# and malls & plazas, for February, May and August, store them in the lists. They will be
# used in training the data
police.min.distance.crime.points <- list()
park.min.distance.crime.points <- list()
mall.min.distance.crime.points <- list()
for (i in 1:3) {
  police.min.distance.crime.points[[3*i-1]] <- get.min.distances(crime.points.monthly[[3*i-1]][,c("x","y")], 
                                                             police.points)
  park.min.distance.crime.points[[3*i-1]] <- get.min.distances(crime.points.monthly[[3*i-1]][,c("x","y")], 
                                                           park.points)
  mall.min.distance.crime.points [[3*i-1]]<-get.min.distances(crime.points.monthly[[3*i-1]][,c("x","y")], 
                                                              mall.points)
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





############################################################################################
# logistic regression for the newly added variable: distance to the nearest mall (or plaza)
# (for predicting March, June, September theft rates)
############################################################################################

#run logistic regression for data in February, May and August, store the p_values for the
#coefficients of the 3 variables in a data frame
p_values <- data.frame()

for (i in 1:3) {
  # combine positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[3*i-1]])
  
  # combine minimum distance points
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[3*i-1]])
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[3*i-1]])
  mall.min.distance.training <- c(mall.min.distance,mall.min.distance.crime.points[[3*i-1]])
  
  training.data = cbind(training.data.response, kde.for.training[[3*i-1]], 
                        police.min.distance.training,
                        park.min.distance.training, mall.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2","x3","x4")
  #x1 is the theft density estimated from KDE, 
  #x2 is the distance to the nearest police station,
  #x3 is the distance to the nearest park, x4 is the distance to the nearest mall (or plaza)
  training.data$response <- as.factor(training.data$response)
  
  #logistic regression
  glm.fit = glm(response ~ . -x -y, data = training.data, family=binomial)
  glm_summary <- summary(glm.fit)$coefficients
  glm_coef <- glm_summary[c(18,19,20)]

  p_values <- rbind(p_values,glm_coef)
}

colnames(p_values) <- c("p_x2","p_x3","p_X4")





############################################################################################
# Use 3 different SVM models (probability models) for crime prediction:
# 1. February as training data, March as testing data
# 2. May as training data, June as testing data
# 3. August as training data, September as testing data
############################################################################################

for (i in 1:3) {
# combine positive (1) and negative (0) points
training.data.response = rbind(non.crime.points, crime.points.monthly[[3*i-1]])

# combine minimum distance points
police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[3*i-1]])
park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[3*i-1]])
mall.min.distance.training <- c(mall.min.distance,mall.min.distance.crime.points[[3*i-1]])

training.data = cbind(training.data.response, kde.for.training[[3*i-1]], 
                      police.min.distance.training,
                      park.min.distance.training, mall.min.distance.training)
colnames(training.data) <- c("response","x","y","x1","x2","x3","x4")
training.data$response <- as.factor(training.data$response)



#model 1: linear SVM
svm.fit = ksvm(response ~ .-x-y, data = training.data, type="C-svc", kernel="vanilladot", C=100, prob.model = TRUE)

#predict
testing.data = as.data.frame(cbind(prediction.points, kde.for.testing[[3*i]], 
                                   police.min.distance,
                                   park.min.distance, mall.min.distance))
colnames(testing.data) <- c("x","y","x1","x2","x3","x4")
pred.threats <- as.data.frame(predict(svm.fit, newdata = testing.data,type = "probabilities"))[,2]

theft.prediction = cbind(prediction.points, pred.threats)
names(theft.prediction) = c("x", "y", "threat")

#surveillance plot (use the predicted probability for positive (1) in the surveillance plot)

file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(3*i),
                   " by linear SVM.png",sep="")
png(file_name,height=458,width=717)
plot.surveillance.curve(theft.prediction, crime.points.monthly[[3*i]], prediction.resolution.meters, city.boundary)
title(sub=paste("Month ",as.character(3*i)," by linear SVM",sep=""))
dev.off()



#model 2: degree-2 polynomial kernel SVM 
svm.fit = ksvm(response ~ .-x-y, data = training.data, type="C-svc", kernel="polydot", kpar=list(degree=2), C=100, prob.model = TRUE)

#predict
pred.threats <- as.data.frame(predict(svm.fit, newdata = testing.data,type = "probabilities"))[,2]

theft.prediction = cbind(prediction.points, pred.threats)
names(theft.prediction) = c("x", "y", "threat")

#surveillance plot

file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(3*i),
                   " by degree-2 polynomial kernel SVM.png",sep="")
png(file_name,height=458,width=717)
plot.surveillance.curve(theft.prediction, crime.points.monthly[[3*i]], prediction.resolution.meters, city.boundary)
title(sub=paste("Month ",as.character(3*i)," by degree-2 polynomial kernel SVM",sep=""))
dev.off()



#model 3: RBF kernel SVM
svm.fit = ksvm(response ~ .-x-y, data = training.data, type="C-svc", kernel="rbfdot", C=100, prob.model = TRUE)

#predict
pred.threats <- as.data.frame(predict(svm.fit, newdata = testing.data,type = "probabilities"))[,2]

theft.prediction = cbind(prediction.points, pred.threats)
names(theft.prediction) = c("x", "y", "threat")

#surveillance plot

file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(3*i),
                   " by RBF kernel SVM.png",sep="")
png(file_name,height=458,width=717)
plot.surveillance.curve(theft.prediction, crime.points.monthly[[3*i]], prediction.resolution.meters, city.boundary)
title(sub=paste("Month ",as.character(3*i)," by RBF kernel SVM",sep=""))
dev.off()

}





############################################################################################
##### plot the heat map of the crime rates distribution predicted by SVM
############################################################################################

#Plot the September crime rates distribution predicted by RBF kernel SVM
prediction.sept.all <- rep(0,nrow(grid.points.all))
for (i in 1:nrow(prediction.points)) {
  prediction.sept.all[points.in.boundary.indices[i]] <- pred.threats[i]
}
#plot the heat map by virtue of the plot.spatial.kde() function, save the image
file_name <- "Images/Heat_Maps/Month 9 - by RBF kernel SVM.png"
png(file_name,height=720,width=580)
plot.spatial.kde(prediction.sept.all,grid.points.all)
plot(city.boundary,add=T)
vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
title_name <- paste("Month 9 - by RBF kernel SVM",sep="")
title(title_name)
dev.off()

#Plot the September crime distribution heat map predicted solely by KDE for comparison
prediction.sept.all.kde <- rep(0,nrow(grid.points.all))
for (i in 1:nrow(prediction.points)) {
  prediction.sept.all.kde[points.in.boundary.indices[i]] <- kde.for.testing[[9]][i]
}
#plot the KDE estimation for the crime rates, save the image
file_name <- paste("Images/Heat_Maps/Month 9 - by KDE.png",sep="")
png(file_name,height=720,width=580)
plot.spatial.kde(prediction.sept.all.kde,grid.points.all)
plot(city.boundary,add=T)
vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
title_name <- paste("Month 9 - by KDE",sep="")
title(title_name)
dev.off()





############################################################################################
#Tune the cost of constraints violation parameter c in the linear SVM model:
#for predicting March, June and September
#try c = 0.01, 0.1, 1, 10
############################################################################################

for (j in 1:3) {
  
  # combine the positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[3*j-1]])
  
  # combine minimum distance points for the training data
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[3*j-1]])
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[3*j-1]])
  mall.min.distance.training <- c(mall.min.distance,mall.min.distance.crime.points[[3*j-1]])
  
  training.data = cbind(training.data.response, kde.for.training[[3*j-1]], 
                        police.min.distance.training,
                        park.min.distance.training, mall.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2","x3","x4")
  training.data$response <- as.factor(training.data$response)
  
  #combine the testing data
  testing.data = as.data.frame(cbind(prediction.points, kde.for.testing[[3*j]], 
                                     police.min.distance,
                                     park.min.distance, mall.min.distance))
  colnames(testing.data) <- c("x","y","x1","x2","x3","x4")
  
  # run the linear SVM for each selected month and each c value
  for (i in -2:1) {
    #linear SVM
    svm.fit = ksvm(response ~ .-x-y, data = training.data, type="C-svc", kernel="vanilladot", C=10^i, prob.model = TRUE)
    
    #prediction
    pred.threats <- as.data.frame(predict(svm.fit, newdata = testing.data,type = "probabilities"))[,2]
    theft.prediction = cbind(prediction.points, pred.threats)
    names(theft.prediction) = c("x", "y", "threat")
    
    #surveillance plot
    file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(3*j),
                       " by linear SVM - cost 10^",as.character(i),".png",sep="")
    png(file_name,height=458,width=717)
    plot.surveillance.curve(theft.prediction, crime.points.monthly[[3*j]], prediction.resolution.meters, city.boundary)
    title(sub=paste("Month ",as.character(3*j)," by linear SVM - cost 10^",as.character(i),sep=""))
    dev.off()
  }
  
}



############################################################################################
#Tune the cost of constraints violation parameter c in the RBF kernel SVM model:
#for predicting March, June and September
#try c = 0.01, 1
############################################################################################

for (j in 1:3) {
  
  # combine the positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[3*j-1]])
  
  # combine minimum distance points for the training data
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[3*j-1]])
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[3*j-1]])
  mall.min.distance.training <- c(mall.min.distance,mall.min.distance.crime.points[[3*j-1]])
  
  training.data = cbind(training.data.response, kde.for.training[[3*j-1]], 
                        police.min.distance.training,
                        park.min.distance.training, mall.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2","x3","x4")
  training.data$response <- as.factor(training.data$response)
  
  #combine the testing data
  testing.data = as.data.frame(cbind(prediction.points, kde.for.testing[[3*j]], 
                                     police.min.distance,
                                     park.min.distance, mall.min.distance))
  colnames(testing.data) <- c("x","y","x1","x2","x3","x4")
  
  # run the RBF kernel SVM for each selected month and each c value
  for (i in 1:2) {
    #RBF kernel SVM
    svm.fit = ksvm(response ~ .-x-y, data = training.data, type="C-svc", kernel="rbfdot", C=10^(2*i-4), prob.model = TRUE)
    
    #prediction
    pred.threats <- as.data.frame(predict(svm.fit, newdata = testing.data,type = "probabilities"))[,2]
    theft.prediction = cbind(prediction.points, pred.threats)
    names(theft.prediction) = c("x", "y", "threat")
    
    #surveillance plot
    file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(3*j),
                       " by RBF kernel SVM - cost 10^",as.character(2*i-4),".png",sep="")
    png(file_name,height=458,width=717)
    plot.surveillance.curve(theft.prediction, crime.points.monthly[[3*j]], prediction.resolution.meters, city.boundary)
    title(sub=paste("Month ",as.character(3*j)," by RBF kernel SVM - cost 10^",as.character(2*i-4),sep=""))
    dev.off()
  }
  
}



############################################################################################
#Tune the cost of constraints violation parameter c in the degree-2 polynomial kernel SVM model:
#for predicting March, June and September
#try c = 0.01, 1
############################################################################################

for (j in 1:3) {
  
  # combine the positive (1) and negative (0) points
  training.data.response = rbind(non.crime.points, crime.points.monthly[[3*j-1]])
  
  # combine minimum distance points for the training data
  police.min.distance.training <- c(police.min.distance,police.min.distance.crime.points[[3*j-1]])
  park.min.distance.training <- c(park.min.distance,park.min.distance.crime.points[[3*j-1]])
  mall.min.distance.training <- c(mall.min.distance,mall.min.distance.crime.points[[3*j-1]])
  
  training.data = cbind(training.data.response, kde.for.training[[3*j-1]], 
                        police.min.distance.training,
                        park.min.distance.training, mall.min.distance.training)
  colnames(training.data) <- c("response","x","y","x1","x2","x3","x4")
  training.data$response <- as.factor(training.data$response)
  
  #combine the testing data
  testing.data = as.data.frame(cbind(prediction.points, kde.for.testing[[3*j]], 
                                     police.min.distance,
                                     park.min.distance, mall.min.distance))
  colnames(testing.data) <- c("x","y","x1","x2","x3","x4")
  
  # run the degree-2 polynomial kernel SVM for each selected month and each c value
  for (i in 1:2) {
    #degree-2 polynomial kernel SVM
    svm.fit = ksvm(response ~ .-x-y, data = training.data, type="C-svc", kernel="polydot", kpar=list(degree=2),C=10^(2*i-4), prob.model = TRUE)
    
    #prediction
    pred.threats <- as.data.frame(predict(svm.fit, newdata = testing.data,type = "probabilities"))[,2]
    theft.prediction = cbind(prediction.points, pred.threats)
    names(theft.prediction) = c("x", "y", "threat")
    
    #surveillance plot
    file_name <- paste("Images/Surveillance_Plots/Surveillance plot for Month ",as.character(3*j),
                       " by degree-2 polynomial kernel SVM - cost 10^",as.character(2*i-4),".png",sep="")
    png(file_name,height=458,width=717)
    plot.surveillance.curve(theft.prediction, crime.points.monthly[[3*j]], prediction.resolution.meters, city.boundary)
    title(sub=paste("Month ",as.character(3*j)," by degree-2 polynomial kernel SVM - cost 10^",as.character(2*i-4),sep=""))
    dev.off()
  }
  
}