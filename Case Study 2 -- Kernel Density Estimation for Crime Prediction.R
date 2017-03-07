library(ks)
library(RColorBrewer)
library(geoR)
library(aqfig)
source("CrimeUtil.R")
#I made a small change to the plot.spatial.kde() function in the original
#"CrimeUtil.R" file, so I attached it to the Collab as well



#define a new function which only keeps the crime kde prediction values in the hotter x% area
keep.hottest.areas <- function(threat.prediction,area_percent,boundary) {
  sp.points <-  SpatialPoints(threat.prediction[,c("x", "y")], proj4string=boundary@proj4string)
  points.in.boundary <-  !is.na(over(sp.points, boundary)$OBJECTID)
  #set all kde estimation values that are not within the range of the city boundary to be zero
  threat.prediction$threat[which(points.in.boundary==F)] <- 0
  threat.prediction_2 <-  threat.prediction[points.in.boundary,]
  cut_off <- quantile(threat.prediction_2$threat,probs=(1-area_percent))
  #set all kde estimation values that are smaller than the cut-off value to be zero
  indx <- which(threat.prediction$threat<cut_off)
  threat.prediction$threat[indx]<-0
  return(threat.prediction)
}



kde.resolution.meters <-  200

# read chicago boundary
city.boundary <-  read.shapefile("Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")
#This R script is in the same Directory as the "CrimeUtil.R" and the "Examples" folder

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points <-  get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# read in all of the valid assault crime data in 2014
assault_all <- sample.crime("2014_ASSAULT.csv", -1)



#Part 1: Evaluate the KDE prediction for April and October
for (i in 1:2) {
  
  #read in the test data: a single month
  assault_4 <- assault_all[assault_all$month==6*i-2,]

  #read in the train data: the 3 months prior to the month of the test data
  assault_1 <- assault_all[assault_all$month==6*i-5,]
  assault_2 <- assault_all[assault_all$month==6*i-4,]
  assault_3 <- assault_all[assault_all$month==6*i-3,]

  #Plot and save the predicted KDE based on the train set, using different weights for different months
  kde.est.train <- run.spatial.kde(assault_3[,c("x","y")], kde.est.points)
                  +run.spatial.kde(assault_2[,c("x","y")], kde.est.points,nrow(assault_2)*2/3)
                  +run.spatial.kde(assault_1[,c("x","y")], kde.est.points,nrow(assault_1)*1/3)
  file_name <- paste("Images/KDEPlots/Prediction for Month ",as.character(6*i-2),".png",sep="")
  #This R script is in the same Directory as the folder "Images"
  png(file_name,height=720,width=580)
  plot.spatial.kde(kde.est.train, kde.est.points)
  plot(city.boundary, add=TRUE)
  vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
  title_name <- paste("Prediction for Month ",as.character(6*i-2),sep="")
  title(title_name)
  dev.off()

  #Plot and save the KDE of the test set
  kde.est.test <- run.spatial.kde(assault_4[,c("x","y")], kde.est.points)
  file_name <- paste("Images/KDEPlots/Month ",as.character(6*i-2),".png",sep="")
  png(file_name,height=720,width=580)
  plot.spatial.kde(kde.est.test, kde.est.points)
  plot(city.boundary, add=TRUE)
  vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
  title_name <- paste("Month ",as.character(6*i-2),sep="")
  title(title_name)
  dev.off()

  #build prediction and test matrix for evaluation of performance
  kde.prediction <- as.data.frame(cbind(kde.est.points,kde.est.train))
  names(kde.prediction) <- c("x","y","threat")
  eval.crime.points <- as.data.frame(assault_4[,c("x","y")])
  names(eval.crime.points) <- c("x","y")

  #Plot and save the surveillance plot
  file_name <- paste("Images/SurveillancePlots/Surveillance plot for Month ",as.character(6*i-2),".png",sep="")
  png(file_name,height=458,width=717)
  plot.surveillance.curve(kde.prediction, eval.crime.points, kde.resolution.meters, city.boundary)
  title(sub=paste("Month ",as.character(6*i-2),sep=""))
  dev.off()
  
  #Plot and save the predicted KDE in the selected hottest area
  file_name <- paste("Images/KDEPlots/Prediction for Month ",as.character(6*i-2),
                     " in the hottest 25 percent area.png",sep="")
  png(file_name,height=720,width=580)
  plot.spatial.kde(keep.hottest.areas(kde.prediction,0.25,city.boundary)$threat, kde.est.points)
  plot(city.boundary, add=TRUE)
  vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
  title_name <- paste("Prediction for Month ",as.character(6*i-2),
                      " in the hottest 25% area", sep="")
  title(title_name)
  dev.off()
  
}



#Part 2: Evaluate the KDE prediction for Tuesday and Saturday
for (i in 1:2) {
  
  #read in the day data
  assault_day <- assault_all[assault_all$day.of.week==4*i-1,]
  
  #run 3 cross-validations: (train: month 1-3, test: month 4;
  #                          train: month 4-6, test: month 7;)
  #                          train: month 7-9, test: month 10;
  for (j in 1:3) {
  
    #read in the test data
    assault_4 <- assault_day[assault_day$month==3*j+1,]
    #read in the train data: the 3 months prior to the month of the test data
    assault_1 <- assault_day[assault_day$month==3*j-2,]
    assault_2 <- assault_day[assault_day$month==3*j-1,]
    assault_3 <- assault_day[assault_day$month==3*j,]
  
  #Plot and save the predicted KDE based on the train set, using different weights for different months
    kde.est.train <- run.spatial.kde(assault_3[,c("x","y")], kde.est.points)
    +run.spatial.kde(assault_2[,c("x","y")], kde.est.points,nrow(assault_2)*2/3)
    +run.spatial.kde(assault_1[,c("x","y")], kde.est.points,nrow(assault_1)*1/3)
    file_name <- paste("Images/KDEPlots/Prediction for Day ",as.character(4*i-1),
                      " - ",as.character(j),".png",sep="")
    png(file_name,height=720,width=580)
    plot.spatial.kde(kde.est.train, kde.est.points)
    plot(city.boundary, add=TRUE)
    vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
    title_name <- paste("Prediction for Day ",as.character(4*i-1)," - ",as.character(j),sep="")
    title(title_name)
    dev.off()
  
    #Plot and save the KDE of the test set
    kde.est.test <- run.spatial.kde(assault_4[,c("x","y")], kde.est.points)
    file_name <- paste("Images/KDEPlots/Day ",as.character(4*i-1),
                      " - ",as.character(j),".png",sep="")
    png(file_name,height=720,width=580)
    plot.spatial.kde(kde.est.test, kde.est.points)
    plot(city.boundary, add=TRUE)
    vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
    title_name <- paste("Day ",as.character(4*i-1)," - ",as.character(j),sep="")
    title(title_name)
    dev.off()
  
    #build prediction and test matrix for evaluation of performance
    kde.prediction <- as.data.frame(cbind(kde.est.points,kde.est.train))
    names(kde.prediction) <- c("x","y","threat")
    eval.crime.points <- as.data.frame(assault_4[,c("x","y")])
    names(eval.crime.points) <- c("x","y")
  
    #Plot and save the surveillance plot
    file_name <- paste("Images/SurveillancePlots/Surveillance plot for Day ",as.character(4*i-1),
                      " - ",as.character(j),".png",sep="")
    png(file_name,height=458,width=717)
    plot.surveillance.curve(kde.prediction, eval.crime.points, kde.resolution.meters, city.boundary)
    title(sub=paste("Day ",as.character(4*i-1)," - ",as.character(j),sep=""))
    dev.off()
  
    #Plot and save the predicted KDE based on the train set, using different weights for different months
    file_name <- paste("Images/KDEPlots/Prediction for Day ",as.character(4*i-1),
                      " - ",as.character(j)," in the hottest 25 percent area.png",sep="")
    png(file_name,height=720,width=580)
    plot.spatial.kde(keep.hottest.areas(kde.prediction,0.25,city.boundary)$threat, kde.est.points)
    plot(city.boundary, add=TRUE)
    vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
    title_name <- paste("Prediction for Day ",as.character(4*i-1)," - ",as.character(j),
                        " in the hottest area",sep="")
    title(title_name)
    dev.off()
  
  }
  
}



#Part 3: Evaluate the KDE prediction for 9AM and 9PM
for (i in 1:2) {
  
  #read in the day data
  assault_hour <- assault_all[assault_all$hour==12*i-3,]
  
  #run 3 cross-validations: (train: month 1-3, test: month 4-5;
  #                          train: month 4-6, test: month 7-8;
  #                          train: month 7-9, test: month 10-11;)
  for (j in 1:3) {
    
    #read in the test data: including 2 months
    assault_4 <- assault_hour[assault_hour$month==3*j+1 | assault_hour$month==3*j+2,]
    #read in the train data: the 3 months prior to the months of the test data
    assault_1 <- assault_hour[assault_hour$month>=3*j-2 & assault_hour$month<=3*j,]

    #Plot and save the predicted KDE based on the train set
    kde.est.train <- run.spatial.kde(assault_1[,c("x","y")], kde.est.points)

    file_name <- paste("Images/KDEPlots/Prediction for Hour ",as.character(12*i-3),
                       " - ",as.character(j),".png",sep="")
    png(file_name,height=720,width=580)
    plot.spatial.kde(kde.est.train, kde.est.points)
    plot(city.boundary, add=TRUE)
    vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
    title_name <- paste("Prediction for Hour ",as.character(12*i-3)," - ",as.character(j),sep="")
    title(title_name)
    dev.off()
    
    #Plot and save the KDE of the test set
    kde.est.test <- run.spatial.kde(assault_4[,c("x","y")], kde.est.points)
    file_name <- paste("Images/KDEPlots/Hour ",as.character(12*i-3),
                       " - ",as.character(j),".png",sep="")
    png(file_name,height=720,width=580)
    plot.spatial.kde(kde.est.test, kde.est.points)
    plot(city.boundary, add=TRUE)
    vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
    title_name <- paste("Hour ",as.character(12*i-3)," - ",as.character(j),sep="")
    title(title_name)
    dev.off()
    
    #build prediction and test matrix for evaluation of performance
    kde.prediction <- as.data.frame(cbind(kde.est.points,kde.est.train))
    names(kde.prediction) <- c("x","y","threat")
    eval.crime.points <- as.data.frame(assault_4[,c("x","y")])
    names(eval.crime.points) <- c("x","y")
    
    #Plot and save the surveillance plot
    file_name <- paste("Images/SurveillancePlots/Surveillance plot for Hour ",as.character(12*i-3),
                       " - ",as.character(j),".png",sep="")
    png(file_name,height=458,width=717)
    plot.surveillance.curve(kde.prediction, eval.crime.points, kde.resolution.meters, city.boundary)
    title(sub=paste("Hour ",as.character(12*i-3)," - ",as.character(j),sep=""))
    dev.off()
    
    #Plot and save the predicted KDE based on the train set, using different weights for different months
    file_name <- paste("Images/KDEPlots/Prediction for Hour ",as.character(12*i-3),
                       " - ",as.character(j)," in the hottest 25 percent area.png",sep="")
    png(file_name,height=720,width=580)
    plot.spatial.kde(keep.hottest.areas(kde.prediction,0.25,city.boundary)$threat, kde.est.points)
    plot(city.boundary, add=TRUE)
    vertical.image.legend(colorRampPalette(rev(brewer.pal(11,'Spectral')))(32),zlim=c(0,1))
    title_name <- paste("Prediction for Hour ",as.character(12*i-3)," - ",as.character(j),
                        " in the hottest area",sep="")
    title(title_name)
    dev.off()
    
  }
  
}