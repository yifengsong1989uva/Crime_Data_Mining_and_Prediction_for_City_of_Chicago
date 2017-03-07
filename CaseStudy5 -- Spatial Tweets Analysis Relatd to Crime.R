library(rgdal)
library(maptools)
library(lubridate)
library(XML)
library(tm)
library(topicmodels)
library(ks)
library(RColorBrewer)
library(geoR)
library(aqfig)

source("CrimeUtil.R")
#I made a small change to the plot.spatial.kde() function in the original
#"CrimeUtil.R" file, so I attached it to the Collab as well

city.boundary = read.shapefile(
  "~/Yifeng/SYS6018/Examples/Data/City_Boundary/City_Boundary", 
  "poly", "+init=epsg:3435", "+init=epsg:26971")

# read the tweets sample
tweets = read.csv("tweets_large_sample.csv", header = TRUE, stringsAsFactors = FALSE)

# reproject from degrees to meters
tweets.locations.lonlat = cbind(tweets$longitude, tweets$latitude)
meters.locations = project(tweets.locations.lonlat, proj="+init=epsg:26971")
tweets$x = meters.locations[,1]
tweets$y = meters.locations[,2]
tweets = tweets[is.finite(tweets$x) & is.finite(tweets$y),]  # some tweets are posted from outside the bounds of the chicago projection and their reprojections are infinite. remove such tweets.

# filter all tweets to be in the city
points.in.boundary = points.within.boundary(tweets[,c("x", "y")], "+init=epsg:26971", city.boundary)
tweets = tweets[points.in.boundary,]

# convert timestamp string to date representation, and pull out hour, day of week, and month
tweets$timestamp = as.POSIXct(tweets$timestamp)
tweets$hour = hour(tweets$timestamp)
tweets$day.of.week = wday(tweets$timestamp)
tweets$month = month(tweets$timestamp)
#############################################################################################





#############################################################################################
### Part 2: Topic analyses of tweets contents during weekdays and weekends
#############################################################################################
# split the tweets data sample into weekends and weekdays
tweets.weekend <- tweets[tweets$day.of.week==1 | tweets$day.of.week==7,]
tweets.weekday <- tweets[tweets$day.of.week>1 & tweets$day.of.week<7,]

set.seed(1)

# randomly select a sample of 2500 rows from tweets.weekday,
# and select a sample of 2500 rows from tweets.weekend
tweets.weekday.s <- tweets.weekday[sample(1:nrow(tweets.weekday),2500),]
tweets.weekend.s <- tweets.weekend[sample(1:nrow(tweets.weekend),2500),]
# combine the two parts together
tweets.s <- rbind(tweets.weekday.s,tweets.weekend.s)

# combine all tweets into the corpus
tweets.text <- VCorpus(DataframeSource(as.data.frame(tweets.s$text)))

# clean and compute tf
tweets.clean = tm_map(tweets.text, stripWhitespace)                     # remove extra whitespace
tweets.clean = tm_map(tweets.clean, removeNumbers)                      # remove numbers
tweets.clean = tm_map(tweets.clean, removePunctuation)                  # remove punctuation
tweets.clean = tm_map(tweets.clean, content_transformer(tolower))       # ignore case
tweets.clean = tm_map(tweets.clean, removeWords, stopwords("english"))  # remove stop words
tweets.clean = tm_map(tweets.clean, stemDocument)                       # stem all words
tweets.clean.tf = DocumentTermMatrix(tweets.clean, control = list(weighting = weightTf))

# remove empty documents
row.sums <- apply(tweets.clean.tf, 1, sum)

to.remove <- as.vector(which(row.sums==0))
to.remove.weekday <- to.remove[to.remove<=2500]
to.remove.weekend <- to.remove[to.remove>2500]
tweets.s <- tweets.s[row.sums > 0,]
tweets.clean.tf <- tweets.clean.tf[row.sums > 0,]



###################################
# train topic model with 100 topics
topic.model <- LDA(tweets.clean.tf, 100)

# rank the frequency of the most likely topic for each tweet during weekday
document.most.likely.topic.weekday <-  topics(topic.model, 1)[1:(2500-length(to.remove.weekday))]
sort(table(document.most.likely.topic.weekday),decreasing=T)

# look at the top 20 words within the top 10 topics
terms(topic.model, 20)[,c(5,24,7,8,13,14,4,30,33,35)]

# rank the frequency of the most likely topic for each tweet during weekend
document.most.likely.topic.weekend <-  topics(topic.model, 1)[(2500-length(to.remove.weekday)+1):(5000-length(to.remove))]
sort(table(document.most.likely.topic.weekend),decreasing=T)

# look at the top 20 words within the top 10 topics
terms(topic.model, 20)[,c(94,24,5,11,16,9,36,85,4,18)]

####################################
# train topic model with 300 topics
topic.model2 <- LDA(tweets.clean.tf, 300)

# rank the frequency of the most likely topic for each tweet during weekday
document.most.likely.topic.weekday <-  topics(topic.model2, 1)[1:(2500-length(to.remove.weekday))]
sort(table(document.most.likely.topic.weekday),decreasing=T)
#topic  88  79  80  11  83 180 236  12  61 197 255 277  16  19  56 112 123 249 274 300
#freq   35  31  19  17  16  16  16  15  15  15  15  15  14  14  14  14  14  14  14  14 

# look at the top 20 words within the top 10 topics
terms(topic.model2, 20)[,c(88,79,80,11,83,180,236,12,61,197)]

# rank the frequency of the most likely topic for each tweet during weekend
document.most.likely.topic.weekend <-  topics(topic.model2, 1)[(2500-length(to.remove.weekday)+1):(5000-length(to.remove))]
sort(table(document.most.likely.topic.weekend),decreasing=T)
#topic 79  88 122 112 296  86   6  21 129  61  80 106 153 245 280   2  12  15  45 113
#freq  31  29  24  20  17  16  15  15  15  14  14  14  14  14  14  13  13  13  13  13

# look at the top 20 words within the top 10 topics
terms(topic.model2, 20)[,c(79,88,122,112,296,86,6,21,129,61)]
############################################################################################





############################################################################################
### Part 3: Topic modeling of tweets posted in areas close to schools in Chicago
############################################################################################

tweet.points <- tweets[,c("x","y")]
# calculate distance to nearest school from each tweet point
school.points <- read.shapefile(
  "C:/Users/Yifeng/Documents/Yifeng/SYS6018/Examples/Data/Schools/CPS_School_Locations_SY1415", 
  "points", "+init=epsg:3435", "+init=epsg:26971")@coords
min.distances <- get.min.distances(tweet.points,school.points)

#filter out the points within the range (radius < 50)
points.in.range.index <- as.vector(which(min.distances<50))

# combine the tweet texts data in the 50m radius of schools into the corpus
tweets.in.range <- tweets[points.in.range.index,]$text
tweets.in.range <- VCorpus(DataframeSource(as.data.frame(tweets.in.range)))

# clean and compute tf
tweets.in.range.clean = tm_map(tweets.in.range, stripWhitespace)                          # remove extra whitespace
tweets.in.range.clean = tm_map(tweets.in.range.clean, removeNumbers)                      # remove numbers
tweets.in.range.clean = tm_map(tweets.in.range.clean, removePunctuation)                  # remove punctuation
tweets.in.range.clean = tm_map(tweets.in.range.clean, content_transformer(tolower))       # ignore case
tweets.in.range.clean = tm_map(tweets.in.range.clean, removeWords, stopwords("english"))  # remove stop words
tweets.in.range.clean = tm_map(tweets.in.range.clean, stemDocument)                       # stem all words
tweets.in.range.clean.tf = DocumentTermMatrix(tweets.in.range.clean, control = list(weighting = weightTf))

# remove empty documents
row.sums <- apply(tweets.in.range.clean.tf, 1, sum)
tweets.in.range <- tweets.in.range[row.sums > 0]
tweets.in.range.clean.tf <- tweets.in.range.clean.tf[row.sums > 0,]

##################################
# train topic model with 50 topics
topic.model.for.in.range <- LDA(tweets.in.range.clean.tf, 50)

# rank the frequency of the most likely topic for each document
document.most.likely.topic <- topics(topic.model.for.in.range, 1)
sort(table(document.most.likely.topic),decreasing=T)
#topic 22 45  9 20  1  5  7 26  4 35 38 40 49 14 18 25 29 50 10 12 17 33 46 47  8 28 30  6 11 15 16 21 
#freq  23 23 22 22 21 21 21 21 20 20 20 20 20 19 19 19 19 19 18 18 18 18 18 18 17 17 17 16 16 16 16 16 
#topic 32 39  3 13 23 27 36 37 41  2 24 31 43 42 44 34 48 19 
#freq  16 16 15 15 15 15 15 15 15 14 14 14 14 13 12 11 11 10 

terms(topic.model.for.in.range, 20)[,c(22,45,9,20,1,5,7,26,4,35)]

##################################
# train topic model with 20 topics
topic.model.for.in.range <- LDA(tweets.in.range.clean.tf, 20)

# rank the frequency of the most likely topic for each document
document.most.likely.topic <- topics(topic.model.for.in.range, 1)
sort(table(document.most.likely.topic),decreasing=T)
#topic  9  6 16 20  7  8 10  4 17 12 11  2  3 19 13  5 14 18 15  1 
#freq  56 49 49 49 46 46 46 45 45 44 43 42 42 42 41 39 38 36 32 28

terms(topic.model.for.in.range, 20)[,c(9,6,16,20,7,8,10,4,17,12)]

#calculate the topic-word distributions (top 20 words in each topic)
sort(posterior(topic.model.for.in.range)$terms[9,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[6,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[16,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[20,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[7,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[8,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[10,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[4,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[17,],decreasing=T)[1:20]
sort(posterior(topic.model.for.in.range)$terms[12,],decreasing=T)[1:20]

###################################
# train topic model with 100 topics
topic.model.for.in.range <-  LDA(tweets.in.range.clean.tf, 100)

# rank the frequency of the most likely topic for each document
document.most.likely.topic <-  topics(topic.model.for.in.range, 1)
sort(table(document.most.likely.topic),decreasing=T)
#Topic  76  99  24  30   8  62   3  51  53  74   4  23  37  47  84   1  15  17  31  65  75   2   5   6 
#freq   17  17  15  15  14  14  13  13  13  13  12  12  12  12  12  11  11  11  11  11  11  10  10  10 
#Topic   9  16  45  58  59  63  64  81  89   7  10  14  20  25  38  57  70  73  12  13  18  19  27  34 
#freq   10  10  10  10  10  10  10  10  10   9   9   9   9   9   9   9   9   9   8   8   8   8   8   8 
#topic  41  43  48  54  56  60  66  67  68  72  77  79  80  83  90  91  95  35  36  39  42  52  55  69 
#freq    8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   8   7   7   7   7   7   7   7 
#topic  82  92 100  22  28  29  33  40  46  71  78  85  86  88  93  97  11  21  32  49  50  94  98  26 
#freq    7   7   7   6   6   6   6   6   6   6   6   6   6   6   6   6   5   5   5   5   5   5   5   4 
#topic  44  61  87  96 
#freq    4   4   4   4 

terms(topic.model.for.in.range, 20)[,c(76,99,24,30,8,62,3,51,53,74)]



#############################
#filter out the points within the range (radius < 30)
points.in.range.index2 <- as.vector(which(min.distances<30))

# combine the tweet texts data in the 30m radius of schools into the corpus
tweets.in.range <- tweets[points.in.range.index2,]$text
tweets.in.range <- VCorpus(DataframeSource(as.data.frame(tweets.in.range)))

# clean and compute tf
tweets.in.range.clean = tm_map(tweets.in.range, stripWhitespace)                          # remove extra whitespace
tweets.in.range.clean = tm_map(tweets.in.range.clean, removeNumbers)                      # remove numbers
tweets.in.range.clean = tm_map(tweets.in.range.clean, removePunctuation)                  # remove punctuation
tweets.in.range.clean = tm_map(tweets.in.range.clean, content_transformer(tolower))       # ignore case
tweets.in.range.clean = tm_map(tweets.in.range.clean, removeWords, stopwords("english"))  # remove stop words
tweets.in.range.clean = tm_map(tweets.in.range.clean, stemDocument)                       # stem all words
tweets.in.range.clean.tf = DocumentTermMatrix(tweets.in.range.clean, control = list(weighting = weightTf))

# remove empty documents
row.sums <- apply(tweets.in.range.clean.tf, 1, sum)
tweets.in.range <- tweets.in.range[row.sums > 0]
tweets.in.range.clean.tf <- tweets.in.range.clean.tf[row.sums > 0,]

##################################
# train topic model with 50 topics
topic.model.for.in.range <- LDA(tweets.in.range.clean.tf, 50)

# rank the frequency of the most likely topic for each document
document.most.likely.topic <- topics(topic.model.for.in.range, 1)
sort(table(document.most.likely.topic),decreasing=T)
#topic 29 49 47 17 18  5 50 13 22 25 40 42  2  6  7  8 14 16 32 36  1  3  9 11 24 28 37 38 39 44 46 12 
#freq  15 13 12 11 11 10 10  9  9  9  9  9  8  8  8  8  8  8  8  8  7  7  7  7  7  7  7  7  7  7  7  6 
#topic 15 19 20 21 26 30 33 34 43  4 10 27 31 35 41 45 48 23 
#freq   6  6  6  6  6  6  6  6  6  5  5  5  5  5  5  5  5  4

terms(topic.model.for.in.range, 20)[,c(29,49,47,17,18,5,50,13,22,25)]

##################################
# train topic model with 20 topics
topic.model.for.in.range <- LDA(tweets.in.range.clean.tf, 20)

# rank the frequency of the most likely topic for each document
document.most.likely.topic <- topics(topic.model.for.in.range, 1)
sort(table(document.most.likely.topic),decreasing=T)
#topic  9 15  6 19  8 11  5  2  7  1 12 16  4 10 18 14  3 17 13 20 
#freq  28 27 24 22 21 21 20 19 19 18 18 18 17 17 16 15 14 14 13 11

terms(topic.model.for.in.range, 20)[,c(9,15,6,19,8,11,5,2,7,1)]

###################################
# train topic model with 100 topics
topic.model.for.in.range <-  LDA(tweets.in.range.clean.tf, 100)

# rank the frequency of the most likely topic for each document
document.most.likely.topic <-  topics(topic.model.for.in.range, 1)
sort(table(document.most.likely.topic),decreasing=T)
#Topic  15  67  86   4  82   3  26  36  41  43  46  58  65  68  11  16  19  25  29  35  53  56  60  62 
#freq    9   9   8   7   7   6   6   6   6   6   6   6   6   6   5   5   5   5   5   5   5   5   5   5 
#topic  75  87   1   5   6   7  20  22  34  45  50  61  63  70  71  76  78  80  81  85  94  95  98   2 
#freq    5   5   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   3 
#topic  10  13  17  18  21  28  30  33  38  39  40  44  49  51  52  54  57  59  66  72  73  77  84  88 
#freq    3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3 
#topic  89  90  92  96  97  99   8   9  12  14  23  31  32  37  42  47  48  55  64  69  74  83  91  93 
#freq    3   3   3   3   3   3   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2 
#topic 100  24  27  79 
#freq    2   1   1   1 

terms(topic.model.for.in.range, 20)[,c(15,67,86,4,82,3,26,36,41,43)]
############################################################################################





###################################################################################
### Part 1: Visualization of tweets distribution across Chicago
###################################################################################
resolution <- 1000

#grid.points.all are points in the rectangular area, including both the points within the city boundary
#and points not within the city boundary
grid.points.all <- get.grid.points(city.boundary, resolution, FALSE)
#find the index for each grid point that falls into the city boundary in the entire data frame
#of all of the grid points
sp.points <-  SpatialPoints(grid.points.all[,c("x", "y")], proj4string=city.boundary@proj4string)
points.in.boundary <-  !is.na(over(sp.points, city.boundary)$OBJECTID)
points.in.boundary.indices <- which(points.in.boundary==T)

#get the coordinates of the grid points falling within the city boundary
grid.points <- get.grid.points(city.boundary, resolution, TRUE)
grid.points$x <- round(grid.points$x,1)
grid.points$y <- round(grid.points$y,1)
min.x <- round(get.shapefile.range(city.boundary)[1],1)
min.y <- round(get.shapefile.range(city.boundary)[3],1)
row.names(grid.points) <- as.character(1:nrow(grid.points))

#the count.tweets() function calculates the total number of tweets within
#the square neighborhood region (1000 * 1000 m^2)
count.tweets <- function(single.tweet.point,counts) {
  remainder.x <- as.numeric((single.tweet.point[1]-min.x) %% 1000)
  if (remainder.x<500) {
    x.coord <- as.numeric(single.tweet.point[1])-remainder.x
  }
  else {
    x.coord <- as.numeric(single.tweet.point[1])+1000-remainder.x
  }
  remainder.y <- as.numeric((single.tweet.point[2]-min.y) %% 1000)
  if (remainder.y<500) {
    y.coord <- as.numeric(single.tweet.point[2])-remainder.y
  }
  else {
    y.coord <- as.numeric(single.tweet.point[2])+1000-remainder.y
  }
  row.num <- which(grid.points[,1]==x.coord & grid.points[,2]==y.coord)
  if (length(row.num)==1) {
    counts[row.num] <- counts[row.num]+1
  }
  return(counts)
}

#caluclate the tweet counts
counts <- rep(0,nrow(grid.points))
for (i in 1:nrow(tweet.points)) {
  counts <- count.tweets(tweet.points[i,],counts)
}

#assign the counts values to each square neighborhood area
counts.all <- rep(0,nrow(grid.points.all))
for (i in 1:length(counts)) {
  counts.all[points.in.boundary.indices[i]] <- counts[i]
}

#plot the tweets spatial distribution heat map by virtue of the plot.spatial.kde() function
file_name <- "Tweets spatial distribution in Chicago.png"
png(file_name,height=720,width=580)
plot.spatial.kde(counts.all,grid.points.all)
plot(city.boundary,add=T)
vertical.image.legend(colorRampPalette(rev(brewer.pal(8,'Spectral')))(64),zlim=c(0,1))
title("Tweets spatial distribution in Chicago")
dev.off()