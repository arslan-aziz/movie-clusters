library(tidyverse)
library(jsonlite)
library(dplyr)
library(ggplot2)
setwd("~/UVA/Semester 6/4630/HW4")

#Parse movies_metadata.csv into movie_genre.Rdata using abstract_movie_genre.R code.

#Load data
load('movie_genreID_genre.Rdata')
load('ratings.Rdata')
#ratings<-read_csv('movie_rating_data.csv')

#Combine ratings and movie_genre by movieId vector
combined<-inner_join(ratings,movie_genre)
#yields data.frame of rating of each user of each genre of each movie
attach(combined)

#Create vector of genre_id's k (covariates)
genres<-as.integer(levels(factor(combined$genre_id)))
k<-length(genres)

#Find total num of users n (observation)
n<-length(unique(combined$userId))

ratingsMeans<-combined %>% group_by(userId,genre_id)%>% summarise(mean(rating))%>%spread(genre_id,'mean(rating)')
user.genre.rating1<-ratingsMeans[,-1]

viewsCount<-combined %>% group_by(userId,genre_id)%>% summarise(n())%>%spread(genre_id,'n()')
user.genre.rating2<-viewsCount[,-1]

user.genre.rating<-ratingsMeans[,-1]*viewsCount[,-1]

pr<-prcomp(na.omit(user.genre.rating),scale=TRUE)
biplot(pr,scale=0)
pr.varExplained<-pr$sdev^2
pve<-pr.varExplained/sum(pr.varExplained)
data<-pr$x[,1:2]
#choosing number of clusters
withinSS<-NULL
for (i in 1:10){
  withinSS[i]<-kmeans(data,i,nstart=20)$tot.withinss
}
SSplot<-ggplot(as.data.frame(withinSS),aes(x=1:10,y=withinSS))+geom_point()
SSplot
km.pr<-kmeans(data,5,nstart=20)


#km<-kmeans(na.omit(user.genre.rating),10,nstart=20,algorithm='MacQueen')

clusterPlot<-ggplot(as.data.frame(data),aes(x=PC1,y=PC2))+geom_point(aes(color=factor(km.pr$cluster)))
clusterPlot
#start.time <- Sys.time()
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken