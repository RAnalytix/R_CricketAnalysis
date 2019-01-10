library(tidyverse)
library(cluster)
library(factoextra)
library(data.table)
library(reshape2)


#http://www.sthda.com/english/wiki/print.php?id=236

df <- read.csv("ODIData.csv")
df <- subset(df, select = -c(X, Dismissal, Opposition, Ground, StartDate, MatchType, PlayerName, Country, Link, VenueType.y, Result))

df <- scale(df)

distance <- get_dist(df)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = df)

#Finding optimal number of clusters using the Elbow Method
#https://uc-r.github.io/kmeans_clustering

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")

#Finding optimal number of clusters using the silhouette method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

fviz_nbclust(df, kmeans, method = "silhouette")


#Finding optimal clusters using the gap statistic method
set.seed(123)

gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## Final Results - get the optimal number of clusters from the three methods
set.seed(123)
final <- kmeans(df, 2, nstart=25)
fviz_cluster(final, data=df)


df$cluster <- k2$cluster

write.csv(df,
          file="extraction1.csv",
          row.names = FALSE,
          quote = FALSE)
