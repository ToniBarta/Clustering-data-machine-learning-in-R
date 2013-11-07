library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname="momentus_development")


dbListConnections(drv)

# library(RPostgreSQL)

dbGetInfo(drv)
summary(con)

# rs <- dbSendQuery(con,"select songs_skipped from intra_weather_categories")

# fetch(rs,n=-1) ## return all elements

dbListFields(con,"intra_time_of_days"
)

# THIS DATA IS FOR INTRA_TIME_OF_DAYS
intra_times = dbGetQuery(con,"select distinct user_id, 
				 case 
				 when variable = 'early morning' 	then 1
				 when variable = 'morning'	 	then 2
				 when variable = 'late morning'		then 3
				 when variable = 'afternoon'		then 4
				 when variable = 'late afternoon'	then 5
				 when variable = 'evening'		then 6
				 when variable = 'night'		then 7
				 when variable = 'late night'		then 8
			      end,
		  (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_time_of_days where songs_skipped != 0")

# dada = as.matrix(da)

plot(intra_times[3:2])

# THIS DATA IS FOR INTRA_DAY_OF_WEEKS
intra_days = dbGetQuery(con,"select distinct user_id, 
				 case 
				 when variable = 'monday' 		then 1
				 when variable = 'tuesday'	 	then 2
				 when variable = 'wednesday'		then 3
				 when variable = 'thursday'		then 4
				 when variable = 'friday'		then 5
				 when variable = 'saturday'		then 6
				 when variable = 'sunday'		then 7
				
			      end,
		  (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_day_of_weeks where songs_skipped != 0")
		  
plot(intra_days[3:2])

# THIS DATA IS FOR INTRA_MONTHS
intra_months = dbGetQuery(con,"select distinct user_id, 
				 case
				 when variable = 'january' 		then 1
				 when variable = 'february'	 	then 2
				 when variable = 'march'		then 3
				 when variable = 'april'		then 4
				 when variable = 'may'			then 5
				 when variable = 'june'			then 6
				 when variable = 'july'			then 7
				 when variable = 'august'		then 8
				 when variable = 'september'		then 9
				 when variable = 'october'		then 10
				 when variable = 'november'		then 11
				 when variable = 'december'		then 12
		
				
			      end,
		  (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_months where songs_skipped != 0")
		  
plot(intra_months[3:2])





# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
  	 centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")
  
  
  # K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)





# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
  	 labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)



with(intra_months, plot(discoverability, case, col= user_id))