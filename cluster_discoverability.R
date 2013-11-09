library(RPostgreSQL)
library(hash)

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


# THIS DATA IS FOR INTRA_WEATHER
intra_weathers = dbGetQuery(con, "SELECT DISTINCT iw.user_id, w.row_number, 
				(1 - iw.new_song_skipped*1.0/iw.songs_skipped) AS discoverability 
				from intra_weather_categories iw, (SELECT row_number() OVER(ORDER BY category), category 
				FROM weathers GROUP BY category) w 
				WHERE w.category = iw.variable AND iw.songs_skipped != 0 ORDER BY iw.user_id, w.row_number")
  
plot(intra_weathers[3:2])


# THIS DATA IS FOR INTRA_LOCATION
intra_locations = dbGetQuery(con, "SELECT DISTINCT ic.user_id, c.row_number, 
				 (1 - ic.new_song_skipped*1.0/ic.songs_skipped) AS discoverability 
				 from intra_cities ic, (SELECT row_number() OVER(ORDER BY city), city 
				 FROM locations GROUP BY city) c 
				 WHERE c.city = ic.variable AND ic.songs_skipped != 0 ORDER BY ic.user_id, c.row_number")

plot(intra_locations[3:2])	


# THIS DATA IS FOR TEMPERATURE
intra_temperatures = dbGetQuery(con, "SELECT DISTINCT iw.user_id, w.row_number, 
				(1 - iw.new_song_skipped*1.0/iw.songs_skipped) AS discoverability 
				from intra_temperatures iw, (SELECT row_number() OVER(ORDER BY temperature), temperature 
				FROM weathers GROUP BY temperature) w 
				WHERE w.temperature = iw.variable AND iw.songs_skipped != 0 ORDER BY iw.user_id, w.row_number")
  
plot(intra_temperatures[3:2])
  
orderd_intra = intra_times[with(intra_times, order(case)), ] 
j = 1
intra_times_3dMatrix = array(0, dim=c(8,200,2))
 rowCount = 1
for (i in 1:(nrow(orderd_intra) - 1)){

    if (orderd_intra$case[i] == orderd_intra$case[i+1]){
    
	intra_times_3dMatrix[rowCount, j, 1] = orderd_intra[["user_id"]][i]
	intra_times_3dMatrix[rowCount, j, 2] = orderd_intra[["discoverability"]][i]	
	j = j + 1
     }
     else{
       intra_times_3dMatrix[rowCount, j, 1] = orderd_intra[["user_id"]][i]
       intra_times_3dMatrix[rowCount, j, 2] = orderd_intra[["discoverability"]][i]	
       rowCount = rowCount + 1
       j = 1
     }
  if (i == (nrow(orderd_intra) - 1)){
    intra_times_3dMatrix[rowCount,j,1] = orderd_intra[["user_id"]][i + 1]
    intra_times_3dMatrix[rowCount,j,2] = orderd_intra[["discoverability"]][i + 1]
  }
}	

# to get rid of the 0's from the matrixs
numberMatrix <- intra_times_3dMatrix[ , rowSums(abs(intra_times_3dMatrix[, ,]))>0 & rowSums(abs(intra_times_3dMatrix[, ,]))>0,  ]






# library(mclust)
# # Run the function to see how many clusters
# # it finds to be optimal, set it to search for
# # at least 1 model and up 20.
# d_clust <- Mclust(as.matrix(intra_times), G=1:40)
# m.best <- dim(d_clust$z)[2]
# cat("model-based optimal number of clusters:", m.best, "\n")
# # 4 clusters
# plot(d_clust)
# 
# 
# 
# fit <- cascadeKM(scale(intra_times, center = TRUE,  scale = TRUE), 1, 20, iter = 5000)
# plot(fit, sortg = TRUE, grpmts.plot = TRUE)
# calinski.best <- as.numeric(which.max(fit$results[2,]))
# cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# # 5 clusters!
# 
# 
# mydata <- scale(mydata) # standardize variables
# 
# # Determine number of clusters
# wss <- (nrow(intra_times)-1)*sum(apply(intra_times,2,var))
# for (i in 2:30) wss[i] <- sum(kmeans(intra_times, 
#   	 centers=i)$withinss)
# plot(1:30, wss, type="b", xlab="Number of Clusters",
#   ylab="Within groups sum of squares")
# 
# 
#   
#  library(fpc)
# pamk.best <- pamk(intra_times)
# cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
# plot(pam(intra_times, pamk.best$nc))


# # Determine number of clusters
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
#   	 centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#   ylab="Within groups sum of squares")
#   
#   
#   # K-Means Cluster Analysis
# fit <- kmeans(mydata, 5) # 5 cluster solution
# # get cluster means 
# aggregate(mydata,by=list(fit$cluster),FUN=mean)
# # append cluster assignment
# mydata <- data.frame(mydata, fit$cluster)
# 
# 
# 
# 
# 
# # K-Means Clustering with 5 clusters
# fit <- kmeans(mydata, 5)
# 
# # Cluster Plot against 1st 2 principal components
# 	
# # vary parameters for most readable graph
# library(cluster

# clusplot(intra_times, fit$cluster, color=TRUE, shade=TRUE, 
  	 labels=2, lines=0)
# 
# # Centroid Plot against 1st 2 discriminant functions
# library(fpc)
# plotcluster(mydata, fit$cluster)
# 
# 
# 
# with(intra_months, plot(discoverability, case, col= user_id))