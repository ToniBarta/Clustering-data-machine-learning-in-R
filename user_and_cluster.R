
# ALWAYS SET THE NEW PATH 
# getwd()
# setwd("/home/tonibarta/Desktop/PlotsFromR")


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

dbListFields(con,"intra_time_of_days")

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


pdf('intra_times.pdf')
plot(intra_times[3:2])
dev.off()

colnames(intra_times) <- c("user_id", "row_number", "discoverability")
  

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
	
pdf('intra_months.pdf')	
plot(intra_months[3:2])
dev.off()

colnames(intra_months) <- c("user_id", "row_number", "discoverability")

# THIS DATA IS FOR INTRA_WEATHER
intra_weathers = dbGetQuery(con, "SELECT DISTINCT iw.user_id, w.row_number, 
				(1 - iw.new_song_skipped*1.0/iw.songs_skipped) AS discoverability 
				from intra_weather_categories iw, (SELECT row_number() OVER(ORDER BY category), category 
				FROM weathers GROUP BY category) w 
				WHERE w.category = iw.variable AND iw.songs_skipped != 0 ORDER BY iw.user_id, w.row_number")
pdf('intra_weathers.pdf')
plot(intra_weathers[3:2])
dev.off()


# THIS DATA IS FOR INTRA_LOCATION
intra_locations = dbGetQuery(con, "SELECT DISTINCT ic.user_id, c.row_number, 
				 (1 - ic.new_song_skipped*1.0/ic.songs_skipped) AS discoverability 
				 from intra_cities ic, (SELECT row_number() OVER(ORDER BY city), city 
				 FROM locations GROUP BY city) c 
				 WHERE c.city = ic.variable AND ic.songs_skipped != 0 ORDER BY ic.user_id, c.row_number")
pdf('intra_locations.pdf')
plot(intra_locations[3:2])	
dev.off()


# THIS DATA IS FOR TEMPERATURE
intra_temperatures = dbGetQuery(con, "SELECT DISTINCT iw.user_id, w.row_number, 
				(1 - iw.new_song_skipped*1.0/iw.songs_skipped) AS discoverability 
				from intra_temperatures iw, (SELECT row_number() OVER(ORDER BY temperature), temperature 
				FROM weathers GROUP BY temperature) w 
				WHERE w.temperature = iw.variable AND iw.songs_skipped != 0 ORDER BY iw.user_id, w.row_number")
  
pdf('intra_temperatures')  
plot(intra_temperatures[3:2])
dev.off()

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& BEGIN &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

kmeansClustering <- function(intra_matrix, nr_clusters, nr_iterations, nr_start){
  da <- intra_matrix[3:2]
  cl <- kmeans(da, nr_clusters, iter.max = nr_iterations, nstart = nr_start)
  plot(da, col=cl$cluster)
  require(graphics)
  points(cl$centers, col = 1:15, pch = 8)
    
  return(cl)
}

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& END &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& BEGIN &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

getUsersFromTheSameCluster <- function(intra_matrix, intra_cluster){

# TODO think about a way to know about the size of the matrix.
  cluster_user_matrix <- matrix(0, 500, 1000)

  for ( i in 1:(nrow(intra_matrix))){
    j = 1
    while (cluster_user_matrix[intra_cluster$cluster[i], j] != 0 )
    {
      j = j + 1
    }
    cluster_user_matrix[intra_cluster$cluster[i] , j] = intra_matrix[["user_id"]][i]
  }
  return (cluster_user_matrix)
}
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& END &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&





