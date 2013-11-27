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

plot(intra_times[3:2])
colnames(intra_times) <- c("user_id", "row_number", "discoverability")
  
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
colnames(intra_days) <- c("user_id", "row_number", "discoverability")

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
colnames(intra_months) <- c("user_id", "row_number", "discoverability")

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
  
   
#  @@@@@@@@@@@@@ FUNCTION TO RETURN A 3d MATRIX BASED ON VARIABLE @@@@@@@@@@@@@@@@
getInfoBasedOnVariables <- function(intra_matrix){

  ordered_intra = intra_matrix[with(intra_matrix, order(row_number)), ] 
  j = 1
  intra_3dMatrix = array(0, dim=c(8,200,2))
  rowCount = 1

  for (i in 1:(nrow(ordered_intra) - 1)){
      if (ordered_intra$row_number[i] == ordered_intra$row_number[i+1]){
      
	  intra_3dMatrix[rowCount, j, 1] = ordered_intra[["user_id"]][i]
	  intra_3dMatrix[rowCount, j, 2] = ordered_intra[["discoverability"]][i]	
	  j = j + 1
      }
      else{
	intra_3dMatrix[rowCount, j, 1] = ordered_intra[["user_id"]][i]
	intra_3dMatrix[rowCount, j, 2] = ordered_intra[["discoverability"]][i]	
	rowCount = rowCount + 1
	j = 1
      }
    if (i == (nrow(ordered_intra) - 1)){
      intra_3dMatrix[rowCount,j,1] = ordered_intra[["user_id"]][i + 1]
      intra_3dMatrix[rowCount,j,2] = ordered_intra[["discoverability"]][i + 1]
    }
  }	
  return(intra_3dMatrix)
}  
#  @@@@@@@@@@@@@@@@@@@@@@@@@@ END OF FUNCTION @@@@@@@@@@@@@@@@@@@@@@@@@@
  
intra_times_3dMatrix = getInfoBasedOnVariables(intra_times)

# to get rid of the 0's from the matrixs
# numberMatrix <- intra_3dMatrix[ , rowSums(abs(intra_3dMatrix[, ,]))>0 & rowSums(abs(intra_3dMatrix[, ,]))>0,  ]

#  @@@@@@@@@@@@@ FUNCTION TO RETURN A 3d MATRIX BASED ON USERS @@@@@@@@@@@@@
getInfoBasedOnUsers <- function(intra_matrix){

  ordered_intra = intra_matrix[with(intra_matrix, order(user_id)), ] 

# TODO set up  the actual array size 
  userIdArray <<- array(0, 500)
  user3DMatrix = array(0, dim=c(400,20,2) )
  
  count = 1
  arrayCount = 1
  j = 1

  for (i in 1:(nrow(ordered_intra) - 1)){
      if ( i == 1){
	userIdArray[arrayCount] <<- ordered_intra[["user_id"]][i]
	arrayCount = arrayCount + 1
      }
      if (ordered_intra$user_id[i] == ordered_intra$user_id[i+1]){	 
	  user3DMatrix[count, j, 1] = ordered_intra[["row_number"]][i]
	  user3DMatrix[count, j, 2] = ordered_intra[["discoverability"]][i]
	  j = j + 1
      }
      else{
        if (i != 1) {
	  userIdArray[arrayCount] <<- ordered_intra[["user_id"]][i + 1]
	  arrayCount = arrayCount + 1
        }
        user3DMatrix[count, j, 1] = ordered_intra[["row_number"]][i]
        user3DMatrix[count, j, 2] = ordered_intra[["discoverability"]][i]
        count = count + 1
        j = 1
      }
    if (i == (nrow(ordered_intra) - 1)){
      user3DMatrix[count,j,1] = ordered_intra[["row_number"]][i + 1]
      user3DMatrix[count,j,2] = ordered_intra[["discoverability"]][i + 1]
    }
  }	 	
  return(user3DMatrix)   
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@ END OF FUNCTION @@@@@@@@@@@@@@@@@@@@@@@@@@


intra_times_user_3dMatrix = getInfoBasedOnUsers(intra_times)
intra_times_userID = userIdArray

intra_days_user_3dMatrix = getInfoBasedOnUsers(intra_days)
intra_days_userID = userIdArray

intra_months_user_3dMatrix = getInfoBasedOnUsers(intra_months)
intra_months_userID = userIdArray

intra_weathers_user_3dMatrix = getInfoBasedOnUsers(intra_weathers)
intra_weathers_userID = userIdArray

intra_locations_user_3dMatrix = getInfoBasedOnUsers(intra_locations) 
intra_locations_userID = userIdArray

intra_temperatures_user_3dMatrix = getInfoBasedOnUsers(intra_temperatures)
intra_temperatures_userID = userIdArray



user3DMatrix[1, ,]  
numberMatrix <- user3DMatrix[ 1 , rowSums(abs(user3DMatrix[1 , ,]))>0 & rowSums(abs(user3DMatrix[1 , ,]))>0,  ]
plot(numberMatrix)
  
numberMatrix <- intra_times_user_3dMatrix[ 1 , rowSums(abs(intra_times_user_3dMatrix[1 , ,]))>0 & rowSums(abs(intra_times_user_3dMatrix[1 , ,]))>0,  ]

numberMatrix <- intra_days_user_3dMatrix[ 3 , rowSums(abs(intra_days_user_3dMatrix[3 , ,]))>0 & rowSums(abs(intra_days_user_3dMatrix[3 , ,]))>0,  ]


# USING K MEANS CLUSTERING
da <- intra_times[3:2]
cl <- kmeans(da, 24, iter.max = 20, nstart = 50)
plot(da, col=cl$cluster)
require(graphics)
points(cl$centers, col = 1:5, pch = 8)
 

da <- intra_months[3:2]
cl <- kmeans(da, 33, iter.max = 2, nstart = 1)
plot(da, col=cl$cluster)
require(graphics)
points(cl$centers, col = 1:5, pch = 8)

# END K MEANS CLUSTRING	


kmeansClustering <- function(intra_matrix, nr_clusters, nr_iterations, nr_start){
  da <- intra_matrix[3:2]
  cl <- kmeans(da, nr_clusters, iter.max = nr_iterations, nstart = nr_start)
  plot(da, col=cl$cluster)
  require(graphics)
  points(cl$centers, col = 1:15, pch = 8)
    
  return(cl)
}

intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)	
intra_days_cluster = kmeansClustering(intra_days, 14, 50, 50)	
intra_months_cluster = kmeansClustering(intra_months, 30, 50, 50)

intra_locations_cluster = kmeansClustering(intra_locations, 50, 50, 50)

intra_weathers_cluster = kmeansClustering(intra_weathers, 10, 100, 100)
intra_temperatures_cluster = kmeansClustering(intra_temperatures, 50, 50 ,50)
  
# GETTING the users that are in the same cluster  

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
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

cluster_user_matrix_intra_times = getUsersFromTheSameCluster(intra_times, intra_times_cluster)
cluster_user_matrix_intra_days = getUsersFromTheSameCluster(intra_days, intra_days_cluster)
cluster_user_matrix_intra_times[1 , ]
cluster_user_matrix_intra_times[3 , ]


  
  
  


