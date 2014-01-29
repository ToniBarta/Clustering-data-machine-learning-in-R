library(RPostgreSQL)
library(hash)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="momentus_development")
dbListConnections(drv)

# library(RPostgreSQL)
dbGetInfo(drv)
summary(con)

# THIS DATA IS FOR INTRA_TIME_OF_DAYS
intra_times = dbGetQuery(con,"select distinct user_id, 
                         case 
                         when variable = 'early morning' 	then 1
                         when variable = 'morning'	 	    then 2
                         when variable = 'late morning'		then 3
                         when variable = 'afternoon'		then 4
                         when variable = 'late afternoon'	then 5
                         when variable = 'evening'		then 6
                         when variable = 'night'		then 7
                         when variable = 'late night'		then 8
                         end,
                         (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_time_of_days where songs_skipped != 0")

plot(intra_times[3:2])
colnames(intra_times) <- c("user_id", "row_number", "discoverability")

# THIS DATA IS FOR INTRA_DAY_OF_WEEKS
intra_days = dbGetQuery(con,"select distinct user_id, 
                        case 
                        when variable = 'monday'                 then 1
                        when variable = 'tuesday'                then 2
                        when variable = 'wednesday'              then 3
                        when variable = 'thursday'               then 4	
                        when variable = 'friday'                 then 5
                        when variable = 'saturday'               then 6
                        when variable = 'sunday'                 then 7
                        end,
                        (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_day_of_weeks where songs_skipped != 0")

plot(intra_days[3:2])
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

###### -----------------------END OF GETTING THE DATA ---------------------------------------------- ######


kmeansClustering <- function(intra_matrix, nr_clusters, nr_iterations, nr_start){
  da <- intra_matrix[3:2]
  cl <- kmeans(da, nr_clusters, iter.max = nr_iterations, nstart = nr_start)
  
  #pdf('intra_temperature_cluster')  
  plot(da, col=cl$cluster)
  require(graphics)
  points(cl$centers, col = 1:15, pch = 8)
  #dev.off()
  return(cl)
}  		


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


gettingNeighboursForEachCategory <- function(input_variable, intra_cluster, intra_variable, index){
  
  #&&&&&&&&&&&Getting the clusters of the variable &&&&&&&&&&&&&&
  j = 1
  clustersArray = c(0)
  for ( i in 1: (nrow(intra_cluster[["centers"]]))){
    if (input_variable == intra_cluster[["centers"]][i,2]){
      clustersArray[j] = i
      j = j + 1
    }
  }
  
  # getting the users for each cluster
  cluster_user_matrix = getUsersFromTheSameCluster(intra_variable, intra_cluster)
  
  # need to check if the user is in this clusters! 
  # if it is => save all users from the cluster, if not => do nothing
  neighbour_users = c(0)
  
  for ( i in 1: length(clustersArray)){
    
    users =  cluster_user_matrix[clustersArray[i], ]
    # getting rid of extra 0's
    users = users[users != 0]
    
    answer =  any(users == input$user_id)
    if (answer == TRUE){
      for (j in 1: length(users)){
        neighbour_users[j] = users[j]
      }
      for (z in 1: length(neighbour_users)){
        if (index == 1)
          output$intra_times[z] = neighbour_users[z]
        else if (index == 2)
          output$intra_days[z] = neighbour_users[z]
        else if (index == 3)
          output$intra_months[z] = neighbour_users[z]
        else if (index == 4)
          output$intra_locations[z] = neighbour_users[z]
        else if (index == 5)
          output$intra_temperatures[z] = neighbour_users[z]
      }  
      break
    }	
  }
  #     print(neighbour_users)
}


getNeighboursForAUser <- function(input){
  
  #   user = input$user_id
  #   output = hash(user_id = user, intra_times = c(0), intra_days = c(0), intra_months = c(0), intra_locations = c(0),intra_temperatures = c(0))  
  # 
  #   print(output)
  
  intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)    
  gettingNeighboursForEachCategory(input$intra_times, intra_times_cluster, intra_times, 1)
  
  intra_days_cluster = kmeansClustering(intra_days, 14, 50, 50)
  gettingNeighboursForEachCategory(input$intra_days, intra_days_cluster, intra_days, 2)	
  
  intra_months_cluster = kmeansClustering(intra_months, 30, 50 ,50)
  gettingNeighboursForEachCategory(input$intra_months, intra_months_cluster, intra_months, 3)
  
  intra_locations_cluster = kmeansClustering(intra_locations, 250, 50, 50)
  gettingNeighboursForEachCategory(input$intra_locations, intra_locations_cluster, intra_locations, 4)
  
  intra_temperatures_cluster = kmeansClustering(intra_temperatures, 75, 50, 50)
  gettingNeighboursForEachCategory(input$intra_temperatures, intra_temperatures_cluster, intra_temperatures, 5)
  
}  # END OF getNeighboursForAUser



input = hash(user_id = 44, intra_times = 3, intra_days = 5, 
             intra_months = 10, intra_locations = 316 ,intra_temperatures = 18)  








getNeighboursForAUser(input)

output  


