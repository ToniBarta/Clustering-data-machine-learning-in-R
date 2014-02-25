
kmeansClustering <- function(intra_matrix, nr_clusters, nr_iterations, nr_start){
  da <- intra_matrix[3:2]
  cl <- kmeans(da, nr_clusters, iter.max = nr_iterations, nstart = nr_start)
  
  #pdf('intra_temperature_cluster')  
  plot(da, col=cl$cluster)
  require(graphics)
  points(cl$centers, col = 1:150, pch = 8)
  #dev.off()
  return(cl)
}  		


getUsersFromTheSameCluster <- function(intra_matrix, intra_cluster){
  
  cluster_user_matrix <- matrix(0, nrow(intra_matrix), max(intra_cluster$size))
  
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
  
  if (nrow(intra_times) > 999)
    intra_times_cluster = kmeansClustering(intra_times, (10 * max(intra_times$row_number)), 500, (2 * (10 * max(intra_times$row_number))) )
  else
    intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)   
  gettingNeighboursForEachCategory(input$intra_times, intra_times_cluster, intra_times, 1)
  
  
  if (nrow(intra_days) > 999)
    intra_days_cluster = kmeansClustering(intra_days, (10 * max(intra_days$row_number)), 500, (2 * (10 * max(intra_days$row_number))) )
  else
    intra_days_cluster = kmeansClustering(intra_days, 14, 50, 50)
  gettingNeighboursForEachCategory(input$intra_days, intra_days_cluster, intra_days, 2)	


  if (nrow(intra_months) > 999)
    intra_months_cluster = kmeansClustering(intra_months, (10 * max(intra_months$row_number)), 500, (2 * (10 * max(intra_months$row_number))) )
  else
    intra_months_cluster = kmeansClustering(intra_months, 30, 50 ,50)
  gettingNeighboursForEachCategory(input$intra_months, intra_months_cluster, intra_months, 3)
  

  if (nrow(intra_locations) > 999)
    intra_locations_cluster = kmeansClustering(intra_locations, (10 * max(intra_locations$row_number)), 500, 1 )
  else
    intra_locations_cluster = kmeansClustering(intra_locations, 250, 50, 50)
  gettingNeighboursForEachCategory(input$intra_locations, intra_locations_cluster, intra_locations, 4)
  

   if (nrow(intra_temperatures) > 999)
    intra_temperatures_cluster = kmeansClustering(intra_temperatures, (10 * max(intra_temperatures$row_number)), 500, 1 )
  else
    intra_temperatures_cluster = kmeansClustering(intra_temperatures, 75, 50, 50)
  gettingNeighboursForEachCategory(input$intra_temperatures, intra_temperatures_cluster, intra_temperatures, 5)
  
}  # END OF getNeighboursForAUser

