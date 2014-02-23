
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


gettingNeighboursForEachCategory <- function(input_variable, intra_cluster, intra_variable, index){

  cluster_table = c()
  neighbour_users = c(0)
  for ( i in 1:(length(intra_cluster$size))) {
  
    cluster_table = intra_variable[intra_cluster$cluster== i , ]
    
    if (cluster_table$row_number[1] == as.integer(input_variable)){
   
      if ( any((cluster_table$user_id) == as.integer(input$user_id))){
        neighbour_users = cluster_table$user_id
        break
      }
    }
  }

   if (index == 1)
     output$intra_times = neighbour_users
   else if (index == 2)
     output$intra_days = neighbour_users
   else if (index == 3)
     output$intra_months = neighbour_users
   else if (index == 4)
     output$intra_locations = neighbour_users
   else if (index == 5)
     output$intra_temperatures = neighbour_users
       
} 


getNeighboursForAUser <- function(input){
  
  if (nrow(intra_times) > 999)
    intra_times_cluster = kmeansClustering(intra_times, (10 * max(intra_times$row_number)), 1500, 1)
  else
    intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)   
  gettingNeighboursForEachCategory(input$intra_times, intra_times_cluster, intra_times, 1)
  
  
  if (nrow(intra_days) > 999){
    intra_days_cluster = kmeansClustering(intra_days, (10 * max(intra_days$row_number)), 1500, 1)
  }
  else{
    intra_days_cluster = kmeansClustering(intra_days, 14, 50, 50)
  }
  gettingNeighboursForEachCategory(input$intra_days, intra_days_cluster, intra_days, 2)	


  if (nrow(intra_months) > 999)
    intra_months_cluster = kmeansClustering(intra_months, (10 * max(intra_months$row_number)), 1500, 1)
  else
    intra_months_cluster = kmeansClustering(intra_months, 30, 50 ,50)
  gettingNeighboursForEachCategory(input$intra_months, intra_months_cluster, intra_months, 3)
  

  if (nrow(intra_locations) > 999)
    intra_locations_cluster = kmeansClustering(intra_locations, (10 * max(intra_locations$row_number)), 1500, 1 )
  else
    intra_locations_cluster = kmeansClustering(intra_locations, 250, 50, 50)
  gettingNeighboursForEachCategory(input$intra_locations, intra_locations_cluster, intra_locations, 4)
  

   if (nrow(intra_temperatures) > 999)
    intra_temperatures_cluster = kmeansClustering(intra_temperatures, (10 * max(intra_temperatures$row_number)), 1500, 1 )
  else
    intra_temperatures_cluster = kmeansClustering(intra_temperatures, 75, 50, 50)
  gettingNeighboursForEachCategory(input$intra_temperatures, intra_temperatures_cluster, intra_temperatures, 5)
  
}  # END OF getNeighboursForAUser

