source("cluster_discoverability.R")

# user_id = 21 
# category { afternoon }	
# {user_id: 35, context: {time_of_day: 4, day_of_week: 3, month: 8, city: X, temp: 10}}

  
input_time_of_day = 5
input_day_of_week = 2
input_month = 10
input_city = 122
input_temperature = 17
  
input_user_id = 36

output = hash(user_id = input_user_id, intra_times = c(0), intra_days = c(0), intra_months = c(0), intra_locations = c(0),intra_temperatures = c(0))  


#  @@@@@@@@@@@@@@@@@@@@@@@@ FUNCTION @@@@@@@@@@@@@@@@@@@@@@@@

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
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  index
  # getting the users for each cluster
  cluster_user_matrix = getUsersFromTheSameCluster(intra_variable, intra_cluster)
 
  # need to check if the user is in this clusters! 
  # if it is => save all users from the cluster, if not => do nothing
  neighbour_users = c(0)

  for ( i in 1: length(clustersArray)){

    users =  cluster_user_matrix[clustersArray[i], ]
    # getting rid of extra 0's
    users = users[users != 0]

    answer =  any(users == input_user_id)
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
  print(neighbour_users)
}
#  @@@@@@@@@@@@@@@@@@@@@@@@ END FUNCTION @@@@@@@@@@@@@@@@@@@@@@@@


intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)	  
gettingNeighboursForEachCategory(input_time_of_day, intra_times_cluster, intra_times, 1)

intra_days_cluster = kmeansClustering(intra_days, 14, 50, 50)
gettingNeighboursForEachCategory(input_day_of_week, intra_days_cluster, intra_days, 2)

intra_months_cluster = kmeansClustering(intra_months, 30, 50 ,50)
gettingNeighboursForEachCategory(input_month, intra_months_cluster, intra_months, 3)

intra_locations_cluster = kmeansClustering(intra_locations, 250, 50, 50)
gettingNeighboursForEachCategory(input_city, intra_locations_cluster, intra_locations, 4)

intra_temperatures_cluster = kmeansClustering(intra_temperatures, 75, 50, 50)
gettingNeighboursForEachCategory(input_temperature, intra_temperatures_cluster, intra_temperatures, 5)



# input_user_id = 633
# variable_time_of_day = 'morning'         	
# variable_day_of_week = 'monday'
# 
# variable_time_of_day = switch(variable_time_of_day,
#                       'early morning'  =  1,        
#                       'morning'               =  2,
#                       'late morning'   =  3,
#                       'afternoon'      =  4,
#                       'late afternoon' =  5,
#                       'evening'               =  6,
#                       'night'               =  7,
#                       'late night'     =  8 )
#                           
# variable_day_of_week = switch(variable_day_of_week,
#                      'monday'                 = 1,
#                      'tuesday'                 = 2,        
#                      'wednesday'        = 3,
#                      'thursday'                = 4,
#                      'friday'                = 5,
#                      'saturday'                = 6,                
#                      'sunday'                = 7)
# 
# intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)     
# 
# intra_days_cluster = kmeansClustering(intra_days, 14, 50, 50)
# 
# intra_times_cluster[["centers"]]
# 
# intra_days_cluster[["centers"]]
# 
# #&&&&&&&&&&&Getting the clusters of the variable &&&&&&&&&&&&&&
# j = 1
# clustersArray = c(0)
# 
# for ( i in 1: (nrow(intra_days_cluster[["centers"]]))){
# 
#   if (variable_time_of_day == intra_days_cluster[["centers"]][i,2]){
#     clustersArray[j] = i
#     j = j + 1
#   }
# }
# 
# # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# # getting the users for each cluster
# cluster_user_matrix_intra_times = getUsersFromTheSameCluster(intra_days, intra_days_cluster)
# 
# 
# 
# # need to check if the user is in this clusters! 
# # if it is => save all users from the cluster, if not => do nothing
# neighbour_users = c(0)
# 
# for ( i in 1: length(clustersArray)){
# 
#   da =  cluster_user_matrix_intra_times[clustersArray[i], ]
#   # getting rid of extra 0's
#   da = da[da != 0]
# 
#   answer =  any(da == input_user_id)
#   if (answer == TRUE){
#     for (j in 1: length(da)){
#       neighbour_users[j] = da[j]
#     }
#    break
#   }
# }
# 
# print(neighbour_users)






