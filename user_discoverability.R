source("cluster_discoverability.R")

# user_id = 21 
# category { afternoon }	


input_user_id = 633
variable_time_of_day = 'morning' 	
variable_day_of_week = 'monday'

variable_time_of_day = switch(variable_time_of_day,
		      'early morning'  =  1,	
		      'morning'	       =  2,
		      'late morning'   =  3,
		      'afternoon'      =  4,
		      'late afternoon' =  5,
		      'evening'	       =  6,
		      'night'	       =  7,
		      'late night'     =  8 )
		          
variable_day_of_week = switch(variable_day_of_week,
		     'monday' 		= 1,
		     'tuesday'	 	= 2,	
		     'wednesday'	= 3,
		     'thursday'		= 4,
		     'friday'		= 5,
		     'saturday'		= 6,		
		     'sunday'		= 7)

intra_times_cluster = kmeansClustering(intra_times, 24, 50, 50)	  


intra_times_cluster[["centers"]]

#&&&&&&&&&&&Getting the clusters of the variable &&&&&&&&&&&&&&
j = 1
clustersArray = c(0)

for ( i in 1: (nrow(intra_times_cluster[["centers"]]))){

  if (variable_time_of_day == intra_times_cluster[["centers"]][i,2]){
    clustersArray[j] = i
    j = j + 1
  }
}

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# getting the users for each cluster
cluster_user_matrix_intra_times = getUsersFromTheSameCluster(intra_times, intra_times_cluster)

# need to check if the user is in this clusters! 
# if it is => save all users from the cluster, if not => do nothing
neighbour_users = c(0)

for ( i in 1: length(clustersArray)){

  da =  cluster_user_matrix_intra_times[clustersArray[i], ]
  # getting rid of extra 0's
  da = da[da != 0]

  answer =  any(da == input_user_id)
  if (answer == TRUE){
    for (j in 1: length(da)){
      neighbour_users[j] = da[j]
    }
   break
  }
}

print(neighbour_users)


