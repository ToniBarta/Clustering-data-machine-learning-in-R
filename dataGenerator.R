require(stats)



user_id_intra_times <- sample(1:500, 1000, replace=T)
row_number_intra_times <- sample(1:8, 1000, replace=T)
discoverability_intra_times <- runif(1000, 0, 1)

dataMatrix_times = c( user_id_intra_times, row_number_intra_times, discoverability_intra_times) 

intra_times <- matrix(dataMatrix_times, ncol = 3)
colnames(intra_times) <- c("user_id", "row_number", "discoverability")
intra_times <-as.data.frame(intra_times, header=TRUE)


generateData <- function(variable ,number_of_users, total_number_of_elementes){

	dataMatrix = c(sample(1:number_of_users, total_number_of_elementes, replace=T), sample(1:variable, total_number_of_elementes, replace=T), runif(total_number_of_elementes, 0, 1))
	intra_matrix <- matrix(dataMatrix, ncol = 3)
	colnames(intra_matrix) <- c("user_id", "row_number", "discoverability")
	intra_matrix <-as.data.frame(intra_matrix, header=TRUE)

	return (intra_matrix)
}


intra_times 	= generateData(8, 600, 5000)
intra_days 		= generateData(7, 600, 5000)
intra_months 	= generateData(12, 600, 5000)



for (i in 1:5){
	intra_times = sample(1:30, 20, replace=F)
  if (i == 1)
    dataMatrix = intra_times
  else
	  dataMatrix = c(dataMatrix, intra_times)
}
dataMatrix



generateData2 <- function(variable, number_of_users, total_number_of_elementes){
	intra_variable = 0
	# depends on the number of variables. ex times -> 8 ; day -> 7; months -> 12
	for (i in 1:variable){
		intra_users = sample(1: number_of_users, ceiling(total_number_of_elementes / variable)) 
		length(intra_variable) = ceiling(total_number_of_elementes / variable)
		intra_variable[] = i

		if (i == 1){
			data_users = intra_users
			data_variable = intra_variable
		}
		else{
			data_users = c(data_users, intra_users)
			data_variable = c(data_variable, intra_variable)
		}
	}
	length(data_users) <- total_number_of_elementes	
	length(data_variable) <- total_number_of_elementes


	dataMatrix = c(data_users, data_variable, runif(total_number_of_elementes, 0, 1) )


	intra_matrix <- matrix(dataMatrix, ncol = 3)
	colnames(intra_matrix) <- c("user_id", "row_number", "discoverability")
	intra_matrix <-as.data.frame(intra_matrix, header=TRUE)

	return (intra_matrix)
}


intra_times 	= generateData2(8, 500, 3500)
intra_days 		= generateData2(7, 500, 3500)
intra_months 	= generateData2(12, 500, 3500)


