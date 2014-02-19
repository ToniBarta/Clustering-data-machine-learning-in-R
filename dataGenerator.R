require(stats)



user_id_intra_times <- sample(1:500, 1000, replace=T)

row_number_intra_times <- sample(1:8, 1000, replace=T)

discoverability_intra_times <- runif(1000, 0, 1)

dataMatrix = c( user_id_intra_times, row_number_intra_times, discoverability_intra_times) 

intra_times <- matrix(dataMatrix, ncol = 3)
colnames(intra_times) <- c("user_id", "row_number", "discoverability")
intra_times <-as.data.frame(intra_times, header=TRUE)


