
ordered_times = intra_times[with(intra_times, order(row_number)), ]
input_value_times = as.integer(input$intra_times)
userID_times = as.integer(output$intra_times)



count = 1
discoverability_times = c(0)
for (i in 1:(length(ordered_times))){

	if (ordered_times$row_number[i] == input_value_times){

		for (j in 1:(length(userID_times))){

			if ( ordered_times$user_id[i] == userID_times[j] )
				discoverability_times[count] = ordered_times$discoverability[i]
				count = count + 1
		}
	}
}


# matrix3DOfValues = getInfoBasedOnVariables(intra_times)
# input_value_times = as.integer(input$intra_times)
# userID_times = as.integer(output$intra_times)

# discIndex = 1
# for (count in 1:(nrow(matrix3DOfValues[input_value_times, , ]))){

# 	for (i in 1:(length(userID_times))){

# 		if (matrix3DOfValues[input_value_times, count, 1] == userID_times[i]){
# 			discoverability_times[discIndex] = matrix3DOfValues[input_value_times, count, 2]
# 			discIndex = discIndex + 1
# 		}
# 	}
# }




getDiscoverabilityForUsers <- function(intra_category, input_row_number, input_users){

	matrix3DOfValues = getInfoBasedOnVariables(intra_category)
	discoverability_array = c(0)

	discIndex = 1
	for (count in 1:(nrow(matrix3DOfValues[input_row_number, , ]))){

		for (i in 1:(length(input_users))){

			if (matrix3DOfValues[input_row_number, count, 1] == input_users[i]){
				discoverability_array[discIndex] = matrix3DOfValues[input_row_number, count, 2]
				discIndex = discIndex + 1
			}
		}
	}
	return (discoverability_array)
}


input_row_number_times = as.integer(input$intra_times)
userID_times = as.integer(output$intra_times)
discoverability_times = getDiscoverabilityForUsers(intra_times, input_row_number_times, userID_times)



input_row_number_days = as.integer(input$intra_days)
userID_days = as.integer(output$intra_days)
discoverability_days = getDiscoverabilityForUsers(intra_days, input_row_number_days, userID_days)



input_row_number_months = as.integer(input$intra_months)
userID_months = as.integer(output$intra_months)
discoverability_months = getDiscoverabilityForUsers(intra_months, input_row_number_months, userID_months)



input_row_number_locations = as.integer(input$intra_locations)
userID_locations = as.integer(input$intra_locations)
discovera