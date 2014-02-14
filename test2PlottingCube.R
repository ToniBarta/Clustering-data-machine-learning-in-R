
ordered_times = intra_times[with(intra_times, order(row_number)), ]
input_value_times = as.integer(input$intra_times)
userID_times = as.integer(output$intra_times)



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
userID_locations = as.integer(output$intra_locations)
discoverability_locations = getDiscoverabilityForUsers(intra_locations, input_row_number_locations, userID_locations)


#needs testing 
input_row_number_temperatures = as.integer(input$intra_temperature)
userID_temperatures = as.integer(output$intra_temperature)
discoverability_temperatures = getDiscoverabilityForUsers(intra_temperatures, input_row_number_temperatures, userID_temperatures)


# input_row_number_weathers = as.integer(input$intra_weathers)
# userID_weathers = as.integer(output$intra_weathers)
# discoverability_weathers = getDiscoverabilityForUsers(intra_weathers, input_row_number_weathers, userID_weathers)

lengthArrayOfEachCategoryOFTheUsers_ID = c(0)
lengthArrayOfEachCategoryOFTheUsers_ID[1] = length(userID_times)
lengthArrayOfEachCategoryOFTheUsers_ID[2] = length(userID_days)
lengthArrayOfEachCategoryOFTheUsers_ID[3] = length(userID_months)
lengthArrayOfEachCategoryOFTheUsers_ID[4] = length(userID_locations)




getXArrayAxis <- function(discoverabilityArray, startingIndex){

	for ( index in startingIndex:(length(discoverabilityArray) + startingIndex)){
		# it will be the first position 
    secondIndex = 1
    
		if (index == startingIndex){
			xArrayAxis[index] <<- discoverabilityArray[secondIndex] + 0.1
		}
		else{
		  secondIndex = secondIndex + 1
			xArrayAxis[index] <<- ((discoverabilityArray[secondIndex] + xArrayAxis[index - 1]) * 1.01)
		}
	}
} # end function

xArrayAxis <<- c(0)
getXArrayAxis(discoverability_times,1)
getXArrayAxis(discoverability_days, length(discoverability_times) + 1)
getXArrayAxis(discoverability_months, length(discoverability_days) + 1)
getXArrayAxis(discoverability_locations, length(discoverability_months) + 1)



getYArrayAxis <- function(userIDArray, startingIndex){
	secondIndex = 1

	for (index in startingIndex:(length(userIDArray) + startingIndex)){
		yArrayAxis[index] <<- userIDArray[secondIndex]
		secondIndex = secondIndex + 1
	}
  
}

yArrayAxis <<- c(0)
getYArrayAxis(userID_times,1)
getYArrayAxis(userID_days, length(userID_times) + 1)
getYArrayAxis(userID_months, length(userID_days) + 1)
getYArrayAxis(userID_locations, length(userID_months) + 1)











