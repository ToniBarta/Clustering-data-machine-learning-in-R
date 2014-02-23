# testing cuve ploting
library(rgl) 
require( rgl )      # for 3 d graphics
require( rimage )   # to import jpeg files
require(scatterplot3d)


#  @@@@@@@@@@@@@ FUNCTION TO RETURN A 3d MATRIX BASED ON VARIABLE @@@@@@@@@@@@@@@@
getInfoBasedOnVariables <- function(intra_matrix){
  
  ordered_intra = intra_matrix[with(intra_matrix, order(row_number)), ] 
  j = 1
  intra_3dMatrix = array(0, dim=c(max(ordered_intra$row_number),length(unique(ordered_intra$user_id)),2))
  rowCount = ordered_intra$row_number[1]
  
  for (i in 1:(nrow(ordered_intra) - 1)){
    if (ordered_intra$row_number[i] == ordered_intra$row_number[i+1]){
      
      intra_3dMatrix[ordered_intra$row_number[i], j, 1] = ordered_intra[["user_id"]][i]
      intra_3dMatrix[ordered_intra$row_number[i], j, 2] = ordered_intra[["discoverability"]][i]	
      j = j + 1
    }
    else{
      intra_3dMatrix[ordered_intra$row_number[i], j, 1] = ordered_intra[["user_id"]][i]
      intra_3dMatrix[ordered_intra$row_number[i], j, 2] = ordered_intra[["discoverability"]][i]	
      rowCount = rowCount + 1
      j = 1
    }
    if (i == (nrow(ordered_intra) - 1)){
      intra_3dMatrix[ordered_intra$row_number[i],j,1] = ordered_intra[["user_id"]][i + 1]
      intra_3dMatrix[ordered_intra$row_number[i],j,2] = ordered_intra[["discoverability"]][i + 1]
    }
  }	
  return(intra_3dMatrix)
}  
#  @@@@@@@@@@@@@@@@@@@@@@@@@@ END OF FUNCTION @@@@@@@@@@@@@@@@@@@@@@@@@@


getDiscoverabilityForUsers <- function(intra_category, input_row_number, input_users){
  
  if (input_users != 0){
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
	return (0)
}

input_row_number_times = 0 
userID_times = 0 
discoverability_times = 0
input_row_number_times = as.integer(input$intra_times)
userID_times = as.integer(output$intra_times)
discoverability_times = getDiscoverabilityForUsers(intra_times, input_row_number_times, userID_times)

input_row_number_days = 0 
userID_days = 0 
discoverability_days = 0
input_row_number_days = as.integer(input$intra_days)
userID_days = as.integer(output$intra_days)
discoverability_days = getDiscoverabilityForUsers(intra_days, input_row_number_days, userID_days)

input_row_number_months = 0 
userID_months = 0 
discoverability_months = 0
input_row_number_months = as.integer(input$intra_months)
userID_months = as.integer(output$intra_months)
discoverability_months = getDiscoverabilityForUsers(intra_months, input_row_number_months, userID_months)

input_row_number_locations = 0 
userID_locations = 0 
discoverability_locations = 0
input_row_number_locations = as.integer(input$intra_locations)
userID_locations = as.integer(output$intra_locations)
discoverability_locations = getDiscoverabilityForUsers(intra_locations, input_row_number_locations, userID_locations)

input_row_number_temperatures = 0 
userID_temperatures = 0 
discoverability_temperatures = 0
input_row_number_temperatures = as.integer(input$intra_temperatures)
userID_temperatures = as.integer(output$intra_temperatures)
discoverability_temperatures = getDiscoverabilityForUsers(intra_temperatures, input_row_number_temperatures, userID_temperatures)

# input_row_number_weathers = as.integer(input$intra_weathers)
# userID_weathers = as.integer(output$intra_weathers)
# discoverability_weathers = getDiscoverabilityForUsers(intra_weathers, input_row_number_weathers, userID_weathers)

lengthArrayOfEachCategoryOFTheUsers_ID = c(0)
lengthArrayOfEachCategoryOFTheUsers_ID[1] = length(userID_times)
lengthArrayOfEachCategoryOFTheUsers_ID[2] = length(userID_days)
lengthArrayOfEachCategoryOFTheUsers_ID[3] = length(userID_months)
lengthArrayOfEachCategoryOFTheUsers_ID[4] = length(userID_locations)
lengthArrayOfEachCategoryOFTheUsers_ID[5] = length(userID_temperatures)



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
getXArrayAxis(discoverability_months, length(discoverability_times) + length(discoverability_days) + 1)
getXArrayAxis(discoverability_locations, length(discoverability_times) + length(discoverability_days) +length(discoverability_months) + 1)
getXArrayAxis(discoverability_temperatures, length(discoverability_times) + length(discoverability_days) +length(discoverability_months) + length(discoverability_locations) + 1)
length(xArrayAxis) <- length(xArrayAxis) - 1


getYArrayAxis <- function(userIDArray, startingIndex, variableStatus){
	secondIndex = 1

	for (index in startingIndex:(length(userIDArray) + startingIndex)){
		yArrayAxis[index] <<- userIDArray[secondIndex]

		if (variableStatus == 1)
			variablesArray[index] <<- 1
		if (variableStatus == 2)
			variablesArray[index] <<- 2
		if (variableStatus == 3)
			variablesArray[index] <<- 3
		if (variableStatus == 4)
			variablesArray[index] <<- 4
		if (variableStatus == 5)
			variablesArray[index] <<- 5 

		secondIndex = secondIndex + 1
	}
}

# creating an Array for storing the variables for making the right colors in the 3d cube
variablesArray <<- c(0)
yArrayAxis <<- c(0)
getYArrayAxis(userID_times,1, 1)
getYArrayAxis(userID_days, length(userID_times) + 1,  2)
getYArrayAxis(userID_months, length(userID_times)  + length(userID_days) + 1,  3)
getYArrayAxis(userID_locations, length(userID_times)  + length(userID_days) + length(userID_months) + 1,  4)
getYArrayAxis(userID_temperatures, length(userID_times)  + length(userID_days) + length(userID_months) + length(userID_locations) + 1, 5)
length(yArrayAxis) <- length(yArrayAxis) - 1
length(variablesArray) <- length(variablesArray) - 1


zArrayAxis <<-c(0)
for (index in 1:(sum(lengthArrayOfEachCategoryOFTheUsers_ID))){

	if (index == 1){
			zArrayAxis[index] = input_row_number_times
	}
	else{
		if (index == lengthArrayOfEachCategoryOFTheUsers_ID[1] + 1){
			zArrayAxis[index] = input_row_number_days * 1.7
		}
		else{
			if (index == lengthArrayOfEachCategoryOFTheUsers_ID[2] + lengthArrayOfEachCategoryOFTheUsers_ID[1] + 1){
				zArrayAxis[index] = input_row_number_months * 2
			}
			else{
				if (index == lengthArrayOfEachCategoryOFTheUsers_ID[3] + lengthArrayOfEachCategoryOFTheUsers_ID[2] + lengthArrayOfEachCategoryOFTheUsers_ID[1] + 1){
					zArrayAxis[index] = input_row_number_locations * 0.2
				}
				else{
          if (index == lengthArrayOfEachCategoryOFTheUsers_ID[4] + lengthArrayOfEachCategoryOFTheUsers_ID[3] + lengthArrayOfEachCategoryOFTheUsers_ID[2] + lengthArrayOfEachCategoryOFTheUsers_ID[1] + 1){
            zArrayAxis[index] = input_row_number_temperatures * 1.1
          }
          else
					  zArrayAxis[index] = zArrayAxis[index - 1] + 0.01
				}
			}
		}
	}
}  # for  


#  plot3d(xArrayAxis, yArrayAxis, zArrayAxis , size=4, col = rainbow(sum(lengthArrayOfEachCategoryOFTheUsers_ID)))


#xArrayAxis = c(discoverability_times, discoverability_days, discoverability_months, discoverability_locations)
tableMatrix = c( yArrayAxis, variablesArray, xArrayAxis , yArrayAxis, zArrayAxis)


dataTable <- matrix(tableMatrix, ncol = 5)
colnames(dataTable) <- c("users", "variable", "x" , "y" , "z" )
dataTable <- as.data.frame(dataTable, header = TRUE)


colors =c("green","blue","red", "black") 
p3d<- plot3d(dataTable$x, dataTable$y, dataTable$z, xlab=" discoverability ", ylab=" user number ", 
             zlab=" category ", 
             col=as.integer(dataTable$variable) , 
             box=TRUE, size=5) 


#name.v<-as.vector(dataTable$users) 
#text3d(dataTable$x, dataTable$y, dataTable$z, name.v, cex=0.9, adj = 1) 

