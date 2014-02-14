# testing cuve ploting

require( rgl )      # for 3 d graphics
require( rimage )   # to import jpeg files
require(scatterplot3d)

z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue", main="scatterplot3d - 1", pch=20)


x = c(45, 501, 206, 48)
y = c(41, 511, 4, 12)
z = c(41, 201, 431, 131)

plot3d(x,y,z, size=10)

x <- sort(rnorm(1000))
x
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)
plot3d(x, y, z, col=rainbow(1000))

 

  
          gettingTheDiscoverabilityAndUserIdArray <- function(ordered_intra, input){
            input = as.integer(input)
            userIDsArray <<- c(0)
            discoverabilityArray <<- c(0)
            indexPos = 1
            
            for (i in 1:(nrow(ordered_intra))){
              
              if (ordered_intra$row_number[i] == input)
              {
                userIDsArray[indexPos] <<- ordered_intra$user_id[i]
                discoverabilityArray[indexPos] <<- ordered_intra$discoverability[i]
                indexPos = indexPos + 1
              }
            }
          } #end of function gettingTheDiscoverabilityAndUserIdArray

     
     # index array for remambering the index to look into users and for that user to get discoverability          
        #  gettingIndexArray <- function(userIDsArray, intra_category){
          
            indexArray <- c(0)
            indexPos = 1
            for (i in 1:(length(userIDsArray))){
              for (j in 1:(length(output$intra_times))){
                
                if (output$intra_times[j] == userIDsArray[i]){
                  indexArray[indexPos] = i
                  indexPos = indexPos + 1
                }
              }
            }
        #  } # end of gettingIndexArray

##### ****************** INTRA_TIMES ******************

ordered_intra = intra_times[with(intra_times, order(row_number)), ]
gettingTheDiscoverabilityAndUserIdArray(ordered_intra, input$intra_times)
#gettingIndexArray(userIDsArray, intra_times)

      indexArray <- c(0)
      indexPos = 1
      for (i in 1:(length(userIDsArray))){
        for (j in 1:(length(output$intra_times))){
          
          if (output$intra_times[j] == userIDsArray[i]){
            indexArray[indexPos] = i
            indexPos = indexPos + 1
          }
        }
      }
  
userIDsArray_intra_times = userIDsArray
discoverabilityArray_intra_times = discoverabilityArray
indexArray_intra_times = indexArray

##### ****************** INTRA_TIMES ******************


##### ****************** INTRA_DAYS ******************

ordered_intra = intra_days[with(intra_days, order(row_number)), ] 
gettingTheDiscoverabilityAndUserIdArray(ordered_intra, input$intra_times)
#gettingIndexArray(userIDsArray, intra_days)

      indexArray <- c(0)
      indexPos = 1
      for (i in 1:(length(userIDsArray))){
        for (j in 1:(length(output$intra_days))){
          
          if (output$intra_days[j] == userIDsArray[i]){
            indexArray[indexPos] = i
            indexPos = indexPos + 1
          }
        }
      }
  

userIDsArray_intra_days = userIDsArray
discoverabilityArray_intra_days = discoverabilityArray
indexArray_intra_days = indexArray

##### ****************** INTRA_DAYS ******************



initialLengthOfIndexArray_intra_times = length(indexArray_intra_times)
initialLengthOfIndexArray_intra_days = length(indexArray_intra_days)


  # insertingAllusersAndDiscoverabilityInArrays <- function(){

      indexLength = length(indexArray_intra_times)
      usersLength = length(userIDsArray_intra_times)
      discoverabilityLength = length(discoverabilityArray_intra_times)

      for (i in 1:(length(indexArray_intra_days))){
        # puting the rest of the indexArray for example intra_days at the end of the intra_times
        indexArray_intra_times[i + indexLength] = indexArray_intra_days[i] 
        userIDsArray_intra_times[i + usersLength] = userIDsArray_intra_days[i]
        discoverabilityArray_intra_times[i + discoverabilityLength] = discoverabilityArray_intra_days[i]
      }

  # }

# for intra_times  ->z x 10 
# for intra_days   ->z x 100
# for intra_months ->z x 1000

x = 0
y = 0
z = 0


      for (i in 1:(length(indexArray_intra_times))){
        if ( i > 1)
        {
          if (initialLengthOfIndexArray_intra_times == i){
            x[i] = discoverabilityArray_intra_times[i] + 0.1
          }
          else{
            x[i] = discoverabilityArray_intra_times[i] + x[i-1] + 0.01
          }
        }
        else{
          x[i] = discoverabilityArray_intra_times[i] + 0.1
        }
        
        y[i] = userIDsArray_intra_times[i]
          
        if (i > 1)
        {
          if ( i == initialLengthOfIndexArray_intra_times)
            z[i] = as.integer(input$intra_days[1]) * 11
          else
            z[i] = z[i - 1] + 0.01
        }
        else{
          if ( i < initialLengthOfIndexArray_intra_times)
            z[i] = as.integer(input$intra_times[1]) * 10
        }
      }
  #      if (i > 1)
  #        z[i] = z[i-1] + 0.01
  #      else
  #        z[i] = 4
      


plot3d(x, y, z , size=8, col = rainbow(150))

output$intra_days





