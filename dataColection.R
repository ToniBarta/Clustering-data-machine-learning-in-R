library(RPostgreSQL)
library(hash)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="momentus_development")
dbListConnections(drv)

# library(RPostgreSQL)
dbGetInfo(drv)
summary(con)

# THIS DATA IS FOR INTRA_TIME_OF_DAYS
intra_times = dbGetQuery(con,"select distinct user_id, 
                         case 
                         when variable = 'early morning' 	then 1
                         when variable = 'morning'	 	    then 2
                         when variable = 'late morning'		then 3
                         when variable = 'afternoon'		  then 4
                         when variable = 'late afternoon'	then 5
                         when variable = 'evening'		    then 6
                         when variable = 'night'		      then 7
                         when variable = 'late night'		  then 8
                         end,
                         (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_time_of_days where songs_skipped != 0")

labels = c('ea morn', 'morn', 'la morn'  ,'after',  'la after', 'evening'  ,'night', 'la night' )
plot(main = 'Time of day', xlab ='discoverability', ylab='', intra_times[3:2], yaxt = "n")
axis(2, at=1:8, label=labels[1:8], las = 1)

colnames(intra_times) <- c("user_id", "row_number", "discoverability")



######### Ploting a graph for user 36 ###########
labels = c('ea morn', 'morn', 'la morn'  ,'after',  'la after', 'evening'  ,'night', 'la night' )
userDemoIntraTimes = getInfoBasedOnUsers(intra_times)
numberMatrix <- userDemoIntraTimes[ 1 , rowSums(abs(userDemoIntraTimes[1 , ,]))>0 & rowSums(abs(userDemoIntraTimes[1 , ,]))>0,  ]
plot(main = 'Time of day for user 41', xlab ='discoverability', ylab='', yaxt = "n", numberMatrix[ , 2], numberMatrix [ , 1] )
axis(2, at=1:8, label=labels[1:8], las = 1)
#################### END ########################



# THIS DATA IS FOR INTRA_DAY_OF_WEEKS
intra_days = dbGetQuery(con,"select distinct user_id, 
                        case 
                        when variable = 'monday'        then 1
                        when variable = 'tuesday'       then 2
                        when variable = 'wednesday'     then 3
                        when variable = 'thursday'      then 4	
                        when variable = 'friday'        then 5
                        when variable = 'saturday'      then 6
                        when variable = 'sunday'        then 7
                        end,
                        (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_day_of_weeks where songs_skipped != 0")

labels = c('monday' ,'tuesday'  ,'wednesday','thursday' , 'friday' ,'saturday' ,'sunday' )
plot(main = 'Day of the week', xlab= 'discoverability', ylab='', intra_days[3:2], yaxt = "n")
axis(2, at=1:7, label=labels[1:7], las = 1)

colnames(intra_days) <- c("user_id", "row_number", "discoverability")  

# THIS DATA IS FOR INTRA_MONTHS
intra_months = dbGetQuery(con,"select distinct user_id, 
                          case
                          when variable = 'january' 		then 1
                          when variable = 'february'	 	then 2
                          when variable = 'march'		    then 3
                          when variable = 'april'		    then 4
                          when variable = 'may'			    then 5
                          when variable = 'june'			  then 6
                          when variable = 'july'			  then 7
                          when variable = 'august'		  then 8
                          when variable = 'september'		then 9
                          when variable = 'october'		  then 10
                          when variable = 'november'		then 11
                          when variable = 'december'		then 12
                          end,
                          (1 - new_song_skipped*1.0/songs_skipped) AS discoverability from intra_months where songs_skipped != 0")

labels = c( 'jan' ,'feb', 'march' , 'april' ,'may' ,'june', 'july', 'august' , 'sept' ,'oct' , 'nov' ,'dec')
plot(main = 'Months', xlab= 'discoverability', ylab='', intra_months[3:2], yaxt = "n")
axis(2, at=1:12, label=labels[1:12], las = 1)

colnames(intra_months) <- c("user_id", "row_number", "discoverability")

# THIS DATA IS FOR INTRA_WEATHER
intra_weathers = dbGetQuery(con, "SELECT DISTINCT iw.user_id, w.row_number, 
                            (1 - iw.new_song_skipped*1.0/iw.songs_skipped) AS discoverability 
                            from intra_weather_categories iw, (SELECT row_number() OVER(ORDER BY category), category 
                            FROM weathers GROUP BY category) w 
                            WHERE w.category = iw.variable AND iw.songs_skipped != 0 ORDER BY iw.user_id, w.row_number")

plot(main = 'Weather', xlab = 'discoverability', ylab= 'type of weather' ,intra_weathers[3:2])


# THIS DATA IS FOR INTRA_LOCATION
intra_locations = dbGetQuery(con, "SELECT DISTINCT ic.user_id, c.row_number, 
                             (1 - ic.new_song_skipped*1.0/ic.songs_skipped) AS discoverability 
                             from intra_cities ic, (SELECT row_number() OVER(ORDER BY city), city 
                             FROM locations GROUP BY city) c 
                             WHERE c.city = ic.variable AND ic.songs_skipped != 0 ORDER BY ic.user_id, c.row_number")

plot(main ='Location', xlab ='discoverability', ylab = 'city' , intra_locations[3:2])	


# THIS DATA IS FOR TEMPERATURE
intra_temperatures = dbGetQuery(con, "SELECT DISTINCT iw.user_id, w.row_number, 
                                (1 - iw.new_song_skipped*1.0/iw.songs_skipped) AS discoverability 
                                from intra_temperatures iw, (SELECT row_number() OVER(ORDER BY temperature), temperature 
                                FROM weathers GROUP BY temperature) w 
                                WHERE w.temperature = iw.variable AND iw.songs_skipped != 0 ORDER BY iw.user_id, w.row_number")

plot(main = 'Temperature', xlab = 'discoverability', ylab = 'temperature' ,intra_temperatures[3:2])


dbDisconnect(con)
###### -----------------------END OF GETTING THE DATA ---------------------------------------------- ######







#######################     DEMONSTRATION ONLY      ####################

#  @@@@@@@@@@@@@ FUNCTION TO RETURN A 3d MATRIX BASED ON USERS @@@@@@@@@@@@@
getInfoBasedOnUsers <- function(intra_matrix){
  
  ordered_intra = intra_matrix[with(intra_matrix, order(user_id)), ] 
  
  # TODO set up  the actual array size 
  userIdArray <<- array(0, 500)
  user3DMatrix = array(0, dim=c(400,20,2) )
  
  count = 1
  arrayCount = 1
  j = 1
  
  for (i in 1:(nrow(ordered_intra) - 1)){
    if ( i == 1){
      userIdArray[arrayCount] <<- ordered_intra[["user_id"]][i]
      arrayCount = arrayCount + 1
    }
    if (ordered_intra$user_id[i] == ordered_intra$user_id[i+1]){	 
      user3DMatrix[count, j, 1] = ordered_intra[["row_number"]][i]
      user3DMatrix[count, j, 2] = ordered_intra[["discoverability"]][i]
      j = j + 1
    }
    else{
      if (i != 1) {
        userIdArray[arrayCount] <<- ordered_intra[["user_id"]][i + 1]
        arrayCount = arrayCount + 1
      }
      user3DMatrix[count, j, 1] = ordered_intra[["row_number"]][i]
      user3DMatrix[count, j, 2] = ordered_intra[["discoverability"]][i]
      count = count + 1
      j = 1
    }
    if (i == (nrow(ordered_intra) - 1)){
      user3DMatrix[count,j,1] = ordered_intra[["row_number"]][i + 1]
      user3DMatrix[count,j,2] = ordered_intra[["discoverability"]][i + 1]
    }
  }	 	
  return(user3DMatrix)   
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@ END OF FUNCTION @@@@@@@@@@@@@@@@@@@@@@@@@@

#######################     DEMONSTRATION ONLY      ####################
















