{
    
	user_id_input <- readline ("give user id: ")

	intra_times_input <- readline("Give time of day from 1 - 8: ")

	intra_days_input <- readline("Give what day is it from 1 -7: ")

	intra_months_input <- readline("Give what month is it from 1-12: ")

	location <- readline("Give the location: ")

	#	intra_locations_input <- dbGetQuery(con, paste("SELECT row_number FROM intra_cities, 
	#	                      (SELECT row_number() OVER(ORDER BY city), city FROM locations GROUP BY city) c WHERE c.city =", "'",location,"'"," LIMIT 1 ", sep=""))
	#	# if we write a citi that is not in the database we make the input 0
	#	if (dim (intra_locations_input) == 0) intra_locations_input = 0

	#location <- sample(1:100, 1, replace=TRUE)

	intra_temperature_input <- readline("Give the temperature: ")

	intra_weather_input <- readline("Give the type of weather: ")
	# Start the clock!
	ptm <- proc.time()  

	input = hash(user_id = user_id_input, intra_times = intra_times_input, intra_days = intra_days_input, 
	            intra_months = intra_months_input, intra_locations = intra_locations_input ,
	            intra_temperatures = intra_temperature_input, intra_weathers = intra_weather_input)


	output = hash(user_id = input$user_id, intra_times = c(0), intra_days = c(0), 
	              intra_months = c(0), intra_locations = c(0),
	              intra_temperatures = c(0), intra_weathers = c(0))  

	getNeighboursForAUser(input)

	# Stop the clock
	proc.time() - ptm

	output 



	 
	out_intra_times = output$intra_times 
	out_intra_days  = output$intra_days 
	out_intra_months = output$intra_months 
	out_intra_locations = output$intra_locations 
	out_intra_temperatures = output$intra_temperatures 

	output_matrix <- matrix(0, 5, 20000)
#	output_matrix[1, ] = out_intra_times

  
#   j = 1
# 	for (i in 1:5){
#     output_matrix[i, j] = 
# 	}

 

}



