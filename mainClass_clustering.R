{
user_id_input <- readline ("give user id: ")

intra_times_input <- readline("Give time of day from 1 - 8: ")

intra_days_input <- readline("Give what day is it from 1 -7: ")

intra_months_input <- readline("Give what month is it from 1-12: ")

location <- readline("Give the location: ")

intra_locations_input <- dbGetQuery(con, paste("SELECT row_number FROM intra_cities, 
                      (SELECT row_number() OVER(ORDER BY city), city FROM locations GROUP BY city) c WHERE c.city =", "'",location,"'"," LIMIT 1 ", sep=""))
# if we write a citi that is not in the database we make the input 0
if (dim (intra_locations_input) == 0) intra_locations_input = 0


intra_temperature_input <- readline("Give the temperature: ")


input = hash(user_id = user_id_input, intra_times = intra_times_input, intra_days = intra_days_input, 
            intra_months = intra_months_input, intra_locations = intra_locations_input ,intra_temperatures = intra_temperature_input)


output = hash(user_id = input$user_id, intra_times = c(0), intra_days = c(0), 
              intra_months = c(0), intra_locations = c(0),intra_temperatures = c(0))  

getNeighboursForAUser(input)

output  
}