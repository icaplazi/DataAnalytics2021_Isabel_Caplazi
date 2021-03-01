library(dplyr)
library(nycflights13)

# general data information
head(flights) # first 6 rows
summary(flights) # summarizes flight data

# dplyr filter function
# gives alaska airline flights on october 4th
filter(flights,month == 10, day == 4, carrier =='AA')
head(filter(flights, month == 10, day == 4, carrier == 'AA')) # gives first 6 rows

# dplyr slice function
# gives portion of data by row selection
slice(flights, 1:15)
slice(flights, 23:30)

# dplyr arrange function
# rearranges data rows
arrange(flights,year,month,day, arr_time) # arranges in ascending order year, month, day, and arrival time

head(arrange(flights,year,month,day, desc(arr_time))) # arranges in descending order *arrival time*

# dplyr select function
select(flights,carrier) #shows carriers
head(select(flights, carrier, arr_time)) # shows first 6 rows of carrier and arrival time
head(select(flights, carrier, arr_time, day))

head(rename(flights, airline.carrier = carrier))

# shows distinct values in carrier column
# a.k.a shows all of the flight carriers in data
distinct(select(flights, carrier))

# dplyr mutate and transmutate functions
head(mutate(flights, MyNewColumn = arr_delay - dep_delay)) # creates new column off arrival time minus departure delay
head(transmute(flights, MyNewColumn = arr_delay - dep_delay)) # shows only new column

names(flights) # shows column names
head(mutate(flights, TravelVelocity = distance / hour))
head(transmutate(flights, TravelVelocity = distance / hour))
