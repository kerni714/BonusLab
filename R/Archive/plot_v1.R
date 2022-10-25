library(dplyr)
library(ggplot2)
library(nycflights13)

d <- data(package = "nycflights13")
d

head(flights)
colnames(flights)

#View(planes)
#View(airports)
#View(weather)
#View(flights)

head(airports)
colnames(airports)

#- Add airport information to flights data
flights_1a <- dplyr::left_join(nycflights13::flights, airports,
                               by = c("origin" = "faa"))


head(flights_1a)
colnames(flights_1a)

distinct(select(flights_1a, name, origin, lon, lat))

#visualizes the mean delay of flights for different airports by longitude and latitude u


#- Calculate gain
#flights_1b <- mutate(flights_1a, gain = arr_delay-dep_delay)

#colnames(flights_1b)

#by_name <- group_by(flights_1a, name, lat, lon)
by_name <- group_by(flights_1a, dest, lat, lon)
#by_name <- group_by(flights_1a, name)
delay <- summarise(by_name,
                   count = n(),
                   dist_mean = mean(distance, na.rm = TRUE),
                   delay_mean = mean(arr_delay, na.rm = TRUE))

distinct(select(delay, dest, lon, lat))

distinct(select(delay, dest))

distinct(select(delay, lon))


#by_tailnum <- group_by(flights, tailnum)
#delay <- summarise(by_tailnum,
#                   count = n(),
#                   dist = mean(distance, na.rm = TRUE),
#                   delay = mean(arr_delay, na.rm = TRUE))

##############################################################
#- Check structure of missing values
#- Set data for plot
flights_plot <- flights_1a

#- Create plot
p1 <- ggplot(flights_plot)

p1

#visualize_airport_delays()
