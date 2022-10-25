library(dplyr)
library(ggplot2)
library(nycflights13)
library(interp)

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
                               by = c("dest" = "faa"))


head(flights_1a)
colnames(flights_1a)

distinct(select(flights_1a, name))

distinct(select(flights_1a, dest, lon, lat))



#visualizes the mean delay of flights for different airports by longitude and latitude u


#- Calculate gain
#flights_1b <- mutate(flights_1a, gain = arr_delay-dep_delay)

#colnames(flights_1b)

#by_name <- group_by(flights_1a, name, lat, lon)
by_name <- group_by(flights_1a, dest, lat, lon)
#by_name <- group_by(flights_1a, name,lat)
delay <- summarise(by_name,
                   count = n(),
                   count_missing = sum(is.na(arr_delay)),
                   perc_missing = (count_missing/count)*100,
                   dist_mean = mean(distance, na.rm = TRUE),
                   delay_mean = mean(arr_delay, na.rm = TRUE),
                   .groups = "keep")

delay

max(delay$perc_missing)

arrange(delay,desc(perc_missing))

length(which(is.na(delay$lat)==TRUE))

delay[is.na(delay$lat),]
delay[is.na(delay$lon),]


distinct(select(delay, lon))

distinct(select(delay, lat))

lat_range <- c(min(delay$lat,na.rm=TRUE),max(delay$lat,na.rm=TRUE))
lon_range <- c(min(delay$lon,na.rm=TRUE),max(delay$lon,na.rm=TRUE))

lat_range
lon_range

num_unique <- length(unique(delay$dest))
num_unique

d <- na.omit(delay)

ggplot(delay, aes(lat, delay_mean)) + geom_point()
ggplot(delay, aes(lon, delay_mean)) + geom_point()
delay_sub <- delay[6:10,]
delay_sub2 <- mutate(delay_sub,delay_mean <- abs(delay_mean), lon=abs(lon))

d_plot <- delay_sub2
d2 <- mutate(d,delay_mean = abs(delay_mean), lon=abs(lon))
d3 <- d2[,c("delay_mean","lon","lat")]
d_plot <- d3
d_plot
#contour
v <- ggplot(d_plot, aes(lat, lon, z = delay_mean))
v + geom_contour()

u <- interp(d3$lat, d3$lon, d3$delay_mean)
v <- ggplot(u, aes(lat, lon, z = delay_mean))
v + geom_contour()


d_plot_f <- faithfuld[1:100,]
v <- ggplot(d_plot_f, aes(waiting, eruptions, z = density))
v + geom_contour()

+
  #geom_point(aes(size = lon), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()


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
