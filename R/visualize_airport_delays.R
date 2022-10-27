visualize_airport_delays <- function(){

  #origin<-dest<-dep_delay<-arr_delay<- NULL

  #fligths <- magrittr::`%>%`(flights1, dplyr::select(origin, dest, dep_delay,
  # arr_delay))


  arr_delay <- count <- count_missing <- delay_mean <- desc <- dest <-
    distance <- lat <- lon <- perc_missing <- count_NA <- NULL

  flights_1a <- dplyr::left_join(nycflights13::flights, nycflights13::airports,
                                 by = c("dest" = "faa"))

  by_name <- dplyr::group_by(flights_1a, dest, lat, lon)

  delay <- dplyr::summarise(by_name,
                            count = dplyr::n(),
                            count_missing = sum(is.na(arr_delay)),
                            perc_missing = (count_missing/count)*100,
                            delay_mean = mean(arr_delay, na.rm = TRUE),
                            .groups = "keep")

  #- Remove missing observations

  values_missing <- dplyr::filter(delay,perc_missing>10)
  values_missing1 <- dplyr::arrange(values_missing,desc(perc_missing))

  #(p1 + p2)/p3

  cat("Note I: These observations have more than 10% missing values in the delay grouped
     by airport:","\n")
  print(values_missing1)
  cat("\n")


  delay$count_NA <- apply(delay[,c("lat","lon","delay_mean")], 1,
                          function(x) sum(is.na(x)))
  delay_NA <- dplyr::filter(delay,count_NA>0)

  cat("Note II: The following observations were removed due to missing values in either of
      latitude, longitude or mean delay:","\n")

  print(delay_NA)
  cat("\n")

  delay <- dplyr::filter(delay,count_NA==0)

  p1 <- ggplot2::ggplot(delay, ggplot2::aes(x = lat, y= delay_mean)) +
    ggplot2::geom_point(colour= "red") + ggplot2::geom_smooth() +
    ggplot2::ggtitle("Mean vs Latitude") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  p2 <- ggplot2::ggplot(delay, ggplot2::aes(x = lon, y= delay_mean)) +
    ggplot2::geom_point(colour= "green") + ggplot2::geom_smooth() +
    ggplot2::ggtitle("Mean vs Longitude") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  p3 <- ggplot2::ggplot(delay, ggplot2::aes(x = lat, y= lon, colour = delay_mean)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Latitude vs Longitude") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))



  #p1+p2+p3+ patchwork::plot_layout(ncol = 2)
  (p1 + p2)/p3



}
