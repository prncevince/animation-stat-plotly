# data for plot ----

cols <- c("lon", "lat", "by", "stat", "day.night")

d_col <- d_data[, cols, with=F]

# regions
bin <- 10
breaks_lon <- seq(from = -180, to = 180, by = bin)
breaks_lat <- seq(from = -90, to = 90, by = bin)

d_col[, `:=`(
  bin_lon = cut(lon, breaks = breaks_lon),
  bin_lat = cut(lat, breaks = breaks_lat)
)]

d_col$t_start <- as.POSIXlt(
  paste(trimws(d_data$date), d_data$start, sep = " "),
  format = "%Y/%m/%d %H:%M:%OS", tz = "UTC"
)

d_col$t <- d_col$t_start$hour
