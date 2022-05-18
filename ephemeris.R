library(XML)
library(astrolibR)
library(oce)
library(data.table)

# ground track ----
xml_eph <- xmlParse(file = 'data/data.xml')

d_j2 <- data.table(
  t_j2000 = 
    sapply(xml_eph['//ns3:epoch/*[1]'], as, "numeric") +
    sapply(xml_eph['//ns3:epoch/*[2]'], as, "numeric") * 1e-9,
  x = sapply(xml_eph['//ns3:vp_j2/ns4:x'], as, "numeric"),
  y = sapply(xml_eph['//ns3:vp_j2/ns4:y'], as, "numeric"),
  z = sapply(xml_eph['//ns3:vp_j2/ns4:z'], as, "numeric"),
  v_x = sapply(xml_eph['//ns3:vp_j2/ns5:vx'], as, "numeric"),
  v_y = sapply(xml_eph['//ns3:vp_j2/ns5:vy'], as, "numeric"),
  v_z = sapply(xml_eph['//ns3:vp_j2/ns5:vz'], as, "numeric")
)

# ephemeris to Gregorian Spherical Coords ----
d_j2[, t_g := as.POSIXlt.numeric(t_j200, tz = "UTC", origin = "2000-01-01 12:00:00")]
# Julian Day
# J200 is JD 2451545.0 at 2000-01-01 12:00:00 UTC
t_jd <- 2451545.0 + d_j2$t_j200/(60*60*24)

# Modified Julian Day - not ran ---- 
# Origin at c(1858, 11, 16.5) -- 16-NOV-1858 12:00 UT
# t_jd <- c()
# for (i in 1:length(d_j2$t_g)) {
#   t_jd[i] <- juldate(
#     c(
#       d_j2$t_g[i]$year+1900, d_j2$t_g[i]$mon+1, d_j2$t_g[i]$mday,
#       d_j2$t_g[i]$hour, d_j2$t_g[i]$min, d_j2$t_g[i]$sec
#     )
#   )
# }
# Julian Day from Modified Julian Day
# t_jd <- t_jd + 2400000.0

d_gsc <- eci2geo(
  data.matrix(d_j2[,-c(1,8)]/1000, t_jd)
)
d_gsc <- data.table(
  t_jd, array(d_gsc, dim = c(dim(d_j2)[1], 3))
)
colnames(d_gsc) <- c("t_jd", "lat", "lon", "alt")
# add Gregorian days
d_gsc$t_g <- d_j2$t_g
# longitude -180 to 180
d_gsc$lon180 <- ifelse(d_gsc$lon > 180, d_gsc$lon-360, d_gsc$lon)

# Ground Track ----
col_trk <- c('t_g', 'lon180', 'lat')
d_trk <- d_gsc[, col_trk, with=T]
# functionalize & change accordingly ;) 
d_trk$t <- d_trk$t_g$hour

## Sun Elevation Angle of sub-satellite point ----
sun_ang <- sunAngle(
  t = d_trk$t_g, longitude = d_trk$lon180, latitude = d_trk$lat
)
d_trk$sea <- sun_ang$altitude

## Day of ----
findday <- function(t_days, day) {
  return(
    which(as.numeric(as.factor(round(d_gsc$t_jd, 0)) == day))
  )
}
# `day` is the unique 24-hour day from the start of the ephemeris (a day starts and ends at 12 midnight)
# this is a check to make sure that the ephemeris matches the date of the data
day <- c(2)
day2 <- findday(d_gsc$t_jd, day)
d_trk_day2 <- d_trk[day2, ]


