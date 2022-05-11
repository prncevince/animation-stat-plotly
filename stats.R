library(treemapify)
library(tibble)
library(dplyr)

#' @param d data.table object
#' \describe{
#'   \item{t}{time bin}
#'   \item{stat}{metric that you're using (showing area by)}
#'   \item{by}{category that you're using (splitting/coloring blocks by)}
#' }
#' @param len total time of animation
#' @param dt time delta
#' @return a data.table object
treemap <- function(d, len, dt) {
  d_tree <- c()
  for (i in 0:floor(len/dt)) {
    d_acc <- d[t <= i, ]
    d_acc[, t_acc := i]
    d_tree_dt <- c()
    for (j in d_acc[, sort(unique(bin_lon))]) {
      pos_lons <- which(d_acc$bin_lon == j)
      for (k in d_acc[, sort(unique(bin_lat))]) {
        pos_lats <- which(d_acc$bin_lat == k)
        pos_blocks <- pows_lats[pos_lats %in% pos_lons]
        if (length(pos_blocks) > 0) {
          d_block <- d_acc[pos_blocks, ]
          d_by <- aggregate(
            x = d_block[, .(stat)],
            by = d_block[, .(by, bin_lon, bin_lat, t_acc)],
            FUN = sum
          )
          # unique ID links things back to the correct row of the treemap
          d_by[, ID := 1:.N]
          d_block_tree <- treemapify(
            data = d_by, area = "stat", subgroup = "by"
          )
          d_block_tree[, stat := d_by[, stat[ID]]]
          d_tree_dt <- rbind(d_tree_dt, d_block_tree)
        }
      }
    }
    d_tree <- rbind(d_tree, d_tree_dt)
  }
  # all negatives are upper bounds
  # all positives (including 0) are lower bounds
  # position tiles properly
  d_tree[, lon_min := as.numeric(bin_lon)*10 - (10*xmin) - 180 - 0.2]
  d_tree[, lon_max := as.numeric(bin_lon)*10 - (10*xmax) - 180 + 0.2]
  d_tree[, lat_min := as.numeric(bin_lat)*10 - (10*ymin) - 90 - 0.2]
  d_tree[, lat_max := as.numeric(bin_lat)*10 - (10*ymax) - 90 + 0.2]
  # percentage of bin stat
  d_tree[, percentage := (xmax-xmin)/(ymax-ymin)]
  return(d_tree)
}

rect_plt <- function(d_tree) {
  d_tree[, bin_lon := as.character(bin_lon)]
  d_tree[, bin_lat := as.character(bin_lat)]
  l <- list()
  for (i in 1:nrow(d_tree)) {
    r <- d_tree[i,]
    l[[i]] <- rbind(
      c(r$lon_min, r$lat_min, r$t_acc, r$by, r$stat, r$bin_lon, r$bin_lat, i),
      c(r$lon_min, r$lat_max, r$t_acc, r$by, r$stat, r$bin_lon, r$bin_lat, i),
      c(r$lon_min, r$lat_min, r$t_acc, r$by, r$stat, r$bin_lon, r$bin_lat, i),
      c(r$lon_min, r$lat_min, r$t_acc, r$by, r$stat, r$bin_lon, r$bin_lat, i),
      c(NA, NA, r$t_acc, r$by, r$stat, r$bin_lon, r$bin_lat, i)
    )
  }
  d <- rbindlist(l)
  names(d) <- c("x_r", "y_r", "t_acc", "by", "stat", "bin_lon", "bin_lat", "split")
  d[, x_r := as.numeric(x_r)]
  d[, y_r := as.numeric(y_r)]
  d[, t_acc := as.numeric(t_acc)]
  d[, stat := as.numeric(stat)]
  return(d)
}

text_plt <- function(d_tree) {
  d_text <- data.table(
    x_t = d_tree[, (lon_max+lon_min)/2],
    y_t = d_tree[, (lat_max+lat_min)/2],
    frame = d_tree$t_acc,
    by = d_tree$by,
    text = paste0(
      "Stat Accumulated - Num: ", d$stat, "\n",
      "Stat Accumulated - % of Bin: ", round(100*d_tree$percentage,1), "%", "\n",
      "By: ", d_tree$by, "\n",
      "Latitude Bin: ", d_tree$bin_lat, "\n",
      "Longitude Bin: ", d_tree$bin_lon, "\n"
    )
  )
  return(d_text)
}

