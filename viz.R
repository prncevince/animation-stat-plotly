library(plotly)
library(data.table)
library(sf)
library(RColorBrewer)
library(rworldmap)

library(tibble)
library(rlist)

source('stats.R')
source('data.R')
source('ephemeris.R')
source('animation.R')

# plot data ----
d_tree <- treemap(d_col, 24, 1)

d_rect <- rect_plt(d_tree)
d_text <- text_plt(d_tree)

d_trk_day2$text_trk <- paste0(
  "Time: ", d_trk_day2$t_g, "\n",
  "Longitude: ", round(d_trk_day2$lon180, 3), "\n",
  "Latitude: ", round(d_trk_day2$lat, 3), "\n",
  "SEA of Sub Satellite Point: ", round(d_trk_day2$sea, 3), "\n"
)

# map base layer ----
# rworld <- st_as_sf(getMap(resolution = "low"))
# rworld_sm <- st_simplify(rworld, TRUE, dTolerance = 1) %>%
#  st_cast("MULTIPOLYGON")
#  add_sf(
#    data = rworld_sm, name = "map", hoverinfo = "none",
#    color = I("black"), fillcolor = "transparent"
#  ) %>%

d_sf_ne <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

#  # polygons colored by category type or stat
#  add_trace(
#    data = d_rect, frame = ~t_acc, type = "scatter", mode = "lines",
#    fill = "toself", line = list(color = "transparent"),
#    x = ~x_r, y = ~y_r, color = ~by
#  ) %>%
#  # text hover traces as center points of polygons - same color hover as polygon color
#  add_trace(
#    data = d_text, frame = ~frame, type = "scatter", mode = "markers",
#    text = ~text, hoverinfo = "text", x = ~x_t, y = ~y_t,
#    color = I("transparent"), hoverlabel = list(bgcolor = ~by) # `hoverlabel = list(bgcolor = ~stat)` when d_rect & d_text are one w/ the text 
#  ) %>%
#  add_annotations(
#    text = "By Type", showarrow = FALSE,
#    x = 1.0, xref = "paper", xanchor = "left",
#    y = 0.85, yref = "paper", yanchor = "bottom"
#  ) %>%
#  layout(
#    title = "Accumulation by By Type", 
#    xaxis = list(
#      ticks = "outside", tick0 = 0, dtick = 30, showticklabels = TRUE, 
#      showgrid = TRUE
#    ),
#    yaxis = list(
#      ticks = "outside", tick0 = 0, dtick = 30, showticklabels = TRUE, 
#      showgrid = TRUE
#    )
#  ) %>%

# just imagine below :) 
# `id` is a unique identifier for the polygon - needs to be created
d_plot <- cbind(d_rect, d_text)

# polygon base plot ----
p_rect_base <- plot_ly() %>%
  add_sf(
    name = "map", data = d_sf_ne[, 'geometry'],
    color = I("black"), fillcolor = "transparent", hoverinfo = "none"
  ) %>%
  # polygons colored by category type or stat
  add_polygons(
    data = d_plot %>% group_by(id),
    x = ~x_r, y = ~y_r, showlegend = T, frame = ~t_acc,
    fillcolor = ~stat, stroke = ~counts, hoveron = "fills", text = ~text
  ) %>%
  # text hover traces as center points of polygons - same color hover as polygon color
  add_trace(
    data = d_plot %>% group_by(id), frame = ~frame, type = "scatter", mode = "markers",
    text = ~text, hoverinfo = "text", x = ~x_t, y = ~y_t,
    color = I("transparent"), hoverlabel = list(bgcolor = ~stat)
  ) %>%
  add_trace(
    data = d_trk_day2, frame = ~t, type = "scatter", mode = "markers",
    text = ~text_trk, hoverinfo = "text", x = ~lon180, y = ~lat, color = ~sea
  ) %>%
  add_annotations(
    text = "By Type", showarrow = FALSE,
    x = 1.0, xref = "paper", xanchor = "left",
    y = 0.85, yref = "paper", yanchor = "bottom"
  ) %>%
  layout(
    title = "Accumulation by By Type", 
    xaxis = list(
      ticks = "outside", tick0 = 0, dtick = 30, showticklabels = TRUE, 
      showgrid = TRUE
    ),
    yaxis = list(
      ticks = "outside", tick0 = 0, dtick = 30, showticklabels = TRUE, 
      showgrid = TRUE
    )
  ) %>%
  animation_opts(
    transition = 0, frame = 2000, redraw = FALSE
  ) %>%
  # for debugging in browser
  htmlwidgets::onRender(
    "
      function(el) {
        el.on('plotly_hover', function(d) {
        console.log('Hover: ', d)
        console.log(el)
        })
      }
    "
  )
# could mess w/ easing = 'quad-out' as animation_opts param value

# ground track base plot ----
p_mark_plt <- plot_ly() %>%
  add_trace(
    data = d_trk_day2, frame = ~t, type = "scatter", mode = "markers",
    text = ~text_trk, hoverinfo = "text", x = ~lon180, y = ~lat, color = ~sea,
    colors = rev(brewer.pal(name = "PuOr", n = 11))
  )

# built plotly objects ----
tic()
p_rect_blt <- plotly_build(p_rect_base, registerFrames = T)
toc()
tic()
p_mark_blt <- plotly_build(p_mark_plt, registerFrames = T)
toc()

# after build steps ----
p_blt <- p_rect_blt

len_fs <- length(pl_blt$x$frames)
len_d  <- length(pl_blt$x$data) 

## add names to traces ----
## name NULL value non-unique trace `name` values accordingly
# Plotly does not know how to properly add names to the text info and ground track
# traces. To put our traces into correct order (next step), we need this unique identifier.
# Here we see that each frame has n traces
# unlist(lapply(p_blt$x$frames, function(f) length(f$data)))
p_blt$x$data <- lapply(p_blt$x$data, function(t) {
  if(is.null(t$name)) {
    if(identical(class(t), 'plotly_colorbar')) {
      t$name <- "colorbar"
    }
    if(identical(substr(t$text[1], 1, 3), 'WAC')) {
      t$name <- "info"
    }
  }
})
p_blt$x$frames <- lapply(
  p_blt$x$frames,
  function(f) {
    f$data <- lapply(f$data, function(t) {
      if (is.null(t$name)) {
        if (identical(substr(t$text[1], 1, 4), "Time")) {
          t$name <- "trk"
        } else {
          t$name <- "info"
        }
      }
      return(t)
    })
    return(f)
  }
)

## set visible ----
# description: when we set the `visible` attribute of our traces to "NULL",
# this makes ALL traces visible ALWAYS
# OTHERWISE - when the slider moves - the traces stay put
# the legend toggle of trace visibility works correctly too
pl_blt$x$frames <- lapply(pl_blt$x$frames, function(f) {
  f$datya <- lapply(f$data, function(f) {t$visible <- NULL; return(t)})
  return(f)
})

## order traces correctly ----
# This keeps the traces in the legend in order & correctly shows traces
# for each frame.
#
# Below is not in order
# `unique` orders by 1st occurrence
# order_t <- as.character(unique(d_rect$by))
# order_t <- unlist(lapply(p_blt$x$frames[[1]]$data, function(i) i$name))
by_cats <- as.character(unique(d_rect$by))
order_t <- c("by1", "by2", "by3", "info", "trk")
len_fs <- length(p_blt$x$frames)
for (i in 1:len_fs) {
  len_d <- length(p_blt$x$frames[[i]]$data)
  len_i <- as.list(1:len_d)
  for (j in 1:len_d) {
    l_i[[which(order_t == p_blt$x$frames[[i]]$data[[j]]$name)]] <- p_blt$x$frames[[i]]$data[[j]]
  }
  p_blt$x$frames[[i]]$data <- l_i
}

## delete incorrect data, add dummy data for legend, set correct frames, register traces in each frame ----
# This deletes data in all `frames[[i]]$data` list objects for frames whose data
# is not for the specified frame.
# We delete this data & add transparent dummy data so that the legend is still
# shown and the trace is registered. We make the trace `null` so that plotly 
# toggles the visibility correctly at each frame
# Also registers the frames properly, showing all traces in the legend at each frame
# Also registers traces in each frame.
# These indicies match the index of the traces in the `x$data` list object,
# so here we leave out the index of the base map trace (0)
for (i in 1:len_fs) {
  len_d <- length(p_blt$x$frames[[i]]$data)
  p_blt$x$frames[[i]]$traces <- 1:len_d
  for (j in 1:len_d) {
    if (identical(FALSE, p_blt$x$frames[[i]]$data[[j]]$visible)) {
      p_blt$x$frames[[i]]$data[[j]]$x <- c(0,0,1,1)
      p_blt$x$frames[[i]]$data[[j]]$y <- c(0,1,1,0)
      p_blt$x$frames[[i]]$data[[j]]$frame <- as.character(i-1)
    }
  }
}

## set visibility for trace toggling ----
p_blt$x$frames <- lapply(p_blt$x$frames, function(f) {
  f$data <- lapply(f$data, function(t) { t$visible <- NULL; return(t)})
  return(f)
})


## customize legend ----
# hide traces (map & info) - there is no attribute to add a legend title
# we add an annotation to do this in the beginning
p_blt$x$data[[1]]$showlegend <- FALSE
p_blt$x$layout$legend$yanchor <- "top"
p_blt$x$layout$legend$y <- 0.85

## customize colorbar & fix hover background color ----
# show colorbar for ground track, position & name colorbar & traces, add marker
# info from correctly built plot
pos_trk <- which(order_t == "trk")
pos_info <- which(order_t == "info")
bgcolors <- unlist(lapply(p_blt$x$frames[[1]]$data[1:length(by_cats)], function(t) t$fillcolor))
for (i in 1:len_fs) {
  p_blt$x$frames[[i]]$data[[pos_info]]$hoverlabel$bgcolor <- as.character(
    factor(
      x = p_blt$x$frames[[i]]$data[[pos_info]]$hoverlabel$bgcolor,
      levels = order_t[1:length(by_cats)], labels = bgcolors, ordered = TRUE
    )
  )
  p_blt$x$frames[[i]]$data[[pos_info]]$showlegend <- FALSE
  p_blt$x$frames[[i]]$data[[pos_trk]]$marker <- p_mark_blt$x$frames[[i]]$data[[1]]$marker
  p_blt$x$frames[[i]]$data[[pos_trk]]$marker$showscale <- TRUE
  p_blt$x$frames[[i]]$data[[pos_trk]]$marker$colorbar$yanchor <- "top"
  p_blt$x$frames[[i]]$data[[pos_trk]]$marker$colorbar$y <- 0.65
  p_blt$x$frames[[i]]$data[[pos_trk]]$marker$colorbar$len <- 0.5
  p_blt$x$frames[[i]]$data[[pos_trk]]$marker$colorbar$title <- "SEA"
}

## add data to register all frame traces ----
# Adds all traces from 1st frame to `data` (do not have to ad map trace to `frames[[1]]$data`)
# All frame traces need to be registered in `data` (see `firstFrame` & `registerFrames` in plotlyBuild.R)
p_blt$x$data <- p_blt$x$data[1]
p_blt$x$data <- append(
  p_blt$x$data, p_blt$x$frames[[1]]$data
)


## Layout Attributes ----

### Slider ----
p_blt$x$layout$sliders[[1]]$currentvalue$prefix <- "Time Block: "
p_blt$x$layout$sliders[[1]]$currentvalue$xanchor <- "center"
p_blt$x$layout$sliders[[1]]$currentvalue$font$color <- "#00D0FF"

p_blt$x$layout$sliders[[1]]$steps <- lapply(
  p_blt$x$layout$sliders[[1]]$steps,
  function(s) {
    s$label <- paste0(s$value, ":00", " - ", as.numeric(s$value)+1, ":00")
    return(s)
  }
)

# Save Widget :) 
library(htmlwidgets)
setwd("site")
saveWidget(
  as_widget(p_blt), file = "index.html", selfcontained = FALSE, libdir = "libs"
)
setwd("..")
