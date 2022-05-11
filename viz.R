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

d_tree <- treemap(d_col, 24, 1)

d_rect <- rect_plt(d_tree)
d_text <- text_plt(d_tree)

d_samp <- d_rect

d_trk_day2$text_trk <- paste()

