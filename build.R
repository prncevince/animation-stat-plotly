source('animation.R')
source('widgets.R')

tic()
pl_blt
toc()

file <- "public/index.html"

tic()
saveWidgetFix(widget = pl_blt, file = file)
toc()
