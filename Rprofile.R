local(
  {
    r <- list()
    r$RSPM <- ""
    options(repos=r)
  }
)

options(download.file.method = "curl")
options(download.file.extra = "--ssl-no-revoke")