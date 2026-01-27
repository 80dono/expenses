# Clean up directory before rendering site
setwd(Sys.getenv("HOME_DIRECTORY"))
unlink(c("_site", "site_libs", "expenses_files"), recursive = TRUE, force = TRUE)

rmarkdown::render_site()
# unlink(c("site_libs", "expenses_files"), recursive = TRUE, force = TRUE)
