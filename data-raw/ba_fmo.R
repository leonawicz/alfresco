library(alfresco)
library(dplyr)

b <- readAll(brick("firescarbrick_annual_observed_Statewide_lightning_1950_2013.tif"))
id <- 0:5
labels <- c("Unmanaged", "Limited", "Modified", "Critical", "Full", "Other")
r <- snapgrid::swfmo
idx <- purrr::map(id, ~which(r[] == .x))
f <- function(b, i, idx) purrr::map_dbl(idx, ~length(which(as.numeric(raster::extract(subset(b, i), .x)) > 0)))

x <- purrr::map(1:64, ~f(b, .x, idx))
x <- as_data_frame(do.call(rbind, x))
x <- cbind(1950:2013, x)
names(x) <- c("Year", labels)
x <- tbl_df(x)
x$Set <- "Observed"
x <- select(x, c(8, 1:7))
saveRDS(x, "/workspace/UA/mfleonawicz/ba_fmo.rds")
