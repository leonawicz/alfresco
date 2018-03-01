obs_fmo_ba <- readRDS("data-raw/ba_fmo.rds")
usethis::use_data(obs_fmo_ba)

x <- raster::shapefile("C:/github/DataExtraction/data/shapefiles/AFS_Fire_History_030118/FireAreaHistory.shp")
saveRDS(x, "C:/github/DataExtraction/data/afs_fire_history.rds")
