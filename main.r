source("./gsidem2raster.r", encoding = "UTF-8")
source("./util.r", encoding = "UTF-8")


config <- read_yaml("config.yaml")
target_dir <- purrr::pluck(config, "TARGET_DIR")
output_dir <- purrr::pluck(config, "OUTPUT_DIR")
parameters <- purrr::pluck(config, "RASTER_PARAMETER")
ipaths <-
	target_dir %>%
    list.files(full.names=TRUE, pattern="xml$") %>%
    keep( ~ str_detect(.x, "DEM5A"))

for (ipath in ipaths) {
    rst <- gsidem2raster(ipath, crs = parameters$CRS, na_value = parameters$NA_VALUE)
    values(rst) <- ifelse(is.na(values(rst)), parameters$NA_VALUE, values(rst))
    opath <- ipath %>% basename() %>% sub("xml$", "grd", .) %>% file.path(output_dir, .)
    writeRasterGrd(rst, opath, parameters)
}



library(fs)
d5a <- dir_ls("FG-GML-5638-04-DEM5A/", glob = "*.xml$")
d5b <- dir_ls("FG-GML-5338-35-DEM5B/", glob = "*.xml$")
d10a <- dir_ls("FG-GML-4931-20-DEM10A/", glob = "*.xml$")
d10b <- dir_ls("FG-GML-5338-56-DEM10B/", glob = "*.xml$")


layout(matrix(c(1:4), nrow=2, byrow =TRUE))
plot(gsidem2raster(d5a[1], crs = parameters$CRS, na_value = parameters$NA_VALUE))
plot(gsidem2raster(d5b[1], crs = parameters$CRS, na_value = parameters$NA_VALUE))
plot(gsidem2raster(d10a[1], crs = parameters$CRS, na_value = parameters$NA_VALUE))
plot(gsidem2raster(d10b[1], crs = parameters$CRS, na_value = parameters$NA_VALUE))




