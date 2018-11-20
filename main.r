source("./convert_dem5a_to_raster.r", encoding = "UTF-8")

config <- read_yaml("config.yaml")
target_dir <- purrr::pluck(config, "TARGET_DIR")
output_dir <- purrr::pluck(config, "OUTPUT_DIR")
parameters <- purrr::pluck(config, "RASTER_PARAMETER")
ipaths <-
    list.files(target_dir, full.names=TRUE, pattern="xml$") %>%
    keep( ~ str_detect(.x, "DEM5A"))

for (ipath in ipaths) {
    rst <- convert_dem5a_to_raster(ipath, crs = parameters$CRS, na_value = parameters$NA_VALUE)
    values(rst) <- ifelse(is.na(values(rst)), parameters$NA_VALUE, values(rst))
    opath <- ipath %>% basename() %>% sub("xml$", "grd", .) %>% file.path(output_dir, .)
    writeRasterGrd(rst, opath, parameters)
}

