source("./convert_dem5a_to_raster.r", encoding = "UTF-8")

config <- read_yaml("config.yaml")
target_dir <- purrr::pluck(config, "TARGET_DIR")
output_dir <- purrr::pluck(config, "OUTPUT_DIR")
parameters <- purrr::pluck(config, "RASTER_PARAMETER")
ipaths <-
    list.files(target_dir, full.names=TRUE, pattern="xml$") %>%
    keep( ~ str_detect(.x, "DEM5A"))
## xml to grd
for (ipath in ipaths) {
    rst <- convert_dem5a_to_raster(ipath, parameters)
    opath <- ipath %>% basename() %>% sub("xml$", "grd", .) %>% file.path(output_dir, .)
    writeRasterGrd(rst, opath, parameters)
}
## grd to tiff
# grds <- list.files(output_dir, full.names=TRUE, pattern="grd$")
# tifs <- sub ("grd$", "tif", grds)
# Map(grd2tif, grds, tifs)
# system ('python gdal_merge.py -o merged.tif ./out/*.tif')
# system ('gdalwarp -r bilinear -s_srs EPSG:4612 -t_srs EPSG:2450 merged.tif merged_2450.tif')
