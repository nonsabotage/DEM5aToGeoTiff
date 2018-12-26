# + --------------------------------------------------------------------------- +
# 基盤地図情報5mメッシュをGeoTiff形式に変換する
# Rでgrdファイルに書き出した後に, gdal_translateでgeotiffファイルに変換
# + --------------------------------------------------------------------------- +
libs <- c ("xml2", "rgdal", "raster", "dplyr", "tidyr", "readr", "purrr", "stringr", "yaml")
for (lib in libs[!sapply(libs, require, character.only=TRUE)]) {
    install.packages(lib, repos="https://cran.ism.ac.jp/", depend=TRUE)
    library (lib)
}
gsidem2raster <- function (ipath, crs, na_value) {
    coverage <-
        ipath %>%
        read_xml() %>%
        xml_find_all("/d1:Dataset/d1:DEM/d1:coverage")
    boundedby <-
        coverage %>%
        xml_find_all("./gml:boundedBy/gml:Envelope") %>%
        xml_children() %>%
        xml_text() %>%
        str_split(" ") %>%
        lapply(type.convert) %>%
        set_names(c("lower", "upper")) %>%
        lapply(set_names, c ("y", "x"))
    sp <-
        coverage %>%
        xml_find_all("./gml:coverageFunction/gml:GridFunction/gml:startPoint") %>%
        xml_text() %>%
        str_split(" ") %>%
        lapply(type.convert) %>%
        lapply(setNames, c ("x", "y")) %>%
        unlist()
    tuplelist <-
        coverage %>%
        xml_find_all("./gml:rangeSet/gml:DataBlock/gml:tupleList") %>%
        xml_text() %>%
        read_csv(col_names=c("type", "val"), col_types="cd")
    gridenvelope <-
    	coverage %>%
        xml_find_all("./gml:gridDomain/gml:Grid/gml:limits/gml:GridEnvelope") %>%
    	xml_children() %>%
    	xml_text() %>%
    	str_split(" ") %>%
    	lapply(parse_integer) %>%
    	set_names(c("low", "high")) %>%
    	lapply(set_names, c("x", "y"))
    grid_size_x <- length(gridenvelope$low["x"]:gridenvelope$high["x"])
    grid_size_y <- length(gridenvelope$low["y"]:gridenvelope$high["y"])

    vals <- numeric(grid_size_x * grid_size_y) + na_value
    vals[sp["x"] + sp["y"] * grid_size_x + seq_along(tuplelist$val)] <- tuplelist$val
    vals[abs(vals - na_value) <= .Machine$double.eps]  <- NA

    rst  <- raster(xmn = boundedby$lower["x"],
                   xmx = boundedby$upper["x"],
                   ymn = boundedby$lower["y"],
                   ymx = boundedby$upper["y"],
                   nrows = grid_size_y,
                   ncols = grid_size_x,
                   crs = crs,
                   vals = vals)
    rst
}
