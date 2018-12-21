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


    # ----------------------------------------------------------------
    # 値の省略と欠測値への対応
    # ----------------------------------------------------------------
    # 値の省略：
    #   値は北西方向と南東方向に省略されている可能性があるのでデータサイズの調整が必要
    #   ex) 海岸線, 河川, 未計測エリア
    # 欠測値：
    #   欠測値については-9999.が記録されている.
    #   省略部分と合わせてNAに変換する.
    #   -9999のままだとラスターオブジェクトの値域が-9999から始まってしまい濃淡が出ない.
    vals <-
        tuplelist$val %>%
        # 北西の省略に対する処理
        append(
            values = rep(
                x = na_value,
                length = sp["x"] + sp["y"] * grid_size_x),
            after = 0
        ) %>%
        # 東南の省略に対する処理
        append(
            values = rep(
                x = na_value,
                length = grid_size_x * grid_size_y - length(.))
        ) %>%
        # 欠測値の変換
        replace(
            abs(. - na_value) < 1.,
            NA
        )

    # ----------------------------------------------------------------
    # ラスター
    # ----------------------------------------------------------------
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
