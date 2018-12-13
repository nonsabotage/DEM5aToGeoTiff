# + --------------------------------------------------------------------------- +
# 基盤地図情報5mメッシュをGeoTiff形式に変換する
# Rでgrdファイルに書き出した後に, gdal_translateでgeotiffファイルに変換
# + --------------------------------------------------------------------------- +
libs <- c ("rvest", "rgdal", "raster", "dplyr", "tidyr", "readr", "purrr", "stringr", "yaml")
for (lib in libs[!sapply(libs, require, character.only=TRUE)]) {
    install.packages(lib, repos="https://cran.ism.ac.jp/", depend=TRUE)
    library (lib)
}
gsidem2raster <- function (ipath, crs, na_value) {
    coverage  <-
        read_html(ipath, encoding="cp932") %>%
        html_nodes(xpath="//dem//coverage")
    boundedby <-
        coverage %>%
        html_nodes(xpath="//coverage//boundedby//envelope") %>%
        html_children() %>%
        html_text() %>%
        str_split(" ") %>%
        lapply(type.convert) %>%
        set_names(c("lower", "upper")) %>%
        lapply(set_names, c ("y", "x"))
    sp <-
        coverage %>%
        html_nodes(xpath="//coverage//coveragefunction//gridfunction//startpoint") %>%
        html_text() %>%
        str_split(" ") %>%
        lapply(type.convert) %>%
        lapply(setNames, c ("x", "y")) %>%
        unlist()
    tuplelist <-
        coverage %>%
        html_nodes(xpath="//coverage//rangeset//datablock//tuplelist") %>%
        html_text() %>%
        read_csv(col_names=c("type", "val"), col_types="cd")
    gridenvelope <-
    	coverage %>%
    	html_nodes(xpath="//gridenvelope") %>%
    	html_children() %>%
    	html_text() %>%
    	str_split(" ") %>%
    	lapply(parse_integer) %>%
    	set_names(c("low", "high")) %>%
    	lapply(set_names, c("x", "y"))
    mesh_size_x <- length(gridenvelope$low["x"]:gridenvelope$high["x"])
    mesh_size_y <- length(gridenvelope$low["y"]:gridenvelope$high["y"])


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
                length = sp["x"] + sp["y"] * mesh_size_x),
            after = 0) %>%
        # 東南の省略に対する処理
        append(
            values = rep(
                x = na_value,
                length = mesh_size_x * mesh_size_y - length(.))) %>%
        # 中間の欠測値に対する処理
        replace(
            abs(. - na_value) < 1.,
            NA)

    # ----------------------------------------------------------------
    # ラスター
    # ----------------------------------------------------------------
    rst  <- raster(xmn = boundedby$lower["x"],
                   xmx = boundedby$upper["x"],
                   ymn = boundedby$lower["y"],
                   ymx = boundedby$upper["y"],
                   nrows = mesh_size_y,
                   ncols = mesh_size_x,
                   crs = crs,
                   vals = vals)
    rst
}
writeRasterGrd <- function (r, opath, p) {
	nr <- dim(r)[1]
	nc <- dim(r)[2]
    vmat <- matrix(values(r), nrow=nr, ncol=nc, byrow=TRUE)
    con  <- file(opath, open="wb")
    cat (sprintf ("ncols %s\n",        nc),      file=con)
    cat (sprintf ("nrows %s\n",        nr),      file=con)
    cat (sprintf ("xllcorner %s\n",    extent(r)@xmin),  file=con)
    cat (sprintf ("yllcorner %s\n",    extent(r)@ymin),  file=con)
    cat (sprintf ("cellsize %s\n",     res(r)[1]),       file=con)
    cat (sprintf ("NODATA_value %s\n", p$NA_VALUE),      file=con)
    for(e in 1:nr) cat(vmat[e,,drop=TRUE], "\n", file=con)
    close (con)
}
grd2tif <- function (i, o) {
    cmd <- sprintf("gdal_translate -a_srs EPSG:4612 %s %s", i, o)
    system(cmd)
}
