# + --------------------------------------------------------------------------- +
# 基盤地図情報5mメッシュをGeoTiff形式に変換する
# Rでgrdファイルに書き出した後に, gdal_translateでgeotiffファイルに変換
# + --------------------------------------------------------------------------- +
libs <- c ("rvest", "rgdal", "raster", "dplyr", "tidyr", "readr", "purrr", "stringr", "yaml")
for (lib in libs[!sapply(libs, require, character.only=TRUE)]) {
    install.packages(lib, repos="https://cran.ism.ac.jp/", depend=TRUE)
    library (lib)
}
convert_dem5a_to_raster <- function (ipath, crs, na_value) {
    coverage  <-
        read_html(ipath, encoding="cp932") %>%
        html_nodes(xpath="//dem//coverage")
    boundedby <-
        coverage %>%
        html_nodes(xpath="//coverage//boundedby//envelope") %>%
        html_children %>%
        html_text %>%
        str_split(" ") %>%
        lapply(type.convert) %>%
        "names<-"(c("lower", "upper")) %>%
        lapply(setNames, c ("y", "x"))
    sp <-
        coverage %>%
        html_nodes(xpath="//coverage//coveragefunction//gridfunction//startpoint") %>%
        html_text %>%
        str_split(" ") %>%
        lapply(type.convert) %>%
        lapply(setNames, c ("x", "y")) %>%
        unlist
    tuplelist <-
        coverage %>%
        html_nodes(xpath="//coverage//rangeset//datablock//tuplelist") %>%
        html_text %>%
        read_csv(col_names=c("type", "val"), col_types="cd")
    gridenvelope_low <-
    	coverage %>%
    	html_nodes(xpath="//gridenvelope//low") %>%
    	html_text() %>%
    	str_split(" ", simplify = TRUE, n = 2) %>%
    	parse_integer() %>%
    	set_names(c("x", "y"))
    gridenvelope_high <-
    	coverage %>%
    	html_nodes(xpath="//gridenvelope//high") %>%
    	html_text() %>%
    	str_split(" ", simplify = TRUE, n = 2) %>%
    	parse_integer() %>%
    	set_names(c("x", "y"))
    mesh_size_x <- length(gridenvelope_low["x"]:gridenvelope_high["x"])
    mesh_size_y <- length(gridenvelope_low["y"]:gridenvelope_high["y"])


    # 値は北西方向と南東方向に省略されている可能性があるので
    # データサイズの調整が必要となる
    # ex) 海岸線, 河川, 未計測エリア
    vals <- tuplelist$val
    vals <- c (rep (na_value, length=sp["x"] + sp["y"] * mesh_size_x), vals)
    vals <- c (vals, rep (na_value, length=mesh_size_x * mesh_size_y - length(vals)))
    # 北西方向と南東方向のデータ省略ではなく
    # その間の欠測値については-9999.が記録されているので省略部分と合わせてNAに変換
    vals[(vals - na_value) < 1.] <- NA
    # ラスターオブジェクトの作成
    rst  <- raster(xmn=boundedby$lower["x"],
                   xmx=boundedby$upper["x"],
                   ymn=boundedby$lower["y"],
                   ymx=boundedby$upper["y"],
                   nrows=mesh_size_y,
                   ncols=mesh_size_x,
                   crs=crs,
                   vals=vals)
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
    for(e in 1:nrow(vmat)) cat(vmat[e,,drop=TRUE], "\n", file=con)
    close (con)
}
grd2tif <- function (i, o) {
    cmd <- sprintf("gdal_translate -a_srs EPSG:4612 %s %s", i, o)
    system(cmd)
}
