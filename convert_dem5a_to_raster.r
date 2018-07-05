# + --------------------------------------------------------------------------- +
# 基盤地図情報5mメッシュをGeoTiff形式に変換する
# Rでgrdファイルに書き出した後に, gdal_translateでgeotiffファイルに変換
# + --------------------------------------------------------------------------- +
libs <- c ("rvest", "rgdal", "raster", "dplyr", "tidyr", "readr", "purrr", "stringr", "yaml")
for (lib in libs[!sapply(libs, require, character.only=TRUE)]) {
    install.packages(lib, repos="https://cran.ism.ac.jp/", depend=TRUE)
    library (lib)
}
convert_dem5a_to_raster <- function (ipath, p) {
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
    vals <- tuplelist$val
    vals <- c (rep (p$NA_VALUE, length=sp["x"] + sp["y"] * p$MESH_SIZE_X), vals)
    vals <- c (vals, rep (p$NA_VALUE, length=p$MESH_SIZE_X * p$MESH_SIZE_Y - length(vals)))
    rst  <- raster(xmn=boundedby$lower["x"],
                   xmx=boundedby$upper["x"],
                   ymn=boundedby$lower["y"],
                   ymx=boundedby$upper["y"],
                   nrows=p$MESH_SIZE_Y,
                   ncols=p$MESH_SIZE_X,
                   crs=p$CRS)
    values(rst) <- vals
    rst[abs(rst - p$NA_VALUE) <= 1.] <- p$NA_VALUE
    rst
}
writeRasterGrd <- function (r, opath, p) {
    vmat <- matrix(values(r), nrow=p$MESH_SIZE_Y, ncol=p$MESH_SIZE_X, byrow=TRUE)
    con  <- file(opath, open="wb")
    cat (sprintf ("ncols %s\n",        ncol(vmat)),      file=con)
    cat (sprintf ("nrows %s\n",        nrow(vmat)),      file=con)
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
