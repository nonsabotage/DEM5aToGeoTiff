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
