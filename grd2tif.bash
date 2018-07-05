GRDS=$(ls out/*.grd)
for p in ${GRDS};
do
	t=${p/grd/tif}
	gdal_translate -a_srs EPSG:4612 $p $t
done;