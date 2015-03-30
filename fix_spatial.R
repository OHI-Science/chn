# Since Github rendering GeoJSON file fine:
#   https://github.com/OHI-Science/chn/blob/draft/subcountry2014/spatial/regions_gcs.geojson
# gonna try recreating the JS file using:
#   from https://github.com/OHI-Science/ohicore/blob/dev/R/shp_to_geojson.R#L45-L49

js      = 'subcountry2014/spatial/regions_gcs.js'
geojson = 'subcountry2014/spatial/regions_gcs.geojson'

fw = file(js, 'wb')
cat('var regions = ', file=fw)
cat(readLines(geojson, n = -1), file=fw)
close(fw)
