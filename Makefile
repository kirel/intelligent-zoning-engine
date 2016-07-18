data/re_schulstand.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_schulstand" fis:re_schulstand
schulen: data/re_schulstand.geojson

data/HKO_2015_EPSG5650.txt:
	unzip -o -d data data/HKO_EPSG5650.zip
	rm data/HKO_EPSG5650.zip
	touch $@
adressen: data/HKO_2015_EPSG5650.txt

data/ESB2012_WGS84_EWR2012-12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/ESB2012_WGS84_EWR2012-12.zip -O data/ESB2012_WGS84_EWR2012-12.zip
	unzip -o -d data/ESB2012_WGS84_EWR2012-12 data/ESB2012_WGS84_EWR2012-12.zip
	rm data/ESB2012_WGS84_EWR2012-12.zip
	ogr2ogr -t_srs WGS84 -f geoJSON $@ data/ESB2012_WGS84_EWR2012-12/ESB2012_WGS84_EWR2012-12.shp
	rm -rf data/ESB2012_WGS84_EWR2012-12
einzugsbereiche: data/ESB2012_WGS84_EWR2012-12.geojson

data/LOR_SHP_EPSG_25833.geojson:
	wget http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/download/LOR_SHP_EPSG_25833.zip -O data/LOR_SHP_EPSG_25833.zip
	unzip -o -d data/LOR_SHP_EPSG_25833 data/LOR_SHP_EPSG_25833.zip
	rm data/LOR_SHP_EPSG_25833.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ data/LOR_SHP_EPSG_25833/Planungsraum.shp
	rm -rf data/LOR_SHP_EPSG_25833
LOR: data/LOR_SHP_EPSG_25833.geojson

data/re_alkis_tatsaechlichenutzungflaechen.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_alkis_tatsaechlichenutzungflaechen" fis:re_alkis_tatsaechlichenutzungflaechen
flaechen: data/re_alkis_tatsaechlichenutzungflaechen.geojson

data/EWR201512E_Matrix.csv:
	wget https://www.statistik-berlin-brandenburg.de/opendata/EWR201512E_Matrix.csv -O $@

data/re_einwohnerdichte2015.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_einwohnerdichte2015" fis:re_einwohnerdichte2015
einwohner: data/EWR201512E_Matrix.csv data/re_einwohnerdichte2015.geojson

# osm
osm/berlin-latest.osm.pbf:
	wget http://download.geofabrik.de/europe/germany/berlin-latest.osm.pbf -O $@

# all
all: schulen adressen einzugsbereiche LOR flaechen einwohner
