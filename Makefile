# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_schulstand@senstadt&type=WFS&themeType=spatial
data/re_schulstand.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_schulstand" fis:re_schulstand
schulen: data/re_schulstand.geojson

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=a_hauskoordinaten@senstadt&type=FEED
data/HKO_2015_EPSG5650.txt:
	wget http://fbarc.stadt-berlin.de/FIS_Broker_Atom/Hauskoordinaten/HKO_EPSG5650.zip -O data/HKO_EPSG5650.zip
	unzip -o -d data data/HKO_EPSG5650.zip
	rm data/HKO_EPSG5650.zip
	touch $@
adressen: data/HKO_2015_EPSG5650.txt

data/HKO_2015.geojson: data/HKO_2015_EPSG5650.txt HKO_2015.R
	rm -f $@
	Rscript HKO_2015.R

# http://daten.berlin.de/datensaetze/grundschuleinzugsbereiche-geometrien-berlin-2012-mit-demographischen-merkmalen
data/ESB2012_WGS84_EWR2012-12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/ESB2012_WGS84_EWR2012-12.zip -O data/ESB2012_WGS84_EWR2012-12.zip
	unzip -o -d data/ESB2012_WGS84_EWR2012-12 data/ESB2012_WGS84_EWR2012-12.zip
	rm data/ESB2012_WGS84_EWR2012-12.zip
	ogr2ogr -t_srs WGS84 -f geoJSON $@ data/ESB2012_WGS84_EWR2012-12/ESB2012_WGS84_EWR2012-12.shp
	rm -rf data/ESB2012_WGS84_EWR2012-12
einzugsbereiche: data/ESB2012_WGS84_EWR2012-12.geojson

# http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/de/download.shtml
data/LOR_SHP_EPSG_25833.geojson:
	wget http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/download/LOR_SHP_EPSG_25833.zip -O data/LOR_SHP_EPSG_25833.zip
	unzip -o -d data/LOR_SHP_EPSG_25833 data/LOR_SHP_EPSG_25833.zip
	rm data/LOR_SHP_EPSG_25833.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ data/LOR_SHP_EPSG_25833/Planungsraum.shp
	rm -rf data/LOR_SHP_EPSG_25833
LOR: data/LOR_SHP_EPSG_25833.geojson

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_alkis_tatsaechlichenutzungflaechen@senstadt&type=WFS&themeType=spatial
data/re_alkis_tatsaechlichenutzungflaechen.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_alkis_tatsaechlichenutzungflaechen" fis:re_alkis_tatsaechlichenutzungflaechen
flaechen: data/re_alkis_tatsaechlichenutzungflaechen.geojson

# http://daten.berlin.de/datensaetze/einwohnerinnen-und-einwohner-berlin-lor-planungsr%C3%A4umen-am-31122015
data/EWR201512E_Matrix.csv:
	wget https://www.statistik-berlin-brandenburg.de/opendata/EWR201512E_Matrix.csv -O $@

# http://daten.berlin.de/datensaetze/rbs-bl%C3%B6cke-dezember-2015
data/RBS_OD_BLK_2015_12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_BLK_2015_12.zip -O data/RBS_OD_BLK_2015_12.zip
	unzip -o -d data/RBS_OD_BLK_2015_12 data/RBS_OD_BLK_2015_12.zip
	rm data/RBS_OD_BLK_2015_12.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ data/RBS_OD_BLK_2015_12/RBS_OD_BLK_2015_12.shp
	rm -rf data/RBS_OD_BLK_2015_12

data/RBS_OD_BEZ_2015_12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_BEZ_2015_12.zip -O data/RBS_OD_BEZ_2015_12.zip
	unzip -o -d data/RBS_OD_BEZ_2015_12 data/RBS_OD_BEZ_2015_12.zip
	rm data/RBS_OD_BEZ_2015_12.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ data/RBS_OD_BEZ_2015_12/RBS_OD_BEZ_2015_12.shp
	rm -rf data/RBS_OD_BEZ_2015_12

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_einwohnerdichte2015@senstadt&type=WFS&themeType=spatial
data/re_einwohnerdichte2015.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_einwohnerdichte2015" fis:re_einwohnerdichte2015
einwohner: data/EWR201512E_Matrix.csv data/re_einwohnerdichte2015.geojson

# https://github.com/ini20/berliner-schulen
data/Schuldaten2015.csv:
	wget https://raw.githubusercontent.com/ini20/berliner-schulen/master/data/Schuldaten2015.csv -O $@

# osm
osm/berlin-latest.osm.pbf:
	wget http://download.geofabrik.de/europe/germany/berlin-latest.osm.pbf -O $@

data/routen.geojson: data/umkreissuche.csv data/re_schulstand.geojson data/HKO_2015.geojson routen.R
	rm -f $@
	Rscript routen.R

data/routen_matrix.csv: data/HKO_2015.geojson data/re_schulstand.geojson routen_matrix.R
	rm -f $@
	Rscript routen_matrix.R

# all
all: schulen adressen einzugsbereiche LOR flaechen einwohner
