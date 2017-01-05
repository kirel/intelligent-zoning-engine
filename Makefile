# *** Download data

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_schulstand@senstadt&type=WFS&themeType=spatial
download/re_schulstand.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_schulstand" fis:re_schulstand
schulen: download/re_schulstand.geojson

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=a_hauskoordinaten@senstadt&type=FEED
download/HKO_2015_EPSG5650.txt:
	wget http://fbarc.stadt-berlin.de/FIS_Broker_Atom/Hauskoordinaten/HKO_EPSG5650.zip -O download/HKO_EPSG5650.zip
	unzip -o -d data download/HKO_EPSG5650.zip
	rm download/HKO_EPSG5650.zip
	touch $@
adressen: download/HKO_2015_EPSG5650.txt

# http://daten.berlin.de/datensaetze/grundschuleinzugsbereiche-geometrien-berlin-2012-mit-demographischen-merkmalen
download/ESB2012_WGS84_EWR2012-12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/ESB2012_WGS84_EWR2012-12.zip -O download/ESB2012_WGS84_EWR2012-12.zip
	unzip -o -d download/ESB2012_WGS84_EWR2012-12 download/ESB2012_WGS84_EWR2012-12.zip
	rm download/ESB2012_WGS84_EWR2012-12.zip
	ogr2ogr -t_srs WGS84 -f geoJSON $@ download/ESB2012_WGS84_EWR2012-12/ESB2012_WGS84_EWR2012-12.shp
	rm -rf download/ESB2012_WGS84_EWR2012-12
einzugsbereiche: download/ESB2012_WGS84_EWR2012-12.geojson

# http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/de/download.shtml
download/LOR_SHP_EPSG_25833.geojson:
	wget http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/download/LOR_SHP_EPSG_25833.zip -O download/LOR_SHP_EPSG_25833.zip
	unzip -o -d download/LOR_SHP_EPSG_25833 download/LOR_SHP_EPSG_25833.zip
	rm download/LOR_SHP_EPSG_25833.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ download/LOR_SHP_EPSG_25833/Planungsraum.shp
	rm -rf download/LOR_SHP_EPSG_25833
LOR: download/LOR_SHP_EPSG_25833.geojson

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_alkis_tatsaechlichenutzungflaechen@senstadt&type=WFS&themeType=spatial
download/re_alkis_tatsaechlichenutzungflaechen.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_alkis_tatsaechlichenutzungflaechen" fis:re_alkis_tatsaechlichenutzungflaechen
flaechen: download/re_alkis_tatsaechlichenutzungflaechen.geojson

# http://daten.berlin.de/datensaetze/rbs-bl%C3%B6cke-dezember-2015
download/RBS_OD_BLK_2015_12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_BLK_2015_12.zip -O download/RBS_OD_BLK_2015_12.zip
	unzip -o -d download/RBS_OD_BLK_2015_12 download/RBS_OD_BLK_2015_12.zip
	rm download/RBS_OD_BLK_2015_12.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ download/RBS_OD_BLK_2015_12/RBS_OD_BLK_2015_12.shp
	rm -rf download/RBS_OD_BLK_2015_12

download/RBS_OD_LOR_2015_12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_LOR_2015_12.zip -O download/RBS_OD_LOR_2015_12.zip
	unzip -o -d download/RBS_OD_LOR_2015_12 download/RBS_OD_LOR_2015_12.zip
	rm download/RBS_OD_LOR_2015_12.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ download/RBS_OD_LOR_2015_12/RBS_OD_LOR_2015_12.shp
	rm -rf download/RBS_OD_LOR_2015_12

download/RBS_OD_BEZ_2015_12.geojson:
	wget https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_BEZ_2015_12.zip -O download/RBS_OD_BEZ_2015_12.zip
	unzip -o -d download/RBS_OD_BEZ_2015_12 download/RBS_OD_BEZ_2015_12.zip
	rm download/RBS_OD_BEZ_2015_12.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ download/RBS_OD_BEZ_2015_12/RBS_OD_BEZ_2015_12.shp
	rm -rf download/RBS_OD_BEZ_2015_12

RBS: download/RBS_OD_BLK_2015_12.geojson download/RBS_OD_LOR_2015_12.geojson download/RBS_OD_BEZ_2015_12.geojson

# http://daten.berlin.de/datensaetze/einwohnerinnen-und-einwohner-berlin-lor-planungsr%C3%A4umen-am-31122015
download/EWR201512E_Matrix.csv:
	wget https://www.statistik-berlin-brandenburg.de/opendata/EWR201512E_Matrix.csv -O $@

# http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_einwohnerdichte2015@senstadt&type=WFS&themeType=spatial
download/re_einwohnerdichte2015.geojson:
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ WFS:"http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_einwohnerdichte2015" fis:re_einwohnerdichte2015

einwohner: download/EWR201512E_Matrix.csv download/re_einwohnerdichte2015.geojson

# https://github.com/ini20/berliner-schulen
# TODO unused - remove?
download/Schuldaten2015.csv:
	wget https://raw.githubusercontent.com/ini20/berliner-schulen/master/data/Schuldaten2015.csv -O $@

download/anmeldezahlen.csv:
	wget https://docs.google.com/spreadsheet/ccc\?key\=1iAGbTdYcC55A6sDXpOnD3y-s8b4b6wJdSS-GkW_iIr0\&output\=csv -O $@
anmeldezahlen: download/anmeldezahlen.csv

# https://www.wahlen-berlin.de/Wahlen/Be2016/DL_BE_AH2016_Strukturdaten.xlsx
download/DL_BE_AH2016_Strukturdaten.xlsx:
	wget --user-agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36" https://www.wahlen-berlin.de/Wahlen/Be2016/DL_BE_AH2016_Strukturdaten.xlsx -O $@

# https://www.wahlen-berlin.de/wahlen/BE2016/Wahlkreiseinteil/wahlkreiseinteil.asp?sel1=1253&sel2=1045
download/RBS_OD_UWB_AGH2016.geojson:
	wget  --user-agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36" https://www.wahlen-berlin.de/Wahlen/Be2016/Wahlkreiseinteil/RBS_OD_UWB_AGH2016.zip?sel1=1253\&sel2=1045 -O RBS_OD_UWB_AGH2016.zip
	unzip -o -d download/RBS_OD_UWB_AGH2016 download/RBS_OD_UWB_AGH2016.zip
	rm download/RBS_OD_UWB_AGH2016.zip
	ogr2ogr -s_srs EPSG:25833 -t_srs WGS84 -f geoJSON $@ download/RBS_OD_UWB_AGH2016/UWB.shp
	rm -rf download/RBS_OD_UWB_AGH2016

sozio: download/DL_BE_AH2016_Strukturdaten.xlsx download/RBS_OD_UWB_AGH2016.geojson

download: schulen adressen einzugsbereiche LOR flaechen RBS einwohner anmeldezahlen sozio


# *** Process data

output/HKO_2015.geojson: download/HKO_2015_EPSG5650.txt HKO_2015.R
	rm -f $@
	Rscript HKO_2015.R

output/sampled_buildings.rds: output/HKO_2015.geojson download/re_alkis_tatsaechlichenutzungflaechen.geojson download/RBS_OD_BLK_2015_12.geojson sampled_buildings.R
	rm -f $@
	Rscript sampled_buildings.R

output/route_matrix.rds: output/sampled_buildings.rds download/re_schulstand.geojson route_matrix_sampled.R
	rm -f $@
	Rscript route_matrix_sampled.R

process: output/HKO_2015.geojson output/sampled_buildings.rds output/route_matrix.rds

optim.nb.html: optim.Rmd output/HKO_2015.geojson output/sampled_buildings.rds output/route_matrix.rds
	Rscript -e "rmarkdown::render('optim.Rmd')"
app_data: optim.nb.html


# *** Python

venv: .venv/bin/activate
.venv/bin/activate: requirements.txt
	test -d .venv || virtualenv .venv --python=python3
	.venv/bin/pip install -U pip
	test -e requirements.txt && .venv/bin/pip install -r requirements.txt || echo 'Warning: No requirements to install'
	touch .venv/bin/activate

app/data/communication.sqlite: app/communication.sql
	sqlite3 app/data/communication.sqlite < app/communication.sql

