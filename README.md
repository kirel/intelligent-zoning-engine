# Back to school

Alle Daten herunterladen und vorverarbeiten via

		make all

## Schulen

http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_schulstand@senstadt&type=WFS&themeType=spatial 

Weitere Daten (allerdings nicht Maschinenlesbar) finden sich unter https://www.berlin.de/sen/bildung/schule/berliner-schulen/schulverzeichnis/

## Hauskoordinaten

http://fbarc.stadt-berlin.de/FIS_Broker_Atom/Hauskoordinaten/HKO_EPSG5650.zip

## Einzugsbereiche

Via https://daten.berlin.de/datensaetze/grundschuleinzugsbereiche-geometrien-berlin-2012-mit-demographischen-merkmalen

https://www.statistik-berlin-brandenburg.de/opendata/ESB2012_WGS84_EWR2012-12.zip

Und via Scraper:
https://www.berlin.de/sen/jugend/familie-und-kinder/kindertagesbetreuung/kitas/umkreis/

## LOR

Via

http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/de/download.shtml


## Flächennutzung

via http://fbinter.stadt-berlin.de/fb/index.jsp?loginkey=showShortInfo&mapId=wmsk_alkis@senstadt&Szenario=fbinter_jsc

## Einwohnerzahlen

http://daten.berlin.de/datensaetze/einwohnerinnen-und-einwohner-berlin-lor-planungsr%C3%A4umen-am-31122015

https://www.statistik-berlin-brandenburg.de/opendata/EWR201512E_Matrix.csv

Erklärung unter https://www.statistik-berlin-brandenburg.de/opendata/Beschreibung_EWR_Open_Data.pdf

Oder als Shape aber weniger aufgelöst:

http://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=re_einwohnerdichte2015@senstadt&type=WFS&themeType=spatial


## Soziale Daten

- http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/monitoring/index.shtml
		
Eventuell http://www.stadtentwicklung.berlin.de/planen/bevoelkerungsprognose/de/download/

## Graphhopper

Via https://github.com/sogorkis/dockerfile/tree/master/graphhopper

    wget http://download.geofabrik.de/europe/germany/berlin-latest.osm.pbf

    docker build . -t kirel/graphhopper
    docker run --rm -v $PWD/osm:/data -p 8990:8989 kirel/graphhopper /graphhopper/start.sh

Routing api https://github.com/graphhopper/graphhopper/blob/master/docs/web/api-doc.md

## OSRM

Via https://github.com/acroca/osrm-docker

    docker run -v /osrm-data --name osrm-data acroca/osrm-docker:latest echo "running data container..."
    
    docker run --restart=always --volumes-from osrm-data -p 5000:5000 cartography/osrm-backend-docker:latest osrm Berlin "http://download.geofabrik.de/europe/germany/berlin-latest.osm.pbf"
