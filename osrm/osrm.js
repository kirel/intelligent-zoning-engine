OSRM = require("osrm")
var osrm = new OSRM("/osrm-data/Berlin.osrm");
var options = {
  coordinates: [
    [13.388860,52.517037],
    [13.428555,52.523219]
  ],
  overview: 'false',
  annotations: false
};
console.time("osrm");
for (i=0;i<100000;i++) {
  (function(i){
    osrm.route(options, function(err, response) {
      //console.log(response.routes[0]); // array of arrays, matrix in row-major order
      if (i===100000-1) console.timeEnd("osrm");
    });
  })(i)
}

//#714/s
