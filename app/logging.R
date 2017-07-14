flog.threshold(DEBUG)
OPTIMIZATION_LOG_FILE = Sys.getenv('OPTIMIZATION_LOG_FILE')
DEBUG_ENV = Sys.getenv('DEBUG') != ""
if (OPTIMIZATION_LOG_FILE != "") {
  flog.appender(appender.file('optimization.log'), name='optimization')
}
flog.debug('Logging setup complete')