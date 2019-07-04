# nat.h5reg (development version)

* Make hdf5r package an essential dependency (i.e. Import) because we
  need to look inside h5 files to run the transforms (#5)
* quote all paths to java command line tool (in case e.g. path to transform 
  file has a space in it) (#4)

# nat.h5reg 0.3.0

* fix a bug in inverse transforms by upgrading transform helper jar file
* export `is.h5reg()` and `read.h5reg.info()`
* add some tests and simple test h5 files

# nat.h5reg 0.2.0

* First public version
* Fully functional transforms using sample h5 format transforms from
  Stephan Saalfeld.
