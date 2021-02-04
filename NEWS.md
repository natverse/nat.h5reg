# nat.h5reg 0.4.2

* try to provide more information about java errors in `saalfeld_xform()`

# nat.h5reg 0.4.1

* `dr_h5reg()` to check java and h5reg installation
* substantial speed-up in transforming `neuronlist` objects with 
  `xformpoints.h5reg()` by memoising the default level check for h5reg objects
  (17148d5219ff1b1b99ab1dfb4f793eede175cf39).

# nat.h5reg 0.4.0

* Faster transforms by using rJava when available (#6)
* Don't warn when using a default registration level (#8)
* fix saalfeld_xform fails with NA points in inputs (#7)

# nat.h5reg 0.3.2

* add package overview documentation
* switch to natverse github organisation

# nat.h5reg 0.3.1

* Make hdf5r package an essential dependency (i.e. Import) because we
  need to look inside h5 files to run the transforms (#5)
* quote all paths to java command line tool (in case e.g. path to transform 
  file has a space in it) (#4)
* update to v0.0.1 release of java command line tool. This changes the behaviour
  of out of range points to affine + nearest non-rigid displacement
  (see https://github.com/saalfeldlab/transform-helpers/issues/3)
* depend on R >= 3.5.0 for rds test files 

# nat.h5reg 0.3.0

* fix a bug in inverse transforms by upgrading transform helper jar file
* export `is.h5reg()` and `read.h5reg.info()`
* add some tests and simple test h5 files

# nat.h5reg 0.2.0

* First public version
* Fully functional transforms using sample h5 format transforms from
  Stephan Saalfeld.
