file <- hdf5r::H5File$new('inst/samples/simple.h5', mode='w')
file[["testdataset"]] <- 1:10
file$close_all()

file <- hdf5r::H5File$new('inst/samples/simpledummyh5reg.h5', mode='w')
file[["dfield"]] <- 1:10
file[["invdfield"]] <- 1:10
file$close_all()

file <- hdf5r::H5File$new('inst/samples/complexdummyh5reg.h5', mode='w')
file$create_group("0")
file[["0/dfield"]] <- 1:10
file[["0/invdfield"]] <- 1:10
file$create_group("1")
file[["1/dfield"]] <- 1:10
file[["1/invdfield"]] <- 1:10
file$close_all()
