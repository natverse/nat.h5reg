#' Wrapper object for one or more HDF5 encoded transforms
#'
#' @description Wrapping paths to one or more h5 registration files on disk in
#'   an \code{antsreg} object means that they can be used by
#'   \code{nat::\link{xform}} and inside \code{nat::\link{reglist}} objects.
#'
#' @details h5 registrations contains both forward and inverse deformation
#'   fields along with the corresponding affine transforms wrapped in a single
#'   HDF5 file. They are therefore naively invertible just by picking out the
#'   correct registration field.
#'
#'   For historical reasons (based on the convention of CMTK), when
#'   \code{swap=FALSE}, we assume we are trying to specify a registration in the
#'   direction required for image transformation. This is the opposite of the
#'   direction required for point transformation, which is the convention for h5
#'   registration files. We therefore set \code{swap=TRUE} when it is not
#'   specified.
#'
#'   If multiple registrations are specified, they are given in order from
#'   floating to template brain, reading from left to right. See
#'   \code{nat::\link{xform}} and \code{nat::\link{reglist}} for further
#'   details.
#'
#' @param ... Path to one or more HDF5 encoded registrations.
#' @param swap When \code{TRUE} implies that the registration specifies the
#'   direction required for transforming points. When \code{FALSE} implies the
#'   inverse. The default implied by \code{swap=NULL} is \code{swap=TRUE} for
#'   all registrations.
#' @export
#' @return A character vector with additional class \code{antsreg}.
#' @examples
#' \dontrun{
#' # define forward and inverse registrations
#' inv=antsreg("JRC2018F_FAFB/JRC2018F_FAFB1InverseWarp_down.nii.gz",
#'     "JRC2018F_FAFB/JRC2018F_FAFB0GenericAffine.mat", swap=c(FALSE,TRUE))
#' fwd=antsreg("JRC2018F_FAFB/JRC2018F_FAFB0GenericAffine.mat",
#'     "JRC2018F_FAFB/JRC2018F_FAFB1Warp_down.nii.gz", swap=c(TRUE,TRUE))
#'
#' # position of DA1 glomerulus in FAFB
#' da1glomr.fafb <- cbind(429316, 217924, 42960)
#' da1glomr.fafbum=da1glomr.fafb/1e3
#' # map position from FAFB to JRC2018
#' res <- xform(da1glomr.fafbum, inv)
#' # and back again
#' res2 <- xform(res, fwd)
#'
#' # print out locations
#' da1glomr.fafbum
#' res
#' res2
#' }
#' @family antsreg

h5reg <- function(..., swap=NULL) {
  x <- path.expand(as.character(list(...)))
  if(!all(file.exists(x))) stop("... must point to files on disk!")
  swap <- if(is.null(swap)) rep(T, length(x)) else swap
  if(length(swap)!=length(x)) stop("swap must have same length as x!")
  attr(x,'swap') <- swap
  class(x)='h5reg'
  x
}

xformpoints.h5reg <- function(reg, points, ...) {
    if(ncol(points)!=3L)
      stop("xformpoints.h5reg only supports 3 dimensions!")

    swapped=attr(reg, 'swap')
    if(is.null(swapped)) swapped=rep(TRUE, length(reg))

    saalfeld_xform(points, reg, inverse = !swapped)
}

# Saalfeld/Bogovic HDF5 format transforms are defined with
# forward being the direction that transforms
saalfeld_xform <- function(points, reg, inverse=FALSE) {
  pointsfile=tempfile(fileext=".txt")
  on.exit(unlink(pointsfile))
  write.table(points, file=pointsfile, row.names=FALSE, col.names=FALSE)

  outfile=tempfile()
  on.exit(unlink(outfile), add=TRUE)

  jarfile <- system.file("transform-helpers-0.0.1-SNAPSHOT-shaded.jar", package = 'nat.ants')

  args = c('-jar',
           jarfile,
           "--transform",
           reg,
           "--coordinates",
           pointsfile)

  if(inverse) args=c(args, "--inverse")
  rval = system2(
    "java",
    args=args,
    stdout = outfile,
    stderr = FALSE
  )

  if(rval!=0) stop("Error running saalfeld xform!")
  outpoints <- read.table(outfile,
                        col.names=c('X', 'Y', 'Z', 'Failed'), row.names=NULL,
                        colClasses=c(rep('numeric', 3), 'factor'), fill=TRUE)
  pointst <- data.matrix(outpoints[,1:3])
  pointst
}

# read information about transforms

read.h5reg.info <- function(x, read.data=FALSE) {
  h5=hdf5r::H5File$new(x, mode = 'r')
  myinfo <- function(field) {
    res=list()
    rawres=field$get_space()$get_simple_extent_dims()
    res$ndim=rawres$rank
    res$dims=rawres$dims
    res=c(res, hdf5r::h5attributes(field))
  }
  res=list()
  list(dfield=myinfo(h5[['dfield']]), invdfield=myinfo(h5[['invdfield']]))
}

is.hdf5 <- function(f = NULL, bytes = NULL) {
  # in theory hdf5r::is_hdf5 should do the trick, but this interface
  # allows us to check bytes which will be useful

  hdf5.magic = as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a))

  magic = try({
    f <- file(f, 'rb')
    magic <- readBin(f, what = raw(), n = length(hdf5.magic))
    close(f)
    magic
  },
  silent = TRUE)

  isTRUE(
    !inherits(magic, 'try-error') &&
      length(magic) == length(hdf5.magic) &&
      all(magic == hdf5.magic)
  )
}
