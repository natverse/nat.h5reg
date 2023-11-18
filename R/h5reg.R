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
#' @return A character vector with additional class \code{h5reg}.
#' @seealso \code{\link{h5reg-utils}}
#' @examples
#' \dontrun{
#' # We will use sample Kenyon Cells in FCWB (FlyCircuit) space
#' library(nat)
#' head(kcs20)
#'
#' # swap=FALSE, so this will map points onto JRC2018F
#' kcs20.jrc2018 = xform(kcs20,
#'                       reg = h5reg('JRC2018F_FCWB_transform_quant16.h5', swap=FALSE)
#' )
#'
#' # map back again (round trip test)
#' kcs20.rt = xform(
#'   kcs20.jrc2018,
#'   reg = h5reg('JRC2018F_FCWB_transform_quant16.h5')
#' )
#'
#' plot3d(kcs20.jrc2018, col='green')
#'
#' clear3d()
#' plot3d(kcs20, col='red')
#' plot3d(kcs20.rt, col='blue')
#'
#' diffs=xyzmatrix(kcs20)-xyzmatrix(kcs20.rt)
#' str(diffs)
#' plot(as.data.frame(diffs))
#' }
#' @family h5reg
h5reg <- function(..., swap=NULL) {
  x <- path.expand(as.character(list(...)))
  if(!all(file.exists(x))) stop("... must point to files on disk!")
  swap <- if(is.null(swap)) rep(T, length(x)) else swap
  if(length(swap)!=length(x)) stop("swap must have same length as x!")
  attr(x,'swap') <- swap
  class(x)='h5reg'
  x
}


#' Read information about h5reg transformations from file
#'
#' @param x path to a \code{.h5} file
#' @param ... Currently ignored
#'
#' @return a named list describing the contents of an \code{h5reg} file.
#' @export
#'
#' @name h5reg-utils
#' @aliases read.h5reg.info
#' @seealso \code{\link{h5reg}}
#' @examples
#' \donttest{
#' h5f <- system.file('samples/complexdummyh5reg.h5', package = 'nat.h5reg')
#' read.h5reg.info(h5f)
#' is.h5reg(h5f)
#' }
read.h5reg.info <- function(x, ...) {
  if(!is.hdf5(x))
    stop("This is not an HDF5 format file!")
  h5=hdf5r::H5File$new(x, mode = 'r')
  on.exit(h5$close_all())
  myinfo <- function(field) {
    res=list()
    rawres=field$get_space()$get_simple_extent_dims()
    res$ndim=rawres$rank
    res$dims=rawres$dims
    res$nbytes=field$get_storage_size()
    res=c(res, hdf5r::h5attributes(field))
  }

  myinfo2 <- function(x) {
    res <- list(dfield=myinfo(x[['dfield']]), invdfield=myinfo(x[['invdfield']]))
    res
  }
  h5_listing <- h5$ls()

  if(all(c("dfield","invdfield") %in% h5_listing$name)) {
    # single level h5
    res <- myinfo2(h5)
    return(res)
  }

  h5_listing$int <- suppressWarnings(as.integer(h5_listing$name))
  good_rows <- is.finite(h5_listing$int) & h5_listing$int %in% 0:20 & h5_listing$obj_type=='H5I_GROUP'
  if(!any(good_rows))
    stop("This does not look like an h5reg file!")
  sapply(h5_listing$name[good_rows], function(n) myinfo2(h5[[n]]), simplify = F)
}

#' @importFrom memoise memoise
read.h5reg.info.memo <- memoise::memoise(read.h5reg.info)


#' @description \code{is.h5reg} checks if a file (or bytes in memory) looks like
#'   \code{h5reg} file.
#'
#' @param f Path to a file on disk
#' @param bytes A set of bytes that are at least as big as the HDF5 magic value.
#' @details if \code{bytes} is passed a raw byte array, then \code{is.h5reg} can
#'   do a very quick check to ensure that it is actually an HDF5 file, before
#'   inspect the contents of the file itself in more detail.
#'
#' @return Logical indicating whether the file is an HDF5 encoded registration
#'   file ('code{h5reg})
#' @export
#'
#' @rdname h5reg-utils
is.h5reg <- function(f = NULL, bytes = NULL) {
  ishdf5 = is.hdf5(f, bytes)
  if (!ishdf5)
    return(FALSE)
  if (is.null(f))
    stop("You must supply the filename as well as some bytes!")
  res = try(read.h5reg.info(f), silent = TRUE)
  ! inherits(res, 'try-error')
}

is.hdf5 <- function(f = NULL, bytes = NULL) {
  # in theory hdf5r::is_hdf5 should do the trick, but this interface
  # allows us to check bytes which will be useful

  hdf5.magic = as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a))

  magic = if (!is.null(bytes))
    bytes
  else
    try({
      readBin(f, what = raw(), n = length(hdf5.magic))
    }, silent = TRUE
    )

  isTRUE(
    !inherits(magic, 'try-error') &&
      length(magic) == length(hdf5.magic) &&
      all(magic == hdf5.magic)
  )
}

default_h5_level <- function(x, cache=TRUE) {
  i <- if(cache) read.h5reg.info.memo(x) else read.h5reg.info(x)
  if('dfield' %in% names(i)) {
    # we have a file that is not
    NA_integer_
  } else {
    ints=suppressWarnings(as.integer(names(i)))
    levels=ints[is.finite(ints)]
    if(!length(levels))
      stop("Could not find any valid registration levels in file: ", x)
    min(levels)
  }
}
