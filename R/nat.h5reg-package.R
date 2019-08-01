#' @keywords internal
#' @section h5 registration format: The
#'   \href{https://github.com/saalfeldlab/template-building/wiki/Hdf5-Deformation-fields}{h5
#'   registration format} has been defined by John Bogovic and Stephan Saalfeld
#'   as an efficient way to distribute deformation fields (and accompanying
#'   affine transformations). This provides a standard way to distribute and
#'   apply such transforms in an HDF5 wrapper.
#'
#'   Although deformation fields are the most general form of specifying a
#'   non-rigid registration, they can be much larger than a parametric
#'   deformation. h5 tools provide a substantial (lossy) compression of these
#'   fields at the expense of a very small (and usually irrelevant) loss of
#'   precision. They also provide the option to have one or more downsampled
#'   versions of the original field, again typically with relatively little
#'   impact on precision.
#'
#'   This package supports use of `.h5` registration files through the
#'   \code{\link{h5reg}} class and associated functions.
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
