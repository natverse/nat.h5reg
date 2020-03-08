#' Map points using h5 registrations
#'
#' @details See \code{\link{h5reg}} for details of the ordering of
#'   registrations.
#'
#' @param reg A \code{\link{h5reg}} object specifying one or more h5
#'   registrations.
#' @param points An Nx3 matrix of 3D points to transform
#' @param ... Additional arguments passed to java transform tool (currently
#'   ignored).
#' @export
#' @return An Nx3 matrix of transformed points
#' @importFrom nat xformpoints
#' @family h5reg
#' @seealso \code{\link{h5reg}} for an example of point transformations using h5
#'   registrations and \code{\link{xform}} and \code{\link{xformpoints}} for
#'   details of how transformations are handled within the neuroanatomy toolbox
#'   suite.
#' @examples
#' \dontrun{
#' # basic usage
#' xform(cbind(50,50,30), h5reg('JRC2018F_FAFB.h5'))
#' xform(cbind(50,50,30), h5reg('JRC2018F_FAFB.h5', swap=TRUE))
#' # specify a particular level for registration containing more than 1 level
#' # of detail
#' xform(cbind(50,50,30), h5reg('JRC2018F_FAFB.h5'), level=0)
#' # print more detailed error messages when trying to debug
#' xform(cbind(50,50,30), h5reg('JRC2018F_FAFB.h5'), level=0, stderr="")
#' }
xformpoints.h5reg <- function(reg, points, ...) {
  if (ncol(points) != 3L)
    stop("xformpoints.h5reg only supports 3 dimensions!")

  # this should happen when we make the h5reg object of course
  swapped = attr(reg, 'swap')
  if (is.null(swapped))
    swapped = rep(TRUE, length(reg))

  saalfeld_xform(points, reg, inverse = !swapped, ...)
}

# Saalfeld/Bogovic HDF5 format transforms are defined with
# forward being the direction that transforms
#' @importFrom utils write.table read.table
saalfeld_xform <- function(points, reg, inverse=FALSE, level=NA, stderr=FALSE, ...) {
  pointsfile=tempfile(fileext=".txt")
  on.exit(unlink(pointsfile))
  write.table(points, file=pointsfile, row.names=FALSE, col.names=FALSE)

  outfile=tempfile()
  on.exit(unlink(outfile), add=TRUE)

  jarfile <- system.file("java/transform-helpers-0.0.1-shaded.jar", package = 'nat.h5reg')

  args = c('-jar',
           shQuote(jarfile),
           "--transform",
           shQuote(reg),
           "--coordinates",
           shQuote(pointsfile))
  #
  if(is.na(level)) {
    level <- default_h5_level(reg)
    if(is.finite(level))
      warning("using default registration level: ", level, " for file: ", reg)
  }
  if(is.finite(level)) args=c(args, "--level", level)

  if(inverse) args=c(args, "--inverse")
  rval = system2(
    "java",
    args=args,
    stdout = outfile,
    stderr = stderr
  )

  if(rval!=0) stop("Error running saalfeld xform!")
  outpoints <- read.table(outfile,
                          col.names=c('X', 'Y', 'Z', 'Failed'), row.names=NULL,
                          colClasses=c(rep('numeric', 3), 'factor'), fill=TRUE)
  pointst <- data.matrix(outpoints[,1:3])
  pointst
}
