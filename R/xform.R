#' Map points using h5 registrations
#'
#' @details See \code{\link{h5reg}} for details of the ordering of
#'   registrations.
#'
#' @section Java: By default \code{xformpoints.h5reg} starts a separate Java
#'   Virtual Machine (JVM) for every call. This has an overhead that might be in
#'   the 0.5s range, which quickly builds up. Alternatively, you can use
#'   \code{method='rjava'}, which depends on the 'rJava' package; since rJava
#'   can be a little fiddly to install, this is only a suggested dependency.
#'   Using rJava transparently starts a single JVM session from R and then
#'   reuses that, substantially speeding up the situation when you have many
#'   small objects to transform. It is also somewhat more efficient in terms of
#'   disk/CPU since points are retained in memory rather than written out as
#'   text files. However in my hands this has limited impact on the elapsed
#'   time.
#'
#' @param reg A \code{\link{h5reg}} object specifying one or more h5
#'   registrations.
#' @param points An Nx3 matrix of 3D points to transform
#' @param method Whether to shell out to java executable (\code{method='java'})
#'   or use the rJava package (\code{method='rjava'}). The default
#'   (\code{method='auto'}) uses rJava if installed. See Java section for
#'   implications of this choice.
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
#' # choose faster, lower resolution registration
#' xform(cbind(50,50,30), h5reg('JRC2018F_FAFB.h5'), level=2)
#' # print more detailed error messages when trying to debug
#' xform(cbind(50,50,30), h5reg('JRC2018F_FAFB.h5'), level=0, stderr="")
#' }
xformpoints.h5reg <- function(reg, points, ..., method=c('auto', 'java', 'rjava')) {
  if (ncol(points) != 3L)
    stop("xformpoints.h5reg only supports 3 dimensions!")

  method=match.arg(tolower(method), choices = c('auto', 'java', 'rjava'))
  if(method=='auto')
    method=ifelse(rjavaok(), 'rjava', 'java')

  # this should happen when we make the h5reg object of course
  swapped = attr(reg, 'swap')
  if (is.null(swapped))
    swapped = rep(TRUE, length(reg))

  nas=is.na(points[,1])
  if(sum(nas)) {
    origpoints=points
    points=points[!nas, , drop=FALSE]
  }

  pointst <- if(method=='java')
    saalfeld_xform(points, reg, inverse = !swapped, ...)
  else
    saalfeld_rjava_xform(points, reg, inverse = !swapped, ...)

  if(sum(nas)){
    origpoints[!nas, ]=pointst
    origpoints
  } else {
    dimnames(pointst)=dimnames(points)
    pointst
  }
}

# Saalfeld/Bogovic HDF5 format transforms are defined with
# forward being the direction that transforms
#' @importFrom utils write.table read.table
saalfeld_xform <- function(points, reg, inverse=FALSE, level=NA, stderr=FALSE, Verbose=FALSE, ...) {
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
    if(is.finite(level) && Verbose)
      message("using default registration level: ", level, " for file: ", reg)
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


## Intention is for this to be run once e.g. by memoisation
saalfeld_jinit <- memoise::memoise(function() {
  if(!requireNamespace('rJava', quietly = TRUE))
    stop("You must install the rJava package in order to use the (faster) rjava transform method!\n",
         "Get it from CRAN with\n",
         "install.packages('rJava')\n",
         "If you have any trouble, please see http://natverse.org/nat.h5reg/")
  rJava::.jinit()
  jarfile <- system.file("java/transform-helpers-0.0.1-shaded.jar", package = 'nat.h5reg')
  rJava::.jaddClassPath(jarfile)
})

rjavaok <- function() isFALSE(inherits(try(saalfeld_jinit(),silent = T), 'try-error'))

saalfeld_rjava_xform <- function(points, reg, inverse=FALSE, level=NA, Verbose=FALSE, ...) {
  saalfeld_jinit()

  transformFile=as.character(reg)
  hdf5Reader=rJava::J("ch.systemsx.cisd.hdf5.HDF5Factory","openForReading", transformFile)
  v=rJava::.jarray(rep(16L,3), "[I")
  n5 = rJava::new(rJava::J("org.janelia.saalfeldlab.n5.hdf5.N5HDF5Reader"), hdf5Reader, v)

  if(is.na(level)) {
    level <- default_h5_level(reg)
    if(is.finite(level) && Verbose)
      message("using default registration level: ", level, " for file: ", reg)
  }

  path=paste0(ifelse(is.finite(level), paste0(level, "/"), ""),
              ifelse(inverse, "/invdfield", "/dfield"))

  mytransform = rJava::J("org.janelia.saalfeldlab.n5.imglib2.N5DisplacementField",
                         "open", n5, path, inverse)
  # output array
  pointst=points
  q=rJava::.jarray(rep(0,3), "[D")
  for(i in 1:nrow(points)) {
    rJava::.jcall(mytransform, "V", "apply", points[i,], q)
    pointst[i,] <- rJava::.jevalArray(q)
  }
  pointst
}
