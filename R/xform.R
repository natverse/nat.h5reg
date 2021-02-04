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
#' \donttest{
#' # basic usage
#' sampleh5path=system.file("samples/JRC2018F_FAFB_extrasmall.h5",
#'   package = 'nat.h5reg')
#' sampleh5reg=h5reg(sampleh5path)
#' xform(cbind(50,50,30), sampleh5reg)
#' xform(cbind(50,50,30), sampleh5reg, swap=TRUE)
#' }
#'
#' \dontrun{
#' # specify a particular level for registration containing more than 1 level
#' # of detail
#' # NB not implemented in this sample
#' xform(cbind(50,50,30), sampleh5reg, level=0)
#' # choose faster, lower resolution registration
#' xform(cbind(50,50,30), sampleh5reg, level=2)
#' # print more detailed error messages when trying to debug
#' xform(cbind(50,50,30), sampleh5reg, level=0, stderr="")
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
  errfile=tempfile()
  on.exit(unlink(c(outfile, errfile)), add=TRUE)

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
    stderr = errfile
  )

  if(rval!=0) {
    warning(readLines(errfile))
    stop("Error running saalfeld xform! To debug run `nat.h5reg::dr_h5reg()`\n")
  }
  outpoints <- read.table(outfile,
                          col.names=c('X', 'Y', 'Z', 'Failed'), row.names=NULL,
                          colClasses=c(rep('numeric', 3), 'factor'), fill=TRUE)
  pointst <- data.matrix(outpoints[,1:3])
  pointst
}

saalfeld_rjava_xform <- function(points, reg, inverse=FALSE, level=NA, Verbose=FALSE, progress.rjava=FALSE, ...) {
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
  if(isTRUE(progress.rjava)) {
    pb <- progress::progress_bar$new(
      total = nrow(points),
      format = "  :current/:total [:bar]  eta: :eta",
      show_after = 2)
    mod <- ceiling(nrow(points)/1000)
  }
  # profiling suggests that :: takes signifiant time (order 1/6 of total)
  # but we can't just import because rJava is suggested not imported.
  # Not sure that force is required.
  myjcall=rJava::.jcall
  myjeval=rJava::.jevalArray
  for(i in 1:nrow(points)) {
    if(isTRUE(progress.rjava) && (i %% mod)==0) {
      pb$tick(mod)
    }
    myjcall(mytransform, "V", "apply", points[i,], q)
    pointst[i,] <- myjeval(q)
  }
  pointst
}
