## Intention is for this to be run once e.g. by memoisation
saalfeld_jinit <- memoise::memoise(function(pid=Sys.getpid()) {
  if(!requireNamespace('rJava', quietly = TRUE))
    stop("Install the rJava package in order to use the (faster) rjava ",
         "transform method!\n",
         java_install_instructions())
  res <- rJava::.jinit()
  if(res<0)
    stop("Unable to intialise the JVM")
  else if(res>0)
    warning("Only partially successful intialising JVM. See ?jinit for details")
  jarfile <- system.file("java/transform-helpers-0.0.1-shaded.jar", package = 'nat.h5reg')
  rJava::.jaddClassPath(jarfile)
})

java_install_instructions <- function() {
  paste0(
    "* install at least version 8 of a JDK (>=v11 recommended)\n",
    "* you could get this from https://www.oracle.com/java/technologies/javase-jdk11-downloads.html\n",
    " * run `R CMD javareconf` in the terminal\n",
    " * run `install.packages('rJava')` in a clean R session\n",
    "* If you have any trouble, please see http://natverse.org/nat.h5reg/"
  )
}

rjavaok <- function() isFALSE(inherits(try(saalfeld_jinit(),silent = T), 'try-error'))


#' Check the local java configuration (required for h5reg transforms)
#'
#' @export
#'
#' @examples
#' \donttest{
#' dr_java()
#' }
dr_java <- function() {
  rjava <- rjavaok()
  message("rJava\n----")
  if(rjava) {
    message("The rJava package is linked to the following JVM")
    rjava.props <- parse_java_props()
    message('java.version=', rjava.props['java.version'])
    message('java.home=', rjava.props['java.home'])
    message('java.class.version=', rjava.props['java.class.version'])
    if(numeric_version(rjava.props['java.class.version'])<'52.0') {
      message("Unfortunately rJava is linked to an old version of Java.",
              "You must:\n", java_install_instructions())
    }
  } else {
    message("rJava is not available! This is the preferred way to access java and you can do this as follows!\n", java_install_instructions())
  }
  java <- Sys.which('java')
  message("\nSystem java\n----\n(used as a fallback when rJava not available)")
  if(nzchar(java)) {
    message("java run time found at: ", java)
    jv <- java_version()
    message("java.version: ",jv)
    if(jv<"8.0" && isFALSE(rjava)) {
      message("Unfortunately you do not have rJava and your system java (",
              jv, ") is too old to substitute.")
    }
  } else {
    message("No java command line tool found in your path!")
  }

  message("\nh5reg test\n----")
  reg = system.file('samples/JRC2018F_FAFB_extrasmall.h5', package = 'nat.h5reg')
  JRC2018F_FAFB.h5=h5reg(reg, swap=FALSE)
  JRC2018F_FAFB.h5.i=h5reg(reg, swap=TRUE)
  res=try(nat::xform(test.pts[1,, drop=F], reg = JRC2018F_FAFB.h5))
  if(inherits(res, 'try-error'))
    message("failure in h5reg xform infrastructure!")
  else {
    message("h5reg xform infrastructure OK!")
    baseline = matrix(
      c(434.893966568567, 48.8813320055595, 159.133844268717),
      ncol = 3,
      dimnames = list(NULL, c("X", "Y", "Z"))
    )
    if(!isTRUE(all.equal(res, baseline, tolerance=1e-6)))
      message("xform test gave incorrect result")
    else
      message("xform test gave correct results")
  }
}

dr_h5reg <- function() {

}

dr_java_oneoff <- memoise::memoise(dr_java)

parse_java_props <- function(x=NULL) {
  if(is.null(x)) {
    jsys = rJava::.jnew("java.lang.System")
    x = jsys$getProperties()$toString()
  }
  # remove leading and trailing {
  x = substr(x, 2, nchar(x)-1)
  jsys.propsc = unlist(strsplit(x, ", ", fixed=T))
  pn=sub("^([^=]+)=(.*)","\\1", jsys.propsc)
  pp=sub("^([^=]+)=(.*)","\\2", jsys.propsc)
  props=structure(pp, .Names=pn)
  props
}

java_version <- function() {
  versionstr=tryCatch(system2("java",  args = "-version", stdout = TRUE, stderr = T),
           warning=function(e) "")[1]
  if(nzchar(versionstr)) {
    cands=unlist(strsplit(versionstr, " "))
    found=grep("[0-9]+\\.[0-9]+\\.[0-9]+", cands, value = T)
    found=sub("_.*", "", found)
    found=gsub('"', '', found)
    if(length(found)) {
      # if we start with 1. then we should remove that
      found=sub("^1\\.", "", as.character(found))
      numeric_version(found)
    } else NA
  } else {
    NA
  }

}
