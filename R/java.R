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


#' @export
#' @rdname dr_h5reg
dr_java <- function() {
  rjava <- rjavaok()
  message("rJava\n----")
  if(rjava) {
    cat("The rJava package is linked to the following JVM\n")
    rjava.props <- parse_java_props()
    cat('java.version:', rjava.props['java.version'], "\n")
    cat('java.home:', rjava.props['java.home'], "\n")
    cat('java.class.version:', rjava.props['java.class.version'], "\n")
    if(numeric_version(rjava.props['java.class.version'])<'52.0') {
      message("Unfortunately rJava is linked to an old version of Java.",
              "You must:\n", java_install_instructions())
    }
  } else {
    message("rJava is not available! This is the preferred way to access java and you can do this as follows!\n", java_install_instructions())
  }
  java <- Sys.which('java')
  message("\nSystem java\n----")
  cat("(used as a fallback when rJava not available)\n")
  if(nzchar(java)) {
    cat("java run time found at:", java, "\n")
    jv <- java_version()
    cat("java.version:", as.character(jv), "\n")
    if(jv<"8.0" && isFALSE(rjava)) {
      message("Unfortunately you do not have rJava and your system java (",
              jv, ") is too old to substitute.")
    }
  } else {
    message("No java command line tool found in your path!")
  }
}



#' Check the local h5reg and java configuration (required for h5reg transforms)
#'
#' @examples
#' \donttest{
#' dr_h5reg()
#' }
#' @export
dr_h5reg <- function() {
  dr_java()
  message("\nh5reg test\n----")
  reg = system.file('samples/JRC2018F_FAFB_extrasmall.h5', package = 'nat.h5reg')
  JRC2018F_FAFB.h5=h5reg(reg, swap=FALSE)
  JRC2018F_FAFB.h5.i=h5reg(reg, swap=TRUE)
  testpts=matrix(c(672.5979, 110.4452, 169.01), ncol = 3,
                 dimnames = list(NULL, c("X", "Y", "Z")))
  res=try(nat::xform(testpts, reg = JRC2018F_FAFB.h5))
  if(inherits(res, 'try-error'))
    message("failure in h5reg xform infrastructure!")
  else {
    cat("h5reg xform infrastructure OK!\n")
    baseline = matrix(
      c(434.8939, 48.85098, 159.0788),
      ncol = 3,
      dimnames = list(NULL, c("X", "Y", "Z"))
    )
    check=all.equal(res, baseline, tolerance=1e-6)
    if(!isTRUE(check)){
      message("xform test gave incorrect result")
      cat(check,"\n")
    } else cat("xform test gave correct results")
  }
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
