## Intention is for this to be run once e.g. by memoisation
saalfeld_jinit <- memoise::memoise(function() {
  if(!requireNamespace('rJava', quietly = TRUE))
    stop("Install the rJava package in order to use the (faster) rjava ",
         "transform method!\n",
         java_install_instructions())
  res <- rJava::.jinit()
  if(res<0)
    stop("Unable to intialise the JVM")
  else if(res>0)
    warning("Only partially successful intialising JVM. See ?jinit fot details")
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
