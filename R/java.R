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
