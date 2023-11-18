<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.org)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://natverse.github.io/nat.h5reg/reference/)
<img src="man/figures/logo.svg" align="right" height="139" /> 
[![R-CMD-check](https://github.com/natverse/nat.h5reg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/natverse/nat.h5reg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# nat.h5reg

nat.h5reg enables the [Neuroanatomy Toolbox (nat)](https://jefferis.github.io/nat/)
suite to carry out 3D coordinate transforms of arbitrary objects based on deformation fields
encoded in the [HDF5 registration format](https://github.com/saalfeldlab/template-building/wiki/Hdf5-Deformation-fields)
of Stephan Saalfeld and John Bogovic.

## Installation

You can install the development version of nat.h5reg from GitHub with:

``` r
devtools::install_github('natverse/nat.h5reg')
```

See below for information about Java installation.

## Example

A round trip test using a bridging registration between two template brains.

``` r
library(nat.h5reg)

# We will use sample Kenyon Cells in FCWB (FlyCircuit) space
library(nat)
head(kcs20)

# swap=FALSE, so this will map points onto JRC2018F 
kcs20.jrc2018 = xform(kcs20, 
  reg = h5reg('JRC2018F_FCWB_transform_quant16.h5', swap=FALSE)
)

# map back again (round trip test)
kcs20.rt = xform(
  kcs20.jrc2018,
  reg = h5reg('JRC2018F_FCWB_transform_quant16.h5')
)

plot3d(kcs20.jrc2018, col='green')

clear3d()
plot3d(kcs20, col='red')
plot3d(kcs20.rt, col='blue')

diffs=xyzmatrix(kcs20)-xyzmatrix(kcs20.rt)
str(diffs)
plot(as.data.frame(diffs))
```

## Java

You will need to have Java installed on your system to use h5 format registrations.
This will usually mean installing a Java Development Kit (JDK). These now come
in a confusing number of flavours, but for personal use, the easiest is probably
to install the latest stable 
[Oracle JDK](https://www.oracle.com/java/technologies/javase-downloads.html). At
the time of writing this functionality was tested versus Oracle JDK 11.0.2.

You can find out about your java installation by doing:

```
nat.h5reg::dr_java()
```

You may run into errors installing the rJava package. This typically happens 
because of mismatch between the version of your JDK and what R is expecting.
If this happens try (in the Terminal)

```
# make sure java is in your path
java --version
# tell R about your current version java
R CMD javareconf
```

Now in R, try installing rJava again

```r
install.packages("rJava")
```

## Acknowledgements

Based critically on https://github.com/saalfeldlab/transform-helpers/ 
kindly first written during a trip to Cambridge by Stephan Saalfeld.


