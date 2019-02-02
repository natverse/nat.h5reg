# nat.h5reg

nat.h5reg enables the [Neuroanatomy Toolbox (nat)](https://jefferis.github.io/nat/)
suite to carry out 3D coordinate transforms of arbitraty objects based on deformation fields
encoded in the [HDF5 registration format](https://github.com/saalfeldlab/template-building/wiki/Hdf5-Deformation-fields)
of Stephan Saalfeld and John Bogovic.

## Installation

You can install the development version of nat.h5reg from GitHub with:

``` r
devtools::install_github('jefferis/nat.h5reg')
```

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

## Acknowledgements

Based critically on https://github.com/saalfeldlab/transform-helpers/ 
kindly first written during a trip to Cambridge by Stephan Saalfeld.
