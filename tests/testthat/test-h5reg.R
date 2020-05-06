context("test-h5reg")

test.pts <-
  structure(
    c(
      672.59794766919,
      543.418732368701,
      809.785936535812,
      748.318961486852,
      491.140456650774,
      808.036498669529,
      110.445261565453,
      90.9981079016641,
      226.695795267429,
      234.899446055409,
      145.885862080405,
      386.956771102929,
      169.099912273163,
      51.0857672826451,
      62.5526964434789,
      42.9513522927721,
      165.304493410898,
      258.989631274684
    ),
    .Dim = c(6L,
             3L),
    .Dimnames = list(NULL, c("X", "Y", "Z"))
  )


test_that("xform works with hi res", {
  skip_on_cran()
  skip_on_travis()

  reg=path.expand("~/projects/JFRC/JohnBogovic/quant/JRC2018F_FAFB.h5")
  skip_if_not(file.exists(reg))

  JRC2018F_FAFB.h5=h5reg(reg, swap=FALSE)
  JRC2018F_FAFB.h5.i=h5reg(reg, swap=TRUE)

  expect_silent(test.pts.t <- nat::xform(test.pts, reg = JRC2018F_FAFB.h5))

  expect_known_value(test.pts.t, 'testdata/invpts-hi.rds')
  expect_silent(test.pts.rt <- nat::xform(test.pts.t, reg = JRC2018F_FAFB.h5.i, level=0))
  # round trip test - not exact because of quantisation
  expect_equivalent(test.pts.rt, test.pts, tolerance=1e-4)
})

reg = system.file('samples/JRC2018F_FAFB_extrasmall.h5', package = 'nat.h5reg')

JRC2018F_FAFB.h5=h5reg(reg, swap=FALSE)
JRC2018F_FAFB.h5.i=h5reg(reg, swap=TRUE)

test_that("xform works with bundled low res", {

  expect_known_value(test.pts.t <- nat::xform(test.pts, reg = JRC2018F_FAFB.h5),
                     'testdata/invpts-lo.rds')
  # round trip test - not exact because of quantisation
  # note lower tolerance compared with previous test
  expect_equivalent(nat::xform(test.pts.t, reg = JRC2018F_FAFB.h5.i),
                    test.pts, tolerance=1e-3)

  # check that we can handle NAs
  test.pts.na <- test.pts
  test.pts.na[3,]=NA
  test.pts.na.t <- test.pts.t
  test.pts.na.t[3,]=NA

  expect_warning(nares <- nat::xform(test.pts.na, reg = JRC2018F_FAFB.h5))
  expect_equal(nares, test.pts.na.t)

  testthat::skip_if_not_installed('rJava')

  expect_equal(nat::xform(test.pts.t, reg = JRC2018F_FAFB.h5.i, method='rjava', progress.rjava=T),
                      nat::xform(test.pts.t, reg = JRC2018F_FAFB.h5.i,
                                 method='java'))

})

test_that("h5 basics", {
  sample.file <- function(x) system.file(file.path('samples', x), package = 'nat.h5reg')

  expect_true(is.hdf5(sample.file('simple.h5')))
  expect_true(is.hdf5(sample.file('simpledummyh5reg.h5')))
  expect_false(is.hdf5(sample.file('bad.h5')))

  expect_false(is.h5reg(sample.file('simple.h5')))
  expect_true(is.h5reg(sample.file('simpledummyh5reg.h5')))

  expect_known_value(read.h5reg.info(sample.file('complexdummyh5reg.h5')),
                     'testdata/complexdummyh5reg.info.rds')
})

