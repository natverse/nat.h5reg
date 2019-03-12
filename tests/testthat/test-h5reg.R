context("test-h5reg")

test_that("xform works", {
  skip_on_cran()
  skip_on_travis()

  reg=path.expand("~/projects/JFRC/JohnBogovic/quant/JRC2018F_FAFB.h5")
  skip_if_not(file.exists(reg))

  JRC2018F_FAFB.h5=h5reg(reg, swap=FALSE)
  JRC2018F_FAFB.h5.i=h5reg(reg, swap=TRUE)
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

  # warn if we have to choose a default registration without being told
  expect_warning(test.pts.t <- nat::xform(test.pts, reg = JRC2018F_FAFB.h5),
                 'using default registration')

  expect_known_value(test.pts.t, 'testdata/invpts.rds')
  expect_silent(test.pts.rt <- nat::xform(test.pts.t, reg = JRC2018F_FAFB.h5.i, level=0))
  # round trip test - not exact because of quantisation
  expect_equivalent(test.pts.rt, test.pts, tolerance=1e-4)
})
