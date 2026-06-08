test_that("confints.bootpls compatibility helper handles scalar boot statistics", {
  set.seed(314)
  xran <- matrix(rnorm(30 * 5), 30, 5)
  boot_object <- boot::boot(data = xran, statistic = coefs.plsR.CSim,
                            sim = "ordinary", stype = "i", R = 99)

  conf <- suppressWarnings(
    bootPLS:::.confints.bootpls.compat(boot_object, typeBCa = FALSE)
  )

  expect_true(is.matrix(conf))
  expect_equal(dim(conf), c(1L, 6L))
  expect_equal(colnames(conf),
               c("Normal.Lower", "Normal.Upper",
                 "Basic.Lower", "Basic.Upper",
                 "Percentile.Lower", "Percentile.Upper"))
  expect_false(attr(conf, "typeBCa"))
  expect_true(all(is.finite(conf)))
})

test_that("coefs.plsR.adapt.ncomp example path handles scalar intervals", {
  set.seed(314)
  ncol <- 5
  xran <- matrix(rnorm(30 * ncol), 30, ncol)

  coefs <- suppressWarnings(
    coefs.plsR.adapt.ncomp(xran, sample(1:30), R = 80)
  )

  expect_type(coefs, "double")
  expect_length(coefs, ncol + 1L)
  expect_true(all(is.finite(coefs)))
})
