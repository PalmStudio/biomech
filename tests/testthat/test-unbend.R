filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
df = read_mat(filepath)


test_that("unbend works", {
  df_unbend = unbend(df)
  expect_equal(round(df_unbend$x,5),c(0.00066, 0.8833, 1.76595, 2.27314, 2.78033))
  expect_equal(round(df_unbend$y,5),c(0, 0, 0, 0, 0))
  expect_equal(round(df_unbend$z,5),c(0.00075, 1.00899, 2.01722, 2.59658, 3.17594))
})

