library(data.table)
filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
df = read_mat(filepath)


test_that("unbend works", {
  testthat::expect_snapshot(unbend(2000,400,df))
})
