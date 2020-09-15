library(data.table)
filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "deformation")
df = read_mat(filepath)


test_that("CreaPoints works", {
  testthat::expect_snapshot(CreaPoints(2000,400,df))
})
