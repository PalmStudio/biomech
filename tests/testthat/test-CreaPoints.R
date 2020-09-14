library(data.table)
filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "deformation")
matExp = data.table::fread(filepath, data.table = FALSE)


test_that("CreaPoints works", {
  testthat::expect_snapshot(CreaPoints(2000,400,matExp))
})
