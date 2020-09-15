library(data.table)
filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
df = unbend(2000,400,read_mat(filepath))

pas= 0.02 # in meter. -> Length of the segments that discretize the object.
Ncalc= 100 # number of points used in the grid that discretized the section.
Nboucle= 15 # if we want to compute the torsion after the bending step by step instead of

test_that("bend works", {
  testthat::expect_snapshot(bend(data = df, pas, Ncalc, Nboucle,
                                 verbose = TRUE))
})
