filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
df = unbend(read_mat(filepath))
df$distance_application = distance_weight_sine(df$x)

pas= 0.02 # in meter. -> Length of the segments that discretize the object.
Ncalc= 100 # number of points used in the grid that discretized the section.
Nboucle= 15 # if we want to compute the torsion after the bending step by step instead of

test_that("bend works", {
  expect_known_value(bend(data = df, elastic_modulus =  2000, shear_modulus = 400,
                          pas, Ncalc, Nboucle, verbose = TRUE), file = "bend.test")
})
