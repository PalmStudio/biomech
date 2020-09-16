filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
df = read_mat(filepath)


test_that("unbend works", {
  # expect_snapshot(round(unbend(2000,400,df),17))
  expect_known_output(round(unbend(2000,400,df),17),
                      file = "unbend.test")
})

