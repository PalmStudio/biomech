
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biomech

<!-- badges: start -->

[![R build
status](https://github.com/PalmStudio/biomech/workflows/R-CMD-check/badge.svg)](https://github.com/PalmStudio/biomech/actions)
<!-- badges: end -->

`biomech` helps to compute bending and torsion of beams following the
Euler-Bernoulli beam theory. It is specifically designed to be applied
on tree branches (or e.g. palm leaves), but can be applied to any other
beam-shaped structure.

## Installation

You can install biomech from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PalmStudio/biomech")
```

## Example

Here is an example usage:

``` r
library(biomech)
file_path = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
field_data = read_mat(file_path)
# Un-bending the field measurements:
df_unbent = unbend(field_data)

# Adding the distance of application of the left and right weight:
df_unbent$distance_application = distance_weight_sine(df_unbent$x)

# (Re-)computing the deformation:
df_bent = bend(df_unbent, elastic_modulus = 1200, shear_modulus = 100, step = 0.02, points = 100, iterations = 15, verbose = TRUE)
#>  Final torsion angle at the tip (degree) =  37.91962

df_bent
#>          x            y         z    length   angle_xy     angle_xz   torsion
#> 1 0.000000 0.0000000000 0.0000000 0.0000000   0.000000  0.000000000  4.000000
#> 2 1.085347 0.0001879442 0.7668137 1.3289023  35.241771  0.009921628  4.130507
#> 3 2.390765 0.0086790523 1.0299259 1.3316966  11.395287  0.372675923  6.727513
#> 4 3.154218 0.0675902388 0.9025891 0.7762381  -9.441675  4.412435490 16.918753
#> 5 3.712503 0.2987318001 0.4561007 0.7513063 -36.461526 22.490548518 37.919620
```

You can plot the results using:

``` r
plot_bending(Observed = field_data, "Un-Bent obs." = df_unbent, Modeled = df_bent)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Note that the `unbent` argument is optional but can be very usefull to
compare with the initial conditions.

You can even make 3d plots using `plot_bent_3d()`:

``` r
plot_bending_3d(Observed = field_data, "Un-Bent obs." = df_unbent, Modeled = df_bent)
```

![](https://raw.githubusercontent.com/PalmStudio/biomech/master/www/bent.png)
