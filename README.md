## ePiE
ePiE is a spatially explicit model that estimates concentrations of active pharmaceutical ingredients (APIs) in surface waters across Europe. This repository contains the R package of the ePiE model alongside all the required input parameters, such as the parameterized European river catchment and discharge data. 

## ePiE: Installation

Run the code below to make sure all dependencies are installed (because ePiE is not yet in CRAN, this needs to be done manually).

``` r
if(!require("Rcpp")) install.packages("Rcpp")
if(!require("fst")) install.packages("fst")
if(!require("glue")) install.packages("glue")
if(!require("leaflet")) install.packages("leaflet")
if(!require("htmlwidgets")) install.packages("htmlwidgets")
if(!require("mapview")) install.packages("mapview")
if(!require("plyr")) install.packages("plyr")
if(!require("sf")) install.packages("sf")
if(!require("stringr")) install.packages("stringr")
if(!require("terra")) install.packages("terra")
```

Next, the ePiE package can be directly installed from R using the regular `install.packages()` function, see the code below.

``` r
# Install the R package on Windows
install.packages("https://github.com/SHoeks/ePiE/raw/refs/heads/main/ePiE_1.21.zip", 
                 repos=NULL, 
                 method="libcurl")

# Install the R package on Mac/Linux
install.packages("https://github.com/SHoeks/ePiE/raw/refs/heads/main/ePiE_1.21.tar.gz", 
                 repos=NULL, 
                 method="libcurl")
```

## ePiE: Example run

The code below illustrates a simple example run of the ePiE model for Ibuprofen. This example is mainly intended to provide an overview of the ePiE R package workflow.

```r
Available soon!
```

## TODO

- [ ] Update the package with all European basins
- [ ] Add example codes