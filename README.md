## ePiE
ePiE is a spatially explicit model that estimates concentrations of active pharmaceutical ingredients (APIs) in surface waters across Europe. This repository contains the R package of the ePiE model. 

## ePiE Installation

The ePiE package can be directly installed from R using the regular `install.packages()` function:

``` r
# Load the remotes package
library('remotes') # or use library('devtools')

# Install the MadingleyR package
install_github('MadingleyR/MadingleyR', subdir='Package', build_vignettes = TRUE)

# Load MadingleyR package 
library('MadingleyR')

# Get version MadingleyR and C++ source code
madingley_version( )

# View the MadingleyR tutorial vignette
vignette('MadingleyR')
```