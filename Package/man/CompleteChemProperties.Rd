\name{CompleteChemProperties}
\alias{CompleteChemProperties}
\title{CompleteChemProperties}
\usage{
chem = CompleteChemProperties(chem = chem)}
\arguments{
\item{chem}{Input chemical property data set to check and fill missing values}
}
\value{chem}
\description{This functions checks if chemical specific WWTP treatment removal rates are correct and aims to fill the missing values by calculation of degradation rates, sorption coefficients ande more. In case these they are not specified by user (NA).
}
