% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{BAraw}
\alias{BAraw}
\title{Bocquet-Appel's (2002) dataset in the raw data format}
\format{
A tibble with 5,115 rows and 4 variables:
\describe{
\item{Site}{The name of the cemetery sample}
\item{Culture}{Period to which Site belongs}
\item{Age_min}{Age_min, Lower limit of age-at-death interval}
\item{Age_min}{Age_max, Upper limit of age-at-death interval. Note that the value of 20, for example, means that an individual died just before her/his 20th birthday, i.e. up to 19.9 years of age}
}
}
\usage{
BAraw
}
\description{
Dataset containing age-at-death estimation for 5,115 skeletons from 68 European Mesolithic and Neolithicsites.
Data were rebuilt from the Bocquet-Appel's summary Table 1 published in
Bocquet-Appel J-P (2002). Paleoanthropological traces of a Neolithic demographic transition. Current Anthropology 43: 637-650.
Note that the data is a reasonable approximation of the original raw data collected by Bocquet-Appel,
which were not available to us.
The dataset is in the raw data format, each row represents one skeleton.
}
\keyword{datasets}
