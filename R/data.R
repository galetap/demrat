#' Original Bocquet-Appel's (2002, Table 1) dataset
#'
#' Bocquet-Appel J-P (2002). Paleoanthropological traces of a Neolithic demographic transition. Current Anthropology 43: 637-650.
#' Summary of distribution of deaths in 68 Mesolithic and Neolithic European cemetery samples.
#' Three new variables were added to the dataset (D5_, D5_D20_, and Culture).

#'
#' @format A tibble with 68 rows and 12 variables:
#' \describe{
#'   \item{Site}{The name of the cemetery sample}
#'   \item{Front}{Absolute C14 date of the Neolithic transition in the region of the Site}
#'   \item{C14}{Absolute C14 date of the Site}
#'   \item{dt}{The number of years from the Neolithic front, C14-Front}
#'   \item{D0_4}{D0-4; number of skeletons between 0 and 4 years of age}
#'   \item{D5_19}{D5-19; number of skeletons between 5 and 19 years of age}
#'   \item{D20_}{D20+; number of skeletons over 20 years of age}
#'   \item{Total}{Total number of skeletons}
#'   \item{D5_19_D5_}{D5-19/D5+ ratio or index P}
#'   \item{D5_}{D5+; number of skeletons over 5 years of age}
#'   \item{D5_D20_}{D5+/D20+ ratio}
#'   \item{Culture}{Period to which Site belongs}
#' }
"BA"


#' Raw Bocquet-Appel's (2002) dataset
#'
#' Dataset containing age-at-death data for ca. 5,000 European Mesolithic and Neolithic skeletons from 68 sites.
#' Data were reconstructed based on Bocquet-Appel's summary Table 1 published in
#' Bocquet-Appel J-P (2002). Paleoanthropological traces of a Neolithic demographic transition. Current Anthropology 43: 637-650.
#' Note that the dataset does not represent the original Bocquet-Appel's raw data that are not available to us.

#'
#' @format A tibble with 5,115 rows and 4 variables:
#' \describe{
#'   \item{Site}{The name of the cemetery sample}
#'   \item{Culture}{Period to which Site belongs}
#'   \item{Age_min}{Age_min, Lower limit of age-at-death interval}
#'   \item{Age_min}{Age_max, Upper limit of age-at-death interval. Note that the value of 20, for example, means that an individual died just before her/his 20th birthday, i.e. up to 19.9 years of age}
#' }
"BAraw"
