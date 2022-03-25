#' Age-at-death ratio calculation
#'
#' Calculate number of skeletons in several age-at-death groups (e.g., D1+, D20+)
#' and six demographic ratios (D1+/D20+, D3+/D20+, D5+/D20+, D5-14/D20+ aka juvenility index,
#' D5-19/D5+ aka P index, and D0-14/D0+) for each cemetery sample.
#' @param data A data frame with individuals' age-at-death estimations.
#' Data frame must contain at least two columns:
#' \code{Age_min}, Lower limit of age-at-death interval;
#' \code{Age_max}, Upper limit of age-at-death interval.
#' For example, the value of 20 means that an individual died
#' before her/his 20th birthday, i.e. up to 19.9 years of age.
#' Two other columns are expected (\code{Site} and \code{Culture}), but they are not required.
#' @param extra_var Extra variables to be joined to results (e.g. description, absolute chronology of the site).
#' Extra variables must be in \code{data}.
#' @return Data frame summarising dstribution of deaths by skeletal samples.
#' @return Summary of the inputs:
#' \itemize{
#'   \item \strong{\code{Site}}: Name of the cemetery sample.
#'   \item \strong{\code{Culture}}: Archaeological culture to which Site belongs.
#' }
#' @return Number of skeletons on various age-at-death categories:
#' \itemize{
#'   \item \strong{\code{n}}: Total number of skeletons.
#'   \item \strong{\code{D0}}: Number of newborn deaths.
#'   \item \strong{\code{D1_, D3_, D5_, D15_, and D20_}}: D1+, D3+, D5+, D15+, and D20+;
#'   number of deaths older than 1, 3, 5, 15, and 20 years respectively.
#'   \item \strong{\code{D0_14, D5_14, and D5_19}}: D0-14, D5-14, and D5-19;
#'   number of deaths between 0-14, 5-14 and 5-19 years, respectively.
#'   \item \strong{\code{D1_D20_, D3_D20_, and D5_D20}}: D1+/D20+, D3+/D20+, and D5+/D20+ age-at-death ratios.
#'   \item \strong{\code{JI, and P}}: Bocquet-Appel's juvenility index (D5-14/D20+) and P index (D5-19/D5+).
#'   \item \strong{\code{D0_14_D0_}}: McFadden and Oxenham D0-14/D0+ age-at-death ratio.
#' }
#' @keywords age-at-death ratio
#' @examples
#' dr(BAraw)
#'
#' BAraw %>%
#' select(-Site) %>%
#' dr()
#' @export


# Demographic ratio calculation / function ----------------------------------------------------
# data: Columns must be named as "Site", "Culture", "Age_min" and "Age_max"

dr <- function(data, extra_var) {
  # Helper df for left_join (see later), contains distinct combinations of Site, culture and extra_var
  # age_div helps to add Site and Culture columns
  extra_var_sel <-
    data %>%
    age_div(extra_var={{extra_var}}) %>%
    dplyr::select(Site, Culture, {{extra_var}}) %>%
    dplyr::distinct()

  res <-
    age_div(data) %>%
    dplyr::select(Site, Culture, starts_with("A_")) %>%
    dplyr::group_by(Site, Culture) %>%
    dplyr::summarise_all(.funs = sum) %>% #sum of all A_ columns by categories
    dplyr::ungroup() %>%
    dplyr::mutate(n = rowSums(.[,paste("A_", 0:99, sep = "")]), #0:99 are age-at-death not column number
                  D0 = rowSums(.[,paste("A_", 0, sep = "")]),
                  D1_ = rowSums(.[,paste("A_", 1:99, sep = "")]),
                  D3_ = rowSums(.[,paste("A_", 3:99, sep = "")]),
                  D5_ = rowSums(.[,paste("A_", 5:99, sep = "")]),
                  D15_ = rowSums(.[,paste("A_", 15:99, sep = "")]),
                  D0_14 = rowSums(.[,paste("A_", 0:14, sep = "")]),
                  D5_14 = rowSums(.[,paste("A_", 5:14, sep = "")]),
                  D5_19 = rowSums(.[,paste("A_", 5:19, sep = "")]),
                  D20_ = rowSums(.[,paste("A_", 20:99, sep = "")])) %>%
    dplyr::select(-starts_with("A_")) %>%
    dplyr::mutate(D1_D20_ = D1_/D20_, #Galeta 2010
                  D3_D20_ = D3_/D20_, #Galeta 2010
                  D5_D20_ = D5_/D20_, #Galeta 2010
                  JI = D5_14/D20_, #Bocquet-Appel, Masset 1983
                  P = D5_19/D5_, #Bocquet-Appel, 2002
                  D0_14_D0_ = D0_14/n) %>%  #McFadden et al. 2018
    # Add extra_var columns
    dplyr::left_join(extra_var_sel, by=c("Site", "Culture")) %>%
    dplyr::select(Site, Culture, {{extra_var}}, everything())

  utils::write.table(res,
                     "clipboard", sep="\t", dec = ",", col.names = NA)
  return(res)
}
