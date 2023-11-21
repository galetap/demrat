#' Divides an individual into parts
#'
#' Proportionally splits an individual into 100 age-at-death categories (0-99 years)
#' on the basis of the estimation of its age-at-death.
#' @param data A data frame with individuals' age-at-death estimations.
#' Data frame must contain at least two columns:
#' \code{Age_min}, lower limit of age-at-death interval;
#' \code{Age_max}, upper limit of age-at-death interval.
#' For example, the value of 20 means that an individual died
#' before her/his 20th birthday, i.e. up to 19.9 years of age.
#' Two other columns are expected (\code{Site} and \code{Culture}), but they are not required.
#' @param show_res If \code{TRUE}, results are printed. Defaults to \code{FALSE}.
#' @param extra_var Extra columns/variables stored in \code{data},
#' which should be added to the results.
#' @return A data frame containing individuals splitted into age-at-death categories.
#' @return Variables are as follows:
#' \itemize{
#'   \item \code{Site, Culture, Age-min, Age_max}: Summary of inputs.
#'   If \code{Site} and \code{Culture} are not given in the \code{data},
#'   then \code{Unknown} values are used.
#'   \item \code{A_0, ..., A_99}: Proportion of the individual
#'   belonging to the age-at-death categories betweeen 0 and 99 years. Row sum equals to 1 for each individual.
#' }
#' @keywords age-at-death
#' @examples
#' age_div(BAraw, show_res = T) %>%
#' as_tibble()
#' @export

# Divide individuals into 0-99 age categories / function ---------------------------
# Columns must be named as "Site", "Culture", "Age_min" and "Age_max"

age_div <- function(data, show_res = F, extra_var) {
  if(length(dplyr::intersect(c("Age_min", "Age_max"), colnames(data)))!=2){
    message("Dataframe must contain columns Age_min and Age_max")
  }
  else {
data <-
  data %>%
  # Is there Site column?
  {if("Site" %in% colnames(.))
    .
    else
      dplyr::mutate(., Site="Unknown")} %>%
  # Is there Culture column?
  {if("Culture" %in% colnames(.))
    .
    else
      dplyr::mutate(., Culture="Unknown")} %>%

  # Subset of complete cases (cases with Age_min and Age_max or Culture)
  dplyr::select(Site, Culture, {{extra_var}}, Age_min, Age_max) %>%
  tidyr::drop_na()

  # Filter unique combinations of Age_min and Age_max
  distinct <-
    data %>%
    dplyr::select(Age_min, Age_max) %>%
    dplyr::distinct()

  # Divide "distinct" into age categories
  # Create data frame of zeros (0), nrow = distinct size, ncol is 101 (0-100 years)
  dist_div <- data.frame(matrix(data = 0, nrow = nrow(distinct), ncol = 101))
  # For every individual:
  for(i in 1:nrow(distinct)) {
    # Temporary vector with 101 age categories
    age_ind <- vector(length = 101)
    # Assign 1 between Age_min and Age_max
    age_ind[(floor(distinct$Age_min[i])+1):(floor(distinct$Age_max[i])+1)] <- 1
    # Adjust last age-at-death category (10.5 years of age <- assign 0.5 of individual)
    age_ind[floor(distinct$Age_max[i])+1] <- (distinct$Age_max[i]-floor(distinct$Age_max[i]))
    # Adjust first  age-at-death category
    age_ind[floor(distinct$Age_min[i])+1] <- 1-(distinct$Age_min[i]-floor(distinct$Age_min[i]))
    # Assign age_ind to ith row of distinct (change only values <> 0)
    dist_div[i,age_ind>0] <- age_ind[age_ind>0]
  }
  # Divide row by the row sum, i.e. scale row to sum up 1 individual
  # And delete last (101st) column (i.e. column with 100th years of age)
  dist_div <- dist_div[1:100]/rowSums(dist_div[1:100])
  colnames(dist_div) <- paste("A_", 0:99, sep="")

  # Join data with distinct and dist_div
  # Data contain numerous copies of individuals in distinct
  res <-
    distinct %>%
    cbind(dist_div) %>%
    dplyr::left_join(data, ., by = c("Age_min", "Age_max")) %>%
    as.data.frame()

  if(show_res == T) {
    return(res)
  }
  else {
    invisible(res)
  }
  }
}
