#' Estimates growth and fertility rate for cemetery samples
#'
#' Estimates growth, crude birth and total fertility rates based on age-at-death ratios
#' (e.g., D5+/D20+) summarising distribution of deaths in a cemetery sample.
#' Function can be applied to several cemetery samples at once.
#' @param dr_data A data frame, each row contains data for a single cemetery sample.
#' Data frame must have two columns at minimum:
#' \code{D20_} (i.e., D20+, number of adult skeletons in a sample)
#' and at least one of the following three columns: \code{D1_D20_, D3_D20_, D5_D20_}
#' (i.e., D1+/D20+, D3+/D20+, D5+/D20+ age-at-death ratios, respectively).
#' @param summary If \code{TRUE}, functions returns only estimations.
#' If \code{FALSE}, function returns full results including model characteristics, reference data, etc.
#' @param pred_level Confidence of the prediction, defaults to 0.95.
#' @param sss If \code{TRUE}, simulated skeletal samples are created as the reference.
#' If \code{FALSE}, reference consists from population data,
#' the algorithm step, in which simulated skeletal samples are created, is skipped.
#' @param samples Number of simulated skeletal samples to be created.
#' @param e0_min Minimum value of life expectancy at birth in population from which skeletal samples are drawn.
#' @param e0_max Maximum value of life expectancy at birth in population from which skeletal samples are drawn.
#' @param growth_min Minimum value of annual growth rate in population from which skeletal samples are drawn.
#' @param growth_max Maximum value of annual growth rate in population from which skeletal samples are drawn.
#' @param extra_var Extra variables to be joined to results (e.g. description, absolute chronology of the site).
#' Extra variables must be in \code{dr_data} data frame.
#' @param ... Other parameters (see \code{simdr_CD} and \code{diest_fun})

#' @return Data frame containing estimation of demographic indicators of population
#' from which a cemetery sample was drawn.
#' @return Summary of the inputs (cemetery sample input data):
#' \itemize{
#'   \item \strong{\code{Site}}: Name of the cemetery sample.
#'   \item \strong{\code{Culture}}: Archaeological culture to which \code{Site} belongs.
#'   \item \strong{\code{dr_data}}: A copy of input data with the age-at-death ratios
#'   for the \code{Site}.
#' }
#' @return Characteristics of model used in prediction.
#' \itemize{
#'   \item \strong{\code{DV}}: Dependent variable (estimated demographic indicator).
#'   \item \strong{\code{IV}}: Independent variable (age-at-death ratio).
#'   \item \strong{\code{Ref}}: Data frame containing reference sample from which regression model is computed.
#'   \item \strong{\code{Glance}}: A tibble with basic characteristics of the regression model
#'   used in the prediction.
#' }
#' @return Results of demographic estimation.
#' \itemize{
#'   \item \strong{\code{Est}}: Estimation of demographic indicator (see \code{DV})
#'   based on age-at-death ratio (see \code{IV}).
#'   \item \strong{\code{Lwr}}: Lower limit of the prediction interval.
#'   \item \strong{\code{Upr}}: Upper limit of the prediction interval.
#' }
#' @keywords demographic estimation
#' @examples
#' # Demographic estimation based on the original Bocquet-Appel (2002, Table 1) data
#' BA %>%
#' slice(12, 24) %>%
#' diest()
#'
#' # Adding extra variables
#' BA %>%
#' slice(12, 24) %>%
#' diest(extra_var = c(Front, C14, dt))
#'
#' # Demographic estimation based on the reconstructed Bocquet-Appel (2002) raw data
#' BAraw %>%
#' dr() %>%
#' slice(1:2) %>%
#' diest()
#'
#' @export



# Demographic indicator estimation for multiple site ---------------------
# Each row of dr_data is dr for individual site
diest <- function(dr_data, summary=T, pred_level=0.95,
                  sss=T, samples = 100,
                  e0_min = 20, e0_max = 30,
                  growth_min = -4.0, growth_max = 8.0,
                  extra_var, ...){
  # Argument glanced must be F to let function work
  if(sum(c("Site", "Culture") %in% names(dr_data))!=2)
    message("Input data frame with demographic ratios must contain columns Site and Culture")
  else {
    dr_data %>%
      tidyr::nest(dr_data=-c(Site, Culture)) %>%
      dplyr::mutate(
        DIest = purrr::map(dr_data,
                           function(.x, glanced=F, ...)
                             diest_fn(dr_data=.x, sss=sss,
                                      samples = samples,
                                      e0_min = e0_min, e0_max = e0_max,
                                      growth_min = growth_min, growth_max = growth_max,
                                      extra_var={{extra_var}}, ...), ...)) %>%
      mutate(DIest=map(.x = DIest,
                       .f = ~dplyr::select(.x, -Site, -Culture,
                                           -Data_pred, -Formula, -Model))) %>%
      {if(summary)
        tidyr::unnest(., DIest) %>%
          tidyr::unnest(Glance, keep_empty = TRUE) %>%
          dplyr::select(Site, Culture, {{extra_var}}, DV, IV, Est, Lwr, Upr)
        else .
      }
  }
}

