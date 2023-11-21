#' Estimates growth and fertility rate for skeletal samples
#'
#' Estimates growth, crude birth and total fertility rates based on age-at-death ratios
#' (e.g., D5+/D20+) summarising distribution of deaths in a cemetery sample.
#' Function can be applied to several cemetery samples at once.
#' @param dr_data A data frame with one row containing data for a single cemetery sample.
#' Data frame must have four columns at minimum:
#' \code{Site}, \code{Culture}, \code{D20_} (i.e., D20+, number of adult skeletons in a sample)
#' and at least one out of the following three columns: \code{D1_D20_, D3_D20_, D5_D20_}
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
#' @param IV Independent variable (the age-at-death ratio) used in the prediction. One or more items from \code{c("All", "D5_D20_", "D3_D20_", "D1_D20_", "P")}.
#' @param DV Dependent (demographic) variable to be estimated. One or more items from \code{c("All", "Growth", "TFR", "CBR")}.
#' @param extra_var Extra variables to be joined to results (e.g. description, absolute chronology of the site).
#' Extra variables must be in \code{dr_data} data frame.
#' @param ... Other parameters (see \code{simdr_CD} and \code{diest_fn})

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
#'   \item \strong{\code{Ratio_lim}}: 97.5 percentile of the age-at-death ratio
#'   computed in the set of simulated reference skeletal samples.
#'   \item \strong{\code{Ratio_eval}}: \code{Out of limits} if the age-at-death ratio observed in the real skeletal sample is higher than the Ratio_lim.
#'   In this case, the ratio may be biased and estimation may be unreliable.
#' }
#' @keywords demographic estimation
#' @examples
#' # Demographic estimation based on the original Bocquet-Appel (2002, Table 1) data
#' BA %>%
#' slice(5, 12) %>%
#' diest()
#'
#' # Demographic estimation based on the reconstructed Bocquet-Appel (2002) raw data
#' BAraw %>%
#' dr() %>%
#' slice(1:2) %>%
#' diest()
#'
#' # Select predicted demographic variables and predictors that are showed in the results
#' BA %>%
#' slice(12) %>%
#' diest(summary = T, IV=c("D5_D20_"), DV=c("Growth", "TFR"))
#'
#' # Adding extra variables
#' BA %>%
#' slice(5, 12) %>%
#' diest(extra_var = c(Front, C14, dt),
#' growth_min = -2, growth_max = 2)
#' @export



# Demographic indicator estimation for multiple site ---------------------
# Each row of dr_data is dr for individual site
diest <- function(dr_data, summary=T, pred_level=0.95,
                  sss=T, samples = 100,
                  e0_min = 18, e0_max = 25,
                  growth_min = -3.0, growth_max = 3.0,
                  IV = c("All", "D5_D20_", "D3_D20_", "D1_D20_", "P"),
                  DV = c("All", "Growth", "TFR", "CBR"),
                  extra_var, ...){

  # Match input parameters
  IV <- match.arg(IV, several.ok = T)
  DV <- match.arg(DV, several.ok = T)

  # Argument glanced must be F to let function work
  if(sum(c("Site", "Culture", "D20_") %in% names(dr_data))!=3 |
     sum(c("D1_D20_", "D3_D20_", "D5_D20_") %in% names(dr_data))==0)
    message("Input data frame must contain columns Site, Culture, D20_ and at least one out of the following three columns: D1_D20_, D3_D20_, D5_D20_ (see help(diest)).")
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
          dplyr::select(Site, Culture, {{extra_var}}, DV, IV, Est, Lwr, Upr, Ratio_eval) %>%
          # Filter only selected DV/demographoc indicators
          {if("All" %in% {{DV}})
            .
            else
              dplyr::filter(., DV %in% {{DV}})
          } %>%
          # Filter only selected IV/predictors
          {if("All" %in% {{IV}})
            .
            else
              dplyr::filter(., IV %in% {{IV}})
          }
        else .
      }

  }
}
