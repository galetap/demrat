#' Create a reference set of simulated skeletal samples
#'
#' Step 1: Function calculate life table of population based on Coale and Demeny West model.
#' Step 2: It applies a growth rate and calculate distribution of deaths in stable population.
#' Step 3: It randomly draws deaths from that distribution of deaths.
#' Step 4: It calculates age-at-death ratios from the distribution of deaths
#' (step 2 or 3, it depends on \code{sss} argument.
#' All three steps are iterated many times to obtain the reference dataset of simulated skeletal samples.

#' @param sss If \code{TRUE}, simulated skeletal samples are created as the reference.
#' If \code{FALSE}, reference consists from population data,
#' the step 3, in which simulated skeletal samples are created, is skipped.
#' @param samples Number of simulated skeletal samples to be created.
#' @param D20_raw D20+, number of adult skeletons (20+ years) in simulated skeletal samples.
#' @param e0_min Minimum value of life expectancy at birth in population from which skeletal samples are drawn.
#' @param e0_max Maximum value of life expectancy at birth in population from which skeletal samples are drawn.
#' @param growth_min Minimum value of annual growth rate in population from which skeletal samples are drawn.
#' @param growth_max Maximum value of annual growth rate in population from which skeletal samples are drawn.
#' @param extra_var Extra variables to be joined to results (e.g. description, absolute chronology of the site).
#' Extra variables must be in \code{dr_data} data frame.
#' @return A data frame with summary results.
#' @return Summary of function inputs:
#' \itemize{sss
#'   \item \strong{\code{SSS}}: Were simulated skeletal samples generated (\code{TRUE} or \code{FALSE})?
#'   See \code{sss} argument.
#'   \item \strong{\code{Samples}}: Total number of skeletal samples created (number of rows in results).
#'   \item \strong{\code{D20_raw}}: D20+, number of adult skeletons (20+ years) in simulated skeletal samples.
#'   If \code{sss} is \code{FALSE}, \code{D20_raw} is \code{NA}.
#'   \item \strong{\code{e0_min}}: Minimum value of life expectancy at birth of population from which skeletal samples were drawn.
#'   \item \strong{\code{e0_max}}: Maximum value of life expectancy at birth of population from which skeletal samples were drawn.
#'   \item \strong{\code{growth_min}}: Minimum value of annual growth rate of population from which skeletal samples were drawn.
#'   \item \strong{\code{growth_max}}: Maximum value of annual growth rate of population from which skeletal samples were drawn.
#'   \item \strong{\code{Alpha, Beta}}: Not applicable.
#' }
#' @return Demographic characteristic of reference population.
#' If \code{sss} is TRUE, this is a population from which a skeletal sample is drawn:
#' \itemize{
#'   \item \strong{\code{Growth}}: Annual growth rate randomly drawn
#'   from interval between \code{growth_min} and \code{growth_max}.
#'   \item \strong{\code{l27.5}}: The number of persons surviving to exact age 27.5,
#'   calculated from a life table.
#'   \item \strong{\code{e0}}: Life expectancy at birth.
#'   \item \strong{\code{CBR}}: Crude birth rate.
#'   \item \strong{\code{TFR}}:\code{ }Total fertility rate.
#' }
#' @return Characteristics of distribution of deaths
#' (number of deaths in the respective age-at-death interval and demographic ratios).
#' \itemize{
#'   \item \strong{\code{n}}: Total number of skeleton in skeletal sample
#'   \item \strong{\code{D0, D1_, ..., D5_19}: Number of individuals in age-at-death groups;
#'   D0+, D1+, ..., D5-19.
#'   \item \strong{\code{D1_D20_, ..., D5_D20_}}: Age-at-death ratios; D1+/D20+, ..., D5+/D20+.
#'   \item \strong{\code{JI}}: Juvenility index (D5-14/D20+).
#'   \item \strong{\code{P index}}: D5-19/D5+.
#'   \item \strong{\code{D0_14_D0_}}: D0-14/D0+ ratio.
#' }
#' @keywords simulation
#' @examples
#' simdr_CD()
#'
#' simdr_CD(sss=F)
#' @export


# Life table simulation & skeletal samples creation / Coale and Denemy ---------------------
simdr_CD <- function(sss=T, samples = 100, D20_raw = 50,
                     e0_min = 20, e0_max = 30,
                     growth_min = -4.0, growth_max = 8.0, ...) {

  # load("W_coef_Est.rdata")
  set.seed(123456789)
  n = samples #How many skeletal sample should be created
  D20_raw <- as.numeric(round(D20_raw, 0))
  growth_min=growth_min/100
  growth_max=growth_max/100

  # Standard life tables / Brass standard and Coale and Demeny West Model
  LTs <-
    W_coef_Est %>%
    dplyr::filter(e0>=e0_min,
                  e0<=e0_max) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(LT=list(tibble(Age=0:100))) %>%
    dplyr::mutate(LT=purrr::pmap(list(LT, a1, b1, a2, a3, b3),
                                 function(LT, a1, b1, a2, a3, b3)
                                 {LT %>%
                                     dplyr::mutate(lx=exp((-a1/b1)*(1-exp(-b1*Age))-
                                                            a2*Age+
                                                            a3/b3*(1-exp(b3*Age))))})) %>%
    dplyr::mutate(LT = map(LT, function(x)
    {x = x %>%
      dplyr::mutate(ax = c(0.3, rep(0.4, 4), rep(0.5, 96)),
                    lx_stac = lx,
                    dx_stac = c(lx_stac[1:100] - lx_stac[2:101], 0),
                    qx_stac = c(dx_stac[1:100] / lx_stac[1:100], 1),
                    Lx_stac = c(lx_stac[2:101]+ax[1:100]*(lx_stac[1:100]-lx_stac[2:101]), 0),
                    ex_stac = rev(cumsum(rev(Lx_stac)))/lx_stac)}))

  # Data frame definition
  iter = 0
  sim_summary <- data.frame(matrix(nrow = 0, ncol = 14)) #Coefficients of simulated samples
  names(sim_summary) <- c("SSS", "Samples", "D20_raw", "e0_min", "e0_max", "Growth_min", "Growth_max",
                          "Alpha", "Beta", "Growth", "l27.5", "e0", "CBR", "TFR")

  while (iter < n) {

    # Sample population growth
    growth_iter <- stats::runif(1, growth_min, growth_max)

    # Life table of simulated stable population
    LT <- within(LTs$LT[[sample(1:nrow(LTs), 1)]], {
      dx_stab <- exp(-growth_iter*(ax+Age))*dx_stac
      dx_cum <- cumsum(dx_stab/sum(dx_stab))
      dx_rstab <- dx_stab/sum(dx_stab)
      cx_stab <- exp(-growth_iter*(ax+Age))*Lx_stac
    }
    )

    iter = iter + 1

    sim_summary[iter, "Alpha"] <- NA
    sim_summary[iter, "Beta"] <- NA
    sim_summary[iter, "Growth"] <- growth_iter*100

    sim_summary[iter, "e0"] <- LT$ex_stac[1] #e0
    sim_summary[iter, "l27.5"] <- (LT$lx_stac[28] + LT$lx_stac[29]) / 2

    sim_summary[iter, "CBR"] <- 1000/sum(LT$cx_stab)
    sim_summary[iter, "TFR"] <-
      exp(growth_iter*27.5)/((100/205)*sim_summary[iter, "l27.5"])

    if(sss==T) {
      # Compute from simulated samples (sample level)
      # Create skeletal samples with specified value of D20+ (skeletons older than 20)
      # and sampled from LT population
      j = 0
      sim_skel_samp <-
        vector() #Simulated skeletal sample, empty vector

      # Age-at-death composition of simulated skeletal sample
      while (sum(sim_skel_samp >= 20) < D20_raw) {
        j = j + 1
        #Compare to cumulative deaths from Life Table and counts TRUE in the vector
        sim_skel_samp[j] <- sum(runif(1, 0, 1) > LT$dx_cum)
      } #End While

      # Assign results to data frame with summary
      sim_summary[iter, "n"] <- length(sim_skel_samp)
      sim_summary[iter, "D0"] <-
        sum(sim_skel_samp == 0, na.rm = TRUE)
      sim_summary[iter, "D1_"] <-
        sum(sim_skel_samp >= 1, na.rm = TRUE)
      sim_summary[iter, "D3_"] <-
        sum(sim_skel_samp >= 3, na.rm = TRUE)
      sim_summary[iter, "D5_"] <-
        sum(sim_skel_samp >= 5, na.rm = TRUE)
      sim_summary[iter, "D15_"] <-
        sum(sim_skel_samp >= 15, na.rm = TRUE)
      sim_summary[iter, "D20_"] <-
        sum(sim_skel_samp >= 20, na.rm = TRUE)

    } else {# Compute directly from life table (population level)

      sim_summary[iter, "n"] <- NA
      sim_summary[iter, "D0"] <- sum(LT$dx_rstab[LT$Age == 0])
      sim_summary[iter, "D1_"] <- sum(LT$dx_rstab[LT$Age >= 1])
      sim_summary[iter, "D3_"] <- sum(LT$dx_rstab[LT$Age >= 3])
      sim_summary[iter, "D5_"] <- sum(LT$dx_rstab[LT$Age >= 5])
      sim_summary[iter, "D15_"] <- sum(LT$dx_rstab[LT$Age >= 15])
      sim_summary[iter, "D20_"] <- sum(LT$dx_rstab[LT$Age >= 20])

    } #End if (sample!=NA)
  } #End While (iter < n)
  sim_summary <-
    sim_summary %>%
    dplyr::mutate(SSS:=sss,
                  Samples := !!samples,
                  e0_min := !!e0_min,
                  e0_max := !!e0_max,
                  Growth_min := !!growth_min*100,
                  Growth_max := !!growth_max*100) %>%
    dplyr::mutate(D20_raw = case_when(SSS==T ~ !!D20_raw)) %>%
    dplyr::mutate(D0_14 = ifelse(SSS==T, n-D15_, 1-D15_),
                  D5_14=D5_-D15_,
                  D5_19=D5_-D20_,
                  D1_D20_=D1_/D20_,
                  D3_D20_=D3_/D20_,
                  D5_D20_=D5_/D20_,
                  JI=D5_14/D20_,
                  P=D5_19/D5_,
                  D0_14_D0_ = ifelse(SSS==T, D0_14/n, D0_14))

  # Summary of simulations
  return(sim_summary)
}
