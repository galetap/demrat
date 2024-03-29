#' Estimates growth and fertility rate for a single cemetery sample (helper function)
#'
#' Estimates growth, crude birth and total fertility rates
#' based on age-at-death ratios (e.g., D5+/D20+)
#' computed in cemetery sample.
#' Helper function, does not show results until they are saved to an R object
#' @param dr_data A data frame, each row contains data for a single cemetery sample.
#' Data frame must have two columns at minimum:
#' \code{D20_} (i.e., D20+, number of adult skeletons in a sample)
#' and at least one of the following three columns: \code{D1_D20_, D3_D20_, D5_D20_}
#' (i.e., D1+/D20+, D3+/D20+, D5+/D20+ age-at-death ratios, respectively).
#' @param glanced If \code{TRUE}, resulting tibble is glanced.
#' @param pred_level Confidence of the prediction, default is 0.95.
#' @param extra_var Extra variables to be joined to results (e.g. description, absolute chronology of the site).
#' Extra variables must be in \code{dr_data} data frame.
#' @param ... Other parameters (see \code{simdr_CD})
#' @return Data frame containing estimation of demographic indicators of population
#' from which a cemetery sample was drawn. See \code{diest()} for details.
#' @keywords demographic estimation
#' @examples
#' res <-
#' BAraw %>%
#' dr() %>%
#' dplyr::slice(1) %>%
#' diest_fn()
#'
#' res
#'
#' @export


diest_fn <- function(dr_data, glanced = F, pred_level = 0.95, extra_var, ...){
  site =
    dr_data %>%
    # Is there D5_19_D5_ column?
    {if("D5_19_D5_" %in% colnames(.))
      rename(., "P"="D5_19_D5_")
      else
        .}

  # Rid off columns where all values are NAs
  site =
    site %>%
    select(where(~sum(!is.na(.x)) > 0))

  # Does dataframe have only one row?
  if(nrow(site)!=1)
    message("Dataframe must contain only one row")
  else
    # Does dataframe contain D20_ column?
    if(!("D20_" %in% names(site)))
      message("Dataframe must contain D20_ column with nonNA value")
  else
    # # Does dataframe contain at least one of following columns?
    if(sum(c("D5_D20_", "D3_D20_", "D1_D20_") %in% names(site))==0)
      message("Dataframe must contain at least one of these columns with nonNA values: D5_D20_, D3_D20_ or D1_D20_")
  else

    # Helper df with Site, Culture and extra_var (add_column, see later)
    extra_var_sel <-
    site %>%
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
    dplyr::select(Site, Culture, {{extra_var}})

  # Reference sample of simulated skeletal samples with same D20_ value
  ref = simdr_CD(D20_raw=site$D20_, ...)

  # Results
  res <-
    tibble(DV=c(rep("TFR", 3), rep("CBR", 3), rep("Growth", 3)),
           IV=c("D5_D20_", "D3_D20_", "D1_D20_",
                "D5_D20_", "D3_D20_", "D1_D20_",
                "D5_D20_", "D3_D20_", "D1_D20_"),
           # Regression formulas used to DI estimation
           Formula = c(log(TFR) ~ s(D5_D20_, bs="cs"),
                       log(TFR) ~ s(D3_D20_, bs="cs"),
                       log(TFR) ~ s(D1_D20_, bs="cs"),
                       CBR ~ s(log(D5_D20_), bs="cs"),
                       CBR ~ s(log(D3_D20_), bs="cs"),
                       CBR ~ s(log(D1_D20_), bs="cs"),
                       Growth ~ s(log(D5_D20_), bs="cs"),
                       Growth ~ s(log(D3_D20_), bs="cs"),
                       Growth ~ s(log(D1_D20_), bs="cs"))) %>%

    # Filter only those DVs that are inlcuded in dataframe
    filter(IV %in% intersect(c("D5_D20_", "D3_D20_", "D1_D20_"),
                             names(site))) %>%

    # Column with reference sample data (same for all lm models)
    mutate(Ref=list(ref)) %>%

    # gam models and its characteristics (Glance)
    mutate(Model = map2(Formula, Ref,
                        ~gam(formula = .x, method="REML", data = .y))) %>%

    # Glance gam model / handy calculation by PG, no official glance() for gam model
    mutate(Glance = map(.x = Model,
                        .f = ~tibble(adj.r.squared = summary(.x)$r.sq,
                                     sigma = sqrt(summary(.x)$scale),
                                     statistic = summary(.x)$s.table[3],
                                     p.value = summary(.x)$s.table[4],
                                     df.res = glance(.x)$df.residual,
                                     AIC = glance(.x)$AIC,
                                     BIC = glance(.x)$BIC,
                                     deviance = glance(.x)$deviance,
                                     deviance_expl = summary(.x)$dev.expl))) %>%

    # Data for prediction (copy od site argument)
    mutate(Data_pred = list(site)) %>%

    # Prediction for site
    # Fit a se.fit
    mutate(Pred = map2(Model, Data_pred,
                       ~predict(.x, newdata = .y, se=T) %>% as_tibble())) %>%
    # SEE a df
    mutate(Pred = map2(.x = Pred, .y = Glance,
                       .f = ~.x %>% mutate(sigma=.y$sigma,
                                           df.res=.y$df.res))) %>%
    # Margin of prediction, Lwr, Upr
    mutate(Pred = map(.x = Pred,
                      .f = ~.x %>%
                        mutate(pred.margin= sqrt(se.fit^2 + sigma^2)*
                                 qt(pred_level+(1-pred_level)/2, df.res)) %>%
                        mutate(Lwr=fit-pred.margin,
                               Upr=fit+pred.margin))) %>%
    # Rename, select
    mutate(Pred = purrr::map(.x = Pred,
                             .f = ~.x %>%
                               rename(Est=fit) %>%
                               select(Est, Lwr, Upr))) %>%

    # Back transformation of prediction (for TFR only)
    mutate(Pred = ifelse(DV=="TFR", map(Pred, exp), Pred)) %>%

    # Nest or unnest Glance column
    {if (glanced)
      unnest(., c(Pred, Glance))
      else
        unnest(., Pred)
    } %>%

    # Add estimation based on Bocquet-Appel 2002 regression formula
    {if("P" %in% names(site))
      bind_rows(., list(DV="CBR", IV="P",
                        Ref=list(ref), Data_pred = list(site),
                        Est=1000*as.double(0.00375 + 0.15334*(site$P^0.89074)))) %>%
        bind_rows(list(DV="Growth", IV="P",
                       Ref=list(ref), Data_pred = list(site),
                       Est=100*as.double(-0.05389 + 0.12555*(site$P^0.47788))))
      else  .
    } %>%

    # Compute 97.5% limit of the ratios in the reference samples
    mutate(Ratio_lim = map(.x = Ref,
                           .f = ~ .x %>%
                             select(D1_D20_, D3_D20_, D5_D20_, P) %>%
                             summarise(across(.cols = everything(),
                                              .fns = ~quantile(., probs=0.975))) %>%
                             pivot_longer(cols = everything(),
                                          names_to = "Ratio", values_to = "U_lim"))) %>%
    # Filter the limit for IV used in the particular row only
    mutate(Ratio_lim = map2_dbl(.x = Ratio_lim, .y = IV,
                                .f = ~ .x %>%
                                  filter(Ratio==.y) %>%
                                  select(U_lim) %>%
                                  pull())) %>%

    # Is the ratio higher than its 97.5% limit of the set of reference samples?
    mutate(Ratio_eval =
             pmap_chr(list(IV, Data_pred, Ratio_lim),
                      function(IV, Data_pred, Ratio_lim)
                      {ifelse(Data_pred %>%
                                select(tidyselect::all_of(IV)) %>%
                                pull() >
                                Ratio_lim,
                              "Out of limits", "Normal")})) %>%

    # Round results (avoid scientific format)
    mutate(dplyr::across(Est:Upr, ~round(.x, 4))) %>%
    add_column(extra_var_sel, .before = 1)

  invisible(res)
}
