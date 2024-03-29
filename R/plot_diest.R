#' Plots prediction model
#'
#' The function visualizes an unique prediction model
#' used for a estimation of demographic parameters at a given Site.
#' In the plot, the age-at-death ratio (independent variable) is on the X axis,
#' the demographic variable (dependent variable) is on the Y axis.
#' The scatterplot contains the reference set of simulated skeletal samples, regression line
#' and the data for a given Site.
#' Function can be applied to a single skeletal sample only.
#' @param dr_data A data frame with one row containing data for a single cemetery sample.
#' Data frame must have four columns at minimum:
#' \code{Site}, \code{Culture}, \code{D20_} (i.e., D20+, number of adult skeletons in a sample)
#' and at least one out of the following three columns: \code{D1_D20_, D3_D20_, D5_D20_}
#' (i.e., D1+/D20+, D3+/D20+, D5+/D20+ age-at-death ratios, respectively).
#' If data frame contains total number of skeletons, its name should be n.
#' @param samples Number of simulated reference skeletal samples to be created.
#' @param e0_min Minimum value of life expectancy at birth in population from which skeletal samples are drawn.
#' @param e0_max Maximum value of life expectancy at birth in population from which skeletal samples are drawn.
#' @param growth_min Minimum value of annual growth rate in population from which skeletal samples are drawn.
#' @param growth_max Maximum value of annual growth rate in population from which skeletal samples are drawn.
#' @param IV Independent variable (the age-at-death ratio) used in the prediction. One or more items from \code{c("All", "D5_D20_", "D3_D20_", "D1_D20_", "P")}.
#' @param DV Dependent (demographic) variable to be estimated. One or more items from \code{c("All", "Growth", "TFR", "CBR")}.
#' @param base_size Base size of fonts in the plot.
#' @param ... Other parameters (see \code{diest})

#' @return Plot of the unique regression model used in the prediction
#' of the selected demographic variable from the selected age-at-death ratio.

#' @keywords demographic estimation

#' @examples
#' # Default plot (the relationship of Growth on the D5+/D20+ ratio)
#' BA %>%
#' dplyr::slice(12) %>%
#' plot_diest()
#'
#' # Plot of the relationship of TFR on the D5+/D20+ ratio
#' BA %>%
#' dplyr::slice(12) %>%
#' plot_diest(DV="TFR")
#'
#' # Changing parameters of the prediction model
#' BA %>%
#' dplyr::slice(12) %>%
#' plot_diest(samples=500, e0_min=25, e0_max=30)
#' @export



# Plot of the unique prediction model for a single Site ---------------------
plot_diest <- function(dr_data,
                       samples = 100,
                       e0_min = 18, e0_max = 25,
                       growth_min = -3.0, growth_max = 3.0,
                       IV = c("D5_D20_", "D3_D20_", "D1_D20_"),
                       DV = c("Growth", "TFR", "CBR"),
                       base_size = 9, ...){
  # Check arguments
  DV <- match.arg(DV)
  IV <- match.arg(IV)

  # Check if data has only one site
  if(nrow(dr_data)!=1)
    message("The data contains multiple sites, select only one site")
  else  {

    # Prediction results for the site
    diest =
      dr_data %>%
      # Prediction
      diest(summary = F,
            samples={{samples}},
            e0_min = {{e0_min}}, e0_max = {{e0_max}},
            growth_min = {{growth_min}}, growth_max = {{growth_max}},
            ...) %>%
      # Restrict results for selected DV (demogr. var.) and IV (ratio) only
      unnest(c(DIest)) %>%
      filter(DV=={{DV}}) %>%
      filter(IV=={{IV}})

    # Data for Site / number of skeletons in age-at-death groups, ratios, etc.
    dr_data =
      diest$dr_data[[1]] %>%
      {if("D5_19_D5_" %in% colnames(.))
        rename(., P="D5_19_D5_")
        else
          .}

    # Reference set of simulated skeletal samples
    Ref =
      diest$Ref[[1]] %>%
      as_tibble() %>%
      select(., {{IV}}, {{DV}}) %>%
      rename(x=1, y=2)

    # The name of the Site
    Site = diest[1, "Site"] %>% dplyr::pull()
    # Predicted value of DV
    Est =
      diest[1, "Est"] %>%
      pull()
    # Number of simulated reference skeletal samples
    Samples =
      diest$Ref[[1]] %>%
      slice(1) %>%
      select(Samples) %>%
      pull()
    # Ratio value at the Site
    Ratio =
      dr_data[1, IV] %>%
      pull()
    # 97.5th percentile of the ratio in the simulated skeletal samples
    Ratio_lim =
      diest[1, "Ratio_lim"] %>%
      pull()
    # Number of skeletons at the Site
    n = ifelse(c("n") %in% colnames(dr_data), dr_data[1, "n"] %>% dplyr::pull(),
               "Unknown number of")
    # Helper, units of the DV, used in the plot
    Units = ifelse(DV=="Growth", "%",
                   ifelse(DV=="TFR", "children", "births per 1000 ind."))
    # Helper, labels for IV, used in the plot
    IV_lab = ifelse(IV=="D5_D20_", "D5+/D20+",
                    ifelse(IV=="D3_D20_", "D3+/D20+", "D1+/D20+"))

    # Plot
    ggplot(Ref, aes(x = x, y = y)) +
      # Reference vertical line at the Site Ratio value
      geom_vline(xintercept = Ratio, linetype = 2, col = "#800000") +
      geom_segment(aes(x=Ratio, xend=Ratio, y=Est, yend=Inf), col = "white") +
      # Reference horizontal line at the predicted value
      geom_hline(yintercept = Est, linetype = 2, col = "#800000") +
      geom_segment(aes(x=Ratio, xend=Inf, y=Est, yend=Est), col = "white") +
      # Simulated reference skeletal samples
      geom_point(shape = 21, alpha=0.7, size=2) +
      # Regression line used for the prediction
      geom_smooth(method = "gam", col = "black",
                  se = F, alpha = 0.1, linewidth=0.7) +
      # Plot title, subtitle, x and y
      labs(title = paste0("Regression model for predicting ", DV, " from the ", IV_lab, " ratio"),
           subtitle = paste0("Site: ", Site, "; ", n, " total skeletons", "; ", dr_data$D20_, " adult skeletons"),
           x=IV_lab,
           y = paste0(DV, " (", Units, ")")) +
      # Plot caption (depends on whether Ratio is higher than Ratio limit)
      {if(Ratio>=Ratio_lim)
        labs(caption = paste0("Points represent the reference set of ", Samples, " simulated skeletal samples.\n",
                              "The ratio at the site is outside the limits of the reference set.\n",
                              "Prediction may be not reliable."))
        else
          labs(caption = paste("Points represent the reference set of", Samples, "simulated skeletal samples."))
      } +
      # If DV is Growth or CBR (is not TFR), then X axis is log-transformed
      {if(DV=="TFR")
        scale_x_continuous(breaks = seq(1, 3.0, 0.2))
        else
          scale_x_continuous(trans = log_trans(), breaks = seq(1, 3.0, 0.2))
      } +
      # If DV is TFR, then Y axis is log-transformed
      {if(DV=="TFR")
        scale_y_continuous(trans = log_trans(), breaks = c(0, 2, 3, 4, 5, 6, 8, 10, 12))
      } +
      # Site information
      geom_text(aes(x = Ratio*1.01, y = min(Ref[,"y"]),
                    label = paste0(Site,"\n", IV_lab, " = ",
                                   number_format(0.01)(Ratio),
                                   "\n", DV, " = ",
                                   number_format(0.1)(Est),
                                   ifelse(Units=="%",""," "), Units)),
                hjust = 0, vjust = -0.1, size = 5/14*base_size, col = "#800000") +
      theme_classic() +
      theme(axis.title = element_text(size = base_size),
            plot.title = element_text(size = base_size+1, face = "bold", hjust = 0),
            plot.subtitle = element_text(size = base_size, hjust = 0),
            plot.caption = element_text(size = base_size-1, hjust = 1))
  }
}
