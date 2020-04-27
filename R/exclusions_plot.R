#' @title Plot with reasons for exclusion
#' @rdname create_exclusions_plot
#' @export
#' @importFrom dplyr filter count mutate
#' @importFrom ggplot2 labs theme element_blank element_text
#' @importFrom ggpubr ggdotchart theme_pubr
create_exclusions_plot <- function(screen) {

  # Assign variable names to all exclusion values
  noAA <- screen %>%
    filter(screenprocedure.factor == "No") %>%
    count()

  exclusion_age <- screen %>%
    filter(screenage.factor == "Yes") %>%
    count()

  exclusion_chronicoxygen <- screen %>%
    filter(screenchronicoxygen.factor == "Yes") %>%
    count()

  exclusion_hypercapnia <- screen %>%
    filter(screenhypercapnia.factor == "Yes") %>%
    count()

  exclusion_pneumothorax <- screen %>%
    filter(screenpneumothorax.factor == "Yes") %>%
    count()

  exclusion_tee <- screen %>%
    filter(screen_tee.factor == "Yes") %>%
    count()

  exclusion_nasalobstruction <- screen %>%
    filter(screennasalobstruction.factor == "Yes") %>%
    count()

  exclusion_airwaysurgery <- screen %>%
    filter(screenairwaysurgery.factor == "Yes") %>%
    count()

  exclusion_prior <- screen %>%
    filter(screenprior.factor == "Yes") %>%
    count()

  exclusion_priorrefusal <- screen %>%
    filter(screenpriorrefusal.factor == "Yes") %>%
    count()

  # Assign table variable names to all exclusion values
  exclusion_reasons <- c("screenage", "screenchronicoxygen",
                         "screenhypercapnia", "screenpneumothorax",
                         "screen_tee", "screennasalobstruction",
                         "screenairwaysurgery", "screenprior",
                         "screenpriorrefusal", "noAA")

  exclusions <- rbind(exclusion_age, exclusion_chronicoxygen,
                      exclusion_hypercapnia, exclusion_pneumothorax,
                      exclusion_tee, exclusion_nasalobstruction,
                      exclusion_airwaysurgery, exclusion_prior,
                      exclusion_priorrefusal, noAA)

  exclusions <- cbind(exclusion_reasons, exclusions)

  # Assign label names to all exclusion values
  exclusionnames <- c("Aged below 16 years", "Chronic oxygen requirements",
                      "Hypercapnia during admission", "Pneumothorax",
                      "TEE planned", "Nasal obstruction",
                      "Airway surgery/skull fracture",
                      "Prior participation in the trial",
                      "Prior refusal to participate", "Sedation not planned")

  exclusionsdf <- exclusions %>%
    mutate(exclusion_reasons = exclusionnames)

  # Plot of frequencies of all participant exclusion reasons
  exclusionsplot <- ggdotchart(exclusionsdf, x = "exclusion_reasons", y = "n",
                               color = "steelblue",
                               sorting = "descending",
                               add = "segments",
                               rotate = TRUE,
                               dot.size = 6,
                               label = exclusionsdf$n,
                               font.label = list(color = "white",
                                                 size = 9,
                                                 vjust = 0.5),
                               ggtheme = theme_pubr()) +
    theme(
      # plot.title = element_text(size = 20,
      #                           face = "bold",
      #                           margin = margin(t = 0, r = 0, b = 30, l = 0)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.caption = element_text(face = "italic")
    )

}
