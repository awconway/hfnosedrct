#' @title CONSORT diagram
#' @rdname create_consort_png
#' @param screen dataframe of screening inclusions and exclusions
#' @param trial dataframe of participants
#' @export
#' @importFrom diagram plotmat
#' @importFrom dplyr filter
#' @importFrom Matrix as.matrix
#' @importFrom utils read.table
create_consort_png <- function(screen, trial) {

  # Participant values
  screened = nrow(screen)
  excluded = nrow(filter(screen,
                         screen_eligible.factor == "No" |
                           screeninvited.factor == "No" |
                           screenconsented.factor == "No"))
  criteria_not_met = nrow(filter(screen, screen_eligible.factor == "No"))
  not_invited = nrow(filter(screen,
                            screen_eligible.factor == "Yes",
                            screeninvited.factor == "No"))
  declined = nrow(filter(screen,
                         screeninvited.factor == "Yes",
                         screenconsented.factor == "No"))
  randomized = nrow(trial)
  facemask = nrow(filter(trial, randomization.factor == "Face mask oxygen"))
  hfno = nrow(filter(trial, randomization.factor == "High Flow nasal oxygen"))
  received_hfno = nrow(filter(trial,
                              randomization.factor == "High Flow nasal oxygen",
                              oxygenbaselineflow >= 30 |
                                oxygenchangeflow1 >= 30 |
                                oxygenchangeflow2 >= 30 |
                                oxygenchangeflow3 >= 30 |
                                oxygenchangeflow4 >= 30))
  received_facemask = facemask + hfno - received_hfno
  died_facemask = 0
  died_hfno = 0
  lost_facemask = died_facemask
  lost_hfno = died_hfno
  no_procedure = 1

  # P084 is not included in the data, as their procedure was not performed
  analyzed_facemask = facemask - no_procedure
  analyzed_hfno = hfno

  # Participant categories
  b = c("Enrollment", "Allocation", "Follow-up", "Analysis")
  l1 = paste("Assessed for eligibility (n=", screened, ")", sep = "")
  l2 = paste("Excluded (n=", excluded, ")\n",
             "•   Not meeting inclusion criteria (n=", criteria_not_met, ")\n",
             "•   Not invited to participate (n=", not_invited, ")\n",
             "•   Declined to participate (n=", declined, ")", sep = "")
  l3 = paste("Randomized (n=", randomized, ")", sep = "")
  l4 = paste("Allocated to facemask oxygen (n=", facemask, ")\n",
             "• Received facemask oxygen (n=",
             received_facemask,
             ")",
             sep = "")
  l5 = paste("Allocated to high flow nasal oxygen (n=", hfno, ")\n",
             "• Received high flow nasal oxygen (n=",
             received_hfno,
             ")",
             sep = "")
  l6 = paste("Lost to follow-up (n=", lost_facemask, ")\n",
             "•   Died (n=", died_facemask, ")", sep = "")
  l7 = paste("Lost to follow-up (n=", lost_hfno, ")\n",
             "•   Died (n=", died_hfno, ")", sep = "")
  l8 = paste("Analyzed (n=", analyzed_facemask, ")", sep = "")
  l9 = paste("Analyzed (n=", analyzed_hfno, ")", sep = "")
  labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, b)
  n.labels = length(labels)

  # Text boxes for categories
  frame = read.table(sep = "\t",
                     stringsAsFactors = F,
                     skip = 0,
                     header = T,
                     text = "
                     i	x	y	box.col	box.type	box.prop	box.size
                     1	0.5	0.94	white	square	0.25	0.16
                     2	0.76	0.82	white	square	0.28	0.21
                     3	0.5	0.7	white	square	0.25	0.15
                     4	0.26	0.5	white	square	0.23	0.2
                     5	0.76	0.5	white	square	0.21	0.225
                     6	0.26	0.33	white	square	0.2	0.2
                     7	0.76	0.33	white	square	0.2	0.2
                     8	0.26	0.15	white	square	0.2	0.2
                     9	0.76	0.15	white	square	0.2	0.2
                     10	0.1	0.95	light blue	round	0.7	0.035
                     11	0.51	0.6	light blue	round	0.7	0.035
                     12	0.51	0.411	light blue	round	0.7	0.035
                     13	0.51	0.235	light blue	round	0.7	0.035")
  pos = as.matrix(subset(frame, select = c(x, y)))
  M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
  M[3, 1] = "' '"
  M[4, 3] = "' '"
  M[5, 3] = "' '"
  M[6, 4] = "' '"
  M[7, 5] = "' '"
  M[8, 6] = "' '"
  M[9, 7] = "' '"
  tcol = rep("black", n.labels)
  to.blank = c(2, 4:9)
  x_coord = pos[to.blank, 1]
  y_coord = pos[to.blank, 2]
  indent_labels = labels[to.blank]
  indent = c(0.185, 0.185, 0.21, 0.185, 0.185, 0.185, 0.185)
  tcol[to.blank] = "transparent"
  # postscript('consort.flow.eps', width = 7.5, height = 7, horiz = F)
  # tiff('consort.flow.tif',
  #      width = 7.5,
  #      height = 7,
  #      units = 'in',
  #      res = 300,
  #      compression = 'lzw')
  png('plots/consort.png', width = 7.5, height = 7, units = 'in', res = 300)

  par(mai = c(0, 0, 0, 0))
  plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size = 0, curve = 0,
          box.lwd = 2, cex.txt = 1, box.size = frame$box.size,
          box.col = frame$box.col, box.type = frame$box.type,
          box.prop = frame$box.prop, txt.col = tcol)

  # Position alignment of text inside boxes
  for (i in 1:7){
    text(x = x_coord[i] - indent[i],
         y = y_coord[i],
         adj = c(0, 0.5),
         labels = indent_labels[i])
  }

  # Arrows to connect boxes
  arrows(x0 = 0.5, x1 = 0.55, y0 = 0.82, length = 0.12)
  dev.off()

}


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
