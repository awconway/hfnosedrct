#' @title Probability above/in/below threshold
#' @rdname prob_threshold
#' @param model_obj thresh pars
#' @param response peak or mean
#' @export
#'
#' @importFrom brms posterior_samples

prob_threshold <- function(model_obj,
                           thresh = 0,
                           pars="randomization_factor"){

  post <- posterior_samples(model_obj, pars="randomization_factor") %>% unlist()

  result <- list()

  result[["above"]] <- sum(post>thresh)/length(post)
  result[["between"]] <- sum(abs(post) < thresh)/length(post)
  result[["below"]] <- sum(post< -thresh)/length(post)

  result <- lapply(result, function(x) sprintf("%.2f", x))

  return(result)

}
