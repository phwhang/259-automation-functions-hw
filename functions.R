plus_minus_SD <- function(vec, num_of_SDs = 1) {
  vec_mean <- mean(vec)
  vec_sd <- sd(vec)
  boundary <- num_of_SDs * vec_sd

  lower <- vec_mean - boundary
  upper <- vec_mean + boundary

  return(c(lower = lower, upper = upper))
}

limit_replace <- function(vec, lower = NULL, upper = NULL) {
  stopifnot(is.numeric(vec))
  stopifnot(length(vec) > 0)

  if (is.null(lower) | is.null(upper)) {
    boundaries <- plus_minus_SD(vec, 1)
    lower <- boundaries["lower"]
    upper <- boundaries["upper"]
  }

  vec <- ifelse(vec < lower | vec > upper, NA, vec)

  return(vec)
}


