ninenum = function(x, na.rm = TRUE){
  
  #' Given a set of values compute the standard summary of 
  #' `[n, min, q25, median, mean, sdev, q75, max, sum]`.
  #' 
  #' @param x numeric vector of values
  #' @param na.rm logical, if TRUE remove NA values before computing summaries. If TRUE then
  #'  `n` will indicate the number of non-NA values in `x`, otherwise it will indicate
  #'  length of input `x`.
  #' @return named vector of summary values
  
  len = length(x)
  if (len == 0){
    eight = c(len, rep(NA_real_, 8))
  } else {
    len = if (na.rm) sum(!is.na(x)) else len
    s = sum(x, na.rm = na.rm)
    m = mean(x, na.rm = na.rm)
    std = sd(x, na.rm = na.rm)
    r = fivenum(x, na.rm = na.rm)
    eight = c(len, r[1:3], m, std, r[4:5], s)
  }
  eight |>
    rlang::set_names(c("n", "min", "q25", "median", "mean", "sdev", "q75", "max", "sum"))
}


eightnum = function(x, na.rm = TRUE){
  
  #' Given a set of values compute the standard summary of 
  #' `[n, min, q25, median, mean, sdev, q75, max]`.
  #' 
  #' @param x numeric vector of values
  #' @param na.rm logical, if TRUE remove NA values before computing summaries. If TRUE then
  #'  `n` will indicate the number of non-NA values in `x`, otherwise it will indicate
  #'  length of input `x`.
  #' @return named vector of summary values
  
  len = length(x)
  if (len == 0){
    eight = c(len, rep(NA_real_, 7))
  } else {
    len = if (na.rm) sum(!is.na(x)) else len
    m = mean(x, na.rm = na.rm)
    std = sd(x, na.rm = TRUE)
    r = fivenum(x, na.rm = na.rm)
    eight = c(len, r[1:3], m, std, r[4:5])
  }
  eight |>
    rlang::set_names(c("n", "min", "q25", "median", "mean", "sdev", "q75", "max"))
}



sevennum = function(x, na.rm = TRUE){
  
  #' Given a set of values compute the standard summary of 
  #' `[n, min, q25, median, mean, q75, max]`.
  #' 
  #' @param x numeric vector of values
  #' @param na.rm logical, if TRUE remove NA values before computing summaries. If TRUE then
  #'  `n` will indicate the number of non-NA values in `x`, otherwise it will indicate
  #'  length of input `x`.
  #' @return named vector of summary values
  
  len = length(x)
  if (len == 0){
    seven = c(len, rep(NA_real_, 6))
  } else {
    len = if (na.rm) sum(!is.na(x)) else len
    m = mean(x, na.rm = na.rm)
    r = fivenum(x, na.rm = na.rm)
    seven = c(len, r[1:3], m, r[4:5])
  }
  seven |>
    rlang::set_names(c("n", "min", "q25", "median", "mean", "q75", "max"))
}

