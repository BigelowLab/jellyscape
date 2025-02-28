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





summary_by_poly = function(x, y,
                           fun = ninenum,
                           na.rm = TRUE){
  
  #' Compute abundance summary across a set of polygons (like hexagons)
  #' 
  #' @param x spatial table of data with one attribute (plus geometry)
  #' @param y spatial polygons
  #' @parma fun the function to apply
  #' @param ba.rm TRUE to remove NAs before stats
  #' @return sf table of summary per polygon
  geom_col = attr(y, "sf_column")
  value = st_contains(y, x) |>
    lapply(
      function(ix, x = NULL){
        v = x |>
          dplyr::slice(ix) |>
          dplyr::pull(var = "value")
        fun(v, na.rm = na.rm) |>
          as.list() |>
          dplyr::as_tibble()
      }, x = x ) |>
    dplyr::bind_rows()
  
  dplyr::bind_cols(y,value) |>
    dplyr::relocate(dplyr::all_of(c("id", geom_col)), .after = dplyr::last_col())
}

summary_abundance = function(x = read_ecomon_spp(form = 'sf') |> ecomon_to_long(), 
                             y = read_hexbin(), 
                             fun = ninenum,
                             na.rm = TRUE,
                             shape = "wide"){
  
  #' Compute abundance summary across a set of polygons (like hexagons)
  #' 
  #' @param x spatial table of data with many groups identified by name
  #' @param y spatial polygons
  #' @parma fun the function to apply
  #' @param na.rm logical, remove NAs before stats?
  #' @param shape chr one of "long" (default) or "wide"
  #' @return sf table of aggregate per polygon
  
  geom_col = attr(y, "sf_column")
  r = x |>
    dplyr::select(dplyr::all_of(c("name", "value"))) |>
    dplyr::group_by(name) |>
    dplyr::group_map(
      function(tbl, key,  na.rm = NULL){
        summary_by_poly(tbl, y, fun = fun, na.rm = na.rm) |>
          dplyr::mutate(name = key$name, .before = 1) 
      }, na.rm = na.rm) |>
    dplyr::bind_rows() |>
    dplyr::relocate(dplyr::all_of(c("id", geom_col)), .after = dplyr::last_col())
  
  if (tolower(shape[1]) == "long"){
    r = longer_summary(r)
  }
  r
}




longer_summary = function(x, pattern = "^.*_10m2$"){
  #' Pivot a long-form summary table to wide
  #' 
  #' @param x the wide-form summary table
  #' @return pivoted table
  
  geom_col = attr(x, "sf_column")
  
  nms = if ("sum" %in% names(x)){
    c("n", "min", "q25", "median", "mean", "sdev", "q75", "max", "sum")
  } else if ("sdev" %in% names(x)){
    c("n", "min", "q25", "median", "mean", "sdev", "q75", "max")
  } else {
    c("n", "min", "q25", "median", "mean", "q75", "max")
  }
  tidyr::pivot_longer(x, 
                      cols = dplyr::all_of(nms), 
                      values_to = "value",
                      names_to = "sname") |>
    dplyr::relocate(dplyr::all_of(c("id", geom_col)), .after = dplyr::last_col())
}


write_summary = function(x, 
                         name = "summary",
                         path =  here::here("data", "summary")){
  #' Write a hexgrid
  #' 
  #' @param x sf hexgrid object with summary
  #' @param name chr, the name of the hex object
  #' @param path chr the directory where the file is written
  #' @return the input object `x`
  
  path = make_path(path)
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::write_sf(x, filename)
}

read_summary = function(name = "summary",
                        path =  here::here("data", "summary")){
  #' Read a hexgrid
  #' 
  #' @param name chr, the name of the hex object with summary
  #' @param path chr the directory where the file is written
  #' @return an object read by [sf::read_sf]
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::read_sf(filename)
}
