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

which_period = function(x, choices= c("month", "year", "decade")){
  
  choices[choices %in% colnames(x)]
}


plot_summary_by_poly = function(x, 
                                ids = 42,
                                by = which_period(x)){
  #' Plot the anomalies for a specified polygon(s)
  #' @param x anomalies table by month or decade
  #' @param ids num, one or more ids to plot
  #' @param by chr either "decade" or "year" but auto detected from `x` variable names
  bySym = rlang::sym(by)
  
  y = longer_summary(x) |>
    sf::st_drop_geometry() |>
    dplyr::filter(id %in% ids,
                  sname %in% c("mean", "median", "q75", "max")) |>
    dplyr::mutate(sname = factor(.data$sname, levels = c("max", "q75", "median", "mean")))

  if (by == "month") y = dplyr::mutate(y, month = as.numeric(.data$month))
  
  gg = ggplot2::ggplot(data = y,
                  mapping = aes(x = {{bySym}}, y = value, color = sname)) +
    geom_line() + 
    facet_wrap(~ id + name)
  
  if (by == "month") gg = gg + ggplot2::scale_x_continuous(breaks = 1:12, 
                                                           labels = substring(month.abb,1,1))
  
  gg
  
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



compute_summary = function(x = read_ecomon_spp(form = 'sf', post = "long-extras") ,
                           by = c("month", "decade", "longterm")[1],
                           y = read_hexgrid(),
                           ...){
  #' Compute a summary for various intervals ("month", "decade", etc)
  #' @param x long form data data with 'month', 'year' and 'decade'
  #' @param by chr the interval for summarizing
  #' @param y the polygons (likely hexgrid) over which to compute
  #' @param ... arguments for summary_by_poly
  #' @return spatial summary table by polygon
  
  r = if (tolower(by[1] == "longterm")){
    summary_abundance(x, y = y, ...)
  } else {
    x |> 
      dplyr::group_by(name, across(all_of(by))) |>
      dplyr::group_map(
        function(tbl, key){
          summary_by_poly(tbl, y, ...) |>
            dplyr::mutate(name = key$name, 
                          #{{bySym}} := key[[by]], 
                          !!by := key[[by]],
                          .before = 1)
        }  ) |>
      dplyr::bind_rows()
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
