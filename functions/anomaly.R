monthly_anomaly = function(x, y,
                           summa = compute_summary(x, y, by = "month"),
                           ...){
  
  #' Compute anomalies for each month
  #' @param x long form spatial table of observations with extras
  #' @param y spatial table of polygons over which anomalies are computed
  #' @param summa summary table for each polygon in `y`
  #' @param ... other arguments for [compute_summary].  Only used if 
  #'   summa is NULL
  #' @return spatial table of monthly anomalies
  
  
  if (is.null(summa)) summa = compute_summary(x, y, by = "month", ...)
  x = dplyr::mutate(x, 
                    monthDate = as.Date(paste(year, month, "01", sep = "-"),
                                        format = "%Y-%b-%d"))
  geom_col = attr(y, "sf_column")
  
  x |>
    dplyr::group_by(name, monthDate) |>
    dplyr::group_map(
      function(tbl, key){
        m = tally_by_poly(tbl, y, fun = mean, na.rm = TRUE) |>
          dplyr::pull(var = 1)
        mon = format(key$monthDate, "%b")
        s = summa |>
          dplyr::filter(name == key$name, month == mon)
        dplyr::mutate(y,
                      name = key$name,
                      date = key$monthDate,
                      anomaly = (m - s$mean)/s$sdev, 
                      .before = dplyr::all_of(geom_col)) |>
          dplyr::select(dplyr::all_of(c("name", "date", "anomaly", "id")))
      }
    ) |> 
    dplyr::bind_rows()

}


heatmap_monthly_anomaly_by_poly = function(x, spp = "total_10m2", ids = 42,
                                           zlim = c(-2,2)){
  #' Plot the anomalies for a specified polygon
  #' @param x anomalies table by month
  #' @param spp chr one or more names of species
  #' @param ids the id of the polygon to plot
  #' @param zlim num, a two element vector specifying the color range in st dev units
  
  x = x |> 
    dplyr::mutate(year = format(.data$date, "%Y") |> as.numeric(),
                  month = format(.data$date, "%b") |> factor(levels = month.abb) )
  years = unique(x$year) |> sort()
  extra = c(-1,1)
  ggplot(data = x |> 
           st_drop_geometry() |>
           filter(name %in% spp, id %in% ids),
         mapping = aes(month, year)) + 
    geom_tile(aes(fill = anomaly), colour = "white") +
    lims(y = rev(range(years) + extra)) + 
    labs(title = sprintf("spp: %s, polygon: %s",
                         paste(spp, collapse = ", "), 
                         paste(ids, collapse = ", "))) +
    scale_fill_distiller(palette = "RdYlBu", na.value = NA, 
                         type = "div", limits = zlim)
  
}


anom_time_col = function(x = read_anomaly(name = "anom_mean_decade"),
                         choices = c("year", "decade")){
  #' Guess the correct anomaly interval
  #' @param x sf table of anomalies with zero or more columns matching zero or more `choices`
  #' @param choices chr vector of possible choices
  #' @return chr the first match (possibly empty)
  match.arg(names(x), choices, several.ok = TRUE)[1]
}


compute_anomaly = function(x = read_ecomon_spp(form = 'sf', post = "long-extras"), 
                           y = read_hexgrid(), 
                           summa = NULL,
                           by = c("year", "decade")[1],
                           ...){
  
  #' Compute anomalies for the specified interval
  #' @param x long form spatial table of observations with extras
  #' @param y spatial table of polygons over which anomlaies are computed
  #' @param summa summary table for each polygon in `y`
  #' @param by chr, one of "year" or "decade"
  #' @param ... other arguments for [compute_summary].  Only used if 
  #'   summa is NULL
  #' @return spatial table of anomalies
  
  if (is.null(summa)) summa = compute_summary(x, y = y, by = by, ...)

  bySym = rlang::sym(by)
  geom_col = attr(y, "sf_column")
  x |>
    group_by(name, month, dplyr::across(dplyr::all_of(by)) ) |>
    group_map(
      function(tbl, key){
        m = tally_by_poly(tbl, y, fun = mean, na.rm = TRUE) |>
          dplyr::pull(var = 1)
        s = summa |>
          dplyr::filter(name == key$name, {{bySym}} == key[[by]])
        dplyr::mutate(y,
                      name = key$name,
                     # {{by}} := key[[by]],
                      !!by := key[[by]],
                      month = key$month,
                      anomaly = (m - s$mean)/s$sdev, 
                      .before = dplyr::all_of(geom_col)) |>
          dplyr::select(dplyr::all_of(c("name", by, "month", "anomaly", "id")))
        
      }
    ) |>
    dplyr::bind_rows() 
}

plot_anomaly_by_poly = function(x, 
                                ids = 42,
                                spp = "total_10m2",
                                by = if ("decade" %in% colnames(x)) "decade" else "year",
                                zlim = c(-2,2)){
  #' Plot the anomalies for a specified polygon
  #' @param x anomalies table by month or decade
  #' @param ids one or more polygons ids
  #' @param spp chr one or more names of species
  #' @param by chr either "decade" or "year" but auto detected from `x` variable names
  #' @param zlim num, a two element vectoir specifying the color range in st dev units
  extra = if(by == "decade") c(-5, 5) else c(-1,1)
  dec = unique(x[[by]]) |> as.numeric() |> sort() 
  ggplot(data = x |> 
           st_drop_geometry() |>
           filter(name %in% spp, id %in% ids) |>
           mutate(decade = as.numeric(decade)),
         mapping = aes(month, decade)) + 
    geom_tile(aes(fill = anomaly), colour = "white") +
    lims(y = rev(range(dec) + extra)) + 
    labs(title = sprintf("spp: %s, interval: decade, polygon: %s",
                         paste(spp, collapse = ", "), 
                         paste(ids, collapse = ", "))) +
    scale_fill_distiller(palette = "RdYlBu", na.value = NA, 
                      type = "div", limits = zlim)
}

write_anomaly = function(x, 
                         name = "anomaly",
                         path =  here::here("data", "anomaly")){
  #' Write a hexgrid
  #' 
  #' @param x sf hexgrid object
  #' @param name chr, the name of the hex object
  #' @param path chr the directory where the file is written
  #' @return the input object `x`
  
  path = make_path(path)
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::write_sf(x, filename)
}

read_anomaly = function(name = "anomaly",
                        path =  here::here("data", "anomaly")){
  #' Read a hexgrid
  #' 
  #' @param name chr, the name of the hex object
  #' @param path chr the directory where the file is written
  #' @return an object read by [sf::read_sf]
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::read_sf(filename)
}