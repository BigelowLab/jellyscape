anom_time_col = function(x = read_anomaly(name = "anom_mean_decade")){
  choices = c("year", "decade")
  match.arg(names(x), choices, several.ok = TRUE)[1]
}

anom_matrix_by_poly = function(x = read_anomaly(name = "anom_mean_decade") |>
                                 dplyr::filter(name == "total_10m2")){
  ids = unique(x[["id"]]) |> sort()
  time_col = anom_time_col(x)
  time = unique(x[[time_col]]) |> sort()
  months = month.abb
  m = matrix(NA_real_, nc = length(months), nr = length(time),
             dimnames = list(time, months) |>
               rlang::set_names(c(time_col, "month")))

  z = lapply(ids,
         function(i, mat = NULL){
            for (j in time){
              for (k in months){
                v = dplyr::filter(x, 
                                  id == i, 
                                  .data[[time_col]] == j,
                                  month == k) |>
                  dplyr::pull(dplyr::all_of("anomaly"))
                mat[j,k] = ifelse(length(v) == 0, NA_real_, v)
              } # k-month
            } # j-time
            dplyr::tibble(id = i, n = sum(!is.na(mat)), mat = list(mat))
           }, mat = m) |>
    dplyr::bind_rows()
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