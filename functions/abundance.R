tally_abundance_one = function(x, y, fun = sum,
                           transform = NULL, 
                           ...){

  #' Compute abundance across a set of polygons (like hexagons)
  #' 
  #' @param x spatial table of data with one attribute (plus geometry)
  #' @param y spatial polygons
  #' @param fun name of the function to apply
  #' @param ... other arguments for fun
  #' @param transform a function for transforming the results
  #' @return sf table of aggregate per polygon
  r = aggregate(x, sf::st_geometry(y), fun, ...)
  if (!inherits(r, "tbl_df")){
    crs = sf::st_crs(r)
    gcol = attr(r, "sf_column")
    r = dplyr::as_tibble(r) |>
      sf::st_as_sf(sf_column_name = gcol, crs  = crs)
  }
  if (!is.null(transform)) r[[1]] = transform(r[[1]] + 0.0001)
  r  
}



tally_abundance = function(x = read_ecomon_spp(form = 'sf') |> ecomon_to_long(), 
                           y = read_hexbin(), 
                           fun = sum,
                           transform = NULL, 
                           na.rm = TRUE){
  
  #' Compute abundance across a set of polygons (like hexagons)
  #' 
  #' @param x spatial table of data with many groups identified by name
  #' @param y spatial polygons
  #' @param fun name of the function to apply
  #' @param ... other arguments for fun
  #' @param transform a function for transforming the results
  #' @return sf table of aggregate per polygon
  
  
  x |>
    dplyr::select(dplyr::all_of(c("name", "value"))) |>
    dplyr::group_by(name) |>
    dplyr::group_map(
      function(tbl, key, fun = sum, transform = log1p, ...){
        tally_abundance_one(tbl, y, fun = fun, transform = transform) |>
          dplyr::mutate(name = key$name, .before = 1) 
      }, fun = fun, transform = transform, na.rm = na.rm) |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::relocate(geometry, .after = dplyr::last_col())
}