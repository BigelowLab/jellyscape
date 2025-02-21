tally_by_poly = function(x, y, fun = sum,
                         na.rm = TRUE,
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
    
    value= st_contains(y, x) |>
      sapply(
        function(ix, x = NULL){
          r = if (length(ix) == 0) {
            NA_real_
          } else {
            x |>
              dplyr::slice(ix) |>
              dplyr::pull(var = "value") |>
              fun(na.rm = na.rm)
          }
        }, x = x )
    
    if(!is.null(transform)) value = transform(value)
    
    y |>
      dplyr::mutate(value = value, .before = 1)
}



tally_abundance = function(x = read_ecomon_spp(form = 'sf') |> ecomon_to_long(), 
                           y = read_hexbin(), 
                           fun = sum,
                           transform = NULL, 
                           na.rm = TRUE,
                           shape = "long"){
  
  #' Compute abundance across a set of polygons (like hexagons)
  #' 
  #' @param x spatial table of data with many groups identified by name
  #' @param y spatial polygons
  #' @param fun name of the function to apply
  #' @param ... other arguments for fun
  #' @param transform a function for transforming the results
  #' @param shape chr one of "long" (default) ior "wide"
  #' @return sf table of aggregate per polygon
  
  geom_col = attr(y, "sf_column")
  r = x |>
    dplyr::select(dplyr::all_of(c("name", "value"))) |>
    dplyr::group_by(name) |>
    dplyr::group_map(
      function(tbl, key, fun = NULL, transform = NULL, na.rm = NULL){
        tally_by_poly(tbl, y, fun = fun, transform = transform, na.rm = na.rm) |>
          dplyr::mutate(name = key$name, .before = 1) 
      }, fun = fun, transform = transform, na.rm = na.rm) |>
    dplyr::bind_rows() |>
    dplyr::relocate(dplyr::all_of(geom_col), .after = dplyr::last_col())
  
    if (tolower(shape[1]) == "wide"){
      r = wider_abundance(r)
    }
  r
}

wider_abundance = function(x){
  #' Pivot a long-form abundance table to wide
  #' 
  #' @param x the long-form table
  #' @return pivoted table
  tidyr::pivot_wider(x, 
                     names_from = dplyr::all_of("name"), 
                     values_from = dplyr::all_of("value"))
}

longer_abundance = function(x, pattern = "^.*_10m2$"){
  #' Pivot a long-form abundance table to wide
  #' 
  #' @param x the long-form table
  #' @param pattern chr, regex for [dplyr::matches]
  #' @return pivoted table
  tidyr::pivot_longer(x, 
                     values_from = dplyr::matches(pattern), 
                     values_to = "value",
                     names_to = "name")
}


write_abundance = function(x, 
                           name = "abundance",
                           path =  here::here("data", "abundance")){
  #' Write a abundance spatial object
  #' 
  #' @param x sf hexgrid object
  #' @param name chr, the name of the abundance object
  #' @param path chr the directory where the file is written
  #' @return the input object `x`
  #' 
  path = make_path(path)
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::write_sf(x, filename)
}

read_abundance= function(name = "abundance",
                         path =  here::here("data", "abundance")){
  #' Read a abundance
  #' 
  #' @param name chr, the name of the abundance object
  #' @param path chr the directory where the file is written
  #' @return an object read by [sf::read_sf]
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::read_sf(filename)
}

