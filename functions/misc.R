
read_coast = function(scale = "medium", form = "sf",
                      bb = sf::st_bbox(c(xmin = -77, 
                             ymin = 34, 
                             xmax = -63, 
                             ymax = 45.5), crs = 4326)){
  
  #' Read the coastline
  #' 
  #' @param scale chr the scale of map as "small", "medium" (default) or "large"
  #' @param form chr one of 'sp' or 'sf' (default)
  #' @return geometry of the coast
  
  rnaturalearth::ne_coastline(scale = scale[1], returnclass = form[1]) |>
    sf::st_geometry() |>
    sf::st_crop(bb)
}


make_path = function(path){
  #' Safely make a path
  #' @param path chr the path description
  #' @return the path description (whether of not it exists)
  ok = dir.exists(path[1])
  if (!ok) ok = dir.create(path, recursive = TRUE)
  path
}


filter_by_key = function(x = iris, key = dplyr::tibble(Species = "setosa")){
  #' A function to aid debugging, filter `x` by the values in `key`
  #' @param x a table of data
  #' @param key a table used to filter x.  Column names in `key` must be matched
  #'   by column names in `x`
  #' @return a filtered version of `x`
  for (nm in colnames(key)){
    nmSym = rlang::sym(nm)
    x = x |> dplyr::filter({{nmSym}} %in% key[[nm]])
  }
  x
}
