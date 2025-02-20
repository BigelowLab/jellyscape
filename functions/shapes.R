read_boundary = function(name = "lme",
                         path = here::here("data", "shapes")){
  #' Read the project boundary polygon
  #' 
  #' @param name chr, the name of the boundary file 
  #' @param path chr the directory where the file is found
  #' @return an object read by [sf::read_sf]
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::read_sf(filename)
}



make_hexgrid = function(x = read_ecomon_spp(), size = 1, clip = TRUE){
  
  #' Make a hex grid of a suggested size from a spatial object
  #' 
  #' @param x spatial object (sf or sfc)
  #' @param size numeric estimate size of the cells in `x` units
  #' @return hexgrid spatial object
  
  
  x = sf::st_geometry(x)
  hexgrid <- st_make_grid(x,
                          cellsize = size, ## unit: metres; change as required
                          what = 'polygons',
                          square = FALSE) |>
    st_as_sf()
  
  ix = sf::st_contains(hexgrid, x)
  hexgrid |>
    dplyr::filter(lengths(ix) > 0) |>
    sf::st_as_sf() |>
    sf::st_set_geometry("geom")
}


write_hexgrid = function(x, 
                         name = "hexgrid",
                         path =  here::here("data", "shapes")){
  #' Write a hexgrid
  #' 
  #' @param x sf hexgrid object
  #' @param name chr, the name of the hex object
  #' @param path chr the directory where the file is written
  #' @return the input object `x`
  #' 
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::write_sf(x, filename)
}

read_hexgrid = function(name = "hexgrid",
                        path =  here::here("data", "shapes")){
  #' Read a hexgrid
  #' 
  #' @param name chr, the name of the hex object
  #' @param path chr the directory where the file is written
  #' @return an object read by [sf::read_sf]
  filename = file.path(path, paste0(name, ".gpkg"))
  sf::read_sf(filename)
}

  