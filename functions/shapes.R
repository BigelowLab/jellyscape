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