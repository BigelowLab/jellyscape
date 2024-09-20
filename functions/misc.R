#' Read the coastline
#' 
#' @param scale chr the scale of map as "small", "medium" (default) or "large"
#' @param form chr one of 'sp' or 'sf' (default)
read_coast = function(scale = "medium", form = "sf"){
  rnaturalearth::ne_coastline(scale = scale[1], returnclass = form[1]) |>
    sf::st_geometry()
}