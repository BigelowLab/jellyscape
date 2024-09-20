#' Retrieve the default groups included in observations set
#' 
#' @return chr named vector of one or more species groups
default_groups = function(){
  c("siph" = "Siphonophores",
    "hydrom" = "Hydromedusea",
    "coel" = "Coelenterates",
    "ctenop" = "Ctenophores",
    "salps" = "Salps")
}

#' Convert a vector group names from short names to long names
#' 
#' @param x chr vector of shortnames
#' @param lut chr names vector translating long and short names
#' @param a vector of long names, unmatched short names are returned as is
as_longname = function(x = paste0(names(default_groups), "_10m2"),
                       lut = default_groups()){
  
  y = x
  for (short in names(lut)){
    ix = grepl(short, x, fixed = TRUE)
    y[ix] = lut[short]
  }
  y
}


#' Retrieve the default variables to extract
#' 
#' @return chr, the columns names to extract when reading ecomon data
default_vars = function(){
  c("cruise_name" = "Cruise", 
    "station" = "Station", 
    "zoo_gear" = "Zooplankton gear", 
    "ich_gear" = "Ichthyoplankton gear", 
    "lat" = "Latitude", 
    "lon" = "Longtitude",
    "date" = "Date", 
    "time" = "Time", 
    "depth" = "Depth", 
    "sfc_temp" = "Surface Temperature (C)", 
    "sfc_salt" ="Surface Salinity", 
    "btm_temp" = "Bottom Temperature (C)", 
    "btm_salt" = "Bottom Salinity",
    "volume_1m2" = "Zooplankton Displacement Volume (ml)")
}




#' Read ecomon data for favored species (groups)
#' 
#' @param groups chr, one or more "species" or group names
#' @param per chr, one of "area" or "volume" to select by those metrics, or "any"
#' @param select_vars chr, the names of the core columns to extract with the groupings
#' @param agg logical, if TRUE and \code{per} is not "any" then sum the metrics
#' @param form chr, one of "table" or "sf"
#' @return table or sf table
read_ecomon_spp = function(groups = names(default_groups()),
                           per = c("any", "area", "volume")[2],
                           select_vars = names(default_vars()),
                           agg = TRUE,
                           form = c("table", "sf")[1]){
  
  
  x = ecomon::read_ecomon(simplify = FALSE) |>
    dplyr::select(dplyr::any_of(select_vars), dplyr::starts_with(groups)) 
  
  x = if(tolower(per[1]) == "area") {
      x = dplyr::select(x, -dplyr::ends_with("_100m3"))
    } else if (tolower(per[1]) == "volume"){
      x = dplyr::select(x, -dplyr::ends_with("_10m2"))
    }
  
  #mtcars %>% mutate(rowsum = rowSums(.[2:4]))
  if (agg){
    if(any(grepl("_10m2", names(x), fixed = TRUE))) {
      v = dplyr::select(x, dplyr::ends_with("_10m2")) |> rowSums(na.rm = TRUE)
      x = dplyr::mutate(x, total_10m2 = as.vector(v))
    }
    if(any(grepl("_100m3", names(x), fixed = TRUE))){
      v = dplyr::select(x, dplyr::ends_with("_100m3")) |> rowSums(na.rm = TRUE)
      x = dplyr::mutate(x, total_100m3 = as.vector(v))
    }
  }
  
  if (tolower(form[1]) == "sf"){
    x = ecomon_as_sf(x)
  }
  
  x
}

#' Convert to sf
#' 
#' @param x table 
#' @return sf table
ecomon_as_sf = function(x = read_ecomon_spp()){
  if (inherits(x, "sfc")) return(x)
  sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
}

#' Pivot an ecomon table to long form
#' 
#' @param x ecomon table with either areal or volume densities
#' @return a table pivoted to long shape
ecomon_to_long = function(x = read_ecomon_spp( form = "table")){
  ia = grepl("_10m2", colnames(x), fixed = TRUE)
  iv = grepl("_100m3", colnames(x), fixed = TRUE)
  if (any(ia) && any(iv)) stop("please provide just areal or volume densities, not both")
  
  if (any(ia)) {
    x = tidyr::pivot_longer(x,
                      dplyr::any_of(colnames(x)[ia]))
  } else {
    x = tidyr::pivot_longer(x,
                            dplyr::any_of(colnames(x)[ia]))
  }
  
  x = dplyr::mutate(x, longname = as_longname(.data$name), .before = dplyr::all_of("name"))
  
  x
}
