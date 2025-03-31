suppressPackageStartupMessages({
  library(copernicus)
})

source("setup.R")

#' This function matches ecomon (long-form) points to copernicus reanalysis layers
#' And then saves a a slightly wider long-form to the data/copernicus directory (not in git)
#' Use  `read_ecomon_spp(bypass = "copernicus")` to read it back.
#' 
merge_analyses = function(x= read_ecomon_spp(form = 'sf', post = "long-extras") ){
  
  dummy_analysis = dplyr::tibble(bottomT = NA_real_,
                                 mlotst = NA_real_,
                                 so = NA_real_,
                                 thetao = NA_real_,
                                 uo = NA_real_,
                                 vo = NA_real_,
                                 zos = NA_real_)
  
  
  path = copernicus_path("GLOBAL_MULTIYEAR_PHY_001_030/nwa")
  DB = read_database(path)
  
  
  if (FALSE){
    thedate = as.Date("1993-01-06")
    tbl = filter(x, date == thedate)
    db = dplyr::filter(DB, date == tbl$date[1])
  }
  
  y = dplyr::group_by(x, date) |>
    dplyr::group_map(
      function(tbl, key){
        #cat(format(tbl$date[1], "%Y-%m-%d"), "\n")
        db = dplyr::filter(DB, date == tbl$date[1])
        if (nrow(db) == 0) {
          return(dplyr::bind_cols(tbl, dummy_analysis))
        }
        s = copernicus::read_copernicus(db, path)
        v = stars::st_extract(s, tbl) |>
          sf::st_drop_geometry() |>
          dplyr::rename_with(function(x) paste0("c_", x))
        dplyr::bind_cols(tbl, v)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    sf::write_sf("data/copernicus/ecomon_abundance.gpkg")
}