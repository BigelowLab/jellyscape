# From nrecord
# We need some salp data for a proposal we’re putting together. Could you take a look at the ecomon data and send along the following:
# 
# Locations and dates of the top 5% (by abundance) of the salp measurements
# Locations and dates of the measurements where salps are zero


source("setup.R")
thisgroup = "Salps"
x = read_ecomon_spp(form = 'table', groups = thisgroup, agg = FALSE) |>
  ecomon_to_long() |>
  dplyr::mutate(year = format(date, "%Y") ,
                month = factor(format(date, "%b"), levels = month.abb))
nonz = dplyr::filter(x, value > 0)
zero = dplyr::filter(x, value <= 0)

qs = quantile(nonz$value, seq(from = 0, to = 0.95, by = 0.05))
ix = findInterval(nonz$value, qs, all.inside = TRUE)
nonz = dplyr::mutate(nonz,
                     percentile = factor(names(qs)[ix], levels = names(qs)))

readr::write_csv(zero, "~/ecomon_salps_zero.csv")
readr::write_csv(nonz, "~/ecomon_salps_nonzero.csv")