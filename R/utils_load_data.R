load_data <- function(){
  data_dir <- "inst"

  flights_sf <- readr::read_rds(glue::glue("{data_dir}/raw_flights_clean.RDS"))

  flights_raw <- flights_sf |>
    sf::st_drop_geometry()

  airspace <- readr::read_rds(glue::glue("{data_dir}/airspace.RDS"))

  terrain_raster <- raster::raster(glue::glue("{data_dir}/uk_dem.gri"))

  club_locations <- readr::read_rds(glue::glue("{data_dir}/club_locations.RDS"))
  sites_raw <- readr::read_rds(glue::glue("{data_dir}/sites_list.RDS"))

  landing_fields <- readr::read_rds(glue::glue("{data_dir}/landing_fields.RDS"))

  usethis::use_data(flights_sf,
                    flights_raw,
                    airspace,
                    terrain_raster,
                    club_locations,
                    sites_raw,
                    landing_fields, overwrite = TRUE)
}
