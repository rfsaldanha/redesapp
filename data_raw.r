# Packages
library(dplyr)
library(readr)
library(sf)
library(geobr)

# Municipality seats
mun_seats <- read_municipal_seat() |>
  mutate(code_muni = as.numeric(substr(code_muni, 0, 6)))  

saveRDS(object = mun_seats, file = "data/mun_seats.rds")


# Procedures
proc <- read_csv2("../inova_redes/procedimentos.csv")
saveRDS(object = proc, file = "data/proc.rds")
