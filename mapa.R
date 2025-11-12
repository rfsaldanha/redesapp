library(dplyr)
library(tidyr)
library(lubridate)
library(tmap)
library(geobr)

mun.shp <- read_municipality(year = "2024")
mun.shp$cod6 <- as.numeric(substr(mun.shp$code_muni, 0, 6))

events <- readRDS("data/events.rds")

res_events <- events |>
  group_by(code_muni, categoria) |>
  summarise(freq = n()) |>
  ungroup() |>
  pivot_wider(names_from = categoria, values_from = freq) |>
  mutate(across(everything(), ~ replace_na(.x, 0))) |>
  pivot_longer(cols = 2:7)

res <- left_join(mun.shp, res_events, by = c("cod6" = "code_muni")) |>
  na.omit()

tm_shape(res) +
  tm_polygons(
    fill = "value",
    lwd = 0,
    fill.free = TRUE,
    fill.legend = tm_legend(show = FALSE),
    fill.scale = tm_scale_intervals(values = "brewer.or_rd")
  ) +
  tm_facets(by = "name", nrow = 2)
