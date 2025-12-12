################################################################################-
#
# Kärneri 3 veearvete tasaarveldus
# Maris Vainre
# 2025
# Seadistused
#
################################################################################-

komakohad <- 2
maja_pindala <- read.csv(here::here("./Andmed/1_puhastatud/Veeandmed_puhastatud.csv")) |>
  group_by(tarbimiskuu) |>
  dplyr::summarise(üldpind = sum(krt_pindala)) |>
  dplyr::select(-tarbimiskuu) |>
  unique()
