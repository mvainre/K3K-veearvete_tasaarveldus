################################################################################-
#
# Kärneri 3 veearvete tasaarveldus
# Maris Vainre
# 2025
# Tasaarvelduste ülevaade
#
################################################################################-

Üldvesi <- readxl::read_xlsx(here::here("./Andmed/0_toorandmed/K3KÜüldvesi_veearved.xlsx"))
Veearved_korrigeeritud_puhas <- read.csv(here::here("./Andmed/2_töödeldud/Veeandmed_tasaarveldused.csv"))

Tasaarveldus_omanikud <- Veearved_korrigeeritud_puhas |>
  dplyr::select(c(tarbimiskuu, krt_nr, krt_omanik, krt_veetasaarvestus)) |>
  dplyr::filter(tarbimiskuu > 202312 & tarbimiskuu < 202509) |>
  dplyr::group_by(krt_nr, krt_omanik) |>
  dplyr::summarise(vee_tasarveldus_kokku = sum(krt_veetasaarvestus))

Tasaarveldus_krt <- Veearved_korrigeeritud_puhas |>
  dplyr::filter(tarbimiskuu > 202312 & tarbimiskuu < 202509) |>
  dplyr::group_by(krt_nr) |>
  dplyr::summarise(vee_tasarveldus_kokku = sum(krt_veetasaarvestus))

Kontroll <- Veearved_korrigeeritud_puhas |>
  dplyr::group_by(tarbimiskuu) |>
  dplyr::summarise(vee_maja_summa = sum(külmsoevesi_summa_õige, na.rm = TRUE)+sum(yldvesi_summa_õige)) |>
  dplyr::left_join(Üldvesi, by = "tarbimiskuu") |>
  dplyr::mutate(vahe = vesi_kogutarb_summa_õige - vee_maja_summa)

Kontroll2 <- Veearved_korrigeeritud_puhas |>
  dplyr::group_by(tarbimiskuu, krt_nr) |>
  dplyr::summarise(vee_tasarveldus_kokku = sum(krt_veetasaarvestus))

Kuude_kaupa <- Veearved_korrigeeritud_puhas |>
  dplyr::filter(tarbimiskuu > 202312 & tarbimiskuu < 202509) |>
  dplyr::group_by(krt_nr, tarbimiskuu) |>
  dplyr::summarise(vee_tasarveldus_kokku = sum(krt_veetasaarvestus))|>
  dplyr::arrange(tarbimiskuu)



################################################################################-
# Kirjuta andmestik failiks ----
################################################################################-

write.csv(Tasaarveldus_omanikud, here::here("./Andmed/3_tulemused/Tasaarveldus_omanikud.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(Tasaarveldus_krt, here::here("./Andmed/3_tulemused/Tasaarveldus_krt.csv"), row.names = FALSE, fileEncoding = "UTF-8")

################################################################################-
# Keskkonna puhastus ----
################################################################################-

rm(list = ls())
