################################################################################-
#
# Kärneri 3 veearvete tasaarveldus
# Maris Vainre
# 2025
# Tasaarvelduste arvutamine
#
################################################################################-



################################################################################-
# Ettevalmistus ----
################################################################################-

library(here)
library(dplyr)

source(here::here("./Kood/0a_K3KÜvesi_seadistus.R"))
Veeandmed_puhas <- read.csv(here::here("./Andmed/1_puhastatud/Veeandmed_puhastatud.csv"))


################################################################################-
# Arvuta vee tegelik kulu ja summa, mida oleks pidanud selle eest maksma ----
################################################################################-


################################################################################-
## Iga korteri tegelik summa ----

Veeandmed_korrigeeritud <- Veeandmed_puhas |>
  dplyr::mutate(vee_kogus = kvesi_kogus + svesi_kogus,
                
                #Arvuta kontrolliks vana hind. Kas tuleb sama, mis arvel oli?
                veehind_vale = ifelse(kvesi_hind_vale == svesi_hind_vale, kvesi_hind_vale, NA_real_),
                vesi_summa_vale_kontroll = round(vee_kogus * veehind_vale, komakohad),
                külmsoevesi_summa_vale = kvesi_summa_vale + svesi_summa_vale,
                
                #tolereeri 10-sendi suurust viga
                vana_kontroll = dplyr::case_when(abs(vesi_summa_vale_kontroll-külmsoevesi_summa_vale) > 0.1 ~ "viga",
                                                 TRUE ~ "OK")) |>
  dplyr::select(-c(kvesi_hind_vale, svesi_hind_vale)) |>
  
  dplyr::mutate(külmsoevesi_summa_õige = vee_kogus * veehind_õige)

# Prindi kas kõik on OK
vead <- Veeandmed_korrigeeritud %>% 
  filter(vana_kontroll == "viga")

if (nrow(vead) > 0) {
  print(vead)
} else {
  message("Tundub, et andmed on õigesti sisse loetud: uus arvutus vana veehinnaga on sama, mis juba esitatud arvetel. Võib tasaarveldusega edasi minna")
}



################################################################################-
## Korterite makstava üldvee tegelik summa ---- 

#Korterite üldvesi tähendab kogust, mis jääb üle pärast seda, 
#kui maja arvel näidatud veearve summast lahutatakse kõigi korterite külma ja sooja arvete summa.

#Arvutame üldvee

# Korterite tarbimise summa iga kuu lõikes
Korterite_veesumma_kuudelõikes <- Veeandmed_korrigeeritud |>
  dplyr::group_by(tarbimiskuu) |>
  dplyr::summarise(krtd_veesumma_sum = sum(külmsoevesi_summa_õige, na.rm = TRUE)) 

# Maja arvel näidatud veesumma
Maja_veesumma_kuudelõikes <- Veeandmed_korrigeeritud |>
  dplyr::select(c(tarbimiskuu, vesi_kogutarb_summa_õige, veehind_õige)) |>
  unique()

# Leia eelmise kahe vahe. 
Üldvesi_jagatav <- Korterite_veesumma_kuudelõikes |>
  dplyr::left_join(Maja_veesumma_kuudelõikes, by = "tarbimiskuu") |>
  # maja korterite vahel jaotatava üldvee kogus on kogu maja tarbimine miinus iga korteri tarbimine
  dplyr::mutate(yldvee_summa_jagamisele = vesi_kogutarb_summa_õige - krtd_veesumma_sum) |>
  dplyr::select(-c(veehind_õige, krtd_veesumma_sum, vesi_kogutarb_summa_õige))

################################################################################-
## Korterite makstava üldvee tegelik summa ----

# Nüüd saab üldvee koguse jaotada korterite vahel laiali. 
Veearved_korrigeeritud <- Veeandmed_korrigeeritud |>
  dplyr::select(-c(vana_kontroll)) |>
  dplyr::left_join(Üldvesi_jagatav, by = "tarbimiskuu") |>
  dplyr::mutate(# maja korterite vahel jaotatava üldvee summa on proportsionaalne korteri pindalaga
                yldvesi_summa_õige = yldvee_summa_jagamisele * krt_pindala/maja_pindala$üldpind,
                #Tasaarvestusel on kaks varianti. Vaid parkimiskohtade omanikel tuleks lähtuda vaid üldvee summadest. 
                # Korteriomanikel aga kogu vee eest makstud summadest.
                krt_veetasaarvestus = dplyr::case_when(külmsoevesi_summa_õige == 0 ~ round(yldvesi_summa_õige - yvesi_summa_vale, komakohad),
                                                       TRUE ~ round((külmsoevesi_summa_õige - külmsoevesi_summa_vale) + (yldvesi_summa_õige - yvesi_summa_vale), komakohad)))


################################################################################-
## Puhasta andmestikku ----

Veearved_korrigeeritud_puhas <- Veearved_korrigeeritud |>
  dplyr::filter(tarbimiskuu > 202312 & tarbimiskuu < 202509) |>
  dplyr::select(c(tarbimiskuu, krt_nr, krt_omanik, krt_pindala, 
                  kvesi_kogus, svesi_kogus, vee_kogus, veehind_vale, 
                  kvesi_summa_vale, svesi_summa_vale, külmsoevesi_summa_vale,
                  yvesi_summa_vale,
                  veehind_õige, külmsoevesi_summa_õige, yldvesi_summa_õige,
                  krt_veetasaarvestus))


################################################################################-
# Kirjuta andmestik failiks ----
################################################################################-

write.csv(Veearved_korrigeeritud_puhas, here::here("./Andmed/2_töödeldud/Veeandmed_tasaarveldused.csv"), 
          fileEncoding = "UTF-8",
          row.names = FALSE)

################################################################################-
# Keskkonna puhastus ----
################################################################################-

rm(list = ls())