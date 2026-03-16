pacman::p_load(tidyverse,
               patchwork,
               here,
               car,
               emmeans,
               nlme,
               grid)

df_surepr<-read_csv(here("su25growr1.csv"))
df_suvege<-read_csv(here("su25Vgrow1.csv"))
df_farepr<-read_csv(here("fa25repr1.csv"))
df_favege<-read_csv(here("fa25veggrow2.csv"))

# tidy data ---------------------------------------------------------------


