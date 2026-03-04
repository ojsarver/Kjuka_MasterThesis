pacman::p_load(tidyverse,
               patchwork,
               here)

#create data frames

df_bmass<-read_csv(here("su25biomass1.csv"))

#na with the weird observations messing up?

df_bmassc<-df_bmass%>%
  group_by(Treatment) %>%
  summarize(mu_mass = mean(Total_AGB))
