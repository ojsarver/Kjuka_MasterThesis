pacman::p_load(tidyverse,
               patchwork,
               here)

#create data frames

df_weight<-read_csv(here("su25larweight1.csv"))

#avg weights by treatment

df_weightc<-df_weight%>%
  group_by(Treatment) %>%
  summarize(mu_wei = mean(Weight_mg))

#aint no way that is significant