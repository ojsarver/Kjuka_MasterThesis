pacman::p_load(tidyverse,
               patchwork,
               here)

#create data frames

df_lar<-read_csv(here("su25larvae1.csv"))

#edits

df_larf<-df_lar%>%
  fill(Plant_ID)

df_larf<-df_larf%>%
  mutate(Adults=as.numeric(Adults))

#how many eggs hatched avg, total, and as a proportion of eggs laid +
##prop that made it to adult stage from eggs hatched and laid

df_larfc<-df_larf%>%
  group_by(Treatment) %>%
  summarize(mu_hat = mean(Eggs_hatched),
            total_hat=sum(Eggs_hatched),
            prop_hatched=(sum(Eggs_hatched))/(sum(Eggs_laid)),
            prop_adult1=(sum(Adults))/(sum(Eggs_hatched)),
            prop_adult2=(sum(Adults))/(sum(Eggs_laid)))

#zeroes may be an issue?

#Kruskal wallis H test