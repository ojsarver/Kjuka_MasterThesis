pacman::p_load(tidyverse,
               patchwork,
               here)

#create data frames

df_egg<-read_csv(here("su25beetleegg1.csv"))

#calculations

df_eggc<-df_egg%>%
group_by(Treatment) %>%
  summarize(mu_egg = mean(Num_Eggs),
            total_egg=sum(Num_Eggs),
            total_mass=sum(Egg_mass_num),
            prop_hatched=(sum(Hatched))/(sum(Egg_mass_num)))

#plots, how to make multiple plots show up at once?

#avg eggs in each mass
df_eggc %>%
  ggplot(aes(x=Treatment,
             y=mu_egg))+
  geom_bar(stat="identity")

#total eggs

df_eggc %>%
  ggplot(aes(x=Treatment,
             y=total_egg))+
  geom_bar(stat="identity")

#total # of egg masses
df_eggc %>%
  ggplot(aes(x=Treatment,
             y=total_mass))+
  geom_bar(stat="identity")

#proportion of eggs that successfully hatched

df_eggc %>%
  ggplot(aes(x=Treatment,
             y=prop_hatched))+
  geom_bar(stat="identity")
