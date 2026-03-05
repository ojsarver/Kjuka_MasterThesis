pacman::p_load(tidyverse,
               patchwork,
               here,
               grid)

#create data frames

df_egg<-read_csv(here("su25beetleegg1.csv"))

#calculations

df_eggc<-df_egg%>%
group_by(Treatment) %>%
  summarize(mu_egg = mean(Num_Eggs),
            SE_egg=sd(Num_Eggs,na.rm=T)/sqrt(length(Num_Eggs)),
            total_egg=sum(Num_Eggs),
            total_mass=sum(Egg_mass_num),
            SE_mass=sd(Egg_mass_num,na.rm=T)/sqrt(length(Egg_mass_num)), #this dont work
            prop_hatched=(sum(Hatched))/(sum(Egg_mass_num)))

#plots, how to make multiple plots show up at once?

#avg eggs in each mass
mu_egg<-df_eggc %>%
  ggplot(aes(x=Treatment,
             y=mu_egg))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_egg-SE_egg,ymax=mu_egg+SE_egg),
                width=0.2)

#total eggs

total_egg<-df_eggc %>%
  ggplot(aes(x=Treatment,
             y=total_egg))+
  geom_bar(stat="identity")

#total # of egg masses
total_mass<-df_eggc %>%
  ggplot(aes(x=Treatment,
             y=total_mass))+
  geom_bar(stat="identity")

#proportion of eggs that successfully hatched

prop_hatched<-df_eggc %>%
  ggplot(aes(x=Treatment,
             y=prop_hatched))+
  geom_bar(stat="identity")

#add error bars? and format graphs

pushViewport(viewport(layout=grid.layout(2,2)))
print(mu_egg,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(total_egg,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(total_mass,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(prop_hatched,vp=viewport(layout.pos.row=2,layout.pos.col = 2))
