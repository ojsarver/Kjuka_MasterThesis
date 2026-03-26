pacman::p_load(tidyverse,
               patchwork,
               here,
               grid)

#create data frames

df_egg<-read_csv(here("su25beetleegg1.csv"))
df_egglar<-read_csv(here("su25EGGLARVAE.csv"))


# calculations ------------------------------------------------------------


df_egglarc<-df_egglar%>%
group_by(Treatment) %>%
  summarize(mu_egg = mean(Num_Eggs),
            SE_egg=sd(Num_Eggs,na.rm=T)/sqrt(length(Num_Eggs)),
            mu_hat = mean(Eggs_hatched),
            SE_hat=sd(Eggs_hatched,na.rm=T)/sqrt(length(Eggs_hatched)),
            total_egg=sum(Num_Eggs),
            mu_mass=mean(Egg_mass),
            SE_mass=sd(Egg_mass,na.rm=T)/sqrt(length(Egg_mass)),
            prop_hatched=(sum(Eggs_hatched))/(sum(Egg_mass)),
            mu_adult=mean(Adults),
            SE_adult=sd(Adults,na.rm=T)/sqrt(length(Adults)))

# plots -------------------------------------------------------------------


#avg eggs in each mass
mu_egg<-df_egglarc %>%
  ggplot(aes(x=Treatment,
             y=mu_egg,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_egg-SE_egg,ymax=mu_egg+SE_egg),
                width=0.2)+
  annotate('text', x = 1, y = 58, label = 'a', size = 6)+
  annotate('text', x = 2, y = 58, label = 'b', size = 6)+
  annotate('text', x = .5, y = 59, label = '(A)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=18),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Number of Eggs in Each Mass")

#avg # of egg masses
mu_mass<-df_egglarc %>%
  ggplot(aes(x=Treatment,
             y=mu_mass,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_mass-SE_mass,ymax=mu_mass+SE_mass),
                width=0.2)+
  annotate('text', x = 1, y = 2, label = 'a', size = 6)+
  annotate('text', x = 2, y = 2, label = 'b', size = 6)+
  annotate('text', x = .5, y = 2, label = '(B)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=19),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Number of Egg Masses Laid")

#avg adults
mu_adults<-df_egglarc%>%
  ggplot(aes(x=Treatment,
             y=mu_adult,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_adult-SE_adult,ymax=mu_adult+SE_adult),
                width=0.2)+
  annotate('text', x = 1, y = 10, label = 'a', size = 6)+
  annotate('text', x = 2, y = 10, label = 'b', size = 6)+
  annotate('text', x = .5, y = 11, label = '(D)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=19),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Number of Adults")

#avg hatched

mu_hat<-df_egglarc%>%
  ggplot(aes(x=Treatment,
             y=mu_hat,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_hat-SE_hat,ymax=mu_hat+SE_hat),
                width=0.2)+
  annotate('text', x = 1, y = 27, label = 'a', size = 6)+
  annotate('text', x = 2, y = 27, label = 'b', size = 6)+
  annotate('text', x = .5, y = 28, label = '(C)', size = 7)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_text(size=19),
        axis.text=element_text(size=15))+
  labs(x = "",
       y = "Average Number of Eggs that Hatched")

pushViewport(viewport(layout=grid.layout(2,2)))
print(mu_egg,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(mu_mass,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(mu_hat,vp=viewport(layout.pos.row=2,layout.pos.col = 1))
print(mu_adults,vp=viewport(layout.pos.row=2,layout.pos.col = 2))

#stats for eggs and larvae

#separate models for total mass and total egg

wilcox.test(Num_Eggs~Treatment,
       data=df_egglar)

wilcox.test(Eggs_hatched~Treatment,
       data=df_egglar)

wilcox.test(Egg_mass~Treatment,
       data=df_egglar)

wilcox.test(Adults~Treatment,
       data=df_egglar)

wilcox.test(prop_hat~Treatment,
            data=subset(df_egglar,!is.na(df_egglar$prop_hat)))

wilcox.test(prop_adult~Treatment,
            data=subset(df_egglar,!is.na(df_egglar$prop_adult)))

#No need for tables, just put in pvalue, df=1, test stat (w value)
