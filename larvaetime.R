#Make plot pretty

pacman::p_load(tidyverse,
               patchwork,
               here)

#create data frames

df_lar<-read_csv(here("su25larvae1.csv"))

#edits

df_larf<-df_lar%>%
  fill(Plant_ID)

df_larf<-df_larf%>%
  mutate(Adults=as.numeric(Adults))%>%
  mutate(Adults_0=ifelse(is.na(Adults),0,Adults))

#how many eggs hatched avg, total, and as a proportion of eggs laid +
##prop that made it to adult stage from eggs hatched and laid

df_larfc<-df_larf%>%
  mutate(prop_hatched=Eggs_hatched/Eggs_laid,
         prop_adults_h=Adults_0/Eggs_hatched,
         prop_adults_l=Adults_0/Eggs_laid)%>%
  group_by(Treatment) %>%
  summarize(mu_hat = mean(Eggs_hatched,na.rm=T),
            SE_hat=sd(Eggs_hatched,na.rm=T)/sqrt(length(Eggs_hatched)),
            mu_prop_adults=mean(prop_adults_l,na.rm=T),
            SE_prop_a=sd(prop_adults_l,na.rm=T)/sqrt(length(prop_adults_l)),
            mu_prop_hatched=mean(prop_hatched,na.rm=T),
            SE_prop_h=sd(prop_hatched,na.rm=T)/sqrt(length(prop_hatched)))

#emphasize prop of adults ONLY includes egg masses that had eggs successfully hatch

mu_hat<-df_larfc %>%
  ggplot(aes(x=Treatment,
             y=mu_hat))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_hat-SE_hat,ymax=mu_hat+SE_hat),
                width=0.2)

mu_prop_adults<-df_larfc %>%
  ggplot(aes(x=Treatment,
             y=mu_prop_adults))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_prop_adults-SE_prop_a,ymax=mu_prop_adults+SE_prop_a),
                width=0.2)

mu_prop_hatch<-df_larfc %>%
  ggplot(aes(x=Treatment,
             y=mu_prop_hatched))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_prop_hatched-SE_prop_h,ymax=mu_prop_hatched+SE_prop_h),
                width=0.2)

pushViewport(viewport(layout=grid.layout(2,2)))
print(mu_hat,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(mu_prop_adults,vp=viewport(layout.pos.row=1,layout.pos.col = 2))
print(mu_prop_hatch,vp=viewport(layout.pos.row=2,layout.pos.col = 1))

#Kruskal wallis H test