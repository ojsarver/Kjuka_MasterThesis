pacman::p_load(tidyverse,
               patchwork,
               here,
               nlme,
               emmeans)

#create data frames

df_weight<-read_csv(here("su25larweight1.csv"))

#avg weights by treatment

df_weightc<-df_weight%>%
  group_by(Treatment) %>%
  summarize(mu_wei = mean(Weight_mg),
            SE_wei=sd(Weight_mg)/sqrt(length(Weight_mg)))

#graph

df_weightc %>%
  ggplot(aes(x=Treatment,
             y=mu_wei))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_wei-SE_wei,ymax=mu_wei+SE_wei),
                width=0.2)

#stats

lwlme<-lme(Weight_mg~Treatment,
           data=df_weight,
           random=~1|Plant_ID)

anova(lwlme)

#No need for tables, just put in pvalue, df, test stat (f value)
