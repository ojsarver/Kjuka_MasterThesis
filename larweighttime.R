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

df_weightc$Treatment[df_weightc$Treatment == "C"] = "Control"
df_weightc$Treatment[df_weightc$Treatment == "MP"] = "Microplastic"

#graph

df_weightc %>%
  ggplot(aes(x=Treatment,
             y=mu_wei,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu_wei-SE_wei,ymax=mu_wei+SE_wei),
                width=0.2)+
  annotate('text', x = 1, y = 19, label = '(a)', size = 6)+
  annotate('text', x = 2, y = 19, label = '(a)', size = 6)+
  scale_fill_manual(values = c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Average Weight of Larvae")

#stats

lwlme<-lme(Weight_mg~Treatment,
           data=df_weight,
           random=~1|Plant_ID)

anova(lwlme)

#No need for tables, just put in pvalue, df, test stat (f value)
