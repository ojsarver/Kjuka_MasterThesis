pacman::p_load(tidyverse,
               patchwork,
               here)
df_fungpa<-read_csv(here("fung_presenceabsence.csv"))
df_fung<-read_csv(here("su25fungv2.csv"))
df_fung2<-read_csv(here("su25fungv3.csv"))

#fill

df_fungf<-df_fung%>%
  fill(Week)

#plots

df_fungf%>%
  ggplot(aes(x= Week,
             y= Treatment_sum,
             color=Treatment))+
  geom_line()+
  labs(x="Week Number",
       y="Cumulative Plants W/ Damage")

df_fung2%>%
  ggplot(aes(x= Week,
             y= Treatment_sum,
             color=Treatment))+
  geom_line()

#stats

fglmm<-glm(Week8~Beetle*Treatment,
           data=df_fungpa,
           family=binomial)

Anova(fglmm, type=3)

lsmeans(fglmm, pairwise~Treatment*Beetle, adjust='tukey')

#beetle plants have less fungal presence even after adjusting for observations            