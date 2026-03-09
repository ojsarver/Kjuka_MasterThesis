pacman::p_load(tidyverse,
               patchwork,
               here)
df_fungpa<-read_csv(here("fung_presenceabsence.csv"))
df_fung<-read_csv(here("su25fungv2.csv"))
df_fung2<-read_csv(here("su25fungv3.csv"))
df_favg<-read_csv(here("su25fungavg.csv"))

#tidy data

df_fung<-df_fung%>%
  fill(Week)

df_fpa<-df_fungpa%>%
  pivot_longer(cols=Week_1:Week_11, #converts the named col to row
               names_to="Week", #name the new col to put the new row data into
               values_to="value")%>% #specifying data in new col should be a value
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove) #removes the Week_ in front of all data in the Week col

df_fpa<-df_fpa%>%
  mutate(Week=as.numeric(Week))

#plots

df_fung%>%
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

ggplot()+
  geom_line(data=df_favg,aes(x=Week,
                                 y=AVG, color=Treatment))+
  geom_point(data=df_favg,aes(x=Week,
                                y=AVG, color=Treatment))

+
  geom_errorbar(data=df_herbclse, aes(x=Week,ymin=mu-SE,ymax=mu+SE),width=.2)+
  geom_vline(xintercept=3)

#stats

fglmm<-glm(Week8~Beetle*Treatment,
           data=df_fungpa,
           family=binomial)

Anova(fglmm, type=3)

lsmeans(fglmm, pairwise~Treatment*Beetle, adjust='tukey')

#beetle plants have less fungal presence even after adjusting for observations            