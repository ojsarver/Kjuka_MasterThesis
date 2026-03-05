pacman::p_load(tidyverse,
               patchwork,
               here)

df_fung<-read_csv(here("su25fungv2.csv"))
df_fung2<-read_csv(here("su25fungv3.csv"))

#plots

df_fung%>%
  ggplot(aes(x= Week,
             y= Treatment_sum,
             color=Treatment))+
  geom_line(position='dodge',stat='identity')

df_fung2%>%
  ggplot(aes(x= Week,
             y= Treatment_sum,
             color=Treatment))+
  geom_line(position='dodge',stat='identity')
