pacman::p_load(tidyverse,
               patchwork,
               here)

df_fung<-read_csv(here("su25fungv2.csv"))

df_fung%>%
  ggplot(aes(x= Week,
             y= Prop,
             color=Treatment))+
  geom_line(position='dodge',stat='identity')

df_fung%>%
  ggplot(aes(x= Week,
             y= Treatment_sum,
             color=Treatment))+
  geom_line(position='dodge',stat='identity')
