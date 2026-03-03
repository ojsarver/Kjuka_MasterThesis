pacman::p_load(tidyverse,
               patchwork,
               here)

df_fung<-read_csv(here("su25fungv2.csv"))

df_fung2<-df_fung%>%
  filter(Treat=="C")

df_fung3<-df_fung%>%
  filter(Treat=="MP")

df_fung2%>%
  ggplot(aes(x= Week,
             y= Prop_t,
             fill=Treatment))+
  geom_area()

df_fung3%>%
  ggplot(aes(x= Week,
             y= Prop_t,
             fill=Treatment))+
  geom_area()

df_fung%>%
  ggplot(aes(x= Week,
             y= Treatment_sum,
             color=Treatment))+
  geom_line(position='dodge',stat='identity')
