pacman::p_load(tidyverse,
               patchwork,
               here)

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

            