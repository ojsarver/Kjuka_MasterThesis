pacman::p_load(tidyverse,
               patchwork,
               here)

#create data frames

df_herbc<-read_csv(here("Herbivory25v1.csv"))

df_herbavg<-read_csv(here("herbavgv2.csv"))

#tidy data frames

df_herbcl<-df_herbc%>%
  pivot_longer(cols=Week_1:Week_9, #converts the named col to row
               names_to="Week", #name the new col to put the new row data into
               values_to="value")%>% #specifying data in new col should be a value
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove) #removes the Week_ in front of all data in the Week col

df_herbclse<-df_herbcl%>%
  mutate(Week=as.numeric(Week))%>% #column was character, changed to numeric
  group_by(Treatment,Week)%>%
  summarize(mu=mean(value),
    SE=sd(value,na.rm=T)/sqrt(length(value)))%>%
  ungroup()

df_herbavgf<-df_herbavg%>%
  fill(Week)#fills blanks in week column by copying data from the row above

df_herbcl<-df_herbcl%>%
  mutate(Week=as.numeric(Week))

#plots

#combo
ggplot()+
  geom_line(data=df_herbavgf,aes(x=Week,
                                  y=AVG, color=Treatment))+
  geom_point(data=df_herbcl,aes(x=Week,
                                 y=value, color=Treatment))+
  geom_point(data=df_herbavgf,aes(x=Week,y=AVG,color=Treatment),
             size=5)+
  geom_errorbar(data=df_herbclse, aes(x=Week,ymin=mu-SE,ymax=mu+SE),width=.2)+
  geom_vline(xintercept=3)

#Bar graph only on week 3 mu and se

#Ask if we wanna include both line and bar graph

#beetles removed after week 3 but before week 4, have a line idk about labeling?

#individual
df_herbavgf%>%
  ggplot(aes(x=Week,
             y=AVG,
             color=Treatment))+
  geom_smooth()+
  geom_point()

df_herbcl%>%
  ggplot(aes(x=Week,
             y=value,
             color=Treatment))+
  geom_smooth()+
  geom_point()

#stats 

haov<-aov(value~Treatment,
          data=subset(df_herbcl,Week==3))

anova(haov)

emmeans(haov,~Treatment)

