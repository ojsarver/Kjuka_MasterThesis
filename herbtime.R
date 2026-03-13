pacman::p_load(tidyverse,
               patchwork,
               here,
               grid)

#create data frames

df_herbc<-read_csv(here("Herbivory25v1.csv"))
df_faherb<-read_csv(here("fall25herb.csv"))


# tidy data frames --------------------------------------------------------

#Summer

df_herbcl<-df_herbc%>%
  pivot_longer(cols=Week_1:Week_9, #converts the named col to row
               names_to="Week", #name the new col to put the new row data into
               values_to="value")%>% #specifying data in new col should be a value
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove) #removes the Week_ in front of all data in the Week col

df_herbcl$Treatment[df_herbcl$Treatment == "C"] = "Control"
df_herbcl$Treatment[df_herbcl$Treatment == "MP"] = "Microplastic"

df_herbclse<-df_herbcl%>%
  mutate(Week=as.numeric(Week))%>% #column was character, changed to numeric
  group_by(Treatment,Week)%>%
  summarize(mu=mean(value,na.rm=T),
    SE=sd(value,na.rm=T)/sqrt(length(value)),
    N=length(value),
    sd=sd(value))%>%
  ungroup()

df_herbcl<-df_herbcl%>%
  mutate(Week=as.numeric(Week))

sub_dfherb<-subset(df_herbclse,Week==3)

sub_dfherb$Treatment[sub_dfherb$Treatment == "C"] = "Control"
sub_dfherb$Treatment[sub_dfherb$Treatment == "MP"] = "Microplastic"

#fall

df_faherb<-df_faherb%>%
  pivot_longer(cols=Week_1:Week_7,
               names_to="Week",
               values_to="value")%>%
  separate(col=Week,
           into=c("remove","Week"),
           sep="_")%>% 
  select(-remove)

df_faherb<-df_faherb%>%
  mutate(Week=as.numeric(Week))

#length function not ignoring na

df_fase<-df_faherb%>%
  mutate(Week=as.numeric(Week))%>%
  group_by(Week,Treatment)%>%
  summarize(mu=mean(value,na.rm=T),
            SE=sd(value,na.rm=T)/sqrt(length(value)),
            N=length(value),
            sd=sd(value,na.rm=T))%>%
  ungroup()


# plots -------------------------------------------------------------------


suline<-suline<-ggplot()+
  geom_line(data=df_herbclse,aes(x=Week,
                                 y=mu, color=Treatment))+
  geom_point(data=df_herbcl,aes(x=Week,
                                y=value, color=Treatment))+
  geom_point(data=df_herbclse,aes(x=Week,y=mu,color=Treatment),
             size=5)+
  geom_errorbar(data=df_herbclse, aes(x=Week,ymin=mu-SE,ymax=mu+SE),width=.2)+
  theme_bw()+
  labs(x="Week",
       y="Average Percent Herbivory")+
  theme(legend.position = "none")

faline<-ggplot()+
  geom_line(data=df_fase,aes(x=Week,
                                  y=mu, color=Treatment))+
  geom_point(data=df_faherb,aes(x=Week,
                                 y=value, color=Treatment))+
  geom_point(data=df_fase,aes(x=Week,y=mu,color=Treatment),
             size=5)+
  geom_errorbar(data=df_fase, aes(x=Week,ymin=mu-SE,ymax=mu+SE),width=.2)+
  theme_bw()+
  labs(x="Week",
       y="")


sub_dfherb%>%
  ggplot(aes(x=Treatment,
             y=mu,
             fill=Treatment))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mu-(sd/sqrt(N)), 
                    ymax=mu+(sd/sqrt(N))),
                width=0.2)+
  scale_fill_manual(values=c("lightcyan1", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Week 3",
       y="Average Percent Herbivory")

pushViewport(viewport(layout=grid.layout(1,2)))
print(suline,vp=viewport(layout.pos.row=1,layout.pos.col = 1))
print(faline,vp=viewport(layout.pos.row=1,layout.pos.col = 2))

# stats -------------------------------------------------------------------

haov<-aov(value~Treatment,
          data=subset(df_herbcl,Week==3))

anova(haov)

emmeans(haov,~Treatment)

