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

df_herbcl<-df_herbcl%>%
  mutate(Week=as.numeric(Week)) #column was character, changed to numeric


df_herbavgf<-df_herbavg%>%
  fill(Week) #fills blanks in week column by copying data from the row above

#plots

#combo
ggplot()+
  geom_line(data=df_herbavgf,aes(x=Week,
                                  y=AVG, color=Treatment))+
  geom_point(data=df_herbcl,aes(x=Week,
                                 y=value, color=Treatment))+
  geom_vline(xintercept=3)

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
