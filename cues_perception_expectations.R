
# typeface ----------------------------------------------------------------

typeface<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="typeface")

#bitterness
##expected 
typeface_ebitter<-lmerTest::lmer(e_bitter~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
typeface_t_ebitter<-t.test(e_bitter~typeface, data=typeface)
typeface_t_ebitter
summary(typeface_ebitter)
##perceived
typeface_bitter<-lmerTest::lmer(bitter~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_bitter)

typeface_t_bitter<-t.test(bitter~typeface, data=typeface)
typeface_t_bitter


#body
##expected
typeface_ebody<-lmerTest::lmer(e_body~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_ebody)

typeface_t_ebody<-t.test(e_body~typeface, data=typeface)
typeface_t_ebody

##perceived

typeface_body<-lmerTest::lmer(body~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_body)

typeface_t_body<-t.test(body~typeface, data=typeface)
typeface_t_body

#refreshment
##expected
typeface_erefreshing<-lmerTest::lmer(e_refreshing~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_erefreshing)

typeface_t_erefreshing<-t.test(e_refreshing~typeface, data=typeface)
typeface_t_erefreshing

##perceived
typeface_refreshing<-lmerTest::lmer(refreshing~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_refreshing)

typeface_t_refreshing<-t.test(refreshing~typeface, data=typeface)
typeface_t_refreshing

#liking
##expected
typeface_eliking<-lmerTest::lmer(e_liking~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_eliking)

typeface_t_eliking<-t.test(e_liking~typeface, data=typeface)
typeface_t_eliking

##perceived
typeface_liking<-lmerTest::lmer(liking~1+color+descriptor + typeface  + (1|pp_id),REML=FALSE, typeface)
summary(typeface_liking)

typeface_t_liking<-t.test(liking~typeface, data=typeface)
typeface_t_liking




#pivot longer, so all can be plotted in one graph

long_typeface<-typeface%>%
  pivot_longer(bitter:e_body, names_to = "percept", values_to = "rating" )

#rename variables


long_typeface<-long_typeface%>%
  mutate(percept=recode(percept, "e_bitter" = "expected bitterness", "e_refreshing"= "expected refreshment", "e_body" = "expected body", "e_liking"="expected liking", "refreshing"= "refreshment" ))

#long_typeface$percept<-as.factor(long_typeface$percept)
long_typeface$percept<-factor(long_typeface$percept, levels=c("bitter", "refreshment", "liking", "body", "expected bitterness", "expected refreshment", "expected liking", "expected body"))


#typeface graph
ggplot2::ggplot(aes(x=typeface, y=rating, fill=typeface), data=long_typeface)+
  geom_bar(stat="summary", fun.y="mean", position="dodge",colour="black")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  labs( y="rating")+
  scale_fill_manual(values=c("grey30", "grey67"))+
  facet_wrap("percept", nrow = 2, ncol = 4, strip.position="bottom")+
  scale_x_discrete(labels=c(" ", " "), name=" ")+
  scale_y_continuous(limits=c(0, 100))+
  theme(strip.text.x = element_text( size = 6, color = "black", face = "bold"), strip.background = element_rect(colour = "black", fill = "white"),panel.background = element_rect(fill = "white", colour = "grey94"), panel.grid.major.y = element_line(colour = "white"), axis.ticks = element_blank())


#save
ggsave(filename="facetype.png",
       scale = 2,
       dpi = 900)

library(kableExtra)

long_typeface%>%
  group_by(percept, typeface)%>%
  summarise(mean=round(mean(rating),2), se=round(sd(rating)/sqrt(n()),2))%>%
  kable()