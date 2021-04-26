

# bitter --------------------------------------------------------------

##bitter


bitter_model0<-lmerTest::lmer(bitter~1 + (1|pp_number),REML=FALSE, BITTER_P)
bitter_model1<-update(bitter_model0,.~.+taste,REML=FALSE)
bitter_model2<-update(bitter_model1,.~.+colour,REML=FALSE)
bitter_model3<-update(bitter_model2, .~.+label,REML=FALSE)
bitter_model4<-update(bitter_model3,.~.+taste:colour)
bitter_model5<-update(bitter_model4,.~., +taste:label)
bitter_model6<-update(bitter_model5,.~.,+ colour:label)
bitter_model7<-update(bitter_model6,.~.,+colour:label:taste)

anova(bitter_model0, bitter_model1, bitter_model2, bitter_model3, bitter_model4, bitter_model5, bitter_model6, bitter_model7)

summary(bitter_model3)

pairwise.t.test(BITTER_P$bitter, BITTER_P$label, paired =TRUE, p.adjust.method="bonferroni")



#effect of label and colour
ggplot(BITTER_P, aes(x=label, y=bitter, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  #geom_signif( comparisons=list(c("standard", "bitter")), annotations="***", y_position=60,        tip_length=0.01)+
  #geom_signif( comparisons=list(c("nolabel", "bitter")), annotations="***", y_position=65,        tip_length=0.01)+
  labs(x="label", y="bitter")+
  #geom_signif(y_position=c(55, 55, 55), xmin=c(0.9, 1.9, 2.9), xmax=c(1.1,2.1, 3.1),
  #annotation=c("***"), tip_length=0) +
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()+
  facet_wrap(~taste)

ggplot(BITTER_P, aes(x=taste, y=bitter, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif( comparisons=list(c("bitter", "mild")), annotations="**", y_position=65,        tip_length=0.01)+
  labs(x="taste", y="bitter")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()



# refreshment -------------------------------------------------------------
refreshing_model0<-lmer(refreshing~1 + (1|pp_number), REFRESHING_P, REML=FALSE)
refreshing_model1<-update(refreshing_model0,.~.+taste)
refreshing_model2<-update(refreshing_model1,.~.+colour)
refreshing_model3<-update(refreshing_model2, .~.+label)
refreshing_model4<-update(refreshing_model3,.~.+taste:colour)
refreshing_model5<-update(refreshing_model4,.~., +taste:label)
refreshing_model6<-update(refreshing_model5,.~.,+ colour:label)
refreshing_model7<-update(refreshing_model6,.~.,+colour:label:taste)

anova(refreshing_model0, refreshing_model1, refreshing_model2, refreshing_model3, refreshing_model4, refreshing_model5, refreshing_model6, refreshing_model7)

refreshing_lm2<-lm(refreshing ~ taste + colour + label,  data= REFRESHING_P, method="ML")

AIC(refreshing_lm2, refreshing_model2)

#anova(refreshing_lm2, refreshing_model2)


summary(refreshing_model2)



REFRESHING_P$pp_number<-as.factor(REFRESHING_P$pp_number)
bf_refreshing=anovaBF(refreshing~colour*label*taste + pp_number, data=REFRESHING_P, whichRandom="pp_number")
bf_refreshing
plot(bf_refreshing)

ggplot(REFRESHING_P, aes(x=colour, y=refreshing, group=label))+
  geom_line(aes(linetype=label))+
  geom_point()+
  scale_linetype_manual(values=c("twodash", "dotted","longdash", "solid"))+
  theme_light()


ggplot(REFRESHING_P, aes(x=taste, y=refreshing, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif(comparisons=list(c("bitter", "mild")), annotations="**", y_position=67,        tip_length=0.01)+
  geom_signif(y_position=c(58, 58), xmin=c(0.9, 1.9), xmax=c(1.1,2.1),
              annotation=c("***"), tip_length=0) +
  labs(x="taste", y="refreshment")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()


# liking ------------------------------------------------------------------

liking_model0<-lmer(liking~1 + (1|pp_number), LIKING_P)
liking_model1<-update(liking_model0,.~.+taste)
liking_model2<-update(liking_model1,.~.+colour)
liking_model3<-update(liking_model2, .~.+label)
liking_model4<-update(liking_model3,.~.+taste:colour)
liking_model5<-update(liking_model4,.~., +taste:label)
liking_model6<-update(liking_model5,.~.,+ colour:label)
liking_model7<-update(liking_model6,.~.,+colour:label:taste)

anova(liking_model0, liking_model1, liking_model2, liking_model3, liking_model4, liking_model5, liking_model6, liking_model7)

summary(liking_model2)


LIKING_P$pp_number<-as.factor(LIKING_P$pp_number)
bf_liking=anovaBF(liking~colour*label*taste+ pp_number, data=LIKING_P, whichRandom="pp_number")
bf_liking
plot(bf_liking)

ggplot(LIKING_P, aes(x=taste, y=liking, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif(comparisons=list(c("bitter", "mild")), annotations="*", y_position=70,        tip_length=0.01)+
  geom_signif(y_position=c(65, 65), xmin=c(0.9, 1.9), xmax=c(1.1,2.1),
              annotation=c("***"), tip_length=0) +
  labs(x="taste", y="liking")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()

# body --------------------------------------------------------------------

body_model0<-lmer(body~1 + (1|pp_number), BODY_P)
body_model1<-update(body_model0,.~.+taste)
body_model2<-update(body_model1,.~.+colour)
body_model3<-update(body_model2, .~.+label)
body_model4<-update(body_model3,.~.+taste:colour)
body_model5<-update(body_model4,.~., +taste:label)
body_model6<-update(body_model5,.~.,+ colour:label)
body_model7<-update(body_model6,.~.,+colour:label:taste)

anova(body_model0, body_model1, body_model2, body_model3, body_model4, body_model5, body_model6, body_model7)

summary(body_model2)

BODY_P$pp_number<-as.factor(BODY_P$pp_number)
bf_body=anovaBF(body~colour*label*taste+ pp_number, data=BODY_P, whichRandom="pp_number")
bf_body
plot(bf_body)

ggplot(BODY_P, aes(x=colour, y=body))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif(comparisons=list(c("light", "dark")), annotations="*", y_position=80,        tip_length=0.01)+
  labs(x="taste", y="body")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()
