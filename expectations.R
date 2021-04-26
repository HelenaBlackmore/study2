
# bitterness --------------------------------------------------------------



ebitter_model0<-lmer(expected_bitter~1 + (1|pp_number), REML=FALSE, BITTER_E)
ebitter_model1<-update(ebitter_model0,.~.+colour)
ebitter_model2<-update(ebitter_model1, .~.+label)

ebitter_model3<-lmer(expected_bitter~ colour:label + (1|pp_number), REML=FALSE, BITTER_E)



anova(ebitter_model0, ebitter_model1, ebitter_model2, ebitter_model3)

summary(ebitter_model2)

ebitter_lm2<-lm(expected_bitter~ colour + label,method="ML", data= BITTER_E)

AIC(ebitter_lm2, bitter_model2)
anova(ebitter_model2, ebitter_lm2)

pairwise.t.test(BITTER_E$expected_bitter, BITTER_E$label, paired =TRUE, p.adjust.method="bonferroni")

BITTER_E$pp_number<-as.factor(BITTER_E$pp_number)
bf_ebitter=anovaBF(expected_bitter~colour*label + pp_number, data=BITTER_E, whichRandom="pp_number")
bf_ebitter
plot(bf_ebitter)

#effect of label and colour
ggplot(BITTER_E, aes(x=label, y=expected_bitter, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif( comparisons=list(c("standard", "bitter")), annotations="***", y_position=77,        tip_length=0.01)+
  geom_signif( comparisons=list(c("nolabel", "bitter")), annotations="***", y_position=83,        tip_length=0.01)+
  geom_signif( comparisons=list(c("nolabel", "standard")), annotations="***", y_position=73,        tip_length=0.01)+
  labs(x="label", y="expected bitter")+
  geom_signif(y_position=c(72, 55, 67), xmin=c(0.8, 1.8, 2.8), xmax=c(1.2,2.2, 3.2),
              annotation=c("***"), tip_length=0) +
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()

# refreshment -------------------------------------------------------------

erefreshing_model0<-lmer(expected_refreshing~1 + (1|pp_number), REFRESHING_E)
erefreshing_model1<-update(erefreshing_model0,.~.+colour)
erefreshing_model2<-update(erefreshing_model1, .~.+label)
erefreshing_model3<-update(erefreshing_model2,.~.,+ colour:label)


anova(erefreshing_model0, erefreshing_model1, erefreshing_model2, erefreshing_model3)

summary(erefreshing_model2)

pairwise.t.test(REFRESHING_E$expected_refreshing, REFRESHING_E$label, paired =TRUE, p.adjust.method="bonferroni")

REFRESHING_E$pp_number<-as.factor(REFRESHING_E$pp_number)
bf_erefreshing=anovaBF(expected_refreshing~colour*label+ pp_number, data=REFRESHING_E, whichRandom="pp_number")
bf_erefreshing
plot(bf_erefreshing)

#effect of label and colour
ggplot(REFRESHING_E, aes(x=label, y=expected_refreshing, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif( comparisons=list(c("standard", "bitter")), annotations="***", y_position=70,        tip_length=0.01)+
  geom_signif( comparisons=list(c("nolabel", "bitter")), annotations="***", y_position=75,        tip_length=0.01)+
  labs(x="label", y="expected refreshment")+
  geom_signif(y_position=c(60, 60, 67), xmin=c(0.9, 1.9, 2.9), xmax=c(1.1,2.1, 3.1),
              annotation=c("***"), tip_length=0) +
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()
# liking ------------------------------------------------------------------
eliking_model0<-lmer(expected_liking~1 + (1|pp_number), LIKING_E)
eliking_model1<-update(eliking_model0,.~.+colour)
eliking_model2<-update(eliking_model1, .~.+label)
eliking_model3<-update(eliking_model2,.~.,+ colour:label)


anova(eliking_model0, eliking_model1, eliking_model2, eliking_model3)

summary(eliking_model2)

LIKING_E$pp_number<-as.factor(LIKING_E$pp_number)
bf_eliking=anovaBF(expected_liking~colour*label+ pp_number, data=LIKING_E, whichRandom="pp_number")
bf_eliking
plot(bf_eliking)

#effect of label and colour
ggplot(LIKING_E, aes(x=label, y=expected_liking, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif( comparisons=list(c("standard", "bitter")), annotations="***", y_position=70,        tip_length=0.01)+
  geom_signif( comparisons=list(c("nolabel", "bitter")), annotations="***", y_position=75,        tip_length=0.01)+
  labs(x="label", y="expected liking")+
  geom_signif(y_position=c(60, 65, 69), xmin=c(0.9, 1.9, 2.9), xmax=c(1.1,2.1, 3.1),
              annotation=c("***"), tip_length=0) +
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()

# body --------------------------------------------------------------------
ebody_model0<-lmer(expected_body~1 + (1|pp_number), BODY_E)
ebody_model1<-update(ebody_model0,.~.+colour)
ebody_model2<-update(ebody_model1, .~.+label)
ebody_model3<-update(ebody_model2,.~.,+ colour:label)


anova(ebody_model0, ebody_model1, ebody_model2, ebody_model3)

pairwise.t.test(BODY_E$expected_body, BODY_E$label, paired =TRUE, p.adjust.method="bonferroni")


BODY_E$pp_number<-as.factor(BODY_E$pp_number)
bf_ebody=anovaBF(expected_body~colour*label+ pp_number, data=BODY_E, whichRandom="pp_number")
bf_ebody
plot(bf_ebody)  

#effect of label and colour
ggplot(BODY_E, aes(x=label, y=expected_body, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  geom_signif( comparisons=list(c("standard", "bitter")), annotations="***", y_position=80,        tip_length=0.01)+
  geom_signif( comparisons=list(c("nolabel", "bitter")), annotations="***", y_position=86,        tip_length=0.01)+
  geom_signif( comparisons=list(c("nolabel", "standard")), annotations="***", y_position=78,        tip_length=0.01)+
  labs(x="label", y="expected body")+
  geom_signif(y_position=c(73, 65, 73), xmin=c(0.9, 1.9, 2.9), xmax=c(1.1,2.1, 3.1),
              annotation=c("***"), tip_length=0) +
  scale_fill_manual(values=c("orange", "orangered4"))+
  theme_light()

