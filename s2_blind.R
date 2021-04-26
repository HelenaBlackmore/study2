# script for study 2: data wrangling and some descriptive analysis

# libraries ---------------------------------------------------------------



library(ggsignif)
library(gvlma)
library(mediation)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(pastecs)
library(multcomp)
library(nlme)
library(lmerTest)
library(MASS)
library(tidyverse)
library(mvoutlier)
library(lavaan)
library(BayesFactor)
library(jtools)


# read and transform blind data -------------------------------------------



##data transformation
####*reading data from excel and transforming it into a long format*

###blind data bitter

BLIND_WIDE_B<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="blind_bitter")

BLIND_B<-gather(BLIND_WIDE_B, condition, blind_bitter, BL_BITTER:MD_BITTER, factor_key=TRUE )
BLIND_B$colour<-gl(2, 148, 296, labels=c("light", "dark"))
BLIND_B$taste<-gl(2,74,296, labels=c("bitter", "mild"))


###blind data refreshing
BLIND_WIDE_R<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="blind_refreshing")

BLIND_R<-gather(BLIND_WIDE_R, condition, blind_refreshing, BL_REFRESHING:MD_REFRESHING, factor_key=TRUE )
BLIND_R$colour<-gl(2, 148, 296, labels=c("light", "dark"))
BLIND_R$taste<-gl(2,74,296, labels=c("bitter", "mild"))

###blind data liking

BLIND_WIDE_L<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="blind_liking")

BLIND_L<-gather(BLIND_WIDE_L, condition, blind_liking, BL_PLEASANT:MD_PLEASANT, factor_key=TRUE )
BLIND_L$colour<-gl(2, 148, 296, labels=c("light", "dark"))
BLIND_L$taste<-gl(2,74,296, labels=c("bitter", "mild"))

###blind data body

BLIND_WIDE_Bo<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="blind_body")

BLIND_Bo<-gather(BLIND_WIDE_Bo, condition, blind_body, BL_BODY:MD_BODY, factor_key=TRUE )
BLIND_Bo$colour<-gl(2, 148, 296, labels=c("light", "dark"))
BLIND_Bo$taste<-gl(2,74,296, labels=c("bitter", "mild"))


# participants age --------------------------------------------------------


###analysing age of participants
stat.desc(BLIND_WIDE_B)
stat.desc(BLIND_WIDE_Bo)
stat.desc(BLIND_WIDE_R)
stat.desc(BLIND_WIDE_L)

age_plot<-ggplot(BLIND_WIDE_B, aes(x=age))+
  geom_dotplot(aes(fill = sex), binwidth=1/1) +
  scale_fill_manual(values = c("pink", "royalblue"))+
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme_light()+ 
  geom_vline(xintercept = 21.23, linetype="dotted", 
             color = "red", size=1.5)+
  annotate("text", x = 25, y = 16, label ="M=21.28 \n SD=3.56")

age_plot ##some issues with count, need to double check this


# blind data analysis -----------------------------------------------------

# **descriptive statistics of the blind data **



##descriptive stats blind tasting

###bitter

#by(BLIND_B, list(BLIND_B$taste, BLIND_B$colour), stat.desc, basic=FALSE)

ggplot(BLIND_B, aes(taste, blind_bitter, fill=colour), group=colour)+
  geom_boxplot(alpha=0.5)+
  geom_jitter( aes(color=colour), position= position_jitterdodge(jitter.width=0.4, dodge.width=0.7))+
  labs(title = "A", x="taste", y="bitterness")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  scale_color_manual(values=c("orange", "orangered4"))+
  geom_signif(y_position=c(102, 102, 110), xmin=c(0.8, 1.8, 1.0), xmax=c(1.2,2.2, 2.0),
              annotation=c("*"), tip_length=0) +
  scale_y_continuous(limits=c(0, 120), breaks=seq(0,100,25))+
  theme_apa()+
  theme(axis.text.y = element_text(face = "bold", size=16), axis.text.x=element_text(face="bold", size=16), axis.title.x = element_text(color="black", size=14, ), axis.title.y = element_text(color="black", size=14))

ggsave("../images/manipulation_check_BITTER.png")

###refreshing
#by(BLIND_R, list(BLIND_R$taste, BLIND_R$colour), stat.desc, basic=FALSE)

ggplot(BLIND_R, aes(taste, blind_refreshing, fill=colour))+
  geom_boxplot(alpha=0.5)+
  geom_jitter( aes(color=colour), position= position_jitterdodge(jitter.width=0.4, dodge.width=0.7))+
  labs(title = "B ", x="taste", y="refreshment")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  scale_color_manual(values=c("orange", "orangered4"))+
  scale_y_continuous(limits=c(0, 120), breaks=seq(0,100,25))+
  theme_apa()+
  theme(axis.text.y = element_text(face = "bold", size=16), axis.text.x=element_text(face="bold", size=16), axis.title.x = element_text(color="black", size=14, ), axis.title.y = element_text(color="black", size=14))

ggsave("../images/manipulation_check_REFRESHING.png")
###liking
#by(BLIND_L, list(BLIND_L$taste, BLIND_L$colour), stat.desc, basic=FALSE)

ggplot(BLIND_L, aes(taste, blind_liking, fill=colour))+
  geom_boxplot(alpha=0.5)+
  geom_jitter( aes(color=colour), position= position_jitterdodge(jitter.width=0.4, dodge.width=0.7))+
  labs(title = "C", x="taste", y="liking")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  scale_color_manual(values=c("orange", "orangered4"))+
  scale_y_continuous(limits=c(0, 120), breaks=seq(0,100,25))+
  theme_apa()+
  theme(axis.text.y = element_text(face = "bold", size=16), axis.text.x=element_text(face="bold", size=16), axis.title.x = element_text(color="black", size=14, ), axis.title.y = element_text(color="black", size=14))
ggsave("../images/manipulation_check_LIKING.png")

###body
#by(BLIND_Bo, list(BLIND_Bo$taste, BLIND_Bo$colour), stat.desc, basic=FALSE)
blind_body_box<-ggplot(BLIND_Bo, aes(taste, blind_body, fill=colour))+
  geom_boxplot(alpha=0.5)+
  geom_jitter( aes(color=colour), position= position_jitterdodge(jitter.width=0.4, dodge.width=0.7))+
  labs(title = "D", x="taste", y="body")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  scale_color_manual(values=c("orange", "orangered4"))+
  geom_signif(y_position=c(102, 102), xmin=c(0.8, 1.8), xmax=c(1.2,2.2),
              annotation=c("**"), tip_length=0) +
  scale_y_continuous(limits=c(0, 120), breaks=seq(0,100,25))+
  theme_apa()+
  theme(axis.text.y = element_text(face = "bold", size=16), axis.text.x=element_text(face="bold", size=16), axis.title.x = element_text(color="black", size=14, ), axis.title.y = element_text(color="black", size=14))

ggsave("../images/manipulation_check_BODY.png")


# manipulation check ------------------------------------------------------


##BITTER

bitter_blind0<-lmer(blind_bitter~1 + (1|pp_number), BLIND_B)
bitter_blind1<-update(bitter_blind0, .~. + taste)
bitter_blind2<-update(bitter_blind1, .~. + colour)
bitter_blind3<-update(bitter_blind2, .~. +taste:colour)

anova( bitter_blind0, bitter_blind1, bitter_blind2, bitter_blind3)

summary(bitter_blind2)

##BODY


body_blind0<-lmer(blind_body~1 + (1|pp_number), BLIND_Bo)
body_blind1<-update(body_blind0, .~. + taste)
body_blind2<-update(body_blind1, .~. + colour)
body_blind3<-update(body_blind2, .~. +taste:colour)

anova( body_blind0, body_blind1, body_blind2, body_blind3)

summary(body_blind2)

##REFRESHMENT

refreshing_blind0<-lmer(blind_refreshing~1 + (1|pp_number), BLIND_R)
refreshing_blind1<-update(refreshing_blind0, .~. + taste)
refreshing_blind2<-update(refreshing_blind1, .~. + colour)
refreshing_blind3<-update(refreshing_blind2, .~. +taste:colour)

anova( refreshing_blind0, refreshing_blind1, refreshing_blind2, refreshing_blind3)

summary(refreshing_blind3)

#LIKING


liking_blind0<-lmer(blind_liking~1 + (1|pp_number), BLIND_L)
liking_blind1<-update(liking_blind0, .~. + taste)
liking_blind2<-update(liking_blind1, .~. + colour)
liking_blind3<-update(liking_blind2, .~. +taste:colour)

anova( liking_blind0, liking_blind1, liking_blind2, liking_blind3)

summary(liking_blind3)

# * MLM with a random intercept, nesting variable is participant:
#   
#   * *no effect of either colour or taste on perceived REFRESHMENT or LIKING*
#   * *effect of colour and taste on perception of BITTERNESS*
#   * *effect of colour on perceived BODY* 
  
  
  
  #PLOTS-MANIPULATION CHECK

#Plots


ggplot(BLIND_B, aes(taste, blind_bitter, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  labs(title="A", x="taste", y="bitterness")+
  geom_signif(comparisons=list(c("bitter", "mild")), annotations="*", y_position=60,        tip_length=0.0)+
  geom_signif(y_position=c(50, 50), xmin=c(0.9, 1.9), xmax=c(1.1,2.1),
              annotation=c("*"), tip_length=0) +
  scale_fill_manual(values=c("orange", "orangered4"))+
  ylim(0,100)+
  theme_light()

ggplot(BLIND_R, aes(taste, blind_refreshing, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  labs(title="B", x="taste", y="refreshment")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  ylim(0,100)+
  theme_light()

ggplot(BLIND_L, aes(taste, blind_liking, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  labs(title="C", x="taste", y="liking")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  ylim(0,100)+
  theme_light()



body_blind_plot<-ggplot(BLIND_Bo, aes(taste, blind_body, fill=colour))+
  geom_bar(stat="summary", fun.y="mean", position="dodge")+
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3, position="dodge")+
  labs(title="D", x="colour", y="body")+
  scale_fill_manual(values=c("orange", "orangered4"))+
  geom_signif(y_position=63, xmin=1, xmax=2, annotation="**", tip_length=0)+
  ylim(0,100)+
  theme_light()


