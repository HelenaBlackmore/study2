# #MEDIATION ANALYSIS
# 
# * expectations as a latent variable
# * expectations mediating the effect of extrinsic and intrinsic product cues on taste/flavour/mouthfeel perception
# * aim: to use a mediation analysis and quantify the relationship between these variables
# * to demonstrate this on behavioural data
# * using mediation package:
#   * uses more recent bootstrapping method (as per Preacher & Hayes, 2004 )--> resulting in better power
# * no assumption of normal distribution of the data
# * especially good for small samples


library(mediation) #Mediation package
library(rockchalk) #Graphing simple slopes; moderation
library(multilevel) #Sobel Test

library(gvlma) #Testing Model Assumptions 
library(stargazer)
library(diagram)

all_merged%>%mutate(descriptor=(if_else(label=="bitter", "1","0")))->all_merged



#BITTER

fitY1<-lmerTest::lmer(bitter ~ taste + colour + descriptor + expected_bitter+ (1|pp_number), data=all_merged)
summary(fitY1)

fitM1<-lme4::lmer(expected_bitter ~ colour + descriptor + (1|pp_number) , data=all_merged)

path_a_bitter<-lmerTest::lmer(expected_bitter ~ colour + descriptor + (1|pp_number) , data=all_merged)
summary(path_a_bitter)  

set.seed(1234)

fit_med1<-mediation::mediate(fitM1, fitY1, treat="colour", mediator="expected_bitter")

fit_med2<-mediation::mediate(fitM1, fitY1, treat="descriptor", mediator = "expected_bitter")

summary(fit_med1)
plot(fit_med1, treatment="treated")

summary(fit_med2, standardized=TRUE)
plot(fit_med2, treatment="treated")



#REFRESHING

fitY_refreshing<-lmerTest::lmer(refreshing ~ taste + colour + descriptor + expected_refreshing+ (1|pp_number), data=all_merged)
summary(fitY_refreshing)

path_a_refreshing<-lmerTest::lmer(expected_refreshing ~ colour + descriptor + (1|pp_number) , data=all_merged)
summary(path_a_refreshing)

fitM_refreshing<-lme4::lmer(expected_refreshing ~ colour + descriptor + (1|pp_number) , data=all_merged)

set.seed(1234)

fit_medR_colour<-mediation::mediate(fitM_refreshing, fitY_refreshing, treat="colour", mediator="expected_refreshing")

fit_medR_desc<-mediation::mediate(fitM_refreshing, fitY_refreshing, treat="descriptor", mediator="expected_refreshing")

summary(fit_medR_colour)
plot(fit_medR_colour, treatment = "treated")

summary(fit_medR_desc)
plot(fit_medR_desc, treatment="treated")




fitY_liking<-lmerTest::lmer(liking ~ taste + colour + descriptor + expected_liking+ (1|pp_number), data=all_merged)


fitM_liking<-lme4::lmer(expected_liking ~ colour + descriptor + (1|pp_number) , data=all_merged)

summary(fitY_liking)

path_a_liking<-lmerTest::lmer(expected_liking ~ colour + descriptor + (1|pp_number) , data=all_merged)

summary(path_a_liking)

set.seed(1234)

fit_medL_colour<-mediation::mediate(fitM_liking, fitY_liking, treat="colour", mediator="expected_liking")

fit_medL_desc<-mediation::mediate(fitM_liking, fitY_liking, treat="descriptor", mediator="expected_liking")

summary(fit_medL_colour)
plot(fit_medR_colour, treatment = "treated")

summary(fit_medL_desc)
plot(fit_medL_desc, treatment="treated")


fitY_body<-lmerTest::lmer(body ~ taste + colour + descriptor + expected_body+ (1|pp_number), data=all_merged)

summary(fitY_body)

fitM_body<-lme4::lmer(expected_body ~ colour + descriptor + (1|pp_number) , data=all_merged)
summary(fitM_body)

a_path_body<-lmerTest::lmer(expected_body ~ colour + descriptor + (1|pp_number) , data=all_merged)
summary(a_path)


set.seed(1234)

fit_medB_colour<-mediation::mediate(fitM_body, fitY_body, treat="colour", mediator="expected_body")

fit_medB_desc<-mediation::mediate(fitM_body, fitY_body, treat="descriptor", mediator="expected_body")

summary(fit_medB_colour)
plot(fit_medB_colour, treatment = "treated")

summary(fit_medB_desc)
plot(fit_medB_desc, treatment="treated")



# mediation plots (diagram) --------------------------------------------


#BODY

library(diagram)
data_medBo <- c(0, "'28.72***'", 0,
                0, 0, 0, 
                "'.20***'", "'  21.50***\n (15.7***)'", 0)
M_body<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data_medBo)
plot_med_body<- plotmat (M_body, pos=c(1,2), 
                         name= c( "expected body","colour", "body"), 
                         box.type = "rect", box.size = 0.12, box.prop=0.4,  curve=0)



#LIKING

data_medL<- c(0, "'-13.71***'", 0,
              0, 0, 0, 
              "'.34***'", "'  -10.85***\n (-5.42***)'", 0)
M_liking<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data_medL)
plot_med_liking<- plotmat (M_liking, pos=c(1,2), 
                           name= c( "expected liking","colour", "liking"), 
                           box.type = "rect", box.size = 0.12, box.prop=0.4,  curve=0)




#REFRESHMENT

data_medR<- c(0, "'-19.84***'", 0,
              0, 0, 0, 
              "'0.35***'", "'  -11.42***\n (-4.43**)'", 0)
M_refreshing<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data_medR)
plot_med_refreshing<- plotmat (M_refreshing, pos=c(1,2), 
                               name= c( "expected refreshment","colour", "refreshment"), 
                               box.type = "rect", box.size = 0.14, box.prop=0.4,  curve=0)


#BITTER

data_medBcol<- c(0, "'20.25***'", 0,
                 0, 0, 0, 
                 "'0.28***'", "'  8.19***\n (2.5)'", 0)
M_bitterC<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data_medBcol)
plot_med_bitter<- plotmat (M_bitterC, pos=c(1,2), 
                           name= c( "expected bitter","colour", "bitter"), 
                           box.type = "rect", box.size = 0.14, box.prop=0.4,  curve=0)

data_medBdesc<- c(0, "'17.24***'", 0,
                  0, 0, 0, 
                  "'0.28***'", "'  7.74***\n (2.9)'", 0)
M_bitterC<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data_medBcol)
plot_med_bitter<- plotmat (M_bitterC, pos=c(1,2), 
                           name= c( "expected bitter","descriptor", "bitter"), 
                           box.type = "rect", box.size = 0.14, box.prop=0.4,  curve=0)







