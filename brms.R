# based on https://www.rensvandeschoot.com/tutorials/brms-started/ tutorial

library(rstan)
library(Rtools)
library(brms) # for the analysis
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(cowplot)

#source('r_docs/main_read_wrangle.R')

# functions and wrangling---------------------------------------------------------------

# To colour code the extremes, we need to write a small function that calculates the regression lines and adds a collumn indicating which clusters have the most extreme.
f1 <- function(data, x, y, grouping, n.highest = 3, n.lowest = 3){
  groupinglevel <- data[,grouping]
  res           <- data.frame(coef = rep(NA, length(unique(groupinglevel))), group = unique(groupinglevel))
  names(res)    <- c("coef", grouping)
  for(i in 1:length(unique(groupinglevel))){
    data2    <- as.data.frame(data[data[,grouping] == i,])
    res[i,1] <- as.numeric(lm(data2[, y] ~ data2[, x])$coefficients[2])
  }
  top    <- res %>% top_n(n.highest, coef)
  bottom <- res %>% top_n(-n.lowest, coef)
  res    <- res %>% mutate(high_and_low = ifelse(coef %in% top$coef, "top",  ifelse(coef %in% bottom$coef, "bottom", "none")))
  data3  <- left_join(data, res)
  return(data3)
}

# head(all_merged)
all_merged%>%mutate(descriptor=(if_else(label=="bitter", "1","0")))->all_merged

# BITTER ------------------------------------------------------------------


## Expectations ------------------------------------------------------------



#intercept only model

interceptonlymodeltest <- brm(expected_bitter ~ 1 + (1 | pp_number), 
                              data   = all_merged, 
                              warmup = 1000, 
                              iter   = 3000, 
                              chains = 2, 
                              inits  = "random",
                              cores  = 2,
                              seed=123)  
#the cores function tells STAN to make use of 2 CPU cores simultaneously instead of just 1.

#be_interceptonly<-summary(interceptonlymodeltest)

#ICC = Intraclass correlation (ICC)
#calculated as below, but for some reason the code does not work
#ICC=(11.32)^2/((11.32)^2+(23.45)^2)=0.18

# hyp <- "sd_class__Intercept^2 / (sd_class__Intercept^2 + sigma^2) = 0"
# hypothesis(interceptonlymodeltest, hyp, class = NULL)


#4. Now we can add first (student) level predictors. For now, we just add them as fixed effects and not yet as random slopes. 
#Furthermore, we do not yet specify any priors for the regression coefficients, which means that BRMS will pick priors that are non or very weakly informative, so that their influence on the results will be negligible.


ebitter_brms <- brm(expected_bitter ~ 1  + colour + descriptor  +  (1|pp_number),  
              data = all_merged, 
              warmup = 1000, iter = 3000, 
              cores = 2, chains = 2, 
              seed = 123) #to run the model

summary(ebitter_brms)


#catterpillar plots:

ebitter_transformed <- ggs(ebitter_brms)
# catterpillar plots: visualise convergence of chains

ggplot(filter(ebitter_transformed, Parameter %in% c("b_Intercept", "b_colourdark", "b_descriptor1", "b_tastemild")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")

# visualising posterior distribution
# we can visually represent them in density plots. If we do so, we clearly see that zero is not included in any of the density plots, 
# meaning that we can be reasonably certain the regression coefficients are different from zero.

#intercept

ggplot(filter(ebitter_transformed, Parameter == "b_Intercept", Iteration > 1000), aes(x = value))+
  geom_density(fill = "orange", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits = c(-1, 50))+ 
 geom_vline(xintercept = summary(ebitter_brms)$fixed[1,3:4], col = "blue", linetype = 2)+
  theme_light()+
  labs(title = "Posterior Density of Regression Coefficient for Intercept")

# colour
ggplot(filter(ebitter_transformed, Parameter == "b_colourdark", Iteration > 1000), aes(x = value))+
  geom_density(fill = "red", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits = c(-1, 30))+ 
  geom_vline(xintercept = summary(ebitter_brms)$fixed[2,3:4], col = "blue", linetype = 2)+
  theme_cowplot()+
  labs(title = "Posterior Density of Regression Coefficient for colour")

#descriptor
ggplot(filter(ebitter_transformed, Parameter == "b_descriptor1", Iteration > 1000), aes(x = value))+
  geom_density(fill = "red", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits=c(-1, 25))+ 
  geom_vline(xintercept = summary(ebitter_brms)$fixed[3,3:4], col = "blue", linetype = 2)+
  theme_cowplot()+
  labs(title = "Posterior Density of Regression Coefficient for descriptor")


# mlm random slopes (all variables)
ebitter_brms2 <- brm(expected_bitter ~ 1 + colour + descriptor  +
                      (1  + descriptor + colour | pp_number),  
                    data = all_merged, 
                    warmup = 1000, iter = 3000, 
                    cores = 2, chains = 2, 
                    seed = 123)

summary(ebitter_brms2)

#adding  first level interactions 
ebitter_brms3 <- brm(expected_bitter ~ 1  + colour + descriptor  + 
                                  colour:descriptor +
                            (1  + descriptor + colour + taste| pp_number),  
              data = all_merged, 
              warmup = 300, iter = 1200, control = list(adapt_delta = 0.97),
              cores = 2, chains = 2, 
              seed = 123)

summary(ebitter_brms3)

ebitter3_transformed <- ggs(ebitter_brms3)

#colour
ggplot(filter(ebitter3_transformed, Parameter == "b_colourdark", Iteration > 1000), aes(x = value))+
  geom_density(fill = "red", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits = c(-1, 30))+ 
  geom_vline(xintercept = summary(ebitter_brms3)$fixed[2,3:4], col = "blue", linetype = 2)+
  geom_vline(xintercept = summary(ebitter_brms3)$fixed[2,1], col = "black", linetype = 3)+
  theme_cowplot()+
  labs(title = "Posterior Density of Regression Coefficient", subtitle = "Effect of dark beer colour on expectations of bitterness")

#descriptor
ggplot(filter(ebitter3_transformed, Parameter == "b_descriptor1", Iteration > 1000), aes(x = value))+
  geom_density(fill = "red", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits=c(-1, 30))+ 
  geom_vline(xintercept = summary(ebitter_brms3)$fixed[3,3:4], col = "blue", linetype = 2)+
  geom_vline(xintercept = summary(ebitter_brms3)$fixed[3,1], col = "black", linetype = 3)+
  theme_cowplot()+
  labs(title = "Posterior Density of Regression Coefficient",subtitle = "Effect of descriptor 'bitter'on expected bitterness ")

#interaction

ggplot(filter(ebitter3_transformed, Parameter == "b_colourdark:descriptor1", Iteration > 1000), aes(x = value))+
  geom_density(fill = "red", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits=c(-15, 1))+ 
  geom_vline(xintercept = summary(ebitter_brms3)$fixed[4,3:4], col = "blue", linetype = 2)+
  geom_vline(xintercept = summary(ebitter_brms3)$fixed[4,1], col = "black", linetype = 3)+
  theme_cowplot()+
  labs(title = "Posterior Density of Regression Coefficient",subtitle = "interaction between descriptor and beer colour")



#making sense of the interaction:
#i need to plot colour and descriptor 

library(sjPlot)
library(sjmisc)
library(ggplot2)

theme_set(theme_cowplot())

plot_int<-plot_model(ebitter_brms3, type = "int")+
  scale_shape_discrete(labels = c("bitter", "control"))+
  scale_y_continuous( name="expected bitterness", limits = c(0,100) )+
  labs(title="expected bitterness", subtitle = "Interaction between beer colour and sensory descriptor 'bitter'")

  
