
#MEDIATION ANALYSIS: REPEATED MEASURES MLM

# set-up ------------------------------------------------------------------



library(bmlm)
library(qgraph)





#prepare dataset

datBIT<-all_merged%>%
  select(pp_number, expected_bitter, bitter, colour, taste, descriptor)

datREF<-all_merged%>%
  select(pp_number, expected_refreshing, refreshing, colour, taste, descriptor)

datLIKE<-all_merged%>%
  select(pp_number, expected_liking, liking, colour, taste, descriptor)

datBODY<-all_merged%>%
  select(pp_number, expected_body, body, colour, taste, descriptor)

# BITTERNESS --------------------------------------------------------------
## colour ------------------------------------------------------------------



datBIT$colour=as.numeric(datBIT$colour)
datBIT$descriptor=as.numeric(datBIT$descriptor)

#isolate within pp effects
dat_bit_col<-isolate(datBIT, by="pp_number", value=c("colour",
                                       "bitter", "expected_bitter"))      # isolating within pp effects//values

set.seed(123)
fit_col_bit <- mlm(d = dat_bit_col, 
                   id = "pp_number",
                   x = "colour_cw",
                   m = "expected_bitter_cw",
                   y = "bitter_cw",
                   iter = 2000, 
                   cores = 4)

mlm_summary(fit_col_bit)

mlm_path_plot(fit_col_bit, level = .95, text = T,
              xlab = "colour",
              mlab = "expected \nbitterness",
              ylab = "bitterness", digits = 2)   # path plot
#ggsave("images/med_col_bit.png")
# plot of coefficients
mlm_pars_plot(fit_col_bit, type = "coef", level = .95)
#ggsave("images/medCoef_col_bit.png")


# spag_plot_bit<-mlm_spaghetti_plot(mod = fit_col_bit, d = dat_bit_col,id="pp_number",
#                                   x = "colour_cw",
#                                   m = "expected_bitter_cw",
#                                   y = "bitter_cw")
#interesting, but not necessary atthe moment. 
#more info can be found in this tutorial: https://mvuorre.github.io/bmlm/articles/bmlm-blch9/bmlm-blch9.html#tips-and-tricks
# library(gridExtra)
# grid.arrange(
#   spag_plot_bit[[1]] + labs(title="Path a (X -> M)"), 
#   spag_plot_bit[[2]] + labs(title="Path b (M -> Y)"), 
#   nrow=1)
# 
# spag_plot_bit[[1]]
# spag_plot_bit[[2]]
# 
# 
# #person specific effects
# 
# mlm_pars_plot(fit_col_bit, pars = "u_cp", type = "coef", level = .95) #credible interval

#p value?


## descriptor --------------------------------------------------------------



datBIT$descriptor=as.numeric(datBIT$descriptor)

#isolate within pp effects
# _cw variables now contain isolated within-person (“subject-mean deviated”) pieces of each variable.
# We’ll use these for the mediation analysis.

dat_bit_desc<-isolate(datBIT, by="pp_number", value=c("descriptor","bitter", "expected_bitter"))      # isolating within pp effects//values

fit_desc_bit <- mlm(d = dat_bit_desc, 
                   id = "pp_number",
                   x = "descriptor_cw",
                   m = "expected_bitter_cw",
                   y = "bitter_cw",
                   iter = 2000, 
                   cores = 4)

mlm_summary(fit_desc_bit)

mlm_path_plot(fit_desc_bit, level = .95, text = T,
              xlab = "descriptor",
              mlab = "expected \nbitterness",
              ylab = "bitterness", digits = 2)   # path plot
#ggsave("images/med_desc_bit.png")

# plot of coefficients
mlm_pars_plot(fit_desc_bit, type = "coef", level = .95)
#ggsave("images/medCoef_desc_bit.png")


# spag_plot_bit2<-mlm_spaghetti_plot(mod = fit_desc_bit, d = dat_bit_col,id="pp_number",
#                                   x = "descriptor_cw",
#                                   m = "expected_bitter_cw",
#                                   y = "bitter_cw")





# REFRESHMENT -------------------------------------------------------------


## descriptor --------------------------------------------------------------


datREF$descriptor=as.numeric(datREF$descriptor)

#isolate within pp effects
# _cw variables now contain isolated within-person (“subject-mean deviated”) pieces of each variable.
# We’ll use these for the mediation analysis.

dat_ref_desc<-isolate(datREF, by="pp_number", value=c("descriptor","refreshing", "expected_refreshing"))      # isolating within pp effects//values

fit_desc_ref <- mlm(d = dat_ref_desc, 
                    id = "pp_number",
                    x = "descriptor_cw",
                    m = "expected_refreshing_cw",
                    y = "refreshing_cw",
                    iter = 2000, 
                    cores = 4)

mlm_summary(fit_desc_ref)

mlm_path_plot(fit_desc_ref, level = .95, text = T,
              xlab = "descriptor",
              mlab = "expected \nrefreshment",
              ylab = "refreshment", digits = 2)   # path plot
#ggsave("images/med_desc_ref.png")
# plot of coefficients
mlm_pars_plot(fit_desc_ref, type = "coef", level = .95)



## colour ------------------------------------------------------------------
datREF$colour=as.numeric(datREF$colour)


dat_ref_col<-isolate(datREF, by="pp_number", value=c("colour","refreshing", "expected_refreshing"))      # isolating within pp effects//values

fit_col_ref <- mlm(d = dat_ref_col, 
                    id = "pp_number",
                    x = "colour_cw",
                    m = "expected_refreshing_cw",
                    y = "refreshing_cw",
                    iter = 2000, 
                    cores = 4)

mlm_summary(fit_col_ref)

mlm_path_plot(fit_col_ref, level = .95, text = T,
              xlab = "colour",
              mlab = "expected \nrefreshment",
              ylab = "refreshment", digits = 2)   # path plot
#ggsave("images/med_col_ref.png")
# plot of coefficients
mlm_pars_plot(fit_col_ref, type = "coef", level = .95)



# LIKING ------------------------------------------------------------------


## colour ------------------------------------------------------------------

datLIKE$colour=as.numeric(datLIKE$colour)


dat_like_col<-isolate(datLIKE, by="pp_number", value=c("colour","liking", "expected_liking"))      # isolating within pp effects//values

fit_col_like <- mlm(d = dat_like_col, 
                   id = "pp_number",
                   x = "colour_cw",
                   m = "expected_liking_cw",
                   y = "liking_cw",
                   iter = 2000, 
                   cores = 4)

mlm_summary(fit_col_like)

mlm_path_plot(fit_col_like, level = .95, text = T,
              xlab = "colour",
              mlab = "expected \nliking",
              ylab = "liking", digits = 2)   # path plot
#ggsave("images/med_col_like.png")
# plot of coefficients
mlm_pars_plot(fit_col_like, type = "coef", level = .95)
#ggsave("images/medCoef_col_like.png")

## descriptor --------------------------------------------------------------


datLIKE$descriptor=as.numeric(datLIKE$descriptor)


dat_like_desc<-isolate(datLIKE, by="pp_number", value=c("descriptor","liking", "expected_liking"))      # isolating within pp effects//values

fit_desc_like <- mlm(d = dat_like_desc, 
                    id = "pp_number",
                    x = "descriptor_cw",
                    m = "expected_liking_cw",
                    y = "liking_cw",
                    iter = 2000, 
                    cores = 4)

mlm_summary(fit_desc_like)

mlm_path_plot(fit_desc_like, level = .95, text = T,
              xlab = "descriptor",
              mlab = "expected \nliking",
              ylab = "liking", digits = 2)   # path plot
#ggsave("images/med_desc_like.png")
# plot of coefficients
mlm_pars_plot(fit_desc_like, type = "coef", level = .95)
#ggsave("images/medCoef_desc_like.png")

# BODY --------------------------------------------------------------------


## descriptor --------------------------------------------------------------




datBODY$descriptor=as.numeric(datBODY$descriptor)


dat_body_desc<-isolate(datBODY, by="pp_number", value=c("descriptor","body", "expected_body"))      # isolating within pp effects//values

fit_desc_body <- mlm(d = dat_body_desc, 
                     id = "pp_number",
                     x = "descriptor_cw",
                     m = "expected_body_cw",
                     y = "body_cw",
                     iter = 2000, 
                     cores = 4)

mlm_summary(fit_desc_body)

mlm_path_plot(fit_desc_body, level = .95, text = T,
              xlab = "descriptor",
              mlab = "expected \nbody",
              ylab = "body", digits = 2)   # path plot
#ggsave("images/med_desc_body.png")
# plot of coefficients
mlm_pars_plot(fit_desc_body, type = "coef", level = .95)
#ggsave("images/medCoef_desc_body.png")

## colour ------------------------------------------------------------------


datBODY$colour=as.numeric(datBODY$colour)


dat_body_col<-isolate(datBODY, by="pp_number", value=c("colour","body", "expected_body"))      # isolating within pp effects//values

fit_col_body <- mlm(d = dat_body_col, 
                     id = "pp_number",
                     x = "colour_cw",
                     m = "expected_body_cw",
                     y = "body_cw",
                     iter = 2000, 
                     cores = 4)

mlm_summary(fit_col_body)

mlm_path_plot(fit_col_body, level = .95, text = T,
              xlab = "colour",
              mlab = "expected \nbody",
              ylab = "body", digits = 2)   # path plot
ggsave("images/med_col_body.png")
# plot of coefficients
mlm_pars_plot(fit_col_body, type = "coef", level = .95)
ggsave("images/medCoef_col_body.png")