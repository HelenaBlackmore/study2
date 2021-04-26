###attempt for long data
#BITTER
BITTER_WIDE_P<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="bitter")
BITTER_WIDE_E<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="bitter_expectations")
BITTER_WIDE_all<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="bitter_BEP")

BITTER_P<-gather(BITTER_WIDE_P, key=condition, value=bitter,BLB:XDM, factor_key=TRUE)
BITTER_P$colour<-gl(2, 148, 888, labels=c("light", "dark"))
BITTER_P$taste<-gl(2,74,888, labels=c("bitter", "mild"))
BITTER_P$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


BITTER_E<-gather(BITTER_WIDE_E, key=condition, value=expected_bitter,EBLB:EXDM, factor_key=TRUE)
BITTER_E$colour<-gl(2, 148, 888, labels=c("light", "dark"))
BITTER_E$taste<-gl(2,74,888, labels=c("bitter", "mild"))
BITTER_E$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )

BITTER<-gather(BITTER_WIDE_all, key=condition, value=bitter,OBBL_BITTER:XDM_BITTER )
BITTER$colour<-gl(2, 148, 2664, labels=c("light", "dark"))
BITTER$taste<-gl(2,74,2664, labels=c("bitter", "mild"))
BITTER$label<-gl(3,296,2664, labels=c("bitter", "standard", "nolabel") )
BITTER$session<-gl(3,888, 2664, labels=c("blind", "expected", "perceived"))

head(BITTER)




#REFRESHING

REFRESHING_WIDE_P<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="refreshing")
REFRESHING_WIDE_E<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="refreshing_expectations")

REFRESHING_P<-gather(REFRESHING_WIDE_P, key=condition, value=refreshing,BLB:XDM, factor_key=TRUE)
REFRESHING_P$colour<-gl(2, 148, 888, labels=c("light", "dark"))
REFRESHING_P$taste<-gl(2,74,888, labels=c("bitter", "mild"))
REFRESHING_P$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


REFRESHING_E<-gather(REFRESHING_WIDE_E, key=condition, value=expected_refreshing,EBLB:EXDM, factor_key=TRUE)
REFRESHING_E$colour<-gl(2, 148, 888, labels=c("light", "dark"))
REFRESHING_E$taste<-gl(2,74,888, labels=c("bitter", "mild"))
REFRESHING_E$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


REFRESHING_WIDE_all<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="refreshing_BEP")

REFRESHING<-gather(REFRESHING_WIDE_all, key=condition, value=refreshing,OBBL_REFRESHING:XDM_REFRESHING )
REFRESHING$colour<-gl(2, 148, 2664, labels=c("light", "dark"))
REFRESHING$taste<-gl(2,74,2664, labels=c("bitter", "mild"))
REFRESHING$label<-gl(3,296,2664, labels=c("bitter", "standard", "nolabel") )
REFRESHING$session<-gl(3,888, 2664, labels=c("blind", "expected", "perceived"))



#REFRESHING<-merge(REFRESHING_E,REFRESHING_P,by=c("pp_number"))



#LIKING

LIKING_WIDE_P<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="pleasant")
LIKING_WIDE_E<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="pleasant_expectations")

LIKING_P<-gather(LIKING_WIDE_P, key=condition, value=liking,BLB:XDM, factor_key=TRUE)
LIKING_P$colour<-gl(2, 148, 888, labels=c("light", "dark"))
LIKING_P$taste<-gl(2,74,888, labels=c("bitter", "mild"))
LIKING_P$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


LIKING_E<-gather(LIKING_WIDE_E, key=condition, value=expected_liking,EBLB:EXDM, factor_key=TRUE)
LIKING_E$colour<-gl(2, 148, 888, labels=c("light", "dark"))
LIKING_E$taste<-gl(2,74,888, labels=c("bitter", "mild"))
LIKING_E$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


LIKING_WIDE_all<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="liking_BEP")

LIKING<-gather(LIKING_WIDE_all, key=condition, value=liking, OBBL_PLEASANT:XDM_PLEASANT )
LIKING$colour<-gl(2, 148, 2664, labels=c("light", "dark"))
LIKING$taste<-gl(2,74,2664, labels=c("bitter", "mild"))
LIKING$label<-gl(3,296,2664, labels=c("bitter", "standard", "nolabel") )
LIKING$session<-gl(3,888, 2664, labels=c("blind", "expected", "perceived"))




#BODY

BODY_WIDE_P<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="body")
BODY_WIDE_E<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="body_expectations")

BODY_P<-gather(BODY_WIDE_P, key=condition, value=body ,BLB:XDM, factor_key=TRUE)
BODY_P$colour<-gl(2, 148, 888, labels=c("light", "dark"))
BODY_P$taste<-gl(2,74,888, labels=c("bitter", "mild"))
BODY_P$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


BODY_E<-gather(BODY_WIDE_E, key=condition, value=expected_body,EBLB:EXDM, factor_key=TRUE)
BODY_E$colour<-gl(2, 148, 888, labels=c("light", "dark"))
BODY_E$taste<-gl(2,74,888, labels=c("bitter", "mild"))
BODY_E$label<-gl(3,296,888, labels=c("bitter", "standard", "nolabel") )


BODY_WIDE_all<-read_excel("../data/sensory_expectations_tidy.xlsx", sheet="body_BEP")

BODY<-gather(BODY_WIDE_all, key=condition, value=body, OBBL_BODY:XDM_BODY )
BODY$colour<-gl(2, 148, 2664, labels=c("light", "dark"))
BODY$taste<-gl(2,74,2664, labels=c("bitter", "mild"))
BODY$label<-gl(3,296,2664, labels=c("bitter", "standard", "nolabel") )
BODY$session<-gl(3,888, 2664, labels=c("blind", "expected", "perceived"))

#but I need to merge blind, expected and perceived datasets



BLIND_BITTER<-rbind(BLIND_B, BLIND_B, BLIND_B)
BLIND_BODY<-rbind(BLIND_Bo, BLIND_Bo, BLIND_Bo)
BLIND_LIKING<-rbind(BLIND_L, BLIND_L, BLIND_L)
BLIND_REFRESHING<-rbind(BLIND_R , BLIND_R, BLIND_R)

all_merged<-do.call("cbind", list( BLIND_BITTER,BLIND_BODY, BLIND_LIKING,BLIND_REFRESHING, BITTER_E, BODY_E, LIKING_E, REFRESHING_E, BITTER_P, BODY_P, LIKING_P, REFRESHING_P ))


all_merged<-subset(all_merged, select=c(pp_number, age, sex, label, colour, taste, condition, blind_bitter, expected_bitter, bitter, blind_body, expected_body, body, blind_liking, expected_liking, liking, blind_refreshing, expected_refreshing, refreshing))

#now creating new variables prediction error= expected- blind, and perception change= perceived-blind

## variable mismatch, ie. prediction error

all_merged<-mutate(all_merged, mismatch_bitter=expected_bitter-blind_bitter, mismatch_refreshing=expected_refreshing-blind_refreshing, mismatch_liking=expected_liking-blind_liking, mismatch_body=expected_body-blind_body)

##new variable perception_change

all_merged<-all_merged%>%
  mutate(change_bitter=bitter-blind_bitter, change_body=body-blind_body, change_liking=liking-blind_liking, change_refreshing=refreshing-blind_refreshing)
