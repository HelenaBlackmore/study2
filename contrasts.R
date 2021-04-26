
# planned comparison ------------------------------------------------------

bittervscontrol<-c(-2,1,1)

novsstandard<-c(0,-1,1)


contrasts(BITTER_P$label)<-cbind(bittervscontrol, novsstandard)
contrasts(BITTER_E$label)<-cbind(bittervscontrol, novsstandard)

contrasts(REFRESHING_P$label)<-cbind(bittervscontrol, novsstandard)
contrasts(REFRESHING_E$label)<-cbind(bittervscontrol, novsstandard)

contrasts(LIKING_P$label)<-cbind(bittervscontrol, novsstandard)
contrasts(LIKING_E$label)<-cbind(bittervscontrol, novsstandard)

contrasts(BODY_P$label)<-cbind(bittervscontrol, novsstandard)
contrasts(BODY_E$label)<-cbind(bittervscontrol, novsstandard)
