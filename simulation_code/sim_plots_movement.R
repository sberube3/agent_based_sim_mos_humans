library(ggplot2)
library(tidyverse)

setwd("~/Desktop/mozzie_sim_downloads")

eir_EvenMove001<- readRDS("eir_EvenMove001")
eir_EvenMove005<- readRDS("eir_EvenMove005")
eir_EvenMove01<- readRDS("eir_EvenMove01")
eir_EvenMove015<- readRDS("eir_EvenMove015")
eir_EvenMove02<- readRDS("eir_EvenMove02")

  mos_age_EvenMove001<- readRDS("mos_age_EvenMove001")
  mos_age_EvenMove005<- readRDS("mos_age_EvenMove005")
  mos_age_EvenMove01<- readRDS("mos_age_EvenMove01")
  mos_age_EvenMove015<- readRDS("mos_age_EvenMove015")
  mos_age_EvenMove02<- readRDS("mos_age_EvenMove02")

mos_moi_EvenMove001<- readRDS("mosquito_MOI_EvenMove001")
mos_moi_EvenMove005<- readRDS("mosquito_MOI_EvenMove005")
mos_moi_EvenMove01<- readRDS("mosquito_MOI_EvenMove01")
mos_moi_EvenMove015<- readRDS("mosquito_MOI_EvenMove015")
mos_moi_EvenMove02<- readRDS("mosquito_MOI_EvenMove02")

hum_moi_EvenMove001<- readRDS("human_MOI_EvenMove001")
hum_moi_EvenMove005<- readRDS("human_MOI_EvenMove005")
hum_moi_EvenMove01<- readRDS("human_MOI_EvenMove01")
hum_moi_EvenMove015<- readRDS("human_MOI_EvenMove015")
hum_moi_EvenMove02<- readRDS("human_MOI_EvenMove02")


symp_moi_EvenMove001<- readRDS("symptomatic_MOI_EvenMove001")
symp_moi_EvenMove005<- readRDS("symptomatic_MOI_EvenMove005")
symp_moi_EvenMove01<- readRDS("symptomatic_MOI_EvenMove01")
symp_moi_EvenMove015<- readRDS("symptomatic_MOI_EvenMove015")
symp_moi_EvenMove02<- readRDS("symptomatic_MOI_EvenMove02")


switch_EvenMove001<- readRDS("number_of_switches_per_personEvenMove001")
switch_EvenMove005<- readRDS("number_of_switches_per_personEvenMove005")
switch_EvenMove01<- readRDS("number_of_switches_per_personEvenMove01")
switch_EvenMove015<- readRDS("number_of_switches_per_personEvenMove015")
switch_EvenMove02<- readRDS("number_of_switches_per_personEvenMove02")

eir_EvenMove001_vect<- as.vector(eir_EvenMove001)
eir_EvenMove005_vect<- as.vector(eir_EvenMove005)
eir_EvenMove01_vect<- as.vector(eir_EvenMove01)
eir_EvenMove015_vect<- as.vector(eir_EvenMove015)
eir_EvenMove02_vect<- as.vector(eir_EvenMove02)

mos_age_EvenMove001_vect<- as.vector(mos_age_EvenMove001)
mos_age_EvenMove005_vect<- as.vector(mos_age_EvenMove005)
mos_age_EvenMove01_vect<- as.vector(mos_age_EvenMove01)
mos_age_EvenMove015_vect<- as.vector(mos_age_EvenMove015)
mos_age_EvenMove02_vect<- as.vector(mos_age_EvenMove02)


mos_moi_EvenMove001_vect<- as.vector(mos_moi_EvenMove001)
mos_moi_EvenMove005_vect<- as.vector(mos_moi_EvenMove005)
mos_moi_EvenMove01_vect<- as.vector(mos_moi_EvenMove01)
mos_moi_EvenMove015_vect<- as.vector(mos_moi_EvenMove015)
mos_moi_EvenMove02_vect<- as.vector(mos_moi_EvenMove02)

mos_moi_EvenMove001_clean<-mos_moi_EvenMove001_vect[-c(which(is.na(mos_moi_EvenMove001_vect)),which(mos_moi_EvenMove001_vect==0))]
mos_moi_EvenMove005_clean<-mos_moi_EvenMove005_vect[-c(which(is.na(mos_moi_EvenMove005_vect)),which(mos_moi_EvenMove005_vect==0))]
mos_moi_EvenMove01_clean<-mos_moi_EvenMove01_vect[-c(which(is.na(mos_moi_EvenMove01_vect)),which(mos_moi_EvenMove01_vect==0))]
mos_moi_EvenMove015_clean<-mos_moi_EvenMove015_vect[-c(which(is.na(mos_moi_EvenMove015_vect)),which(mos_moi_EvenMove015_vect==0))]
mos_moi_EvenMove02_clean<-mos_moi_EvenMove02_vect[-c(which(is.na(mos_moi_EvenMove02_vect)),which(mos_moi_EvenMove02_vect==0))]

hum_moi_EvenMove001_vect<- as.vector(hum_moi_EvenMove001)
hum_moi_EvenMove005_vect<- as.vector(hum_moi_EvenMove005)
hum_moi_EvenMove01_vect<- as.vector(hum_moi_EvenMove01)
hum_moi_EvenMove015_vect<- as.vector(hum_moi_EvenMove015)
hum_moi_EvenMove02_vect<- as.vector(hum_moi_EvenMove02)


hum_moi_EvenMove001_clean<-hum_moi_EvenMove001_vect[-c(which(is.na(hum_moi_EvenMove001_vect)),which(hum_moi_EvenMove001_vect==0))]
hum_moi_EvenMove005_clean<-hum_moi_EvenMove005_vect[-c(which(is.na(hum_moi_EvenMove005_vect)),which(hum_moi_EvenMove005_vect==0))]
hum_moi_EvenMove01_clean<-hum_moi_EvenMove01_vect[-c(which(is.na(hum_moi_EvenMove01_vect)),which(hum_moi_EvenMove01_vect==0))]
hum_moi_EvenMove015_clean<-hum_moi_EvenMove015_vect[-c(which(is.na(hum_moi_EvenMove015_vect)),which(hum_moi_EvenMove015_vect==0))]
hum_moi_EvenMove02_clean<-hum_moi_EvenMove02_vect[-c(which(is.na(hum_moi_EvenMove02_vect)),which(hum_moi_EvenMove02_vect==0))]

symp_moi_EvenMove001_vect<- as.vector(symp_moi_EvenMove001[,(200*365):(200*722)])
symp_moi_EvenMove005_vect<- as.vector(symp_moi_EvenMove005[,(200*365):(200*722)])
symp_moi_EvenMove01_vect<- as.vector(symp_moi_EvenMove01[,(200*365):(200*722)])
symp_moi_EvenMove015_vect<- as.vector(symp_moi_EvenMove015[,(200*365):(200*722)])
symp_moi_EvenMove02_vect<- as.vector(symp_moi_EvenMove02[,(200*365):(200*722)])

symp_moi_EvenMove001_clean<-symp_moi_EvenMove001_vect[-c(which(is.na(symp_moi_EvenMove001_vect)),which(symp_moi_EvenMove001_vect==0))]
symp_moi_EvenMove005_clean<-symp_moi_EvenMove005_vect[-c(which(is.na(symp_moi_EvenMove005_vect)),which(symp_moi_EvenMove005_vect==0))]
symp_moi_EvenMove01_clean<-symp_moi_EvenMove01_vect[-c(which(is.na(symp_moi_EvenMove01_vect)),which(symp_moi_EvenMove01_vect==0))]
symp_moi_EvenMove015_clean<-symp_moi_EvenMove015_vect[-c(which(is.na(symp_moi_EvenMove015_vect)),which(symp_moi_EvenMove015_vect==0))]
symp_moi_EvenMove02_clean<-symp_moi_EvenMove02_vect[-c(which(is.na(symp_moi_EvenMove02_vect)),which(symp_moi_EvenMove02_vect==0))]

hum_moi_EvenMove001_combined<- c(hum_moi_EvenMove001_clean,symp_moi_EvenMove001_clean)
hum_moi_EvenMove005_combined<- c(hum_moi_EvenMove005_clean,symp_moi_EvenMove005_clean)
hum_moi_EvenMove01_combined<- c(hum_moi_EvenMove01_clean,symp_moi_EvenMove01_clean)
hum_moi_EvenMove015_combined<- c(hum_moi_EvenMove015_clean,symp_moi_EvenMove015_clean)
hum_moi_EvenMove02_combined<- c(hum_moi_EvenMove02_clean,symp_moi_EvenMove02_clean)




median_eir_EvenMove001<-median(eir_EvenMove001_vect/2)
median_eir_EvenMove005<-median(eir_EvenMove005_vect/2)
median_eir_EvenMove01<-median(eir_EvenMove01_vect/2)
median_eir_EvenMove015<-median(eir_EvenMove015_vect/2)
median_eir_EvenMove02<-median(eir_EvenMove02_vect/2)

upper95_eir_EvenMove001<- quantile(eir_EvenMove001_vect/2,0.975)
upper95_eir_EvenMove005<- quantile(eir_EvenMove005_vect/2,0.975)
upper95_eir_EvenMove01<- quantile(eir_EvenMove01_vect/2,0.975)
upper95_eir_EvenMove015<- quantile(eir_EvenMove015_vect/2,0.975)
upper95_eir_EvenMove02<- quantile(eir_EvenMove02_vect/2,0.975)

lower95_eir_EvenMove001<- quantile(eir_EvenMove001_vect/2,0.025)
lower95_eir_EvenMove005<- quantile(eir_EvenMove005_vect/2,0.025)
lower95_eir_EvenMove01<- quantile(eir_EvenMove01_vect/2,0.025)
lower95_eir_EvenMove015<- quantile(eir_EvenMove015_vect/2,0.025)
lower95_eir_EvenMove02<- quantile(eir_EvenMove02_vect/2,0.025)


median_mos_age_EvenMove001<-median(mos_age_EvenMove001_vect)
median_mos_age_EvenMove005<-median(mos_age_EvenMove005_vect)
median_mos_age_EvenMove01<-median(mos_age_EvenMove01_vect)
median_mos_age_EvenMove015<-median(mos_age_EvenMove015_vect)
median_mos_age_EvenMove02<-median(mos_age_EvenMove02_vect)

upper95_mos_age_EvenMove001<- quantile(mos_age_EvenMove001_vect,0.975)
upper95_mos_age_EvenMove005<- quantile(mos_age_EvenMove005_vect,0.975)
upper95_mos_age_EvenMove01<- quantile(mos_age_EvenMove01_vect,0.975)
upper95_mos_age_EvenMove015<- quantile(mos_age_EvenMove015_vect,0.975)
upper95_mos_age_EvenMove02<- quantile(mos_age_EvenMove02_vect,0.975)

lower95_mos_age_EvenMove001<- quantile(mos_age_EvenMove001_vect,0.025)
lower95_mos_age_EvenMove005<- quantile(mos_age_EvenMove005_vect,0.025)
lower95_mos_age_EvenMove01<- quantile(mos_age_EvenMove01_vect,0.025)
lower95_mos_age_EvenMove015<- quantile(mos_age_EvenMove015_vect,0.025)
lower95_mos_age_EvenMove02<- quantile(mos_age_EvenMove02_vect,0.025)


median_mos_moi_EvenMove001<-median(mos_moi_EvenMove001_clean)
median_mos_moi_EvenMove005<-median(mos_moi_EvenMove005_clean)
median_mos_moi_EvenMove01<-median(mos_moi_EvenMove01_clean)
median_mos_moi_EvenMove015<-median(mos_moi_EvenMove015_clean)
median_mos_moi_EvenMove02<-median(mos_moi_EvenMove02_clean)

upper95_mos_moi_EvenMove001<- quantile(mos_moi_EvenMove001_clean,0.975)
upper95_mos_moi_EvenMove005<- quantile(mos_moi_EvenMove005_clean,0.975)
upper95_mos_moi_EvenMove01<- quantile(mos_moi_EvenMove01_clean,0.975)
upper95_mos_moi_EvenMove015<- quantile(mos_moi_EvenMove015_clean,0.975)
upper95_mos_moi_EvenMove02<- quantile(mos_moi_EvenMove02_clean,0.975)

lower95_mos_moi_EvenMove001<- quantile(mos_moi_EvenMove001_clean,0.025)
lower95_mos_moi_EvenMove005<- quantile(mos_moi_EvenMove005_clean,0.025)
lower95_mos_moi_EvenMove01<- quantile(mos_moi_EvenMove01_clean,0.025)
lower95_mos_moi_EvenMove015<- quantile(mos_moi_EvenMove015_clean,0.025)
lower95_mos_moi_EvenMove02<- quantile(mos_moi_EvenMove02_clean,0.025)

median_hum_moi_EvenMove001<-median(hum_moi_EvenMove001_combined)
median_hum_moi_EvenMove005<-median(hum_moi_EvenMove005_combined)
median_hum_moi_EvenMove01<-median(hum_moi_EvenMove01_combined)
median_hum_moi_EvenMove015<-median(hum_moi_EvenMove015_combined)
median_hum_moi_EvenMove02<-median(hum_moi_EvenMove02_combined)

upper95_hum_moi_EvenMove001<- quantile(hum_moi_EvenMove001_combined,0.975)
upper95_hum_moi_EvenMove005<- quantile(hum_moi_EvenMove005_combined,0.975)
upper95_hum_moi_EvenMove01<- quantile(hum_moi_EvenMove01_combined,0.975)
upper95_hum_moi_EvenMove015<- quantile(hum_moi_EvenMove015_combined,0.975)
upper95_hum_moi_EvenMove02<- quantile(hum_moi_EvenMove02_combined,0.975)

lower95_hum_moi_EvenMove001<- quantile(hum_moi_EvenMove001_combined,0.025)
lower95_hum_moi_EvenMove005<- quantile(hum_moi_EvenMove005_combined,0.025)
lower95_hum_moi_EvenMove01<- quantile(hum_moi_EvenMove01_combined,0.025)
lower95_hum_moi_EvenMove015<- quantile(hum_moi_EvenMove015_combined,0.025)
lower95_hum_moi_EvenMove02<- quantile(hum_moi_EvenMove02_combined,0.025)


median_switch_EvenMove001<-median(switch_EvenMove001)
median_switch_EvenMove005<-median(switch_EvenMove005)
median_switch_EvenMove01<-median(switch_EvenMove01)
median_switch_EvenMove015<-median(switch_EvenMove015)
median_switch_EvenMove02<-median(switch_EvenMove02)

upper95_switch_EvenMove001<- quantile(switch_EvenMove001,0.975)
upper95_switch_EvenMove005<- quantile(switch_EvenMove005,0.975)
upper95_switch_EvenMove01<- quantile(switch_EvenMove01,0.975)
upper95_switch_EvenMove015<- quantile(switch_EvenMove015,0.975)
upper95_switch_EvenMove02<- quantile(switch_EvenMove02,0.975)

lower95_switch_EvenMove001<- quantile(switch_EvenMove001,0.025)
lower95_switch_EvenMove005<- quantile(switch_EvenMove005,0.025)
lower95_switch_EvenMove01<- quantile(switch_EvenMove01,0.025)
lower95_switch_EvenMove015<- quantile(switch_EvenMove015,0.025)
lower95_switch_EvenMove02<- quantile(switch_EvenMove02,0.025)


moving_df<- as.data.frame(cbind(c(median_eir_EvenMove001,median_eir_EvenMove005,median_eir_EvenMove01,
                                    median_eir_EvenMove015,median_eir_EvenMove02,
                                    median_mos_age_EvenMove001,median_mos_age_EvenMove005,median_mos_age_EvenMove01,
                                    median_mos_age_EvenMove015,median_mos_age_EvenMove02,
                                    median_mos_moi_EvenMove001,median_mos_moi_EvenMove005,median_mos_moi_EvenMove01,
                                    median_mos_moi_EvenMove015,median_mos_moi_EvenMove02,
                                    median_hum_moi_EvenMove001,median_hum_moi_EvenMove005,median_hum_moi_EvenMove01,
                                    median_hum_moi_EvenMove015,median_hum_moi_EvenMove02,
                                    median_switch_EvenMove001,median_switch_EvenMove005,median_switch_EvenMove01,
                                    median_switch_EvenMove015,median_switch_EvenMove02),
                                  c(upper95_eir_EvenMove001,upper95_eir_EvenMove005,upper95_eir_EvenMove01,
                                    upper95_eir_EvenMove015,upper95_eir_EvenMove02,
                                    upper95_mos_age_EvenMove001,upper95_mos_age_EvenMove005,upper95_mos_age_EvenMove01,
                                    upper95_mos_age_EvenMove015,upper95_mos_age_EvenMove02,
                                    upper95_mos_moi_EvenMove001,upper95_mos_moi_EvenMove005,upper95_mos_moi_EvenMove01,
                                    upper95_mos_moi_EvenMove015,upper95_mos_moi_EvenMove02,
                                    upper95_hum_moi_EvenMove001,upper95_hum_moi_EvenMove005,upper95_hum_moi_EvenMove01,
                                    upper95_hum_moi_EvenMove015,upper95_hum_moi_EvenMove02,
                                    upper95_switch_EvenMove001,upper95_switch_EvenMove005, upper95_switch_EvenMove01,
                                    upper95_switch_EvenMove015,upper95_switch_EvenMove02),
                                  c(lower95_eir_EvenMove001,lower95_eir_EvenMove005,lower95_eir_EvenMove01,
                                    lower95_eir_EvenMove015,lower95_eir_EvenMove02,
                                    lower95_mos_age_EvenMove001,lower95_mos_age_EvenMove005,lower95_mos_age_EvenMove01,
                                    lower95_mos_age_EvenMove015,lower95_mos_age_EvenMove02,
                                    lower95_mos_moi_EvenMove001,lower95_mos_moi_EvenMove005,lower95_mos_moi_EvenMove01,
                                    lower95_mos_moi_EvenMove015,lower95_mos_moi_EvenMove02,
                                    lower95_hum_moi_EvenMove001,lower95_hum_moi_EvenMove005,lower95_hum_moi_EvenMove01,
                                    lower95_hum_moi_EvenMove015,lower95_hum_moi_EvenMove02,
                                    lower95_switch_EvenMove001,lower95_switch_EvenMove005,lower95_switch_EvenMove01,
                                    lower95_switch_EvenMove015,lower95_switch_EvenMove02),
                                  c(rep(c("EIR","Mosquito Age (Days)", "Mosquito MOI", "Human MOI", "Number of Switches"),each=5)),
                                  c(rep(c(0.01,0.05,0.1,0.15,0.2),times=5))))

colnames(moving_df)<- c("Y", "Upper95", "Lower95","Variable", "Pr")

moving_df$Y<- as.numeric(moving_df$Y)
moving_df$Upper95<- as.numeric(moving_df$Upper95)
moving_df$Lower95<- as.numeric(moving_df$Lower95)
moving_df$Pr<- as.numeric(moving_df$Pr)

#clearing_df_noAge<- clearing_df%>%
  #filter(Variable!="Mosquito Age (Days)")

moving_plot<-ggplot(moving_df,aes(x=Pr,y=Y))+
  geom_errorbar(aes(ymin=Lower95,ymax=Upper95),width=.1)+
  geom_line()+
  geom_point()+ 
  facet_wrap(~Variable,scales="free",ncol=4)+xlab("Pr of Moving Between Areas")+
  theme_classic()+ylab(NULL)+theme(legend.position="none")

