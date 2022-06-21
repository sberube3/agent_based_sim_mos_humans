library(ggplot2)
library(tidyverse)
library(lubridate)

csp<- read.csv("csp_haps.csv")

human_haps_csp<- csp[-which(str_detect(csp$sample_name_dbs, " H")==T|str_detect(csp$sample_name_dbs, " A")==T),]

human_haps_csp<- human_haps_csp%>%
  mutate(HH_ID=substr(sample_name_dbs,1,3))%>%
  mutate(unq_memID=paste0(substr(sample_name_dbs,1,3),"_",substr(sample_name_dbs,12,12)))%>%
  mutate(sample_date=substr(sample_name_dbs,5,10))%>%
  mutate(village_ID=substr(sample_name_dbs,1,1))

human_haps_csp$sample_date<- as.Date(human_haps_csp$sample_date,"%d%m%y")

human_haps_csp<- human_haps_csp%>%
  mutate(Season= ifelse(month(sample_date)>=6&month(sample_date)<=10,"Dry", "Rainy"))%>%
  mutate(Haplotypes=str_split(haplotype_list,","))


##pairwise proportion of shared haplotypes between all infections, note this will not be a symmetric matrix.

proportion_haps_shared<- matrix(NA, nrow=nrow(human_haps_csp),ncol=nrow(human_haps_csp))



for(i in 1:nrow(human_haps_csp)){
  for(j in 1:nrow(human_haps_csp)){
    proportion_haps_shared[i,j]<- length(intersect(human_haps_csp$Haplotypes[[i]],
                                                   human_haps_csp$Haplotypes[[j]]))/length(human_haps_csp$Haplotypes[[i]])
      
  }
}
  

#vector of within village "M"
#vector of within village "S"
#vector of within village "K"
#always ignoring the diagonal (samples are completely related to themselves)

within_m<- proportion_haps_shared[which(human_haps_csp$village_ID=="M"),which(human_haps_csp$village_ID=="M")]

diag(within_m)<- NA

within_m_vector<- as.vector(within_m)[-which(is.na(as.vector(within_m)))]

m_vs_sk<- proportion_haps_shared[which(human_haps_csp$village_ID=="M"),which(human_haps_csp$village_ID=="S"|human_haps_csp$village_ID=="K")]

diag(m_vs_sk)<-NA

sk_vs_m<- proportion_haps_shared[which(human_haps_csp$village_ID=="S"|human_haps_csp$village_ID=="K"),which(human_haps_csp$village_ID=="M")]
diag(sk_vs_m)<-NA

m_vs_others_vector<- c(as.vector(m_vs_sk)[-which(is.na(as.vector(m_vs_sk)))])
                       #as.vector(sk_vs_m)[-which(is.na(as.vector(sk_vs_m)))])



  