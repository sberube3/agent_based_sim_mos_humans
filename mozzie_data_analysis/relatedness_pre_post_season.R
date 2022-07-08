library(ggplot2)
library(tidyverse)
library(lubridate)
library(Rcpp)

csp<- read.csv("csp_haps.csv")

human_haps_csp<- csp[-which(str_detect(csp$sample_name_dbs, " H")==T|str_detect(csp$sample_name_dbs, " A")==T),]

human_haps_csp<- human_haps_csp%>%
  mutate(HH_ID=substr(sample_name_dbs,1,3))%>%
  mutate(unq_memID=paste0(substr(sample_name_dbs,1,3),"_",substr(sample_name_dbs,12,12)))%>%
  mutate(sample_date=substr(sample_name_dbs,5,10))%>%
  mutate(village_ID=substr(sample_name_dbs,1,1))

human_haps_csp$sample_date<- as.Date(human_haps_csp$sample_date,"%d%m%y")

human_haps_csp<- human_haps_csp%>%
  mutate(Season= ifelse(month(sample_date)>=4&month(sample_date)<=7,"High", "Low"))%>%
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

m_vs_others_vector<- c(as.vector(m_vs_sk)[-which(is.na(as.vector(m_vs_sk)))],
                       as.vector(sk_vs_m)[-which(is.na(as.vector(sk_vs_m)))])

####################################

within_s<- proportion_haps_shared[which(human_haps_csp$village_ID=="S"),which(human_haps_csp$village_ID=="S")]

diag(within_s)<- NA

within_s_vector<- as.vector(within_s)[-which(is.na(as.vector(within_s)))]

s_vs_mk<- proportion_haps_shared[which(human_haps_csp$village_ID=="S"),which(human_haps_csp$village_ID=="M"|human_haps_csp$village_ID=="K")]

diag(s_vs_mk)<-NA

mk_vs_s<- proportion_haps_shared[which(human_haps_csp$village_ID=="M"|human_haps_csp$village_ID=="K"),which(human_haps_csp$village_ID=="S")]
diag(mk_vs_s)<-NA

s_vs_others_vector<- c(as.vector(s_vs_mk)[-which(is.na(as.vector(s_vs_mk)))],
                       as.vector(mk_vs_s)[-which(is.na(as.vector(mk_vs_s)))])

####################
within_k<- proportion_haps_shared[which(human_haps_csp$village_ID=="K"),which(human_haps_csp$village_ID=="K")]

diag(within_k)<- NA

within_k_vector<- as.vector(within_k)[-which(is.na(as.vector(within_k)))]

k_vs_ms<- proportion_haps_shared[which(human_haps_csp$village_ID=="K"),which(human_haps_csp$village_ID=="M"|human_haps_csp$village_ID=="S")]

diag(k_vs_ms)<-NA

ms_vs_k<- proportion_haps_shared[which(human_haps_csp$village_ID=="M"|human_haps_csp$village_ID=="S"),which(human_haps_csp$village_ID=="K")]
diag(ms_vs_k)<-NA

k_vs_others_vector<- c(as.vector(k_vs_ms)[-which(is.na(as.vector(k_vs_ms)))],
                       as.vector(ms_vs_k)[-which(is.na(as.vector(ms_vs_k)))])


#make plot between village

between_village_proportion_shared_df<- as.data.frame(cbind(c(within_m_vector,
                                          m_vs_others_vector,within_k_vector,
                                          k_vs_others_vector,within_s_vector,
                                          s_vs_others_vector),
                                          c(rep("Within", length(within_m_vector)),
                                            rep("Between", length(m_vs_others_vector)),
                                            rep("Within", length(within_k_vector)),
                                            rep("Between", length(k_vs_others_vector)),
                                            rep("Within", length(within_s_vector)),
                                            rep("Between", length(s_vs_others_vector))),
                                          c(rep("M",length(c(within_m_vector,m_vs_others_vector))),
                                            rep("K",length(c(within_k_vector,k_vs_others_vector))),
                                            rep("S",length(c(within_s_vector,s_vs_others_vector))))))

colnames(between_village_proportion_shared_df)<- c("Pairwise_dist", "Comparison", "Village")

between_village_proportion_shared_df$Pairwise_dist<- as.numeric(between_village_proportion_shared_df$Pairwise_dist)

village_prop_shared_plot<- ggplot(between_village_proportion_shared_df,aes(x=Pairwise_dist,color=Comparison))+
  geom_density()+theme_classic()+xlab("Proportion of Shared Haplotypes")+facet_wrap(~Village)


#rainy and dry season comparison

within_High<- proportion_haps_shared[which(human_haps_csp$Season=="High"),which(human_haps_csp$Season=="High")]

diag(within_High)<- NA

within_High_vector<- as.vector(within_High)[-which(is.na(as.vector(within_High)))]

High_vs_Low<- proportion_haps_shared[which(human_haps_csp$Season=="High"),which(human_haps_csp$Season=="Low")]

diag(High_vs_Low)<-NA

Low_vs_High<- proportion_haps_shared[which(human_haps_csp$Season=="Low"),which(human_haps_csp$Season=="High")]
diag(Low_vs_High)<-NA

between_season_vector<- c(as.vector(High_vs_Low)[-which(is.na(as.vector(High_vs_Low)))],
                       as.vector(Low_vs_High)[-which(is.na(as.vector(Low_vs_High)))])


within_Low<- proportion_haps_shared[which(human_haps_csp$Season=="Low"),which(human_haps_csp$Season=="Low")]

diag(within_Low)<- NA

within_Low_vector<- as.vector(within_Low)[-which(is.na(as.vector(within_Low)))]


between_season_proportion_shared_df<- as.data.frame(cbind(c(within_High_vector,
                                                             within_Low_vector,
                                                            between_season_vector),
                                                           c(rep("Within High", length(within_High_vector)),
                                                             rep("Within Low", length(within_Low_vector)),
                                                             rep("Between Seasons", length(between_season_vector)))))
                                                     

colnames(between_season_proportion_shared_df)<- c("Pairwise_dist", "Group")


between_season_proportion_shared_df$Pairwise_dist<- as.numeric(between_season_proportion_shared_df$Pairwise_dist)

season_prop_shared_plot<- ggplot(between_season_proportion_shared_df,aes(x=Pairwise_dist,color=Group))+
  geom_density()+theme_classic()+xlab("Proportion of Shared Haplotypes")



#data frame of haplotypes, time of sample, unq mem ID and village

haps<- unique(unlist(human_haps_csp$Haplotypes))

haps_matrix<- matrix(0, nrow=nrow(human_haps_csp),ncol=length(haps))

for(i in 1:nrow(human_haps_csp)){
  haps_matrix[i,which(haps%in%human_haps_csp$Haplotypes[[i]])]<-1
  
}

colnames(haps_matrix)<- haps

haps_matrix_with_dates<- as.data.frame(cbind(human_haps_csp$unq_memID,
                                             as.character(human_haps_csp$sample_date),
                                             human_haps_csp$village_ID,
                                             haps_matrix))

colnames(haps_matrix_with_dates)<- c("unq_memID","sample_date","village_ID",haps)

saveRDS(haps_matrix_with_dates,"CSP_haplotypes_matrix")


###########################

haps

proportion_with_pair_whole_pop<-matrix(NA, nrow=length(haps),ncol=length(haps))

for(i in 1:nrow(proportion_with_pair_whole_pop)){
  for(j in 1:nrow(proportion_with_pair_whole_pop)){
    have_pair<- rep(0,nrow(human_haps_csp))
    for(k in 1:nrow(human_haps_csp)){
      
      if(haps[i]%in%human_haps_csp$Haplotypes[[k]] & haps[j]%in%human_haps_csp$Haplotypes[[k]]){
        have_pair[k]<-1
      }
      
    }
    proportion_with_pair_whole_pop[i,j]<- sum(have_pair)
  }
  print(i)
}

proportion_with_pair_whole_pop<-proportion_with_pair_whole_pop/nrow(human_haps_csp)



proportion_with_pair_within_k<-matrix(NA, nrow=length(haps),ncol=length(haps))

for(i in 1:nrow(proportion_with_pair_within_k)){
  for(j in 1:nrow(proportion_with_pair_within_k)){
    have_pair<- rep(0,nrow(human_haps_csp[which(human_haps_csp$village_ID=="K"),]))
    for(k in 1:length(which(human_haps_csp$village_ID=="K"))){
      
      if(haps[i]%in%human_haps_csp$Haplotypes[[which(human_haps_csp$village_ID=="K")[k]]] & haps[j]%in%human_haps_csp$Haplotypes[[which(human_haps_csp$village_ID=="K")[k]]]){
        have_pair[k]<-1
      }
      
     
    }
    proportion_with_pair_within_k[i,j]<- sum(have_pair)
  }
  print(i)
}

proportion_with_pair_within_k<-proportion_with_pair_within_k/nrow(human_haps_csp[which(human_haps_csp$village_ID=="K"),])



proportion_with_pair_within_m<-matrix(NA, nrow=length(haps),ncol=length(haps))

for(i in 1:nrow(proportion_with_pair_within_m)){
  for(j in 1:nrow(proportion_with_pair_within_m)){
    have_pair<- rep(0,nrow(human_haps_csp[which(human_haps_csp$village_ID=="M"),]))
    for(k in 1:length(which(human_haps_csp$village_ID=="M"))){
      
      if(haps[i]%in%human_haps_csp$Haplotypes[[which(human_haps_csp$village_ID=="M")[k]]] & haps[j]%in%human_haps_csp$Haplotypes[[which(human_haps_csp$village_ID=="M")[k]]]){
        have_pair[k]<-1
      }
      
      
    }
    proportion_with_pair_within_m[i,j]<- sum(have_pair)
  }
  print(i)
}

proportion_with_pair_within_m<-proportion_with_pair_within_m/nrow(human_haps_csp[which(human_haps_csp$village_ID=="M"),])



proportion_with_pair_within_s<-matrix(NA, nrow=length(haps),ncol=length(haps))

for(i in 1:nrow(proportion_with_pair_within_s)){
  for(j in 1:nrow(proportion_with_pair_within_s)){
    have_pair<- rep(0,nrow(human_haps_csp[which(human_haps_csp$village_ID=="S"),]))
    for(k in 1:length(which(human_haps_csp$village_ID=="S"))){
      
      if(haps[i]%in%human_haps_csp$Haplotypes[[which(human_haps_csp$village_ID=="S")[k]]] & haps[j]%in%human_haps_csp$Haplotypes[[which(human_haps_csp$village_ID=="S")[k]]]){
        have_pair[k]<-1
      }
      
      
    }
    proportion_with_pair_within_s[i,j]<- sum(have_pair)
  }
  print(i)
}

proportion_with_pair_within_s<-proportion_with_pair_within_s/nrow(human_haps_csp[which(human_haps_csp$village_ID=="S"),])


permutation_proportion_with_pair_whole_pop<- array(NA,c(length(haps), length(haps), 50))

hap_freqs<- rep(NA, length(haps))

for(i in 1:length(haps)){
  contains_hap<- rep(0, nrow(human_haps_csp))
  for(j in 1:nrow(human_haps_csp)){
  if(haps[i]%in%human_haps_csp$Haplotypes[[j]]){
    contains_hap[j]<-1
  }
  }
  hap_freqs[i]<- mean(contains_hap)
}

for(l in 1:50){
  human_haps_csp_permuted<- vector(mode="list", length=nrow(human_haps_csp))
  
  for(m in 1:nrow(human_haps_csp)){
    human_haps_csp_permuted[[m]]<- sample(haps, size=human_haps_csp$haplotype_number[m], prob=hap_freqs, replace=F)
  }
  
for(i in 1:length(haps)){
  for(j in 1:length(haps)){
    have_pair<- rep(0,nrow(human_haps_csp))
    for(k in 1:nrow(human_haps_csp)){
      
      if(haps[i]%in%human_haps_csp_permuted[[k]] & haps[j]%in%human_haps_csp_permuted[[k]]){
        have_pair[k]<-1
      }
      
    }
    permutation_proportion_with_pair_whole_pop[i,j,l]<- mean(have_pair)
  }

}
  print(l)
}

saveRDS(permutation_proportion_with_pair_whole_pop, "Permutation_proportion_pairs_haps")


permutation_proportion_array<- readRDS("Permutation_proportion_pairs_haps")

length()





  