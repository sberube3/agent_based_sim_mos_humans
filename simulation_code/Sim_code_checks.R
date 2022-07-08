library(dplyr)
library(extraDistr)
library(microbenchmark)
library(Rcpp)
sourceCpp("/Users/sophieberube/Desktop/Hopkins/Hopkins/Mozzie/Expected_v_observed/agent_based_sim_mos_humans/simulation_code/get_biting_status.cpp")
######################################################
########Fixed Parameters and re-used functions#######
#####################################################


#day numbering, could feed every 3 days sample mosquitoes every 7 days, sample humans every month (except sick visits)
mos_sample_days<- seq(from=0,to=357,by=7)+365
human_sample_days<- c(seq(from=0,to=357,by=30),357)+365

#people
n_p<- 200


#possible  haplotypes & frequencies for 1 area or 2 areas with migration
haps<- rep(1:220)
freq<- c(rep(0.001,140),rep(0.002,15),rep(0.003,8),rep(0.005,4), rep(0.006,3), rep(0.007,5),
         rep(0.008,2), rep(0.009,3), rep(0.011,3), 0.012, 0.013, rep(0.015,2),rep(0.016,2),
         0.023,0.025,0.026,0.027,0.04,0.042,rep(0.044,2),0.047,0.049,0.052,0.055,0.063,0.064,0.07,
         0.077,0.084,0.093,rep(0.098,2),0.111,0.143,0.148,0.213,0.235,0.315,0.318,0.331,0.46,
         0.476,0.547)



mos_haps_a1<- c(1:40,139:143,149,153, 156:159, 165,168,171,174,178,181,184,187,189,
                192,195,197,200,203,206,210,212,214,216,218) 

mos_haps_a2<- c(41:80,144:148,151,155, 160:162, 164,166,169,172,176,179,183,186,188,190,
                194,196,198,202,204,208,211,213,215,217,220) 

mos_haps<- c(1:40,139:143,149,153, 156:159, 165,168,171,174,178,181,184,187,189,
             192,195,197,200,203,206,210,212,214,216,218,41:80,144:148,151,155, 160:162, 164,166,169,172,176,179,183,186,188,190,
                194,196,198,202,204,208,211,213,215,217,220) 

mos_freq_a1<- freq[mos_haps_a1]
mos_freq_a2<- freq[mos_haps_a2]

mos_freq<- freq[mos_haps]

human_haps_a1<- c(60:99,143,148:151, 163,167,170,173,175,177,182,185,189,191,196,
                  199,201,203,206,208,211,214,216,219,220)

human_haps_a2<- c(100:138,152:155, 164:165,169,171,174,176,180,184,187,190,193,
                  197,200,202,205,207,209,213,215,217,218)

human_haps<-c(60:99,143,148:151, 163,167,170,173,175,177,182,185,189,191,196,
              199,201,203,206,208,211,214,216,219,220,100:138,152:155, 164:165,169,171,174,176,180,184,187,190,193,
                  197,200,202,205,207,209,213,215,217,218)

human_freq_a1<- freq[human_haps_a1]

human_freq_a2<-freq[human_haps_a2]

human_freq<- freq[human_haps]

#functions to get infection status and haplotype compostion for humans and mosquitoes

get_infection<- function(x){
  inf<-ifelse(x<2,0,ifelse(x<3,rbinom(1,size=1,p=0.05),ifelse(x<4,rbinom(1,size=1,p=0.1),ifelse(x<5,rbinom(1,size=1,0.15),rbinom(1,size=1,p=0.25)))))
  
  return(inf)
}
get_moi<- function(x){
  moi<- ifelse(x<3,rtpois(1,4,a=0,b=5),ifelse(x<4, rtpois(1,6,a=0,b=10), ifelse(x<5,rtpois(1,6,a=0,b=15),rtpois(1,6,a=0,b=17))))
  return(moi)
}



get_pers_infec<- function(x,haps,freq){
  haps_index<-sample(haps, size=x, prob=freq)
  return(haps_index)
}

#get_mos_infec<- function(x,haps,freq){
  #haps_index<-sample(haps, size=x, prob=freq)
  #return(haps_index)
#}

#haplotype exchange between human and mosquitoes if biting is happening

get_old_p_haps<- function(x){
  old_index<- which(x>=14)
  return(old_index)
}


get_old_p_infec<- function(x){
  old_p_infec<- rep(0,n_p)
  for(i in 1:n_p){
    if(any(x[i,]>=7)){
      old_p_infec[i]<-1
    }
  }
  return(old_p_infec)
}

get_old_p_infec2 <- function(x){
  return((rowSums(x >= 7) > 0) * 1)
}

#old_haps_p<-apply(age_haps_p,1,get_old_p_haps)

get_old_m_haps<- function(x){
  old_index<- which(x>=9)
  return(old_index)
}



#function to get the mosquito death as a function of age
get_mos_death<- function(x){
  death_m<- ifelse(x<3,rbinom(1,size=1,p=0.01),
                   ifelse(x<6,rbinom(1,size=1,p=0.05),
                          ifelse(x<9,rbinom(1,size=1,p=0.1),
                                 ifelse(x<12,rbinom(1,size = 1,p=0.25),
                                        ifelse(x<15,rbinom(1,size=1,p=0.5),rbinom(1,size=1,p=0.7))))))
  return(death_m)
}


get_mos_death2 <- function(x){
  thresholds <- c(3, 6, 9, 12, 15, Inf)
  probs <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.7)
  
  return(rbinom(1, size=1, p=probs[sum(x > thresholds) + 1]))
}


get_mos_death3 <- function(x){
  thresholds <- c(3, 6, 9, 12, 15, Inf)
  probs <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.7)
  return(rbinom(length(x), size=1, p=probs[sapply(x, function(i) sum(i > thresholds)) + 1]))
}


remove_0_values_take_min<- function(x){
  if(length(x[x>0])>0){
  min<- min(x[x>0])
  }
  else{
    min<-0
  }
}


#############################
####Simulation function#####
############################

run_biting_sim<- function(pr_symp_infec, pr_symp_non_infec, pr_clear, pr_off_feed, pr_on_feed, pr_hum_to_mos, pr_mos_to_hum,
                          pr_num_biting, n_m, scenario_name, n_sim,n_p_a1,n_m_a1, pr_move_a1, pr_move_a2){
  
  humans_loc<-c(rep("A1",n_p_a1),rep("A2", n_p-n_p_a1))
  mos_loc<- c(rep("A1",n_m_a1),rep("A2",n_m-n_m_a1))
  
  symptomatic_MOI_df<- matrix(NA, nrow=n_sim, ncol=n_p*722)
  mosquito_MOI_df<- matrix(NA, nrow=n_sim, ncol=30*722)
  human_MOI_df<- matrix(NA,nrow=n_sim,ncol=n_p*722)
  eir_df<- matrix(NA, nrow=n_sim,ncol=n_p)
  age_mos_df<- matrix(NA, nrow=n_sim, ncol=n_m)
  age_human_haps_array<- array(NA, c(n_p,length(haps)*722, n_sim))
  symptoms<-array(NA, c(n_p, 722,n_sim) )
  
  
  for(q in 1:n_sim){
    #mosquito ages
   
    age_m<- rtpois(n_m, 4,a=0,b=14)
    
    
    
    #starting infection status and MOIs for people
    inf_p<-rbinom(n_p, size=1, p=0.3)
    moi_p<- rep(NA, n_p)
    for(i in 1:length(inf_p)) moi_p[i]<-ifelse(inf_p[i]==1,rtpois(1,2,a=0,b=16),0)
    
    
    #starting infection status for mosquito these are mosquito-age dependent older =more likely to be infected
    
    inf_m<- sapply(age_m, get_infection)
    
    
    #starting MOIs for mosquitoes, these are also age dependent,older=higher MOI
    
    moi_m<- ifelse(inf_m==0,0,NA)
    
    
    
    moi_m[which(is.na(moi_m))]<-sapply(age_m[which(is.na(moi_m))],get_moi)
    
    #starting haplotypes for people and age of those haplotypes (time they have been )
    
    
    infec_p<- vector(mode = "list", length = n_p)
    for(i in 1:n_p_a1){
      infec_p[[which(humans_loc=="A1")[i]]]<- get_pers_infec(moi_p[which(humans_loc=="A1")[i]],
                                                             human_haps,human_freq)
    }
    
    #for(i in 1:(n_p-n_p_a1)){
      #infec_p[[which(humans_loc=="A2")[i]]]<- get_pers_infec(moi_p[which(humans_loc=="A2")[i]],
                                                             #human_haps_a2,human_freq_a2)
    #}
    
    age_haps_p<- matrix(0,nrow=n_p, ncol=length(haps))
    
    for(i in 1:n_p){
      if(length(infec_p[[i]])>0){
        age_haps_p[i,infec_p[[i]]]<- rpois(length(infec_p[[i]]),20)
      }
      
    }
    #starting haplotypes for mosquitoes  and age of those haplotypes
    
    
    
    infec_m<-assign_mos_haps(moi=moi_m, freq=mos_freq, haps=mos_haps)
    
    #for(i in 1:(n_m-n_m_a1)){
      #infec_m[[which(mos_loc=="A2")[i]]]<- get_mos_infec(moi_m[which(mos_loc=="A2")[i]],
                                                         #mos_haps_a2,mos_freq_a2)
    #}
    
    age_haps_m<- matrix(0,nrow=n_m, ncol=length(haps))
    
    for(i in 1:n_m){
      if(length(infec_m[[i]])>0){
        age_haps_m[i,infec_m[[i]]]<- sample(1:age_m[i],size=length(infec_m[[i]]), replace=T)
      }
    }
    
    
    old_pers_infec<- get_old_p_infec2(age_haps_p)
    
    
    symp_index[old_pers_infec==1]<-rbinom(length(which(old_pers_infec==1)),1,pr_symp_infec)
    symp_index[old_pers_infec==0]<-rbinom(length(which(old_pers_infec==0)),1,pr_symp_non_infec)
    
    
    symp_age<-rep(0,n_p)
    
   
    symp_age[which(symp_index==1&old_pers_infec==1)]<- 1
    
    
    
    num_infec_bites<- rep(0,n_p)
    
    num_switches<- rep(0, n_p)
    
    
    
    min_age_haps<- apply(age_haps_m,1,remove_0_values_take_min)
  
    
    prob_bit_last_3_days<- ifelse(age_m<2,0,ifelse(min_age_haps==0 | (age_m-min_age_haps)>=3, pr_on_feed, pr_off_feed))
    
    bit_last_3_days<- rep(0,n_m)
   
    bit_last_3_days[prob_bit_last_3_days==pr_on_feed]<-rbinom(length(prob_bit_last_3_days[prob_bit_last_3_days==pr_on_feed]),1,pr_on_feed)
    bit_last_3_days[prob_bit_last_3_days==pr_off_feed]<-rbinom(length(prob_bit_last_3_days[prob_bit_last_3_days==pr_off_feed]),1,pr_off_feed)

    ##############################
    ######Start of timed sim#####
    #############################
    
    for(r in 1:722){
      
      #age 1 day 
      age_m<- age_m+1
      
      # age_haps_m[age_haps_m!=0]<- age_haps_m[age_haps_m!=0]+1
      # 
      # age_haps_p[age_haps_p!=0]<- age_haps_p[age_haps_p!=0]+1
      
      age_haps_m <- age_haps_m + 1
      age_haps_m[age_haps_m == 1] <- 0
      
      age_haps_p <- age_haps_p + 1
      age_haps_p[age_haps_p == 1] <- 0
      
      #is the mosquito dead
      # death_m<- sapply(age_m,get_mos_death)
      death_m <- get_mos_death3(age_m)
      #if so replace it with a new mosquito(newborn)
      
      age_m[which(death_m==1)]<- 0
      
      
      inf_m[which(death_m==1)]<- 0
      
      
      moi_m[which(death_m==1)]<- 0
      
      infec_m[which(death_m==1)]<- NA
      
      
      age_haps_m[death_m == 1, ] <- 0
      
      bit_last_3_days[death_m==1]<-0
      
      # for(i in 1:n_m){
      #   if(death_m[i]==1 ){
      #     
      #     
      #     age_haps_m[i,]<- 0
      #     
      #     
      #     
      #   }
      # }
      
      
      
      # symptomatic infections get  sampled (sick visit) and cleared immediately (drugs)
      
      symp_age[which(symp_age!=0)]<- symp_age[which(symp_age!=0)]+1
      # symp_age<- ifelse(symp_age>14,0,symp_age)
      symp_age[symp_age > 14] <- 0
      
      # ifelse(symp_age>14,0,symp_age)
      
      infec_p[which(symp_age==2)]<- NA
      age_haps_p[which(symp_age==2),]<-0
      
      
      
      
      #if(symp_index[i] == 0){
      #if(max(age_haps_p[i,])!=0){
      #if(length(which(age_haps_p[i,]>=7))>0& length(which(age_haps_p[i,]<=30))>0){
      old_infec_p<-get_old_p_infec2(age_haps_p)
      
      symp_index[old_infec_p==1]<- rbinom(length(old_infec_p[old_infec_p==1]),1,pr_symp_infec)
      symp_index[old_infec_p==0]<-rbinom(length(old_infec_p[old_infec_p==0]),1,pr_symp_non_infec)
      
      
      
     symp_age[which(symp_index==1&old_infec_p==1)]<-1
      #}
      #}
      #}
      
      
      
      # if(length(which(age_haps_p[i,]>=7))>0& length(which(age_haps_p[i,]<=30))>0&max(age_haps_p[i,])!=0
      #    &symp_index[i]==0){
      #   new_index<-rbinom(1,1,pr_symp)
      #   if(new_index==1){
      #     symp_index[i]<-1
      #     symp_age[i]<- 1
      #   }
      # }
      
      
      
      
      symptomatics<- which(symp_age==1)
      if(length(symptomatics)>0){
        symp_moi<- rep(NA, length(symptomatics))
        for(i in 1:length(symptomatics)){
          symp_moi[i]<- length(na.omit(unlist(infec_p[[symptomatics[i]]])))
        }
        if(sum(symp_moi,na.rm=T)>0){
          symptomatic_MOI_df[q,(1+(n_p*(r-1))):(((n_p*(r-1)))+length(symp_moi))]<- symp_moi
        }
      }
      
      symptoms[which(symp_age==1),r,q]<- 1
      
      
      
      
      
      #then clear human parasites by age of parasites
      
      
      
      for(i in 1:n_p){
        # if(length(na.omit(infec_p[[i]]))>0){      
        if(sum(!is.na(infec_p[[i]]))>0){           
          for(j in c(na.omit(infec_p[[i]]))){
            clear<- ifelse(age_haps_p[i,j]>=90,rbinom(1,1,0.95),ifelse(age_haps_p[i,j]>=65,rbinom(1,1,0.85),ifelse(age_haps_p[i,j]>=30,rbinom(1,1,pr_clear),0)))
          }
          
          if(clear==1){
            infec_p[[i]][which(infec_p[[i]]==j)]<- NA
            age_haps_p[i,j]<- 0
          }
        }
      }
      
      
      
      
      humans_moving <- rbinom(n_p, 1, pr_move_a1 + (pr_move_a2 - pr_move_a1) * 
                                (humans_loc=="A1"))
      # humans_moving<- rep(0,n_p)
      # for(i in 1:n_p){
      #   if(humans_loc[i]=="A1"){
      #     humans_moving[i]<-rbinom(1,1,pr_move_a1)
      #   }
      #   else{
      #     humans_moving[i]<-rbinom(1,1,pr_move_a2)
      #   }
      # }
      
      
      num_switches<- num_switches+humans_moving
      humans_loc[humans_moving == 1 & 
                   humans_loc == "A1"] <- "A2"
      
      humans_loc[humans_moving == 1 & 
                   humans_loc == "A2"] <- "A1"
      
      # for(i in 1:n_p){
      #   if(humans_loc[i]=="A1"&humans_moving[i]==1){
      #     humans_loc[i]<-"A2"
      #   }
      #   else if(humans_loc[i]=="A1"&humans_moving[i]==0){
      #     humans_loc[i]<-"A1"
      #   }
      #   else if(humans_loc[i]=="A2"&humans_moving[i]==1){
      #     humans_loc[i]<-"A1"
      #   }
      #   else{
      #     humans_loc[i]<-"A2"
      #   }
      # }
      
      #if its a feeding day
      
      
      
      mos_bite<- matrix(0, n_m,n_p)
      which_mos_bite <- rep(0, n_m)
      which_hum_bite <- rep(0, n_p)
      person_bitten <- rep(0, n_p)
      
      bit_last_3_days[bit_last_3_days!=0]<-bit_last_3_days[bit_last_3_days!=0]+1
      bit_last_3_days[bit_last_3_days>3] = 0
      
      mos_biting_probs<-rep(0,n_m)
      mos_biting_probs[bit_last_3_days>=1|age_m<2]<-rbinom(length(mos_biting_probs[bit_last_3_days>=1|age_m<2]),1,pr_off_feed)
      mos_biting_probs[bit_last_3_days<1&age_m>=2]<-rbinom(length(mos_biting_probs[bit_last_3_days<1&age_m>=2]),1,pr_on_feed)
      
      
      bites <- rbinom(n_m, 1, mos_biting_probs)
      which_mos_bite <- bites == 1
      
      for(i in which(which_mos_bite)){
        num_biting<- sample(c(1,2,3,4,5,6,7),size=1,prob=pr_num_biting)
        
        ###CHANGED
        which_people_bite <- sample(which(humans_loc==mos_loc[i]),size=num_biting,replace=F)
        person_bitten[which_people_bite] <- 1
        mos_bite[i,which_people_bite] <- 1
        which_hum_bite[which_people_bite] <- 1
        
      }
      
      bit_last_3_days[which_mos_bite==1]<-1
      
      # for(i in 1:n_m){
      #   if(age_m[i]>=2){
      #     if(min(age_haps_m[i,])<=(age_m[i]-3)){
      #       bite <- rbinom(1,1,pr_off_feed)
      #     }else{
      #       bite <- rbinom(1,1,pr_on_feed)
      #     }
      # 
      #     # bite<- ifelse(min(age_haps_m[i,])<=(age_m[i]-3),rbinom(1,1,pr_off_feed),rbinom(1,1,pr_on_feed))
      # 
      #     if(bite==1){
      #       num_biting<- sample(c(1,2,3,4,5,6,7),size=1,prob=pr_num_biting)
      # 
      #       ###CHANGED
      #       which_people_bite <- sample(which(humans_loc==mos_loc[i]),size=num_biting,replace=F)
      #       person_bitten[which_people_bite] <- 1
      #       mos_bite[i,which_people_bite]<-1
      #       which_hum_bite[which_people_bite] <- 1
      #       # mos_bite[i,sample(1:200,size=num_biting,replace=F)]<-1
      #       which_mos_bite[i] <- 1
      #     }
      #   }
      # 
      # }
      
      #haplotype exchange if biting is happening
      
      # old_haps_p<-apply(age_haps_p,1,get_old_p_haps)
      # 
      # 
      # old_haps_m<-apply(age_haps_m,1,get_old_m_haps)
      # 
      # ##########################
      # # which_mos_bite<- ifelse(apply(mos_bite,1,sum)>0,1,0)
      # 
      # 
      # ##CHANGED
      # # which_mos_bite<- (rowSums(mos_bite)>0) * 1
      # 
      # old_hap_mosquitoes<- rep(0,n_m)
      # 
      # for(i in 1:n_m){
      #   if(length(old_haps_m)>0){
      #     if(length(old_haps_m[[i]])>0){
      #       old_hap_mosquitoes[i]<-1
      #     }
      #   }
      # }
      # 
      # hap_transfer_mos<- rep(0,n_m)
      # 
      # for(i in 1:n_m){
      #   if(old_hap_mosquitoes[i]==1&which_mos_bite[i]==1){
      #     hap_transfer_mos[i]<-1
      #   }
      # }
      
      
      ####################
      
      # # which_hum_bite<- ifelse(apply(mos_bite,2,sum)>0,1,0)
      # # which_hum_bite<- (colSums(mos_bite)>0)*1
      # 
      # old_hap_hum<- rep(0,n_p)
      # 
      # 
      # for(i in 1:n_p){
      #   if(length(old_haps_p)>0){
      #     if(length(old_haps_p[[i]])>0){
      #       old_hap_hum[i]<-1
      #     }
      #   }
      # }
      # 
      # hap_transfer_hum<- rep(0,n_p)
      # 
      # for(i in 1:n_p){
      #   if(old_hap_hum[i]==1&which_hum_bite[i]==1){
      #     hap_transfer_hum[i]<-1
      #   }
      # }
      # 
      ##########################
      for(i in which(person_bitten == 1)){
        mos_index1<- mos_bite[,i]==1
        mos_index<- which(mos_index1)
        inf_bites<- rep(0,length(mos_index))
        for(j in 1:length(mos_index)){
          
          
          old_haps_p_i <- which(age_haps_p[i, ] >= 14)
          if(length(old_haps_p_i)>0&symp_age[i]<1){
            transfer_haps<- rep(NA,length(old_haps_p_i))
            for(k in 1:length(old_haps_p_i)){
              prob<- pr_hum_to_mos
              transfer<- rbinom(1,1,prob)
              if(transfer==1){
                transfer_haps[k]<- old_haps_p_i[k]
              }
            }
            new_haps<- setdiff(na.omit(transfer_haps),infec_m[[mos_index[j]]])
            infec_m[[mos_index[j]]]<-c(infec_m[[mos_index[j]]],new_haps)
            age_haps_m[mos_index[j],new_haps]<-1
          }
          
          
          
          old_haps_m_j <- which(age_haps_m[mos_index[j], ] >= 9)
          if(length(old_haps_m_j)>0&symp_age[i]==0){
            transfer_haps_m<- rep(NA,length(old_haps_m_j))
            for(l in 1:length(old_haps_m_j)){
              prob_m<- pr_mos_to_hum
              transfer_m<- rbinom(1,1,prob_m)
              if(transfer_m==1){
                transfer_haps_m[l]<- old_haps_m_j[l]
              }
            }
            new_haps_m<- setdiff(na.omit(transfer_haps_m), infec_p[[i]])
            infec_p[[i]]<- c(infec_p[[i]],new_haps_m)
            age_haps_p[i,new_haps_m]<- 1
            if(length(new_haps_m)>0){
            inf_bites[j]<-1
            }
          }
          # }
          
          
          
          
        }
        if(r>=365){
          num_infec_bites[i]<- num_infec_bites[i]+sum(inf_bites)
        }
        # }
        
      }
      
      
      # for(i in 1:n_p){
      #   mos_index<- which(mos_bite[,i]==1)
      #   if(length(mos_index)>0){
      #     inf_bites<- rep(0,length(mos_index))
      #     for(j in 1:length(mos_index)){
      #       if(length(old_haps_p)>0){
      #         if(length(old_haps_p[[i]])>0&symp_age[i]<2){
      #           transfer_haps<- rep(NA,length(old_haps_p[[i]]))
      #           for(k in 1:length(old_haps_p[[i]])){
      #             prob<- pr_hum_to_mos
      #             transfer<- rbinom(1,1,prob)
      #             if(transfer==1){
      #               transfer_haps[k]<- old_haps_p[[i]][k]
      #             }
      #           }
      #           new_haps<- setdiff(na.omit(transfer_haps),infec_m[[mos_index[j]]])
      #           infec_m[[mos_index[j]]]<-c(infec_m[[mos_index[j]]],new_haps)
      #           age_haps_m[mos_index[j],new_haps]<-1
      #         }
      #       }
      #       
      #       if(length(old_haps_m)>0){
      #         if(length(old_haps_m[[mos_index[j]]])>0&symp_index[i]<1){
      #           transfer_haps_m<- rep(NA,length(old_haps_m[[mos_index[j]]]))
      #           for(l in 1:length(old_haps_m[[mos_index[j]]])){
      #             prob_m<- pr_mos_to_hum
      #             transfer_m<- rbinom(1,1,prob_m)
      #             if(transfer_m==1){
      #               transfer_haps_m[l]<- old_haps_m[[mos_index[j]]][l]
      #             }
      #           }
      #           new_haps_m<- setdiff(na.omit(transfer_haps_m), infec_p[[i]])
      #           infec_p[[i]]<- c(infec_p[[i]],new_haps_m)
      #           age_haps_p[i,new_haps_m]<- 1
      #           inf_bites[j]<-1
      #         }
      #         
      #         
      #         
      #       }
      #     }
      #     if(r>=365){
      #       num_infec_bites[i]<- num_infec_bites[i]+sum(inf_bites)
      #     }
      #   }
      #   
      # }
      
      
      #if its a human and mosquito sample day     
      if(r%in%human_sample_days&r%in%mos_sample_days){
        sample_index<- sample(1:n_m,size=30)
        mos_moi<- rep(NA,30)
        for(t in 1:30){
          mos_moi[t]<- length(na.omit(unlist(infec_m[[sample_index[t]]])))
        }
        mosquito_MOI_df[q,(1+(30*(r-1))):(30*r)]<- mos_moi
        
        
        
        age_m[sample_index]<- 0
        #rtpois(length(sample_index), 4,a=1,b=14)
        
        inf_m[sample_index]<- 0
        #sapply(age_m[sample_index],get_infection)
        
        moi_m[c(sample_index)]<- 0
        #,which(inf_m==1)
        #sapply(age_m[c(sample_index,which(inf_m==1))],get_moi)
        infec_m[c(sample_index)]<-0   
        #,which(inf_m==1&moi_m>0)
        #sapply(moi_m[c(sample_index,which(inf_m==1&moi_m>0))],get_mos_infec)
        bit_last_3_days[sample_index]<-0
        for(i in 1:n_m){
          if(i%in%sample_index ){
            #& inf_m[i]==1 & moi_m[i]>0
            
            age_haps_m[i,]<- 0
            #c(infec_m[[i]])
            #sample(1:age_m[i],size=length(infec_m[[i]]),replace=T)
            
          }
        }
        hum_moi<- rep(NA, n_p)
        for(w in 1:n_p){
          hum_moi[w]<- length(na.omit(unlist(infec_p[[w]])))
        }
        
        human_MOI_df[q,((1+(200*(r-1))):(200*r))]<- hum_moi
        
      }  
      #if its only a mosquito sample day
      if(r%in% mos_sample_days){
        sample_index<- sample(1:n_m,size=30)
        mos_moi<- rep(NA,30)
        for(t in 1:30){
          mos_moi[t]<- length(na.omit(unlist(infec_m[[sample_index[t]]])))
        }
        mosquito_MOI_df[q,(1+(30*(r-1))):(30*r)]<- mos_moi
        
        
        
        age_m[sample_index]<- 0
        #rtpois(length(sample_index), 4,a=1,b=14)
        
        inf_m[sample_index]<- 0
        #sapply(age_m[sample_index],get_infection)
        
        moi_m[c(sample_index)]<- 0
        #,which(inf_m==1)
        #sapply(age_m[c(sample_index,which(inf_m==1))],get_moi)
        infec_m[c(sample_index)]<- NA
        #,which(inf_m==1&moi_m>0)
        #sapply(moi_m[c(sample_index,which(inf_m==1&moi_m>0))],get_mos_infec)
        
        bit_last_3_days[sample_index]<-0
        for(i in 1:n_m){
          if(i%in%sample_index ){
            
            #& inf_m[i]==1 & moi_m[i]>0
            age_haps_m[i,]<- 0
            #c(infec_m[[i]])
            #sample(1:age_m[i],size=length(infec_m[[i]]),replace=T)
          }
        }
        
        
      }
      #if its only a human sample day
      if(r%in% human_sample_days){
        
        hum_moi<- rep(NA, n_p)
        for(w in 1:n_p){
          hum_moi[w]<- length(na.omit(unlist(infec_p[[w]])))
        }
        
        human_MOI_df[q,((1+(200*(r-1))):(200*r))]<- hum_moi
        
        
      }
      age_human_haps_array[,((1+(length(haps)*(r-1))):(length(haps)*r)),q]<- age_haps_p
      
      print(r)
    }
    
    eir_df[q,]<- num_infec_bites
    age_mos_df[q,]<- age_m
    
  }
  saveRDS(symptomatic_MOI_df,file=paste0("symptomatic_MOI_",scenario_name))
  saveRDS(mosquito_MOI_df, file =paste0("mosquito_MOI_",scenario_name))
  saveRDS(human_MOI_df,file =paste0("human_MOI_",scenario_name))
  saveRDS(eir_df, file =paste0("eir_", scenario_name))
  saveRDS(age_mos_df, file=paste0("mos_age_",scenario_name))
  saveRDS(symptoms, file=paste0("symptom_status_",scenario_name))
  saveRDS(age_human_haps_array, file=paste0("haplotype_age_",scenario_name))
  saveRDS(humans_loc, file=paste0("human_location_",scenario_name))
  saveRDS(num_switches, file=paste0("number_of_switches_per_person",scenario_name))
}


run_biting_sim(pr_symp_infec = 0.05, pr_symp_non_infec = 0.05, pr_clear = 0.85, pr_off_feed = 0.01, 
               pr_on_feed = 0.1,pr_hum_to_mos = 0.6, pr_mos_to_hum = 0.3, 
               pr_num_biting = c(0.6,0.35,0.04,0.01,0,0,0), n_m=30000, 
               n_p_a1 = 200, n_m_a1 = 30000, pr_move_a1 = 0, pr_move_a2 = 0,
               scenario_name = "test", 
               n_sim=1)


eir<- readRDS("eir_test")
symptoms<- readRDS("symptom_status_test")
mos_moi<- readRDS("mosquito_MOI_test")
mos_moi<-as.vector(mos_moi)[-which(is.na(mos_moi)|mos_moi==0)]
human_moi<- readRDS("human_MOI_test")
human_moi<- as.vector(human_moi)[-which(is.na(human_moi)|human_moi==0)]
