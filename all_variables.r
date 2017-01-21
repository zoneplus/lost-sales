
###############initial parameters###################################
N=10
M=10
alpha=c(0.2,0.4,0.6) 
mean_a=100
sd_a=c(20,10,5)
mean_b=c(200,100,50)
sd_b=c(40,20,10)
##############truncated level equal to 1-3sigma#############
truncated_level_b<-vector(mode="numeric",length=0)
truncated_level_a<-vector(mode="numeric",length=0)

###############initial parameters end##############################

###############loop###############################################

m_times_mes_mean<-vector(mode="numeric",length=0)
m_times_mes_sd<-vector(mode="numeric",length=0)
m_times_sds_mean<-vector(mode="numeric",length=0)
m_times_sds_sd<-vector(mode="numeric",length=0)
mean_o<-vector(mode="numeric",length=0)
sd_o<-vector(mode="numeric",length=0)
m_times_mean_o_mean<-vector(mode="numeric",length=0)
m_times_mean_o_sd<-vector(mode="numeric",length=0)
m_times_sd_o_mean<-vector(mode="numeric",length=0)
m_times_sd_o_sd<-vector(mode="numeric",length=0)
n_vec_set<-vector(mode="numeric",length=0)
mean_vec_set_x_a<-vector(mode="numeric",length=0)
sd_vec_set_x_a<-vector(mode="numeric",length=0)
mean_vec_set_x_b<-vector(mode="numeric",length=0)
sd_vec_set_x_b<-vector(mode="numeric",length=0)
truncate_level_set_x_a<-vector(mode="numeric",length=0)
truncate_level_set_x_b<-vector(mode="numeric",length=0)
alpha_vec_set<-vector(mode="numeric",length=0)
r_a_set<-vector(mode="numeric",length=0)
r_b_set<-vector(mode="numeric",length=0)
sd_origin_set<-vector(mode="numeric",length=0)
mean_orig_b_set<-vector(mode="numeric",length=0)
x_b_set<-vector(mode="numeric",length=0)
target_mean<-vector(mode="numeric",length=0)
target_sd<-vector(mode="numeric",length=0)
for (k in 1:length(alpha)){
  #############change of Sd_a###################
  for (l in 1:length(sd_a)){
    #############change of mean_b###################
    for (m in 1:length(mean_b)){
      #############change of sd_b###################
      for (n in 1:length(sd_b)){
        #############change of S_a###################
        for (p in 1:3){
          #############change of S_b###################
          for (q in 1:3){
            #############change of experiment times###################
            mes=0
            mes_count = 0
            mes_mean = 0
            sdsum = 0
            mean_orig_b = 0
            filtered_set<-vector(mode="numeric",length=0)
            sds = 0
            for (j in 1:M){
              r_a = 0
              r_b = 0
              #########generate random data###############
              x_b<-rnorm(N,mean_b[m],sd_b[n])
              x_b_set<-c(x_b_set,x_b)
              x_a<-rnorm(N,mean_a,sd_a[l])
              ##########truncate data#####################
              truncated_level_b[q]<- mean_b[m]+alpha[k]*mean_a+q*sqrt(sd_b[n]^2+alpha[k]^2*sd_a[l]^2)-30  
              truncated_level_a[p]<-mean_a+p*sd_a[l]
              truncated_x_a<-vector(mode="numeric",length=0)
              truncated_x_b<-vector(mode="numeric",length=0)
              truncated_part_of_x_b<-vector(mode="numeric",length=0)
              for (s in 1:length(x_a)){
                if (x_a[s]>truncated_level_a[p]){
                  truncated_x_a[s] = truncated_level_a[p]
                  r_a = r_a+1
                  
                }
                
                else{
                  truncated_x_a[s] = x_a[s]
                }
                
              }
              for (t in 1:length(x_b)){
                x_b[t] = x_b[t]+alpha[k]*x_a[t]
              }
              for (u in 1:length(x_b)){
                if (x_b[u]>truncated_level_b[q]){
                  #cc=paste("x_b",x_b[u],length(x_b)) 
                  #print (cc)
                  #dd=paste("truncated_level_b<x_b",truncated_level_b[q])
                  #print (dd)
                  truncated_x_b[u] = truncated_level_b[q]
                  r_b = r_b+1
                  #ee=paste("r_b",r_bn)
                  #print(ee)
                  
                }
                else{
                  truncated_x_b[u] = x_b[u]
                  truncated_part_of_x_b <- c(truncated_part_of_x_b,x_b[u])
                  #cc=paste("x_b",x_b[u]) 
                  #print (cc)
                  #dd=paste("truncated_level_b>x_b",truncated_level_b[q])
                  #print (dd)
                }
              }
              
              ###########origin mean and sd##############
              mean_orig_b=mean_orig_b+mean(truncated_x_b)
              
              if(length(truncated_x_b)==1){
                sd_orig_b=0
              }
              else{
                sd_orig_b=var(truncated_x_b)
              }
              
              
              ###########apply the estimators function#####
              if (r_a!=0&&r_b!=0&&r_a!=N&&r_b!=N&&length(truncated_part_of_x_b)>2){
                results=simplifiedf(truncated_part_of_x_b,r_b,alpha[k],mean_a,sd_a[l])
                xes=results$xes# mean from new estimator
                #print(xes)
                sdes=results$sdes# sd from new estimator
                #mes<-c(mes,xes)# mean vactor set from new estimator
                mes=mes+xes
                mes_count = mes_count+1
                filtered_set <-c(filtered_set,xes)
                sds<-c(sds,sqrt(sdes))# sd vector set from new estimator
                
              }
              
              
              
            }##############end of exp times#####################
            ############## m_times_mes&sds_mean&sd ###############
            #print(mes)
            #print(sds)
            if (mes!==0&&mes_count!==0&&sdes!==0){
              mes_mean=mes/mes_count
              for (v in 1:mes_count){
                sdsum = sdsum+(filtered_set[v]-mes_mean)^2
              }
              mean_orig_b = mean_orig_b/mes_count
              sdsum = sqrt(sdsum)/(mes_count-1)
              r_a_set<-c(r_a_set,r_a)
              r_b_set<-c(r_b_set,r_b)
              sd_origin_set<-c(sd_origin_set,sqrt(sd_orig_b-alpha[k]^2*sd_a[l]^2))
              m_times_mes_mean <-c(m_times_mes_mean, mes_mean)
              m_times_mes_sd <- c(m_times_mes_sd,sdsum)
              m_times_sds_mean <- c(m_times_sd_o_mean,mean(sds))
              m_times_sds_sd <- c(m_times_sd_o_sd,sqrt(var(sds)))
              m_times_mean_o_mean <- c(m_times_mean_o_mean,mean(mean_o))
              m_times_mean_o_sd <- c(m_times_mean_o_sd,sqrt(var(mean_o)))
              m_times_sd_o_mean <-c(m_times_sd_o_mean,mean(sd_o))
              m_times_sd_o_sd <- c(m_times_sd_o_sd,sqrt(var(sd_o)))
              mean_orig_b_set<-c(mean_orig_b_set,mean_orig_b-alpha[k]*mean_a)
              #################corresponding parameters###############
              n_vec_set<-c(n_vec_set,N)
              mean_vec_set_x_a<-c(mean_vec_set_x_a,mean_a)
              sd_vec_set_x_a<-c(sd_vec_set_x_a,sd_a[l])
              mean_vec_set_x_b<-c(mean_vec_set_x_b,mean_b[m])
              sd_vec_set_x_b<-c(sd_vec_set_x_b,sd_b[n])
              truncate_level_set_x_a<-c(truncate_level_set_x_a,truncated_level_a[p])
              truncate_level_set_x_b<-c(truncate_level_set_x_b,truncated_level_b[q])
              alpha_vec_set<-c(alpha_vec_set,alpha[k])
              target_mean<-c(target_mean,mean_b[m])
              target_sd<-c(target_sd,sd_b[n])}
          }##############end of S_b#####################
        }##############end of S_a#####################
      }##############end of sd_b#####################
    }##############end of mean_b#####################
  }##############end of sd_a#####################
  
}
###############loop end###################################
####################write to file##########################

result_out<-cbind(n_vec_set,mean_vec_set_x_a,target_mean,mean_vec_set_x_b,sd_vec_set_x_b,
                  target_sd,truncate_level_set_x_a,truncate_level_set_x_b,alpha_vec_set,
                  m_times_mes_mean,m_times_mes_sd,m_times_sds_mean,m_times_sds_sd,r_a_set,r_b_set,
                  m_times_mean_o_mean,m_times_mean_o_sd,m_times_sd_o_mean,m_times_sd_o_sd,
                  mean_orig_b_set, sd_origin_set)
write.csv(result_out,file = "lostsales_truncate_data_all_variables.csv")


###################################FUNCTION BEGIN##########################
simplifiedf=function(truncated_part_of_x_b,r,alpha,mean_a,sd_a){
  
  p=(N-r)/N
  z=qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  mi=dnorm(z,0,1)
  xmean=mean(truncated_part_of_x_b)
  xvar=0
  #if(length(truncated_part_of_x_b)==1){
  #  xvar<-0
  #}
  #else{
  xvar=var(truncated_part_of_x_b)
  #}
  
  vares=p^2*xvar/(p^2-p*z*mi-mi^2)-alpha^2*sd_a^2
  #print(paste('truncated_part_of_x_b',truncated_part_of_x_b))
  xes=xmean+(vares+alpha*sd_a^2)^0.5*mi/p-alpha*mean_a
  #print(xmean-alpha*mean_a)
  #sdes= sqrt(vares)
  #print(xmean)
  #print(xmean+(vares+alpha*sd_a^2)^0.5*mi/p)
  #print ((vares+alpha*sd_a^2)^0.5*mi/p)
  #print (xes)
  #result<-list()
  #if (!is.na(xes)&&!is.na(sdes)){
  #print (xes)
  #print (vares)
  if (vares< 0 | vares == 0){
    sdes = 0
  }
  else{
    sdes = sqrt(vares)
  }
  print (sdes)
  result <- list(xes=xes, sdes=sdes)  
  return(result) 
  #return (xes)
}

###################################FUNCTION END##########################