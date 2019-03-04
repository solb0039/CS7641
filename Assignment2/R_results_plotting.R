library(tidyverse)
library(ggplot2)
# Neural Net backpropagation error
backprop_test_data<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/output/ITERtestSET_ANN_OF_adult.csv")
bp_gather<-backprop_test_data%>%gather('test acc', 'train acc', key="types", value="error")

bkp<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/output/ITER_base_ANN_adult.csv")
bkp_gather<-bkp%>%gather('mean_test_score', 'mean_train_score', key="types", value="error")


rhc_data<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/RHC_LOG_56.txt")
sum_rhc_data<-rhc_data%>%group_by(iteration)%>%summarize(mse_trg=mean(MSE_trg), mse_val=mean(MSE_val), 
              mse_tst=mean(MSE_tst), time_mean=mean(elapsed))%>%mutate(trg_acc=(1-mse_trg), tst_acc=(1-mse_tst), val_acc=(1-mse_val))

sum_rhc_data_gather<-sum_rhc_data%>%gather('mse_trg', 'mse_tst', 'mse_val', key="types",value="error")

# SA data
sa_data_7<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/SA0.7_LOG_56.txt")
sa_data_7b<-sa_data_7%>%group_by(iteration)%>%summarize(mse_trg=mean(MSE_trg), mse_val=mean(MSE_val), mse_tst=mean(MSE_tst), time=mean(elapsed))

sa_data_15<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/SA0.15_LOG_56.txt")
sa_data_15b<-sa_data_15%>%group_by(iteration)%>%summarize(mse_trg=mean(MSE_trg), mse_val=mean(MSE_val), mse_tst=mean(MSE_tst), time=mean(elapsed))

sa_data_35<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/SA0.35_LOG_56.txt")
sa_data_35b<-sa_data_35%>%group_by(iteration)%>%summarize(mse_trg=mean(MSE_trg), mse_val=mean(MSE_val), mse_tst=mean(MSE_tst), time=mean(elapsed))

sa_data_55<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/SA0.55_LOG_56.txt")
sa_data_55b<-sa_data_55%>%group_by(iteration)%>%summarize(mse_trg=mean(MSE_trg), mse_val=mean(MSE_val), mse_tst=mean(MSE_tst), time=mean(elapsed))

sa_data_95<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/SA0.95_LOG_56.txt")
sa_data_95b<-sa_data_95%>%group_by(iteration)%>%summarize(mse_trg=mean(MSE_trg), mse_val=mean(MSE_val), mse_tst=mean(MSE_tst), time=mean(elapsed))

# Combine into single dataframe for plotting
sa_trg_data<-data.frame(iterations=c(sa_data_7b$iteration, sa_data_15b$iteration, sa_data_35b$iteration, sa_data_55b$iteration, sa_data_95b$iteration), 
                        Error=c(sa_data_7b$mse_tst, sa_data_15b$mse_tst, sa_data_35b$mse_tst, sa_data_55b$mse_tst, sa_data_95b$mse_tst), Time=c(sa_data_7b$time, sa_data_15b$time, sa_data_35b$time, sa_data_55b$time, sa_data_95b$time), Temp=c(rep(".07", nrow(sa_data_7b)), rep("0.15", nrow(sa_data_15b)), rep("0.35", nrow(sa_data_35b)), rep("0.55", nrow(sa_data_55b)), rep("0.95", nrow(sa_data_95b))) )

# GA Data
ga_data_501020<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/NN_OUTPUT/GA_50_10_20_LOG_56.txt")


# Backpropagtion test 
p<-ggplot(data=bkp_gather) + geom_smooth(mapping=aes(x=param_MLP__max_iter, y=error, color=types)) +
  labs(x="No. Iterations", y="MSE", title="Backprogation Fitting of ANN Model vs No of Iterations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") 
p$labels$colour<-""
p


# RHC Plot
p<-ggplot(data=sum_rhc_data_gather)+geom_smooth(mapping=aes(x=iteration, y=error, color=types)) + 
  labs(x="No. Iterations", y="MSE", title="Random Hill Climbing of ANN Model") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") 
p$labels$colour<-""
p

# Simulated Annealing Plot
ggplot(data=sa_trg_data) + geom_smooth(mapping=aes(x=iterations, y=Error, color=Temp)) +
  labs(x="No. Iterations", y="Test MSE", title="Simulated Annealing of ANN Model vs. No of Iterations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") 

#GA Plot
p<-ggplot(data=ga_data_501020) + geom_smooth(mapping=aes(x=iteration, y=MSE_trg, color="Train")) +
  labs(x="No. Iterations", y="Test MSE", title="Genetic Algorithm of ANN Model vs. No of Iterations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  geom_smooth(mapping=aes(x=iteration, y=MSE_tst, color="Test"))
p$labels$colour<-""
p

# Summary  test plot
sum_data<-data.frame(Iterations=c(bkp$param_MLP__max_iter, sum_rhc_data$iteration, sa_data_55b$iteration, ga_data_501020$iteration), 
                     Error=c(bkp$mean_test_score, sum_rhc_data$mse_tst, sa_data_55b$mse_tst, ga_data_501020$MSE_tst), 
                     Method=c(rep("BackProp", nrow(bkp)), rep("RHC", nrow(sum_rhc_data)), rep("SA-0.55",nrow(sa_data_55b)), rep("GA-50-10-20", nrow(ga_data_501020))))

ggplot(data=sum_data) + geom_smooth(mapping=aes(x=Iterations, y=Error, color=Method)) +
  labs(x="No. Iterations", y="Test MSE", title="Comparison of Fitting Methods on ANN Model vs. No of Iterations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") 

# Summary training
sum_data<-data.frame(Iterations=c(bkp$param_MLP__max_iter, sum_rhc_data$iteration, sa_data_55b$iteration, ga_data_501020$iteration), 
                     Error=c(bkp$mean_train_score, sum_rhc_data$mse_trg, sa_data_55b$mse_trg, ga_data_501020$MSE_trg), 
                     Method=c(rep("BackProp", nrow(bkp)), rep("RHC", nrow(sum_rhc_data)), rep("SA-0.55",nrow(sa_data_55b)), rep("GA-50-10-20", nrow(ga_data_501020))))

ggplot(data=sum_data) + geom_smooth(mapping=aes(x=Iterations, y=Error, color=Method)) +
  labs(x="No. Iterations", y="Train MSE", title="Comparison of Fitting Methods on ANN Model vs. No of Iterations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") 


# Time Plot
sum_time_data<-data.frame(Iterations=c(bkp$param_MLP__max_iter, sum_rhc_data$iteration, sa_data_55b$iteration, ga_data_501020$iteration), 
                     Time=c(bkp$mean_fit_time , sum_rhc_data$time_mean , sa_data_55b$time, ga_data_501020$elapsed), 
                     Method=c(rep("BackProp", nrow(bkp)), rep("RHC", nrow(sum_rhc_data)), rep("SA-0.55",nrow(sa_data_55b)), rep("GA-50-10-20", nrow(ga_data_501020))))


ggplot(data=sum_time_data) + geom_smooth(mapping=aes(x=Iterations, y=Time, color=Method)) +
  labs(x="No. Iterations", y="Elapsed Time(s)", title="Comparison of Fitting Methods on ANN Model vs. Fit Time") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top")

# TSP 

tsp_ga<-readr::read_tsv("/Users/Sean/PycharmProjects/CS7641_HW1/TSP/TSP_GA_N_0.2N_0.2N_LOG.txt")
tsp_rhc<-readr::read_tsv("/Users/Sean/PycharmProjects/CS7641_HW1/TSP/TSP_RHC_LOG.txt")
tsp_sa<-readr::read_tsv("/Users/Sean/PycharmProjects/CS7641_HW1/TSP/TSP_SA0.35_LOG.txt")
tsp_mimic<-readr::read_tsv("/Users/Sean/PycharmProjects/CS7641_HW1/TSP/TSP_MIMIC_N_.5N_0.9_LOG.txt")

tsp_rhc_10<-filter(tsp_rhc, tsp_rhc$N==10) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*10), time=mean(time), fevals=mean(fevals), N=10)
tsp_rhc_20<-filter(tsp_rhc, tsp_rhc$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*20), time=mean(time), fevals=mean(fevals), N=20)
tsp_rhc_30<-filter(tsp_rhc, tsp_rhc$N==30) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*30), time=mean(time), fevals=mean(fevals), N=30)
tsp_rhc_40<-filter(tsp_rhc, tsp_rhc$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*40), time=mean(time), fevals=mean(fevals), N=40)
tsp_rhc_50<-filter(tsp_rhc, tsp_rhc$N==50) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*50), time=mean(time), fevals=mean(fevals), N=50)
tsp_rhc_60<-filter(tsp_rhc, tsp_rhc$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*60), time=mean(time), fevals=mean(fevals), N=60)
tsp_rhc_70<-filter(tsp_rhc, tsp_rhc$N==70) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*70), time=mean(time), fevals=mean(fevals), N=70)
tsp_rhc_80<-filter(tsp_rhc, tsp_rhc$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*80), time=mean(time), fevals=mean(fevals), N=80)
tsp_rhc_90<-filter(tsp_rhc, tsp_rhc$N==90) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*90), time=mean(time), fevals=mean(fevals), N=90)
tsp_rhc_100<-filter(tsp_rhc, tsp_rhc$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*100), time=mean(time), fevals=mean(fevals), N=100)

tsp_fitness_rhc<-data.frame(iterations=c(tsp_rhc_10$iterations, tsp_rhc_20$iterations, tsp_rhc_30$iterations, tsp_rhc_40$iterations, tsp_rhc_50$iterations, tsp_rhc_60$iterations, tsp_rhc_70$iterations, tsp_rhc_80$iterations, tsp_rhc_90$iterations, tsp_rhc_100$iterations), 
                        fitness=c(tsp_rhc_10$fitness, tsp_rhc_20$fitness, tsp_rhc_30$fitness, tsp_rhc_40$fitness, tsp_rhc_50$fitness, tsp_rhc_60$fitness, tsp_rhc_70$fitness, tsp_rhc_80$fitness, tsp_rhc_90$fitness, tsp_rhc_100$fitness), 
                        fevals=c(tsp_rhc_10$fevals, tsp_rhc_20$fevals, tsp_rhc_30$fevals, tsp_rhc_40$fevals, tsp_rhc_50$fevals, tsp_rhc_60$fevals, tsp_rhc_70$fevals, tsp_rhc_80$fevals, tsp_rhc_90$fevals, tsp_rhc_100$fevals), 
                        time=c(tsp_rhc_10$time, tsp_rhc_20$time, tsp_rhc_30$time, tsp_rhc_40$time, tsp_rhc_50$time, tsp_rhc_60$time, tsp_rhc_70$fevals, tsp_rhc_80$time, tsp_rhc_90$time, tsp_rhc_100$time), 
                        N=c(rep(10,nrow(tsp_rhc_10)), rep(20,nrow(tsp_rhc_20)), rep(30,nrow(tsp_rhc_30)), rep(40,nrow(tsp_rhc_40)),rep(50,nrow(tsp_rhc_50)),rep(60,nrow(tsp_rhc_60)),rep(70,nrow(tsp_rhc_70)), rep(80,nrow(tsp_rhc_80)),rep(90,nrow(tsp_rhc_90)),rep(100,nrow(tsp_rhc_100))),
                        Type=c(rep("RHC", 9872))
                        )

tsp_mimic_10<-filter(tsp_mimic, tsp_mimic$N==10) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*10), time=mean(time), fevals=mean(fevals), N=10)
tsp_mimic_20<-filter(tsp_mimic, tsp_mimic$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*20), time=mean(time), fevals=mean(fevals), N=20)
tsp_mimic_30<-filter(tsp_mimic, tsp_mimic$N==30) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*30), time=mean(time), fevals=mean(fevals), N=30)
tsp_mimic_40<-filter(tsp_mimic, tsp_mimic$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*40), time=mean(time), fevals=mean(fevals), N=40)
tsp_mimic_50<-filter(tsp_mimic, tsp_mimic$N==50) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*50), time=mean(time), fevals=mean(fevals), N=50)
tsp_mimic_60<-filter(tsp_mimic, tsp_mimic$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*60), time=mean(time), fevals=mean(fevals), N=60)
tsp_mimic_70<-filter(tsp_mimic, tsp_mimic$N==70) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*70), time=mean(time), fevals=mean(fevals), N=70)
tsp_mimic_80<-filter(tsp_mimic, tsp_mimic$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*80), time=mean(time), fevals=mean(fevals), N=80)
tsp_mimic_90<-filter(tsp_mimic, tsp_mimic$N==90) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*90), time=mean(time), fevals=mean(fevals), N=90)
tsp_mimic_100<-filter(tsp_mimic, tsp_mimic$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*100), time=mean(time), fevals=mean(fevals), N=100)

tsp_mimic_fitness<-data.frame(iterations=c(tsp_mimic_10$iterations, tsp_mimic_20$iterations, tsp_mimic_30$iterations, tsp_mimic_40$iterations, tsp_mimic_50$iterations, tsp_mimic_60$iterations, tsp_mimic_70$iterations, tsp_mimic_80$iterations, tsp_mimic_90$iterations, tsp_mimic_100$iterations), 
                              fitness=c(tsp_mimic_10$fitness, tsp_mimic_20$fitness, tsp_mimic_30$fitness, tsp_mimic_40$fitness, tsp_mimic_50$fitness, tsp_mimic_60$fitness, tsp_mimic_70$fitness, tsp_mimic_80$fitness, tsp_mimic_90$fitness, tsp_mimic_100$fitness), 
                              N=c(rep(10,nrow(tsp_mimic_10)), rep(20,nrow(tsp_mimic_20)), rep(30,nrow(tsp_mimic_30)), rep(40,nrow(tsp_mimic_40)),rep(50,nrow(tsp_mimic_50)),rep(60,nrow(tsp_mimic_60)),rep(70,nrow(tsp_mimic_70)), rep(80,nrow(tsp_mimic_80)),rep(90,nrow(tsp_mimic_90)),rep(100,nrow(tsp_mimic_100))),
                              fevals=c(tsp_mimic_10$fevals, tsp_mimic_20$fevals, tsp_mimic_30$fevals, tsp_mimic_40$fevals, tsp_mimic_50$fevals, tsp_mimic_60$fevals, tsp_mimic_70$fevals, tsp_mimic_80$fevals, tsp_mimic_90$fevals, tsp_mimic_100$fevals),
                              time=c(tsp_mimic_10$time, tsp_mimic_20$time, tsp_mimic_30$time, tsp_mimic_40$time, tsp_mimic_50$time, tsp_mimic_60$time, tsp_mimic_70$time, tsp_mimic_80$time, tsp_mimic_90$time, tsp_mimic_100$time),
                              Type=c(rep("mimic", 2672)) )
                              
  
tsp_sa_10<-filter(tsp_sa, tsp_sa$N==10) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*10), time=mean(time), fevals=mean(fevals), N=10)
tsp_sa_20<-filter(tsp_sa, tsp_sa$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*20), time=mean(time), fevals=mean(fevals), N=20)
tsp_sa_30<-filter(tsp_sa, tsp_sa$N==30) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*30), time=mean(time), fevals=mean(fevals), N=30)
tsp_sa_40<-filter(tsp_sa, tsp_sa$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*40), time=mean(time), fevals=mean(fevals), N=40)
tsp_sa_50<-filter(tsp_sa, tsp_sa$N==50) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*50), time=mean(time), fevals=mean(fevals), N=50)
tsp_sa_60<-filter(tsp_sa, tsp_sa$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*60), time=mean(time), fevals=mean(fevals), N=60)
tsp_sa_70<-filter(tsp_sa, tsp_sa$N==70) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*70), time=mean(time), fevals=mean(fevals), N=70)
tsp_sa_80<-filter(tsp_sa, tsp_sa$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*80), time=mean(time), fevals=mean(fevals), N=80)
tsp_sa_90<-filter(tsp_sa, tsp_sa$N==90) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*90), time=mean(time), fevals=mean(fevals), N=90)
tsp_sa_100<-filter(tsp_sa, tsp_sa$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*100), time=mean(time), fevals=mean(fevals), N=100)

tsp_sa_fitness<-data.frame(iterations=c(tsp_sa_10$iterations, tsp_sa_20$iterations, tsp_sa_30$iterations, tsp_sa_40$iterations, tsp_sa_50$iterations, tsp_sa_60$iterations, tsp_sa_70$iterations, tsp_sa_80$iterations, tsp_sa_90$iterations, tsp_sa_100$iterations), 
                              fitness=c(tsp_sa_10$fitness, tsp_sa_20$fitness, tsp_sa_30$fitness, tsp_sa_40$fitness, tsp_sa_50$fitness, tsp_sa_60$fitness, tsp_sa_70$fitness, tsp_sa_80$fitness, tsp_sa_90$fitness, tsp_sa_100$fitness), 
                              N=c(rep(10,nrow(tsp_sa_10)), rep(20,nrow(tsp_sa_20)), rep(30,nrow(tsp_sa_30)), rep(40,nrow(tsp_sa_40)),rep(50,nrow(tsp_sa_50)),rep(60,nrow(tsp_sa_60)),rep(70,nrow(tsp_sa_70)), rep(80,nrow(tsp_sa_80)),rep(90,nrow(tsp_sa_90)),rep(100,nrow(tsp_sa_100))),
                              time=c(tsp_sa_10$time, tsp_sa_20$time, tsp_sa_30$time, tsp_sa_40$time, tsp_sa_50$time, tsp_sa_60$time, tsp_sa_70$time, tsp_sa_80$time, tsp_sa_90$time, tsp_sa_100$time),
                              fevals=c(tsp_sa_10$fevals, tsp_sa_20$fevals, tsp_sa_30$fevals, tsp_sa_40$fevals, tsp_sa_50$fevals, tsp_sa_60$fevals, tsp_sa_70$fevals, tsp_sa_80$fevals, tsp_sa_90$fevals, tsp_sa_100$fevals),
                              Type=c(rep("SA", 8980)) )

tsp_ga_10<-filter(tsp_ga, tsp_ga$N==10) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*10), time=mean(time), fevals=mean(fevals), N=10)
tsp_ga_20<-filter(tsp_ga, tsp_ga$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*20), time=mean(time), fevals=mean(fevals), N=20)
tsp_ga_30<-filter(tsp_ga, tsp_ga$N==30) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*30), time=mean(time), fevals=mean(fevals), N=30)
tsp_ga_40<-filter(tsp_ga, tsp_ga$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*40), time=mean(time), fevals=mean(fevals), N=40)
tsp_ga_50<-filter(tsp_ga, tsp_ga$N==50) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*50), time=mean(time), fevals=mean(fevals), N=50)
tsp_ga_60<-filter(tsp_ga, tsp_ga$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*60), time=mean(time), fevals=mean(fevals), N=60)
tsp_ga_70<-filter(tsp_ga, tsp_ga$N==70) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*70), time=mean(time), fevals=mean(fevals), N=70)
tsp_ga_80<-filter(tsp_ga, tsp_ga$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*80), time=mean(time), fevals=mean(fevals), N=80)
tsp_ga_90<-filter(tsp_ga, tsp_ga$N==90) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*90), time=mean(time), fevals=mean(fevals), N=90)
tsp_ga_100<-filter(tsp_ga, tsp_ga$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness*100), time=mean(time), fevals=mean(fevals), N=100)

tsp_ga_fitness<-data.frame(iterations=c(tsp_ga_10$iterations, tsp_ga_20$iterations, tsp_ga_30$iterations, tsp_ga_40$iterations, tsp_ga_50$iterations, tsp_ga_60$iterations, tsp_ga_70$iterations, tsp_ga_80$iterations, tsp_ga_90$iterations, tsp_ga_100$iterations), 
                           fitness=c(tsp_ga_10$fitness, tsp_ga_20$fitness, tsp_ga_30$fitness, tsp_ga_40$fitness, tsp_ga_50$fitness, tsp_ga_60$fitness, tsp_ga_70$fitness, tsp_ga_80$fitness, tsp_ga_90$fitness, tsp_ga_100$fitness), 
                           fevals=c(tsp_ga_10$fevals, tsp_ga_20$fevals, tsp_ga_30$fevals, tsp_ga_40$fevals, tsp_ga_50$fevals, tsp_ga_60$fevals, tsp_ga_70$fevals, tsp_ga_80$fevals, tsp_ga_90$fevals, tsp_ga_100$fevals), 
                           time=c(tsp_ga_10$time, tsp_ga_20$time, tsp_ga_30$time, tsp_ga_40$time, tsp_ga_50$time, tsp_ga_60$time, tsp_ga_70$time, tsp_ga_80$time, tsp_ga_90$time, tsp_ga_100$time), 
                           N=c(rep(10,nrow(tsp_ga_10)), rep(20,nrow(tsp_ga_20)), rep(30,nrow(tsp_ga_30)), rep(40,nrow(tsp_ga_40)),rep(50,nrow(tsp_ga_50)),rep(60,nrow(tsp_ga_60)),rep(70,nrow(tsp_ga_70)), rep(80,nrow(tsp_ga_80)),rep(90,nrow(tsp_ga_90)),rep(100,nrow(tsp_ga_100))),
                           Type=c(rep("GA", 3224)) 
)


tsp_fitness<-rbind(tsp_fitness_rhc,tsp_mimic_fitness,tsp_sa_fitness,tsp_ga_fitness)

ts_time<-filter(tsp_fitness, tsp_fitness$iterations==100 )
ts_time<-filter(ts_time, ts_time$Type!="RHC")  

ggplot(data=tsp_fitness) + geom_smooth(mapping=aes(x=N, y=fitness, color=Type)) +
  labs(x="Number Points", y="Fitness", title="TSP: Fitness vs. Number of Points") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 100))

ggplot(data=tsp_fitness) + geom_smooth(mapping=aes(x=N, y=fevals, color=Type)) +
  labs(x="Number Points", y="Function Evaluations", title="TSP: Number of Points vs. Function Evaluations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 100))

ggplot(data=ts_time) + geom_smooth(mapping=aes(x=N, y=time, color=Type)) +
  labs(x="Number Points", y="Time (s)", title="TSP: Number of Points vs. Clock Time") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 100))

#Flip Flop
ff_ga<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/FLIPFLOP/FLIPFLOP_GA_N_0.5N_0.2N_LOG.txt")
ff_rhc<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/FLIPFLOP/FLIPFLOP_RHC_LOG.txt")
ff_sa<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/FLIPFLOP/FLIPFLOP_SA0.95_LOG.txt")
ff_mimic<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/FLIPFLOP/FLIPFLOP_MIMIC_N_.5N_0.5_LOG.txt")

ff_rhc_20<-filter(ff_rhc, ff_rhc$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/20), time=mean(time), fevals=mean(fevals), N=20)
ff_rhc_40<-filter(ff_rhc, ff_rhc$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/40), time=mean(time), fevals=mean(fevals), N=40)
ff_rhc_60<-filter(ff_rhc, ff_rhc$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/60), time=mean(time), fevals=mean(fevals), N=60)
ff_rhc_80<-filter(ff_rhc, ff_rhc$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/80), time=mean(time), fevals=mean(fevals), N=80)
ff_rhc_100<-filter(ff_rhc, ff_rhc$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/100), time=mean(time), fevals=mean(fevals), N=100)
ff_rhc_120<-filter(ff_rhc, ff_rhc$N==120) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/120), time=mean(time), fevals=mean(fevals), N=120)
ff_rhc_140<-filter(ff_rhc, ff_rhc$N==140) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/140), time=mean(time), fevals=mean(fevals), N=140)
ff_rhc_160<-filter(ff_rhc, ff_rhc$N==160) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/160), time=mean(time), fevals=mean(fevals), N=160)
ff_rhc_180<-filter(ff_rhc, ff_rhc$N==180) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/180), time=mean(time), fevals=mean(fevals), N=180)
ff_rhc_200<-filter(ff_rhc, ff_rhc$N==200) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/200), time=mean(time), fevals=mean(fevals), N=200)

ff_fitness_rhc<-data.frame(iterations=c(ff_rhc_20$iterations, ff_rhc_40$iterations, ff_rhc_60$iterations, ff_rhc_80$iterations, ff_rhc_100$iterations, ff_rhc_120$iterations, ff_rhc_140$iterations, ff_rhc_160$iterations, ff_rhc_180$iterations, ff_rhc_200$iterations), 
                            fitness=c(ff_rhc_20$fitness, ff_rhc_40$fitness, ff_rhc_60$fitness, ff_rhc_80$fitness, ff_rhc_100$fitness, ff_rhc_120$fitness, ff_rhc_140$fitness, ff_rhc_160$fitness, ff_rhc_180$fitness, ff_rhc_200$fitness), 
                            fevals=c(ff_rhc_20$fevals, ff_rhc_40$fevals, ff_rhc_60$fevals, ff_rhc_80$fevals, ff_rhc_100$fevals, ff_rhc_120$fevals, ff_rhc_140$fevals, ff_rhc_160$fevals, ff_rhc_180$fevals, ff_rhc_200$fevals), 
                            time=c(ff_rhc_20$time, ff_rhc_40$time, ff_rhc_60$time, ff_rhc_80$time, ff_rhc_100$time, ff_rhc_120$time, ff_rhc_140$fevals, ff_rhc_160$time, ff_rhc_180$time, ff_rhc_200$time), 
                            N=c(rep(20,nrow(ff_rhc_20)), rep(40,nrow(ff_rhc_40)), rep(60,nrow(ff_rhc_60)), rep(80,nrow(ff_rhc_80)),rep(100,nrow(ff_rhc_100)),rep(120,nrow(ff_rhc_120)),rep(140,nrow(ff_rhc_140)), rep(160,nrow(ff_rhc_160)),rep(180,nrow(ff_rhc_180)),rep(200,nrow(ff_rhc_200))),
                            Type=c(rep("RHC", 1746))
)


ff_sa_20<-filter(ff_sa, ff_sa$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/20), time=mean(time), fevals=mean(fevals), N=20)
ff_sa_40<-filter(ff_sa, ff_sa$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/40), time=mean(time), fevals=mean(fevals), N=40)
ff_sa_60<-filter(ff_sa, ff_sa$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/60), time=mean(time), fevals=mean(fevals), N=60)
ff_sa_80<-filter(ff_sa, ff_sa$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/80), time=mean(time), fevals=mean(fevals), N=80)
ff_sa_100<-filter(ff_sa, ff_sa$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/100), time=mean(time), fevals=mean(fevals), N=100)
ff_sa_120<-filter(ff_sa, ff_sa$N==120) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/120), time=mean(time), fevals=mean(fevals), N=120)
ff_sa_140<-filter(ff_sa, ff_sa$N==140) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/140), time=mean(time), fevals=mean(fevals), N=140)
ff_sa_160<-filter(ff_sa, ff_sa$N==160) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/160), time=mean(time), fevals=mean(fevals), N=160)
ff_sa_180<-filter(ff_sa, ff_sa$N==180) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/180), time=mean(time), fevals=mean(fevals), N=180)
ff_sa_200<-filter(ff_sa, ff_sa$N==200) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/200), time=mean(time), fevals=mean(fevals), N=200)

ff_fitness_sa<-data.frame(iterations=c(ff_sa_20$iterations, ff_sa_40$iterations, ff_sa_60$iterations, ff_sa_80$iterations, ff_sa_100$iterations, ff_sa_120$iterations, ff_sa_140$iterations, ff_sa_160$iterations, ff_sa_180$iterations, ff_sa_200$iterations), 
                           fitness=c(ff_sa_20$fitness, ff_sa_40$fitness, ff_sa_60$fitness, ff_sa_80$fitness, ff_sa_100$fitness, ff_sa_120$fitness, ff_sa_140$fitness, ff_sa_160$fitness, ff_sa_180$fitness, ff_sa_200$fitness), 
                           fevals=c(ff_sa_20$fevals, ff_sa_40$fevals, ff_sa_60$fevals, ff_sa_80$fevals, ff_sa_100$fevals, ff_sa_120$fevals, ff_sa_140$fevals, ff_sa_160$fevals, ff_sa_180$fevals, ff_sa_200$fevals), 
                           time=c(ff_sa_20$time, ff_sa_40$time, ff_sa_60$time, ff_sa_80$time, ff_sa_100$time, ff_sa_120$time, ff_sa_140$fevals, ff_sa_160$time, ff_sa_180$time, ff_sa_200$time), 
                           N=c(rep(20,nrow(ff_sa_20)), rep(40,nrow(ff_sa_40)), rep(60,nrow(ff_sa_60)), rep(80,nrow(ff_sa_80)),rep(100,nrow(ff_sa_100)),rep(120,nrow(ff_sa_120)),rep(140,nrow(ff_sa_140)), rep(160,nrow(ff_sa_160)),rep(180,nrow(ff_sa_180)),rep(200,nrow(ff_sa_200))),
                           Type=c(rep("SA", sum(nrow(ff_sa_20),nrow(ff_sa_40),nrow(ff_sa_60),nrow(ff_sa_80),nrow(ff_sa_100),nrow(ff_sa_120),nrow(ff_sa_140),nrow(ff_sa_160),nrow(ff_sa_180),nrow(ff_sa_200)  )))
)

ff_ga_20<-filter(ff_ga, ff_ga$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/20), time=mean(time), fevals=mean(fevals), N=20)
ff_ga_40<-filter(ff_ga, ff_ga$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/40), time=mean(time), fevals=mean(fevals), N=40)
ff_ga_60<-filter(ff_ga, ff_ga$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/60), time=mean(time), fevals=mean(fevals), N=60)
ff_ga_80<-filter(ff_ga, ff_ga$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/80), time=mean(time), fevals=mean(fevals), N=80)
ff_ga_100<-filter(ff_ga, ff_ga$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/100), time=mean(time), fevals=mean(fevals), N=100)
ff_ga_120<-filter(ff_ga, ff_ga$N==120) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/120), time=mean(time), fevals=mean(fevals), N=120)
ff_ga_140<-filter(ff_ga, ff_ga$N==140) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/140), time=mean(time), fevals=mean(fevals), N=140)
ff_ga_160<-filter(ff_ga, ff_ga$N==160) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/160), time=mean(time), fevals=mean(fevals), N=160)
ff_ga_180<-filter(ff_ga, ff_ga$N==180) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/180), time=mean(time), fevals=mean(fevals), N=180)
ff_ga_200<-filter(ff_ga, ff_ga$N==200) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/200), time=mean(time), fevals=mean(fevals), N=200)

ff_fitness_ga<-data.frame(iterations=c(ff_ga_20$iterations, ff_ga_40$iterations, ff_ga_60$iterations, ff_ga_80$iterations, ff_ga_100$iterations, ff_ga_120$iterations, ff_ga_140$iterations, ff_ga_160$iterations, ff_ga_180$iterations, ff_ga_200$iterations), 
                          fitness=c(ff_ga_20$fitness, ff_ga_40$fitness, ff_ga_60$fitness, ff_ga_80$fitness, ff_ga_100$fitness, ff_ga_120$fitness, ff_ga_140$fitness, ff_ga_160$fitness, ff_ga_180$fitness, ff_ga_200$fitness), 
                          fevals=c(ff_ga_20$fevals, ff_ga_40$fevals, ff_ga_60$fevals, ff_ga_80$fevals, ff_ga_100$fevals, ff_ga_120$fevals, ff_ga_140$fevals, ff_ga_160$fevals, ff_ga_180$fevals, ff_ga_200$fevals), 
                          time=c(ff_ga_20$time, ff_ga_40$time, ff_ga_60$time, ff_ga_80$time, ff_ga_100$time, ff_ga_120$time, ff_ga_140$fevals, ff_ga_160$time, ff_ga_180$time, ff_ga_200$time), 
                          N=c(rep(20,nrow(ff_ga_20)), rep(40,nrow(ff_ga_40)), rep(60,nrow(ff_ga_60)), rep(80,nrow(ff_ga_80)),rep(100,nrow(ff_ga_100)),rep(120,nrow(ff_ga_120)),rep(140,nrow(ff_ga_140)), rep(160,nrow(ff_ga_160)),rep(180,nrow(ff_ga_180)),rep(200,nrow(ff_ga_200))),
                          Type=c(rep("GA", sum(nrow(ff_ga_20),nrow(ff_ga_40),nrow(ff_ga_60),nrow(ff_ga_80),nrow(ff_ga_100),nrow(ff_ga_120),nrow(ff_ga_140),nrow(ff_ga_160),nrow(ff_ga_180),nrow(ff_ga_200))))
)

ff_mimic_20<-filter(ff_mimic, ff_mimic$N==20) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/20), time=mean(time), fevals=mean(fevals), N=20)
ff_mimic_40<-filter(ff_mimic, ff_mimic$N==40) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/40), time=mean(time), fevals=mean(fevals), N=40)
ff_mimic_60<-filter(ff_mimic, ff_mimic$N==60) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/60), time=mean(time), fevals=mean(fevals), N=60)
ff_mimic_80<-filter(ff_mimic, ff_mimic$N==80) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/80), time=mean(time), fevals=mean(fevals), N=80)
ff_mimic_100<-filter(ff_mimic, ff_mimic$N==100) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/100), time=mean(time), fevals=mean(fevals), N=100)
ff_mimic_120<-filter(ff_mimic, ff_mimic$N==120) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/120), time=mean(time), fevals=mean(fevals), N=120)
ff_mimic_140<-filter(ff_mimic, ff_mimic$N==140) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/140), time=mean(time), fevals=mean(fevals), N=140)
ff_mimic_160<-filter(ff_mimic, ff_mimic$N==160) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/160), time=mean(time), fevals=mean(fevals), N=160)
ff_mimic_180<-filter(ff_mimic, ff_mimic$N==180) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/180), time=mean(time), fevals=mean(fevals), N=180)
ff_mimic_200<-filter(ff_mimic, ff_mimic$N==200) %>% group_by(iterations) %>% summarise(fitness=mean(fitness/200), time=mean(time), fevals=mean(fevals), N=200)

ff_fitness_mimic<-data.frame(iterations=c(ff_mimic_20$iterations, ff_mimic_40$iterations, ff_mimic_60$iterations, ff_mimic_80$iterations, ff_mimic_100$iterations, ff_mimic_120$iterations, ff_mimic_140$iterations, ff_mimic_160$iterations, ff_mimic_180$iterations, ff_mimic_200$iterations), 
                          fitness=c(ff_mimic_20$fitness, ff_mimic_40$fitness, ff_mimic_60$fitness, ff_mimic_80$fitness, ff_mimic_100$fitness, ff_mimic_120$fitness, ff_mimic_140$fitness, ff_mimic_160$fitness, ff_mimic_180$fitness, ff_mimic_200$fitness), 
                          fevals=c(ff_mimic_20$fevals, ff_mimic_40$fevals, ff_mimic_60$fevals, ff_mimic_80$fevals, ff_mimic_100$fevals, ff_mimic_120$fevals, ff_mimic_140$fevals, ff_mimic_160$fevals, ff_mimic_180$fevals, ff_mimic_200$fevals), 
                          time=c(ff_mimic_20$time, ff_mimic_40$time, ff_mimic_60$time, ff_mimic_80$time, ff_mimic_100$time, ff_mimic_120$time, ff_mimic_140$fevals, ff_mimic_160$time, ff_mimic_180$time, ff_mimic_200$time), 
                          N=c(rep(20,nrow(ff_mimic_20)), rep(40,nrow(ff_mimic_40)), rep(60,nrow(ff_mimic_60)), rep(80,nrow(ff_mimic_80)),rep(100,nrow(ff_mimic_100)),rep(120,nrow(ff_mimic_120)),rep(140,nrow(ff_mimic_140)), rep(160,nrow(ff_mimic_160)),rep(180,nrow(ff_mimic_180)),rep(200,nrow(ff_mimic_200))),
                          Type=c(rep("mimic", sum(nrow(ff_mimic_20),nrow(ff_mimic_40),nrow(ff_mimic_60),nrow(ff_mimic_80),nrow(ff_mimic_100),nrow(ff_mimic_120),nrow(ff_mimic_140),nrow(ff_mimic_160),nrow(ff_mimic_180),nrow(ff_mimic_200))))
)


ff_fitness<-rbind(ff_fitness_rhc,ff_fitness_mimic,ff_fitness_sa,ff_fitness_ga)

ff_time<-filter(ff_fitness, ff_fitness$iterations==1000 )
ff_time<-filter(ff_time, ts_time$Type!="RHC")  


ggplot(data=ff_fitness) + geom_smooth(mapping=aes(x=N, y=fitness, color=Type)) +
  labs(x="Bit Length", y="Fitness (% of Max)", title="Flip Flop: Fitness vs. Length") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 200)) + scale_y_continuous(limits = c(0.5, 1))

ggplot(data=ff_time) + geom_point(mapping=aes(x=N, y=fevals, color=Type)) +
  labs(x="Bit Length", y="Function Evaluations", title="Flip Flop: Length vs. Function Evaluations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 200))

ggplot(data=ff_time) + geom_smooth(mapping=aes(x=N, y=time, color=Type)) +
  labs(x="Bit Length", y="Time (s)", title="Flip Flop: Length vs. Clock Time") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 100))

ggplot(data=ff_fitness) + geom_smooth(mapping=aes(x=N, y=iterations, color=Type)) +
  labs(x="Bit Length", y="Iterations", title="Flip Flop: Length vs. Iterations") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="top") +
  scale_x_continuous(limits = c(0, 200))


# Cont Peaks
cp_ga<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/CONTPEAKS/CONTPEAKS_GA_N_0.5N_0.2N_LOG.txt")
cp_rhc<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/CONTPEAKS/CONTPEAKS_RHC_LOG.txt")
cp_sa<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/CONTPEAKS/CONTPEAKS_SA0.95_LOG.txt")
cp_mimic<-readr::read_csv("/Users/Sean/PycharmProjects/CS7641_HW1/CONTPEAKS/CONTPEAKS_MIMIC_N_.5N_0.5_LOG.txt")

