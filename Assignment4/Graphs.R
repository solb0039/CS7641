library(tidyverse)
setwd("/Users/Sean/School/GeorgiaTech/CS7641-Machine_Learning/Assignment4/")

# EASY MARKOV DP ITERATION-REWARD COMPARISON
ep<-readr::read_csv("EasyGW/Easy Policy.csv", col_names=TRUE)
ev<-readr::read_csv("EasyGW/Easy Value.csv", col_names=TRUE)
ep_df<-data.frame("iter"=ep$iter, "reward"=ep$reward, "conv"=ep$convergence, "Method"="Policy")
ev_df<-data.frame("iter"=ev$iter, "reward"=ev$reward, "conv"=ev$convergence, "Method"="Value")
e_df<-rbind(ep_df, ev_df)

ggplot(data=e_df)+geom_smooth(mapping=aes(x=iter, y=reward, color=Method)) + 
  labs(x="Iterations", y="Reward", title="Markov Decision Process Reward Comparison- Easy") +
  theme( plot.title = element_text(hjust = 0.5), legend.position="top", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_x_continuous(limits = c(0, 100))

# HARD MARKOV DP ITERATION-REWARD COMPARISON
hp<-readr::read_csv("HardGW/Hard Policy.csv", col_names=TRUE)
hv<-readr::read_csv("HardGW/Hard Value.csv", col_names=TRUE)
hp_df<-data.frame("iter"=hp$iter, "reward"=hp$reward, "conv"=hp$convergence, "Method"="Policy")
hv_df<-data.frame("iter"=hv$iter, "reward"=hv$reward, "conv"=hv$convergence, "Method"="Value")
h_df<-rbind(hp_df, hv_df)

ggplot(data=h_df)+geom_smooth(mapping=aes(x=iter, y=reward, color=Method)) + 
  labs(x="Iterations", y="Reward", title="Markov Decision Process Reward Comparison- Hard") +
  theme( plot.title = element_text(hjust = 0.5), legend.position="top", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_x_continuous(limits = c(0, 100))


# MARKOV DP CONVERGENCE COMPARISON
ggplot(data=e_df)+geom_smooth(mapping=aes(x=iter, y=conv, color=Method)) + 
  labs(x="Iterations", y="Convergence", title="Markov Decision Process Convergence Comparison- Easy") +
  theme( plot.title = element_text(hjust = 0.5), legend.position="top", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_x_continuous(limits = c(0, 100))
  
ggplot(data=h_df)+geom_smooth(mapping=aes(x=iter, y=conv, color=Method)) + 
  labs(x="Iterations", y="Convergence", title="Markov Decision Process Convergence Comparison- Hard") +
  theme( plot.title = element_text(hjust = 0.5), legend.position="top", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_x_continuous(limits = c(0, 100))



# RL ITERATION-REWARD COMPARISON
# REINFORCEMENT LEARNING, EASY GRID WORLD
eq_0.1_n100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q-100.0 E0.1.csv", col_names=TRUE)
eq_0.3_n100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q-100.0 E0.1.csv", col_names=TRUE)
eq_0.5_n100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q-100.0 E0.1.csv", col_names=TRUE)
eq_0.7_n100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q-100.0 E0.1.csv", col_names=TRUE)
eq_0.9_n100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q-100.0 E0.1.csv", col_names=TRUE)

eq_0.1_0_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q0.0 E0.1.csv", col_names=TRUE)
eq_0.3_0_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q0.0 E0.1.csv", col_names=TRUE)
eq_0.5_0_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q0.0 E0.1.csv", col_names=TRUE)
eq_0.7_0_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q0.0 E0.1.csv", col_names=TRUE)
eq_0.9_0_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q0.0 E0.1.csv", col_names=TRUE)

eq_0.1_100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q100.0 E0.1.csv", col_names=TRUE)
eq_0.3_100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q100.0 E0.1.csv", col_names=TRUE)
eq_0.5_100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q100.0 E0.1.csv", col_names=TRUE)
eq_0.7_100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q100.0 E0.1.csv", col_names=TRUE)
eq_0.9_100_0.1<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q100.0 E0.1.csv", col_names=TRUE)
#
eq_0.1_n100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q-100.0 E0.3.csv", col_names=TRUE)
eq_0.3_n100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q-100.0 E0.3.csv", col_names=TRUE)
eq_0.5_n100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q-100.0 E0.3.csv", col_names=TRUE)
eq_0.7_n100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q-100.0 E0.3.csv", col_names=TRUE)
eq_0.9_n100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q-100.0 E0.3.csv", col_names=TRUE)

eq_0.1_0_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q0.0 E0.3.csv", col_names=TRUE)
eq_0.3_0_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q0.0 E0.3.csv", col_names=TRUE)
eq_0.5_0_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q0.0 E0.3.csv", col_names=TRUE)
eq_0.7_0_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q0.0 E0.3.csv", col_names=TRUE)
eq_0.9_0_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q0.0 E0.3.csv", col_names=TRUE)

eq_0.1_100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q100.0 E0.3.csv", col_names=TRUE)
eq_0.3_100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q100.0 E0.3.csv", col_names=TRUE)
eq_0.5_100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q100.0 E0.3.csv", col_names=TRUE)
eq_0.7_100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q100.0 E0.3.csv", col_names=TRUE)
eq_0.9_100_0.3<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q100.0 E0.3.csv", col_names=TRUE)
#
eq_0.1_n100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q-100.0 E0.5.csv", col_names=TRUE)
eq_0.3_n100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q-100.0 E0.5.csv", col_names=TRUE)
eq_0.5_n100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q-100.0 E0.5.csv", col_names=TRUE)
eq_0.7_n100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q-100.0 E0.5.csv", col_names=TRUE)
eq_0.9_n100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q-100.0 E0.5.csv", col_names=TRUE)

eq_0.1_0_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q0.0 E0.5.csv", col_names=TRUE)
eq_0.3_0_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q0.0 E0.5.csv", col_names=TRUE)
eq_0.5_0_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q0.0 E0.5.csv", col_names=TRUE)
eq_0.7_0_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q0.0 E0.5.csv", col_names=TRUE)
eq_0.9_0_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q0.0 E0.5.csv", col_names=TRUE)

eq_0.1_100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.1 q100.0 E0.5.csv", col_names=TRUE)
eq_0.3_100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.3 q100.0 E0.5.csv", col_names=TRUE)
eq_0.5_100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.5 q100.0 E0.5.csv", col_names=TRUE)
eq_0.7_100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.7 q100.0 E0.5.csv", col_names=TRUE)
eq_0.9_100_0.5<-readr::read_csv("EasyGW/Easy Q-Learning L0.9 q100.0 E0.5.csv", col_names=TRUE)




#Ep=0.1
df_LR0.1_In100_E0.1<-data.frame("iter"=eq_0.1_n100_0.1$iter, "time"=eq_0.1_n100_0.1$time, "reward"=eq_0.1_n100_0.1$reward, "conv"=eq_0.1_n100_0.1$convergence, "Params"="_LR0.1_In100_E0.1")
df_LR0.3_In100_E0.1<-data.frame("iter"=eq_0.3_n100_0.1$iter, "time"=eq_0.3_n100_0.1$time, "reward"=eq_0.3_n100_0.1$reward, "conv"=eq_0.3_n100_0.1$convergence, "Params"="_LR0.3_In100_E0.1")
df_LR0.5_In100_E0.1<-data.frame("iter"=eq_0.5_n100_0.1$iter, "time"=eq_0.5_n100_0.1$time, "reward"=eq_0.5_n100_0.1$reward, "conv"=eq_0.5_n100_0.1$convergence, "Params"="_LR0.5_In100_E0.1")
df_LR0.7_In100_E0.1<-data.frame("iter"=eq_0.7_n100_0.1$iter, "time"=eq_0.7_n100_0.1$time, "reward"=eq_0.7_n100_0.1$reward, "conv"=eq_0.7_n100_0.1$convergence, "Params"="_LR0.7_In100_E0.1")
df_LR0.9_In100_E0.1<-data.frame("iter"=eq_0.9_n100_0.1$iter, "time"=eq_0.9_n100_0.1$time, "reward"=eq_0.9_n100_0.1$reward, "conv"=eq_0.9_n100_0.1$convergence, "Params"="_LR0.9_In100_E0.1")
df_qeasy_In100_ep0.1<-rbind(df_LR0.1_In100_E0.1, df_LR0.3_In100_E0.1, df_LR0.5_In100_E0.1, df_LR0.7_In100_E0.1, df_LR0.9_In100_E0.1)

df_LR0.1_I0_E0.1<-data.frame("iter"=eq_0.1_0_0.1$iter, "time"=eq_0.1_0_0.1$time, "reward"=eq_0.1_0_0.1$reward, "conv"=eq_0.1_0_0.1$convergence, "Params"="_LR0.1_I0_E0.1")
df_LR0.3_I0_E0.1<-data.frame("iter"=eq_0.3_0_0.1$iter, "time"=eq_0.3_0_0.1$time, "reward"=eq_0.3_0_0.1$reward, "conv"=eq_0.3_0_0.1$convergence, "Params"="_LR0.3_I0_E0.1")
df_LR0.5_I0_E0.1<-data.frame("iter"=eq_0.5_0_0.1$iter, "time"=eq_0.5_0_0.1$time, "reward"=eq_0.5_0_0.1$reward, "conv"=eq_0.5_0_0.1$convergence, "Params"="_LR0.5_I0_E0.1")
df_LR0.7_I0_E0.1<-data.frame("iter"=eq_0.7_0_0.1$iter, "time"=eq_0.7_0_0.1$time, "reward"=eq_0.7_0_0.1$reward, "conv"=eq_0.7_0_0.1$convergence, "Params"="_LR0.7_I0_E0.1")
df_LR0.9_I0_E0.1<-data.frame("iter"=eq_0.9_0_0.1$iter, "time"=eq_0.9_0_0.1$time, "reward"=eq_0.9_0_0.1$reward, "conv"=eq_0.9_0_0.1$convergence, "Params"="_LR0.9_I0_E0.1")
df_qeasy_I0_ep0.1<-rbind(df_LR0.1_I0_E0.1, df_LR0.3_I0_E0.1, df_LR0.5_I0_E0.1, df_LR0.7_I0_E0.1, df_LR0.9_I0_E0.1)

df_LR0.1_I100_E0.1<-data.frame("iter"=eq_0.1_100_0.1$iter, "time"=eq_0.1_100_0.1$time, "reward"=eq_0.1_100_0.1$reward, "conv"=eq_0.1_100_0.1$convergence, "Params"="_LR0.1_I100_E0.1")
df_LR0.3_I100_E0.1<-data.frame("iter"=eq_0.3_100_0.1$iter, "time"=eq_0.3_100_0.1$time, "reward"=eq_0.3_100_0.1$reward, "conv"=eq_0.3_100_0.1$convergence, "Params"="_LR0.3_I100_E0.1")
df_LR0.5_I100_E0.1<-data.frame("iter"=eq_0.5_100_0.1$iter, "time"=eq_0.5_100_0.1$time, "reward"=eq_0.5_100_0.1$reward, "conv"=eq_0.5_100_0.1$convergence, "Params"="_LR0.5_I100_E0.1")
df_LR0.7_I100_E0.1<-data.frame("iter"=eq_0.7_100_0.1$iter, "time"=eq_0.7_100_0.1$time, "reward"=eq_0.7_100_0.1$reward, "conv"=eq_0.7_100_0.1$convergence, "Params"="_LR0.7_I100_E0.1")
df_LR0.9_I100_E0.1<-data.frame("iter"=eq_0.9_100_0.1$iter, "time"=eq_0.9_100_0.1$time, "reward"=eq_0.9_100_0.1$reward, "conv"=eq_0.9_100_0.1$convergence, "Params"="_LR0.9_I100_E0.1")
df_qeasy_I100_ep0.1<-rbind(df_LR0.1_I100_E0.1, df_LR0.3_I100_E0.1, df_LR0.5_I100_E0.1, df_LR0.7_I100_E0.1, df_LR0.9_I100_E0.1)

#Ep=0.3
df_LR0.1_In100_E0.3<-data.frame("iter"=eq_0.1_n100_0.3$iter, "time"=eq_0.1_n100_0.3$time, "reward"=eq_0.1_n100_0.3$reward, "conv"=eq_0.1_n100_0.3$convergence, "Params"="_LR0.1_In100_E0.3")
df_LR0.3_In100_E0.3<-data.frame("iter"=eq_0.3_n100_0.3$iter, "time"=eq_0.3_n100_0.3$time, "reward"=eq_0.3_n100_0.3$reward, "conv"=eq_0.3_n100_0.3$convergence, "Params"="_LR0.3_In100_E0.3")
df_LR0.5_In100_E0.3<-data.frame("iter"=eq_0.5_n100_0.3$iter, "time"=eq_0.5_n100_0.3$time, "reward"=eq_0.5_n100_0.3$reward, "conv"=eq_0.5_n100_0.3$convergence, "Params"="_LR0.5_In100_E0.3")
df_LR0.7_In100_E0.3<-data.frame("iter"=eq_0.7_n100_0.3$iter, "time"=eq_0.7_n100_0.3$time, "reward"=eq_0.7_n100_0.3$reward, "conv"=eq_0.7_n100_0.3$convergence, "Params"="_LR0.7_In100_E0.3")
df_LR0.9_In100_E0.3<-data.frame("iter"=eq_0.9_n100_0.3$iter, "time"=eq_0.9_n100_0.3$time, "reward"=eq_0.9_n100_0.3$reward, "conv"=eq_0.9_n100_0.3$convergence, "Params"="_LR0.9_In100_E0.3")
df_qeasy_In100_ep0.3<-rbind(df_LR0.1_In100_E0.3, df_LR0.3_In100_E0.3, df_LR0.5_In100_E0.3, df_LR0.7_In100_E0.3, df_LR0.9_In100_E0.3)

df_LR0.1_I0_E0.3<-data.frame("iter"=eq_0.1_0_0.3$iter, "time"=eq_0.1_0_0.3$time, "reward"=eq_0.1_0_0.3$reward, "conv"=eq_0.1_0_0.3$convergence, "Params"="_LR0.1_I0_E0.3")
df_LR0.3_I0_E0.3<-data.frame("iter"=eq_0.3_0_0.3$iter, "time"=eq_0.3_0_0.3$time, "reward"=eq_0.3_0_0.3$reward, "conv"=eq_0.3_0_0.3$convergence, "Params"="_LR0.3_I0_E0.3")
df_LR0.5_I0_E0.3<-data.frame("iter"=eq_0.5_0_0.3$iter, "time"=eq_0.5_0_0.3$time, "reward"=eq_0.5_0_0.3$reward, "conv"=eq_0.5_0_0.3$convergence, "Params"="_LR0.5_I0_E0.3")
df_LR0.7_I0_E0.3<-data.frame("iter"=eq_0.7_0_0.3$iter, "time"=eq_0.7_0_0.3$time, "reward"=eq_0.7_0_0.3$reward, "conv"=eq_0.7_0_0.3$convergence, "Params"="_LR0.7_I0_E0.3")
df_LR0.9_I0_E0.3<-data.frame("iter"=eq_0.9_0_0.3$iter, "time"=eq_0.9_0_0.3$time, "reward"=eq_0.9_0_0.3$reward, "conv"=eq_0.9_0_0.3$convergence, "Params"="_LR0.9_I0_E0.3")
df_qeasy_I0_ep0.3<-rbind(df_LR0.1_I0_E0.3, df_LR0.3_I0_E0.3, df_LR0.5_I0_E0.3, df_LR0.7_I0_E0.3, df_LR0.9_I0_E0.3)

df_LR0.1_I100_E0.3<-data.frame("iter"=eq_0.1_100_0.3$iter, "time"=eq_0.1_100_0.3$time, "reward"=eq_0.1_100_0.3$reward, "conv"=eq_0.1_100_0.3$convergence, "Params"="_LR0.1_I100_E0.3")
df_LR0.3_I100_E0.3<-data.frame("iter"=eq_0.3_100_0.3$iter, "time"=eq_0.3_100_0.3$time, "reward"=eq_0.3_100_0.3$reward, "conv"=eq_0.3_100_0.3$convergence, "Params"="_LR0.3_I100_E0.3")
df_LR0.5_I100_E0.3<-data.frame("iter"=eq_0.5_100_0.3$iter, "time"=eq_0.5_100_0.3$time, "reward"=eq_0.5_100_0.3$reward, "conv"=eq_0.5_100_0.3$convergence, "Params"="_LR0.5_I100_E0.3")
df_LR0.7_I100_E0.3<-data.frame("iter"=eq_0.7_100_0.3$iter, "time"=eq_0.7_100_0.3$time, "reward"=eq_0.7_100_0.3$reward, "conv"=eq_0.7_100_0.3$convergence, "Params"="_LR0.7_I100_E0.3")
df_LR0.9_I100_E0.3<-data.frame("iter"=eq_0.9_100_0.3$iter, "time"=eq_0.9_100_0.3$time, "reward"=eq_0.9_100_0.3$reward, "conv"=eq_0.9_100_0.3$convergence, "Params"="_LR0.9_I100_E0.3")
df_qeasy_I100_ep0.3<-rbind(df_LR0.1_I100_E0.3, df_LR0.3_I100_E0.3, df_LR0.5_I100_E0.3, df_LR0.7_I100_E0.3, df_LR0.9_I100_E0.3)


#Ep=0.5
df_LR0.1_In100_E0.5<-data.frame("iter"=eq_0.1_n100_0.5$iter, "time"=eq_0.1_n100_0.5$time, "reward"=eq_0.1_n100_0.5$reward, "conv"=eq_0.1_n100_0.5$convergence, "Params"="_LR0.1_In100_E0.5")
df_LR0.3_In100_E0.5<-data.frame("iter"=eq_0.3_n100_0.5$iter, "time"=eq_0.3_n100_0.5$time, "reward"=eq_0.3_n100_0.5$reward, "conv"=eq_0.3_n100_0.5$convergence, "Params"="_LR0.3_In100_E0.5")
df_LR0.5_In100_E0.5<-data.frame("iter"=eq_0.5_n100_0.5$iter, "time"=eq_0.5_n100_0.5$time, "reward"=eq_0.5_n100_0.5$reward, "conv"=eq_0.5_n100_0.5$convergence, "Params"="_LR0.5_In100_E0.5")
df_LR0.7_In100_E0.5<-data.frame("iter"=eq_0.7_n100_0.5$iter, "time"=eq_0.7_n100_0.5$time, "reward"=eq_0.7_n100_0.5$reward, "conv"=eq_0.7_n100_0.5$convergence, "Params"="_LR0.7_In100_E0.5")
df_LR0.9_In100_E0.5<-data.frame("iter"=eq_0.9_n100_0.5$iter, "time"=eq_0.9_n100_0.5$time, "reward"=eq_0.9_n100_0.5$reward, "conv"=eq_0.9_n100_0.5$convergence, "Params"="_LR0.9_In100_E0.5")
df_qeasy_In100_ep0.5<-rbind(df_LR0.1_In100_E0.5, df_LR0.3_In100_E0.5, df_LR0.5_In100_E0.5, df_LR0.7_In100_E0.5, df_LR0.9_In100_E0.5)

df_LR0.1_I0_E0.5<-data.frame("iter"=eq_0.1_0_0.5$iter, "time"=eq_0.1_0_0.5$time, "reward"=eq_0.1_0_0.5$reward, "conv"=eq_0.1_0_0.5$convergence, "Params"="_LR0.1_I0_E0.5")
df_LR0.3_I0_E0.5<-data.frame("iter"=eq_0.3_0_0.5$iter, "time"=eq_0.3_0_0.5$time, "reward"=eq_0.3_0_0.5$reward, "conv"=eq_0.3_0_0.5$convergence, "Params"="_LR0.3_I0_E0.5")
df_LR0.5_I0_E0.5<-data.frame("iter"=eq_0.5_0_0.5$iter, "time"=eq_0.5_0_0.5$time, "reward"=eq_0.5_0_0.5$reward, "conv"=eq_0.5_0_0.5$convergence, "Params"="_LR0.5_I0_E0.5")
df_LR0.7_I0_E0.5<-data.frame("iter"=eq_0.7_0_0.5$iter, "time"=eq_0.7_0_0.5$time, "reward"=eq_0.7_0_0.5$reward, "conv"=eq_0.7_0_0.5$convergence, "Params"="_LR0.7_I0_E0.5")
df_LR0.9_I0_E0.5<-data.frame("iter"=eq_0.9_0_0.5$iter, "time"=eq_0.9_0_0.5$time, "reward"=eq_0.9_0_0.5$reward, "conv"=eq_0.9_0_0.5$convergence, "Params"="_LR0.9_I0_E0.5")
df_qeasy_I0_ep0.5<-rbind(df_LR0.1_I0_E0.5, df_LR0.3_I0_E0.5, df_LR0.5_I0_E0.5, df_LR0.7_I0_E0.5, df_LR0.9_I0_E0.5)

df_LR0.1_I100_E0.5<-data.frame("iter"=eq_0.1_100_0.5$iter, "time"=eq_0.1_100_0.5$time, "reward"=eq_0.1_100_0.5$reward, "conv"=eq_0.1_100_0.5$convergence, "Params"="_LR0.1_I100_E0.5")
df_LR0.3_I100_E0.5<-data.frame("iter"=eq_0.3_100_0.5$iter, "time"=eq_0.3_100_0.5$time, "reward"=eq_0.3_100_0.5$reward, "conv"=eq_0.3_100_0.5$convergence, "Params"="_LR0.3_I100_E0.5")
df_LR0.5_I100_E0.5<-data.frame("iter"=eq_0.5_100_0.5$iter, "time"=eq_0.5_100_0.5$time, "reward"=eq_0.5_100_0.5$reward, "conv"=eq_0.5_100_0.5$convergence, "Params"="_LR0.5_I100_E0.5")
df_LR0.7_I100_E0.5<-data.frame("iter"=eq_0.7_100_0.5$iter, "time"=eq_0.7_100_0.5$time, "reward"=eq_0.7_100_0.5$reward, "conv"=eq_0.7_100_0.5$convergence, "Params"="_LR0.7_I100_E0.5")
df_LR0.9_I100_E0.5<-data.frame("iter"=eq_0.9_100_0.5$iter, "time"=eq_0.9_100_0.5$time, "reward"=eq_0.9_100_0.5$reward, "conv"=eq_0.9_100_0.5$convergence, "Params"="_LR0.9_I100_E0.5")
df_qeasy_I100_ep0.5<-rbind(df_LR0.1_I100_E0.5, df_LR0.3_I100_E0.5, df_LR0.5_I100_E0.5, df_LR0.7_I100_E0.5, df_LR0.9_I100_E0.5)

#Ep=0.1
ggplot(data=df_qeasy_In100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params ), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = -100, Decay = 0.1") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

ggplot(data=df_qeasy_I0_ep0.1)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value =  0, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

ggplot(data=df_qeasy_I100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = +100, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

#Ep=0.3
ggplot(data=df_qeasy_In100_ep0.3)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params ), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = -100, Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

ggplot(data=df_qeasy_I0_ep0.3)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = 0, Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

ggplot(data=df_qeasy_I100_ep0.3)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = 100, Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

#Ep=0.5
ggplot(data=df_qeasy_In100_ep0.5)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params ), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = -100, Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

ggplot(data=df_qeasy_I0_ep0.5)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = 0, Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))

ggplot(data=df_qeasy_I100_ep0.5)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Easy", subtitle="Initial Value = +100, Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-300, 100))


# Plot of max reward vs learning rate
max_lr_In100<-rbind(
        data.frame("LR"=0.1, "Reward"=max(eq_0.1_n100_0.1$reward), "Epsilon"=0.1, "I"=-100), 
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
        data.frame("LR"=0.5, "Reward"=max(eq_0.5_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
        data.frame("LR"=0.7, "Reward"=max(eq_0.7_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
        data.frame("LR"=0.9, "Reward"=max(eq_0.9_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
        
        data.frame("LR"=0.1, "Reward"=max(eq_0.1_n100_0.1$reward), "Epsilon"=0.3, "I"=-100), 
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
        data.frame("LR"=0.5, "Reward"=max(eq_0.5_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
        data.frame("LR"=0.7, "Reward"=max(eq_0.7_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
        data.frame("LR"=0.9, "Reward"=max(eq_0.9_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
        
        data.frame("LR"=0.1, "Reward"=max(eq_0.1_n100_0.1$reward), "Epsilon"=0.5, "I"=-100), 
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_n100_0.1$reward), "Epsilon"=0.5, "I"=-100),
        data.frame("LR"=0.5, "Reward"=max(eq_0.5_n100_0.1$reward), "Epsilon"=0.5, "I"=-100),
        data.frame("LR"=0.7, "Reward"=max(eq_0.7_n100_0.1$reward), "Epsilon"=0.5, "I"=-100),
        data.frame("LR"=0.9, "Reward"=max(eq_0.9_n100_0.1$reward), "Epsilon"=0.5, "I"=-100)
)

max_lr_I0<-rbind(
        data.frame("LR"=0.1, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
        data.frame("LR"=0.5, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
        data.frame("LR"=0.7, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
        data.frame("LR"=0.9, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
        
        
        data.frame("LR"=0.1, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
        data.frame("LR"=0.5, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
        data.frame("LR"=0.7, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
        data.frame("LR"=0.9, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
        
        data.frame("LR"=0.1, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
        data.frame("LR"=0.5, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
        data.frame("LR"=0.7, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
        data.frame("LR"=0.9, "Reward"=max(eq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0)
)
        
        
max_lr_I100<-rbind(     
        data.frame("LR"=0.1, "Reward"=max(eq_0.3_100_0.1$reward), "Epsilon"=0.1, "I"=100),
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_100_0.1$reward), "Epsilon"=0.1, "I"=100),
        data.frame("LR"=0.5, "Reward"=max(eq_0.5_100_0.1$reward), "Epsilon"=0.1, "I"=100),
        data.frame("LR"=0.7, "Reward"=max(eq_0.7_100_0.1$reward), "Epsilon"=0.1, "I"=100),
        data.frame("LR"=0.9, "Reward"=max(eq_0.9_100_0.1$reward), "Epsilon"=0.1, "I"=100),

        data.frame("LR"=0.1, "Reward"=max(eq_0.3_100_0.1$reward), "Epsilon"=0.3, "I"=100),
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_100_0.1$reward), "Epsilon"=0.3, "I"=100),
        data.frame("LR"=0.5, "Reward"=max(eq_0.5_100_0.1$reward), "Epsilon"=0.3, "I"=100),
        data.frame("LR"=0.7, "Reward"=max(eq_0.7_100_0.1$reward), "Epsilon"=0.3, "I"=100),
        data.frame("LR"=0.9, "Reward"=max(eq_0.9_100_0.1$reward), "Epsilon"=0.3, "I"=100),

        data.frame("LR"=0.1, "Reward"=max(eq_0.3_100_0.1$reward), "Epsilon"=0.5, "I"=100),
        data.frame("LR"=0.3, "Reward"=max(eq_0.3_100_0.1$reward), "Epsilon"=0.5, "I"=100),
        data.frame("LR"=0.5, "Reward"=max(eq_0.5_100_0.1$reward), "Epsilon"=0.5, "I"=100),
        data.frame("LR"=0.7, "Reward"=max(eq_0.7_100_0.1$reward), "Epsilon"=0.5, "I"=100),
        data.frame("LR"=0.9, "Reward"=max(eq_0.9_100_0.1$reward), "Epsilon"=0.5, "I"=100)
)

# PLOTS OF MAX REWARD AS FUNCTION OF LEARNING RATE
ggplot(data=max_lr_In100, mapping=aes(x=LR, y=Reward, color=Epsilon))+geom_point()+geom_line()+
  labs(x="Learning Rate", y="Max. Reward", title="Reinforcement Learning-Easy", subtitle = "Max Reward vs Learning Rate at Initial Value of -100") +
  theme( plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

ggplot(data=max_lr_I0, mapping=aes(x=LR, y=Reward, color=Epsilon))+geom_point()+geom_line()+
  labs(x="Learning Rate", y="Max. Reward", title="Reinforcement Learning-Easy", subtitle = "Max Reward vs Learning Rate at Initial Value of 0") +
  theme( plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

ggplot(data=max_lr_I100, mapping=aes(x=LR, y=Reward, color=Epsilon))+geom_point()+geom_line()+
  labs(x="Learning Rate", y="Max. Reward", title="Reinforcement Learning-Easy", subtitle = "Max Reward vs Learning Rate at Initial Value of +100") +
  theme( plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# Plots of convergence vs iteration
ggplot(data=df_qeasy_In100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=conv, color=Params ), se=F)+
  labs(x="Iterations", y="Convergence", title="Reinforcement Learning- Easy", subtitle="Convergence vs Iteration @ Initial Value = -100, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 100))

ggplot(data=df_qeasy_I0_ep0.1)+geom_smooth(mapping=aes(x=iter, y=conv, color=Params ), se=F)+
  labs(x="Iterations", y="Convergence", title="Reinforcement Learning- Easy", subtitle="Convergence vs Iteration @ Initial Value = 0, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 100))

ggplot(data=df_qeasy_I100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=conv, color=Params ), se=F)+
  labs(x="Iterations", y="Convergence", title="Reinforcement Learning- Easy", subtitle="Convergence vs Iteration @ Initial Value = +100, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 100))

# No. iterations to converge
b<-rbind(
  data.frame("LR"=0.1, "Iter"=eq_0.1_n100_0.1$iter[nrow(eq_0.1_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.3, "Iter"=eq_0.3_n100_0.1$iter[nrow(eq_0.3_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.5, "Iter"=eq_0.5_n100_0.1$iter[nrow(eq_0.5_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.7, "Iter"=eq_0.7_n100_0.1$iter[nrow(eq_0.7_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.9, "Iter"=eq_0.9_n100_0.1$iter[nrow(eq_0.9_n100_0.1)], "Epsilon"=0.1, "I"=-100),
  
  data.frame("LR"=0.1, "Iter"=eq_0.1_0_0.1$iter[nrow(eq_0.1_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.3, "Iter"=eq_0.3_0_0.1$iter[nrow(eq_0.3_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.5, "Iter"=eq_0.5_0_0.1$iter[nrow(eq_0.5_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.7, "Iter"=eq_0.7_0_0.1$iter[nrow(eq_0.7_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.9, "Iter"=eq_0.9_0_0.1$iter[nrow(eq_0.9_n100_0.3)], "Epsilon"=0.3, "I"=0),
  
  data.frame("LR"=0.1, "Iter"=eq_0.1_100_0.1$iter[nrow(eq_0.1_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.3, "Iter"=eq_0.3_100_0.1$iter[nrow(eq_0.3_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.5, "Iter"=eq_0.5_100_0.1$iter[nrow(eq_0.5_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.7, "Iter"=eq_0.7_100_0.1$iter[nrow(eq_0.7_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.9, "Iter"=eq_0.9_100_0.1$iter[nrow(eq_0.9_n100_0.5)], "Epsilon"=0.5, "I"=100)
)
b$I<-as.factor(b$I)

ggplot(data=b)+geom_smooth(mapping=aes(x=LR, y=Iter, color=I ), se=F)+
  labs(x="Learning Rate", y="# Iterations", title="Reinforcement Learning- Easy", subtitle="No. Iterations to Convergence") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 50))


# Plot of time to convergence
avg_time_easy<-rbind(     
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_100_0.1$time), "Epsilon"=0.1, "I"=100),
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_100_0.3$time), "Epsilon"=0.3, "I"=100),
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_100_0.5$time), "Epsilon"=0.5, "I"=100),
  
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  
  data.frame("LR"=0.1, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.3, "Time"=mean(eq_0.3_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.5, "Time"=mean(eq_0.5_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.7, "Time"=mean(eq_0.7_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.9, "Time"=mean(eq_0.9_n100_0.1$time), "Epsilon"=0.5, "I"=0)
)
avg_time_easy$I<-as.factor(avg_time_easy$I)
avg_time_easy$Epsilon<-as.factor(avg_time_easy$Epsilon)

ggplot(data=avg_time_easy%>%filter(Epsilon==0.1))+geom_smooth(mapping=aes(x=LR, y=Time, color=I ), se=F)+
  labs(x="Learning Rate", y="Avg. Time", title="Reinforcement Learning- Easy", subtitle="Avg. Time to Convergence: Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 0.1))

ggplot(data=avg_time_easy%>%filter(Epsilon==0.3))+geom_smooth(mapping=aes(x=LR, y=Time, color=I ), se=F)+
  labs(x="Learning Rate", y="Avg. Time", title="Reinforcement Learning- Easy", subtitle="Avg. Time to Convergence: Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 0.1))

ggplot(data=avg_time_easy%>%filter(Epsilon==0.5))+geom_smooth(mapping=aes(x=LR, y=Time, color=I ), se=F)+
  labs(x="Learning Rate", y="Avg. Time", title="Reinforcement Learning- Easy", subtitle="Avg. Time to Convergence: Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 0.1))





# REINFORCEMENT LEARNING, HARD GRID WORLD
hq_0.1_n100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q-100.0 E0.1.csv", col_names=TRUE)
hq_0.3_n100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q-100.0 E0.1.csv", col_names=TRUE)
hq_0.5_n100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q-100.0 E0.1.csv", col_names=TRUE)
hq_0.7_n100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q-100.0 E0.1.csv", col_names=TRUE)
hq_0.9_n100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q-100.0 E0.1.csv", col_names=TRUE)

hq_0.1_0_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q0.0 E0.1.csv", col_names=TRUE)
hq_0.3_0_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q0.0 E0.1.csv", col_names=TRUE)
hq_0.5_0_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q0.0 E0.1.csv", col_names=TRUE)
hq_0.7_0_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q0.0 E0.1.csv", col_names=TRUE)
hq_0.9_0_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q0.0 E0.1.csv", col_names=TRUE)

hq_0.1_100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q100.0 E0.1.csv", col_names=TRUE)
hq_0.3_100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q100.0 E0.1.csv", col_names=TRUE)
hq_0.5_100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q100.0 E0.1.csv", col_names=TRUE)
hq_0.7_100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q100.0 E0.1.csv", col_names=TRUE)
hq_0.9_100_0.1<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q100.0 E0.1.csv", col_names=TRUE)
#
hq_0.1_n100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q-100.0 E0.3.csv", col_names=TRUE)
hq_0.3_n100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q-100.0 E0.3.csv", col_names=TRUE)
hq_0.5_n100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q-100.0 E0.3.csv", col_names=TRUE)
hq_0.7_n100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q-100.0 E0.3.csv", col_names=TRUE)
hq_0.9_n100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q-100.0 E0.3.csv", col_names=TRUE)

hq_0.1_0_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q0.0 E0.3.csv", col_names=TRUE)
hq_0.3_0_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q0.0 E0.3.csv", col_names=TRUE)
hq_0.5_0_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q0.0 E0.3.csv", col_names=TRUE)
hq_0.7_0_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q0.0 E0.3.csv", col_names=TRUE)
hq_0.9_0_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q0.0 E0.3.csv", col_names=TRUE)

hq_0.1_100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q100.0 E0.3.csv", col_names=TRUE)
hq_0.3_100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q100.0 E0.3.csv", col_names=TRUE)
hq_0.5_100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q100.0 E0.3.csv", col_names=TRUE)
hq_0.7_100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q100.0 E0.3.csv", col_names=TRUE)
hq_0.9_100_0.3<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q100.0 E0.3.csv", col_names=TRUE)
#
hq_0.1_n100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q-100.0 E0.5.csv", col_names=TRUE)
hq_0.3_n100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q-100.0 E0.5.csv", col_names=TRUE)
hq_0.5_n100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q-100.0 E0.5.csv", col_names=TRUE)
hq_0.7_n100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q-100.0 E0.5.csv", col_names=TRUE)
hq_0.9_n100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q-100.0 E0.5.csv", col_names=TRUE)

hq_0.1_0_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q0.0 E0.5.csv", col_names=TRUE)
hq_0.3_0_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q0.0 E0.5.csv", col_names=TRUE)
hq_0.5_0_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q0.0 E0.5.csv", col_names=TRUE)
hq_0.7_0_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q0.0 E0.5.csv", col_names=TRUE)
hq_0.9_0_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q0.0 E0.5.csv", col_names=TRUE)

hq_0.1_100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.1 q100.0 E0.5.csv", col_names=TRUE)
hq_0.3_100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.3 q100.0 E0.5.csv", col_names=TRUE)
hq_0.5_100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.5 q100.0 E0.5.csv", col_names=TRUE)
hq_0.7_100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.7 q100.0 E0.5.csv", col_names=TRUE)
hq_0.9_100_0.5<-readr::read_csv("HardGW/Hard Q-Learning L0.9 q100.0 E0.5.csv", col_names=TRUE)




#Ep=0.1
df_LR0.1_In100_E0.1<-data.frame("iter"=hq_0.1_n100_0.1$iter, "reward"=hq_0.1_n100_0.1$reward, "conv"=hq_0.1_n100_0.1$convergence, "Params"="_LR0.1_In100_E0.1")
df_LR0.3_In100_E0.1<-data.frame("iter"=hq_0.3_n100_0.1$iter, "reward"=hq_0.3_n100_0.1$reward, "conv"=hq_0.3_n100_0.1$convergence, "Params"="_LR0.3_In100_E0.1")
df_LR0.5_In100_E0.1<-data.frame("iter"=hq_0.5_n100_0.1$iter, "reward"=hq_0.5_n100_0.1$reward, "conv"=hq_0.5_n100_0.1$convergence, "Params"="_LR0.5_In100_E0.1")
df_LR0.7_In100_E0.1<-data.frame("iter"=hq_0.7_n100_0.1$iter, "reward"=hq_0.7_n100_0.1$reward, "conv"=hq_0.7_n100_0.1$convergence, "Params"="_LR0.7_In100_E0.1")
df_LR0.9_In100_E0.1<-data.frame("iter"=hq_0.9_n100_0.1$iter, "reward"=hq_0.9_n100_0.1$reward, "conv"=hq_0.9_n100_0.1$convergence, "Params"="_LR0.9_In100_E0.1")
df_qhard_In100_ep0.1<-rbind(df_LR0.1_In100_E0.1, df_LR0.3_In100_E0.1, df_LR0.5_In100_E0.1, df_LR0.7_In100_E0.1, df_LR0.9_In100_E0.1)

df_LR0.1_I0_E0.1<-data.frame("iter"=hq_0.1_0_0.1$iter, "reward"=hq_0.1_0_0.1$reward, "conv"=hq_0.1_0_0.1$convergence, "Params"="_LR0.1_I0_E0.1")
df_LR0.3_I0_E0.1<-data.frame("iter"=hq_0.3_0_0.1$iter, "reward"=hq_0.3_0_0.1$reward, "conv"=hq_0.3_0_0.1$convergence, "Params"="_LR0.3_I0_E0.1")
df_LR0.5_I0_E0.1<-data.frame("iter"=hq_0.5_0_0.1$iter, "reward"=hq_0.5_0_0.1$reward, "conv"=hq_0.5_0_0.1$convergence, "Params"="_LR0.5_I0_E0.1")
df_LR0.7_I0_E0.1<-data.frame("iter"=hq_0.7_0_0.1$iter, "reward"=hq_0.7_0_0.1$reward, "conv"=hq_0.7_0_0.1$convergence, "Params"="_LR0.7_I0_E0.1")
df_LR0.9_I0_E0.1<-data.frame("iter"=hq_0.9_0_0.1$iter, "reward"=hq_0.9_0_0.1$reward, "conv"=hq_0.9_0_0.1$convergence, "Params"="_LR0.9_I0_E0.1")
df_qhard_I0_ep0.1<-rbind(df_LR0.1_I0_E0.1, df_LR0.3_I0_E0.1, df_LR0.5_I0_E0.1, df_LR0.7_I0_E0.1, df_LR0.9_I0_E0.1)

df_LR0.1_I100_E0.1<-data.frame("iter"=hq_0.1_100_0.1$iter, "reward"=hq_0.1_100_0.1$reward, "conv"=hq_0.1_100_0.1$convergence, "Params"="_LR0.1_I100_E0.1")
df_LR0.3_I100_E0.1<-data.frame("iter"=hq_0.3_100_0.1$iter, "reward"=hq_0.3_100_0.1$reward, "conv"=hq_0.3_100_0.1$convergence, "Params"="_LR0.3_I100_E0.1")
df_LR0.5_I100_E0.1<-data.frame("iter"=hq_0.5_100_0.1$iter, "reward"=hq_0.5_100_0.1$reward, "conv"=hq_0.5_100_0.1$convergence, "Params"="_LR0.5_I100_E0.1")
df_LR0.7_I100_E0.1<-data.frame("iter"=hq_0.7_100_0.1$iter, "reward"=hq_0.7_100_0.1$reward, "conv"=hq_0.7_100_0.1$convergence, "Params"="_LR0.7_I100_E0.1")
df_LR0.9_I100_E0.1<-data.frame("iter"=hq_0.9_100_0.1$iter, "reward"=hq_0.9_100_0.1$reward, "conv"=hq_0.9_100_0.1$convergence, "Params"="_LR0.9_I100_E0.1")
df_qhard_I100_ep0.1<-rbind(df_LR0.1_I100_E0.1, df_LR0.3_I100_E0.1, df_LR0.5_I100_E0.1, df_LR0.7_I100_E0.1, df_LR0.9_I100_E0.1)

#Ep=0.3
df_LR0.1_In100_E0.3<-data.frame("iter"=hq_0.1_n100_0.3$iter, "reward"=hq_0.1_n100_0.3$reward, "conv"=hq_0.1_n100_0.3$convergence, "Params"="_LR0.1_In100_E0.3")
df_LR0.3_In100_E0.3<-data.frame("iter"=hq_0.3_n100_0.3$iter, "reward"=hq_0.3_n100_0.3$reward, "conv"=hq_0.3_n100_0.3$convergence, "Params"="_LR0.3_In100_E0.3")
df_LR0.5_In100_E0.3<-data.frame("iter"=hq_0.5_n100_0.3$iter, "reward"=hq_0.5_n100_0.3$reward, "conv"=hq_0.5_n100_0.3$convergence, "Params"="_LR0.5_In100_E0.3")
df_LR0.7_In100_E0.3<-data.frame("iter"=hq_0.7_n100_0.3$iter, "reward"=hq_0.7_n100_0.3$reward, "conv"=hq_0.7_n100_0.3$convergence, "Params"="_LR0.7_In100_E0.3")
df_LR0.9_In100_E0.3<-data.frame("iter"=hq_0.9_n100_0.3$iter, "reward"=hq_0.9_n100_0.3$reward, "conv"=hq_0.9_n100_0.3$convergence, "Params"="_LR0.9_In100_E0.3")
df_qhard_In100_ep0.3<-rbind(df_LR0.1_In100_E0.3, df_LR0.3_In100_E0.3, df_LR0.5_In100_E0.3, df_LR0.7_In100_E0.3, df_LR0.9_In100_E0.3)

df_LR0.1_I0_E0.3<-data.frame("iter"=hq_0.1_0_0.3$iter, "reward"=hq_0.1_0_0.3$reward, "conv"=hq_0.1_0_0.3$convergence, "Params"="_LR0.1_I0_E0.3")
df_LR0.3_I0_E0.3<-data.frame("iter"=hq_0.3_0_0.3$iter, "reward"=hq_0.3_0_0.3$reward, "conv"=hq_0.3_0_0.3$convergence, "Params"="_LR0.3_I0_E0.3")
df_LR0.5_I0_E0.3<-data.frame("iter"=hq_0.5_0_0.3$iter, "reward"=hq_0.5_0_0.3$reward, "conv"=hq_0.5_0_0.3$convergence, "Params"="_LR0.5_I0_E0.3")
df_LR0.7_I0_E0.3<-data.frame("iter"=hq_0.7_0_0.3$iter, "reward"=hq_0.7_0_0.3$reward, "conv"=hq_0.7_0_0.3$convergence, "Params"="_LR0.7_I0_E0.3")
df_LR0.9_I0_E0.3<-data.frame("iter"=hq_0.9_0_0.3$iter, "reward"=hq_0.9_0_0.3$reward, "conv"=hq_0.9_0_0.3$convergence, "Params"="_LR0.9_I0_E0.3")
df_qhard_I0_ep0.3<-rbind(df_LR0.1_I0_E0.3, df_LR0.3_I0_E0.3, df_LR0.5_I0_E0.3, df_LR0.7_I0_E0.3, df_LR0.9_I0_E0.3)

df_LR0.1_I100_E0.3<-data.frame("iter"=hq_0.1_100_0.3$iter, "reward"=hq_0.1_100_0.3$reward, "conv"=hq_0.1_100_0.3$convergence, "Params"="_LR0.1_I100_E0.3")
df_LR0.3_I100_E0.3<-data.frame("iter"=hq_0.3_100_0.3$iter, "reward"=hq_0.3_100_0.3$reward, "conv"=hq_0.3_100_0.3$convergence, "Params"="_LR0.3_I100_E0.3")
df_LR0.5_I100_E0.3<-data.frame("iter"=hq_0.5_100_0.3$iter, "reward"=hq_0.5_100_0.3$reward, "conv"=hq_0.5_100_0.3$convergence, "Params"="_LR0.5_I100_E0.3")
df_LR0.7_I100_E0.3<-data.frame("iter"=hq_0.7_100_0.3$iter, "reward"=hq_0.7_100_0.3$reward, "conv"=hq_0.7_100_0.3$convergence, "Params"="_LR0.7_I100_E0.3")
df_LR0.9_I100_E0.3<-data.frame("iter"=hq_0.9_100_0.3$iter, "reward"=hq_0.9_100_0.3$reward, "conv"=hq_0.9_100_0.3$convergence, "Params"="_LR0.9_I100_E0.3")
df_qhard_I100_ep0.3<-rbind(df_LR0.1_I100_E0.3, df_LR0.3_I100_E0.3, df_LR0.5_I100_E0.3, df_LR0.7_I100_E0.3, df_LR0.9_I100_E0.3)


#Ep=0.5
df_LR0.1_In100_E0.5<-data.frame("iter"=hq_0.1_n100_0.5$iter, "reward"=hq_0.1_n100_0.5$reward, "conv"=hq_0.1_n100_0.5$convergence, "Params"="_LR0.1_In100_E0.5")
df_LR0.3_In100_E0.5<-data.frame("iter"=hq_0.3_n100_0.5$iter, "reward"=hq_0.3_n100_0.5$reward, "conv"=hq_0.3_n100_0.5$convergence, "Params"="_LR0.3_In100_E0.5")
df_LR0.5_In100_E0.5<-data.frame("iter"=hq_0.5_n100_0.5$iter, "reward"=hq_0.5_n100_0.5$reward, "conv"=hq_0.5_n100_0.5$convergence, "Params"="_LR0.5_In100_E0.5")
df_LR0.7_In100_E0.5<-data.frame("iter"=hq_0.7_n100_0.5$iter, "reward"=hq_0.7_n100_0.5$reward, "conv"=hq_0.7_n100_0.5$convergence, "Params"="_LR0.7_In100_E0.5")
df_LR0.9_In100_E0.5<-data.frame("iter"=hq_0.9_n100_0.5$iter, "reward"=hq_0.9_n100_0.5$reward, "conv"=hq_0.9_n100_0.5$convergence, "Params"="_LR0.9_In100_E0.5")
df_qhard_In100_ep0.5<-rbind(df_LR0.1_In100_E0.5, df_LR0.3_In100_E0.5, df_LR0.5_In100_E0.5, df_LR0.7_In100_E0.5, df_LR0.9_In100_E0.5)

df_LR0.1_I0_E0.5<-data.frame("iter"=hq_0.1_0_0.5$iter, "reward"=hq_0.1_0_0.5$reward, "conv"=hq_0.1_0_0.5$convergence, "Params"="_LR0.1_I0_E0.5")
df_LR0.3_I0_E0.5<-data.frame("iter"=hq_0.3_0_0.5$iter, "reward"=hq_0.3_0_0.5$reward, "conv"=hq_0.3_0_0.5$convergence, "Params"="_LR0.3_I0_E0.5")
df_LR0.5_I0_E0.5<-data.frame("iter"=hq_0.5_0_0.5$iter, "reward"=hq_0.5_0_0.5$reward, "conv"=hq_0.5_0_0.5$convergence, "Params"="_LR0.5_I0_E0.5")
df_LR0.7_I0_E0.5<-data.frame("iter"=hq_0.7_0_0.5$iter, "reward"=hq_0.7_0_0.5$reward, "conv"=hq_0.7_0_0.5$convergence, "Params"="_LR0.7_I0_E0.5")
df_LR0.9_I0_E0.5<-data.frame("iter"=hq_0.9_0_0.5$iter, "reward"=hq_0.9_0_0.5$reward, "conv"=hq_0.9_0_0.5$convergence, "Params"="_LR0.9_I0_E0.5")
df_qhard_I0_ep0.5<-rbind(df_LR0.1_I0_E0.5, df_LR0.3_I0_E0.5, df_LR0.5_I0_E0.5, df_LR0.7_I0_E0.5, df_LR0.9_I0_E0.5)

df_LR0.1_I100_E0.5<-data.frame("iter"=hq_0.1_100_0.5$iter, "reward"=hq_0.1_100_0.5$reward, "conv"=hq_0.1_100_0.5$convergence, "Params"="_LR0.1_I100_E0.5")
df_LR0.3_I100_E0.5<-data.frame("iter"=hq_0.3_100_0.5$iter, "reward"=hq_0.3_100_0.5$reward, "conv"=hq_0.3_100_0.5$convergence, "Params"="_LR0.3_I100_E0.5")
df_LR0.5_I100_E0.5<-data.frame("iter"=hq_0.5_100_0.5$iter, "reward"=hq_0.5_100_0.5$reward, "conv"=hq_0.5_100_0.5$convergence, "Params"="_LR0.5_I100_E0.5")
df_LR0.7_I100_E0.5<-data.frame("iter"=hq_0.7_100_0.5$iter, "reward"=hq_0.7_100_0.5$reward, "conv"=hq_0.7_100_0.5$convergence, "Params"="_LR0.7_I100_E0.5")
df_LR0.9_I100_E0.5<-data.frame("iter"=hq_0.9_100_0.5$iter, "reward"=hq_0.9_100_0.5$reward, "conv"=hq_0.9_100_0.5$convergence, "Params"="_LR0.9_I100_E0.5")
df_qhard_I100_ep0.5<-rbind(df_LR0.1_I100_E0.5, df_LR0.3_I100_E0.5, df_LR0.5_I100_E0.5, df_LR0.7_I100_E0.5, df_LR0.9_I100_E0.5)


#Ep=0.1
ggplot(data=df_qhard_In100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params ), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = -100, Decay = 0.1") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

ggplot(data=df_qhard_I0_ep0.1)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value =  0, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="riht", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

ggplot(data=df_qhard_I100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = +100, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

#Ep=0.3
ggplot(data=df_qhard_In100_ep0.3)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params ), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = -100, Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

ggplot(data=df_qhard_I0_ep0.3)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = 0, Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

ggplot(data=df_qhard_I100_ep0.3)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = 100, Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

#Ep=0.5
ggplot(data=df_qhard_In100_ep0.5)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params ), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = -100, Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

ggplot(data=df_qhard_I0_ep0.5)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = 0, Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))

ggplot(data=df_qhard_I100_ep0.5)+geom_smooth(mapping=aes(x=iter, y=reward, color=Params), se=F)+
  labs(x="Iterations", y="Reward", title="Reinforcement Learning- Hard", subtitle="Initial Value = +100, Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(-1000, 100))


# Plot of max reward vs learning rate
max_lr_In100<-rbind(
  data.frame("LR"=0.1, "Reward"=max(hq_0.1_n100_0.1$reward), "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.5, "Reward"=max(hq_0.5_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.7, "Reward"=max(hq_0.7_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.9, "Reward"=max(hq_0.9_n100_0.1$reward), "Epsilon"=0.1, "I"=-100),
  
  data.frame("LR"=0.1, "Reward"=max(hq_0.1_n100_0.1$reward), "Epsilon"=0.3, "I"=-100), 
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.5, "Reward"=max(hq_0.5_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.7, "Reward"=max(hq_0.7_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.9, "Reward"=max(hq_0.9_n100_0.1$reward), "Epsilon"=0.3, "I"=-100),
  
  data.frame("LR"=0.1, "Reward"=max(hq_0.1_n100_0.1$reward), "Epsilon"=0.5, "I"=-100), 
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_n100_0.1$reward), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.5, "Reward"=max(hq_0.5_n100_0.1$reward), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.7, "Reward"=max(hq_0.7_n100_0.1$reward), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.9, "Reward"=max(hq_0.9_n100_0.1$reward), "Epsilon"=0.5, "I"=-100)
)

max_lr_I0<-rbind(
  data.frame("LR"=0.1, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.5, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.7, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.9, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.1, "I"=0),
  
  
  data.frame("LR"=0.1, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.5, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.7, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.9, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.3, "I"=0),
  
  data.frame("LR"=0.1, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.5, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.7, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.9, "Reward"=max(hq_0.3_0_0.1$reward), "Epsilon"=0.5, "I"=0)
)


max_lr_I100<-rbind(     
  data.frame("LR"=0.1, "Reward"=max(hq_0.3_100_0.1$reward), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_100_0.1$reward), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.5, "Reward"=max(hq_0.5_100_0.1$reward), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.7, "Reward"=max(hq_0.7_100_0.1$reward), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.9, "Reward"=max(hq_0.9_100_0.1$reward), "Epsilon"=0.1, "I"=100),
  
  data.frame("LR"=0.1, "Reward"=max(hq_0.3_100_0.1$reward), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_100_0.1$reward), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.5, "Reward"=max(hq_0.5_100_0.1$reward), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.7, "Reward"=max(hq_0.7_100_0.1$reward), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.9, "Reward"=max(hq_0.9_100_0.1$reward), "Epsilon"=0.3, "I"=100),
  
  data.frame("LR"=0.1, "Reward"=max(hq_0.3_100_0.1$reward), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.3, "Reward"=max(hq_0.3_100_0.1$reward), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.5, "Reward"=max(hq_0.5_100_0.1$reward), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.7, "Reward"=max(hq_0.7_100_0.1$reward), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.9, "Reward"=max(hq_0.9_100_0.1$reward), "Epsilon"=0.5, "I"=100)
)

# PLOTS OF MAX REWARD AS FUNCTION OF LEARNING RATE
ggplot(data=max_lr_In100, mapping=aes(x=LR, y=Reward, color=Epsilon))+geom_point()+geom_line()+
  labs(x="Learning Rate", y="Max. Reward", title="Reinforcement Learning-Hard", subtitle = "Max Reward vs Learning Rate at Initial Value of -100") +
  theme( plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

ggplot(data=max_lr_I0, mapping=aes(x=LR, y=Reward, color=Epsilon))+geom_point()+geom_line()+
  labs(x="Learning Rate", y="Max. Reward", title="Reinforcement Learning-Hard", subtitle = "Max Reward vs Learning Rate at Initial Value of 0") +
  theme( plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

ggplot(data=max_lr_I100, mapping=aes(x=LR, y=Reward, color=Epsilon))+geom_point()+geom_line()+
  labs(x="Learning Rate", y="Max. Reward", title="Reinforcement Learning-Hard", subtitle = "Max Reward vs Learning Rate at Initial Value of +100") +
  theme( plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))






# No. iterations to converge
bh<-rbind(
  data.frame("LR"=0.1, "Iter"=hq_0.1_n100_0.1$iter[nrow(hq_0.1_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.3, "Iter"=hq_0.3_n100_0.1$iter[nrow(hq_0.3_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.5, "Iter"=hq_0.5_n100_0.1$iter[nrow(hq_0.5_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.7, "Iter"=hq_0.7_n100_0.1$iter[nrow(hq_0.7_n100_0.1)], "Epsilon"=0.1, "I"=-100), 
  data.frame("LR"=0.9, "Iter"=hq_0.9_n100_0.1$iter[nrow(hq_0.9_n100_0.1)], "Epsilon"=0.1, "I"=-100),
  
  data.frame("LR"=0.1, "Iter"=hq_0.1_0_0.1$iter[nrow(hq_0.1_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.3, "Iter"=hq_0.3_0_0.1$iter[nrow(hq_0.3_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.5, "Iter"=hq_0.5_0_0.1$iter[nrow(hq_0.5_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.7, "Iter"=hq_0.7_0_0.1$iter[nrow(hq_0.7_n100_0.3)], "Epsilon"=0.3, "I"=0), 
  data.frame("LR"=0.9, "Iter"=hq_0.9_0_0.1$iter[nrow(hq_0.9_n100_0.3)], "Epsilon"=0.3, "I"=0),
  
  data.frame("LR"=0.1, "Iter"=hq_0.1_100_0.1$iter[nrow(hq_0.1_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.3, "Iter"=hq_0.3_100_0.1$iter[nrow(hq_0.3_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.5, "Iter"=hq_0.5_100_0.1$iter[nrow(hq_0.5_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.7, "Iter"=hq_0.7_100_0.1$iter[nrow(hq_0.7_n100_0.5)], "Epsilon"=0.5, "I"=100), 
  data.frame("LR"=0.9, "Iter"=hq_0.9_100_0.1$iter[nrow(hq_0.9_n100_0.5)], "Epsilon"=0.5, "I"=100)
)
bh$I<-as.factor(bh$I)

ggplot(data=bh)+geom_smooth(mapping=aes(x=LR, y=Iter, color=I ), se=F)+
  labs(x="Learning Rate", y="# Iterations", title="Reinforcement Learning- Hard", subtitle="No. Iterations to Convergence") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 60))


# Plots of convergence vs iteration
ggplot(data=df_qhard_In100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=conv, color=Params ), se=F)+
  labs(x="Iterations", y="Convergence", title="Reinforcement Learning- Hard", subtitle="Convergence vs Iteration @ Initial Value = -100, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 100))

ggplot(data=df_qhard_I0_ep0.1)+geom_smooth(mapping=aes(x=iter, y=conv, color=Params ), se=F)+
  labs(x="Iterations", y="Convergence", title="Reinforcement Learning- Hard", subtitle="Convergence vs Iteration @ Initial Value = 0, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 100))

ggplot(data=df_qhard_I100_ep0.1)+geom_smooth(mapping=aes(x=iter, y=conv, color=Params ), se=F)+
  labs(x="Iterations", y="Convergence", title="Reinforcement Learning- Hard", subtitle="Convergence vs Iteration @ Initial Value = +100, Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 100))


# Plot of time to convergence
avg_time_hard<-rbind(     
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_100_0.1$time), "Epsilon"=0.1, "I"=100),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_100_0.1$time), "Epsilon"=0.1, "I"=100),
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_100_0.3$time), "Epsilon"=0.3, "I"=100),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_100_0.3$time), "Epsilon"=0.3, "I"=100),
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_100_0.5$time), "Epsilon"=0.5, "I"=100),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_100_0.5$time), "Epsilon"=0.5, "I"=100),
  
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_n100_0.1$time), "Epsilon"=0.1, "I"=-100),
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_n100_0.3$time), "Epsilon"=0.3, "I"=-100),
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_n100_0.5$time), "Epsilon"=0.5, "I"=-100),
  
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_n100_0.1$time), "Epsilon"=0.1, "I"=0),
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_n100_0.1$time), "Epsilon"=0.3, "I"=0),
  
  data.frame("LR"=0.1, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.3, "Time"=mean(hq_0.3_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.5, "Time"=mean(hq_0.5_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.7, "Time"=mean(hq_0.7_n100_0.1$time), "Epsilon"=0.5, "I"=0),
  data.frame("LR"=0.9, "Time"=mean(hq_0.9_n100_0.1$time), "Epsilon"=0.5, "I"=0)
)
avg_time_hard$I<-as.factor(avg_time_hard$I)
avg_time_hard$Epsilon<-as.factor(avg_time_hard$Epsilon)

ggplot(data=avg_time_hard%>%filter(Epsilon==0.1))+geom_smooth(mapping=aes(x=LR, y=Time, color=I ), se=F)+
  labs(x="Learning Rate", y="Avg. Time", title="Reinforcement Learning- Hard", subtitle="Avg. Time to Convergence: Decay = 0.1") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 0.9))

ggplot(data=avg_time_hard%>%filter(Epsilon==0.3))+geom_smooth(mapping=aes(x=LR, y=Time, color=I ), se=F)+
  labs(x="Learning Rate", y="Avg. Time", title="Reinforcement Learning- Hard", subtitle="Avg. Time to Convergence: Decay = 0.3") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 0.9))

ggplot(data=avg_time_hard%>%filter(Epsilon==0.5))+geom_smooth(mapping=aes(x=LR, y=Time, color=I ), se=F)+
  labs(x="Learning Rate", y="Avg. Time", title="Reinforcement Learning- Hard", subtitle="Avg. Time to Convergence: Decay = 0.5") +
  theme( plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  scale_y_continuous(limits = c(0, 0.9))



