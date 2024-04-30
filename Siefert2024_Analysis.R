# Siefert2024 behavioral data analysis code
# Liz Siefert, March 2024
# See README for information on data. Email me (Liz; sieferte@pennmedicine.upenn.edu) if you have further questions.

library("ggplot2")
library("dplyr")
library("gridExtra")
library("corrplot")
library('lme4')
library('lmerTest')
library('ggeffects')
library('ggpubr')
library('nlme')
library('emmeans')
library('see')
library('ggResidpanel')
library('patchwork')
library('car')
library("ggbeeswarm")
library("viridis")
library("RColorBrewer")

### load different data files

Siefert2024_data<-read.csv('Siefert2024_behavdata.csv') #test data - main file used
learning_data<-read.csv('learning_data.csv') #learning data
cueing_stages<-read.csv('cueing_stages.csv') # for each item, which sleep stage was it cued in
cluster_array<-read.csv('cluster_array.csv') #grand average cluster values, when clusters are calculated across sound vs no sound. so these just represent the significant difference between sound vs no sound SO. 
sleep_information<-read.csv('sleep_information.csv') # this data frame contains general sleep information about the participants, these values are in epochs, which were 30s each

### make main data frames to work from
Data_new <- Siefert2024_data %>%  
  group_by(SubNum, SatNum, TestType)  %>% 
  summarise(prenap = mean(Accuracy[which(SessionNum == 1)],na.rm = TRUE),
            postnap = mean(Accuracy[which(SessionNum == 2)],na.rm = TRUE),
            CueStyle = CueStyle[1],
            Cued = Cued[1],
            num_times_cued = NumberOfTimesCued[1]) %>% 
  mutate(acc_diff = postnap-prenap)
Data_new['num_cues3'] <- scale(Data_new$num_times_cued) #scale 
Data_new$CueStyle = relevel(as.factor(Data_new$CueStyle), ref = 'uncued') # contrast to uncued
Data_new$TestType<-as.factor(Data_new$TestType)
Data_new$Cued<-as.factor(Data_new$Cued)

Data_no_novel <- filter(Data_new, TestType != 'novel')

items_cued_once <- function(data_to_use) {
  Data_morethan1 <- data_to_use
  Data_morethan1$Include <- NaN
  Data_morethan1$Include[Data_morethan1$CueStyle == 'interleaved' & Data_morethan1$num_times_cued > 0]<-1
  Data_morethan1$Include[Data_morethan1$CueStyle == 'blocked' & Data_morethan1$num_times_cued > 0]<-1
  Data_morethan1$Include[Data_morethan1$CueStyle == 'uncued']<-1
  Data_morethan1<- filter(Data_morethan1, Include == 1)
  Data_morethan1$CueStyle = relevel(as.factor(Data_morethan1$CueStyle), ref = 'uncued') #contrast to uncued
  return(Data_morethan1)
}

Data_once <- items_cued_once(Data_no_novel)

############################################################
#################### Figure 2 ##############################
############################################################

### general learning stats
Data_l <- learning_data %>%  
  group_by(SubNum, BlockNum)  %>% 
  summarise(Overall_acc = mean(Accuracy, na.rm = TRUE)) %>%
  group_by(SubNum)  %>% 
  summarise(max_block = max(BlockNum),
            max_block_acc = Overall_acc[which(BlockNum==max(BlockNum))])
mean(Data_l$max_block)
sd(Data_l$max_block)
mean(Data_l$max_block_acc)
sd(Data_l$max_block_acc)

### Figure 2A
Fig_2A_data <- learning_data %>%  
  group_by(SubNum, BlockNum)  %>% 
  summarise(Unique_acc = mean(Accuracy[which(IsUnique == 1)],na.rm = TRUE),
            Shared_acc = mean(Accuracy[which(IsUnique == 0)],na.rm = TRUE),
            All_acc = mean(Accuracy,na.rm = TRUE))
Fig_2A_data$BlockNum<-as.numeric(Fig_2A_data$BlockNum)

Fig_2A<-ggplot(Fig_2A_data, aes(x = BlockNum, y = All_acc, group = SubNum)) + 
  geom_line(linewidth = 2, alpha = .8, color = 'grey') + theme_classic(base_size = 20)+
  geom_point(alpha = .8, size=5,color = 'black',fill = 'grey') +
  theme(legend.position = "none") + ylim(0,1)+ 
  scale_x_continuous(breaks = c(1,2,3,4,5,6),labels = c("1","2","3","4","5","6"))
Fig_2A

### Figure 2B
Fig_2B_data <- Data_new %>%  
  group_by(SubNum, TestType)  %>% 
  summarise(Sess1Acc = mean(prenap,na.rm = TRUE),
            Sess2Acc = mean(postnap,na.rm = TRUE)) %>%
  mutate(AccDiff = Sess2Acc-Sess1Acc, propDiff = ((Sess2Acc+1)-(Sess1Acc+1))/(Sess1Acc+1))
Fig_2B_data_final<-filter(Fig_2B_data, TestType !='novel')

p1<-ggplot(Fig_2B_data_final, aes(x=TestType, y=Sess1Acc)) +
  stat_summary(fun=mean,position=position_dodge(width=0.65),geom='col',width = 0.9, color = c("#8C59A4","#F68C21"),fill = c("#8C59A4","#F68C21")) +
  geom_jitter(data=Fig_2B_data_final,
              aes(x=TestType, y=Sess1Acc),alpha = 0.23,size=3, width = .2) +theme_classic(base_size = 25)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = 'black', width = 0.15, linewidth = 0.8, position = position_dodge(width=0.65))+
  geom_hline(yintercept=.16, color = "black", linetype = 'dashed', alpha = .5, linewidth = 1) +ylim(0,1)+
  theme(axis.title.x = element_blank())
p2<-ggplot(Fig_2B_data_final, aes(x=TestType, y=Sess2Acc)) +
  stat_summary(fun=mean,position=position_dodge(width=0.65),geom='col',width = 0.9,color = c("#8C59A4","#F68C21"),fill = c("#8C59A4","#F68C21")) +
  geom_jitter(data=Fig_2B_data_final,
              aes(x=TestType, y=Sess2Acc),alpha = 0.23,size=3, width = .2) +theme_classic(base_size = 25)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = 'black', width = 0.15, linewidth = 0.8, position = position_dodge(width=0.65))+
  geom_hline(yintercept=.16, color = "black", linetype = 'dashed', alpha = .5, linewidth = 1) +ylim(0,1)+
  theme(axis.title.x = element_blank())
Fig_2B<-ggarrange(p1,p2, ncol=2)
Fig_2B

# stats
x <- filter(Fig_2B_data_final, TestType=="unique")
y <- filter(Fig_2B_data_final, TestType=="shared")

t.test(x$Sess1Acc, mu = 1/6) 
t.test(y$Sess1Acc, mu = 1/6) 

t.test(x$Sess2Acc, mu = 1/6) 
t.test(y$Sess2Acc, mu = 1/6) 

t.test(x$Sess1Acc, y$Sess1Acc, paired = TRUE, alternative = "two.sided")
t.test(x$Sess2Acc, y$Sess2Acc, paired = TRUE, alternative = "two.sided")

### basic stats for the novel statellites
data_novel<-filter(Fig_2B_data, TestType =='novel')
t.test(data_novel$Sess1Acc, mu = 1/6) 
t.test(data_novel$Sess2Acc, mu = 1/6) 

### Figure 2C: slopes analysis
Fig_2C_data<- Data_no_novel %>%
  group_by(SubNum, SatNum, Cued) %>%
  summarise(Unique_acc_sess1 = mean(prenap[which(TestType == 'unique')],na.rm = TRUE),
            Shared_acc_sess1 = mean(prenap[which(TestType == 'shared')],na.rm = TRUE),
            Unique_acc_sess2 = mean(postnap[which(TestType == 'unique')],na.rm = TRUE),
            Shared_acc_sess2 = mean(postnap[which(TestType == 'shared')],na.rm = TRUE)) %>%
              group_by(SubNum)%>%
              summarise(prenap_slope = round(lm(Shared_acc_sess1 ~ Unique_acc_sess1)$coefficients[2],2),
                        prenap_intercept = round(lm(Shared_acc_sess1 ~ Unique_acc_sess1)$coefficients[1],2),
                        postnap_slope = round(lm(Shared_acc_sess2 ~ Unique_acc_sess2)$coefficients[2],2),
                        postnap_intercept = round(lm(Shared_acc_sess2 ~ Unique_acc_sess2)$coefficients[1],2))

Fig_2C_data_general<-Fig_2C_data %>%
  summarise(prenapmean_slope = mean(prenap_slope, na.rm = TRUE),
            prenapmean_intercept = mean(prenap_intercept, na.rm = TRUE),
            postnapmean_slope = mean(postnap_slope, na.rm = TRUE),
            postnapmean_intercept = mean(postnap_intercept, na.rm = TRUE))

prenapgg<-ggplot()+
  geom_abline(data=Fig_2C_data,aes(slope=prenap_slope,intercept=prenap_intercept),colour="grey", linewidth = 1, alpha = .5)+
  geom_abline(data = Fig_2C_data_general, aes(slope = prenapmean_slope, intercept = prenapmean_intercept), colour = "#54A3D9", linewidth = 4, linetype = "dashed")+
  theme_classic(base_size = 20)+
  ylim(0,1) + xlim(0,1)+
  xlab('Unique Accuracy') +
  ylab('Shared Accuracy')+
  labs(title = "Pre-nap")
prenapgg

mean(Fig_2C_data$prenap_slope, na.rm = TRUE)
t.test(Fig_2C_data$prenap_slope, mu = 0)

postnapgg<-ggplot()+
  geom_abline(data=Fig_2C_data,aes(slope=postnap_slope,intercept=postnap_intercept),colour="grey", linewidth = 1, alpha = .5)+
  geom_abline(data = Fig_2C_data_general, aes(slope = postnapmean_slope, intercept = postnapmean_intercept), colour = "#C75D5D",linewidth = 4, linetype = "dashed")+
  theme_classic(base_size = 20)+
  ylim(0,1) + xlim(0,1) + 
  xlab('Unique Accuracy') +
  ylab('Shared Accuracy')+
  labs(title = "Post-nap")
postnapgg

mean(Fig_2C_data$postnap_slope, na.rm = TRUE)
t.test(Fig_2C_data$postnap_slope, mu = 0) 
t.test(Fig_2C_data$postnap_slope, Fig_2C_data$prenap_slope, paired = TRUE, alternative = "two.sided") 

Fig_2C_data_together<- filter(Siefert2024_data,TestType != 'novel')  %>%
  group_by(SubNum, SatNum, SessionNum) %>%
  summarise(Shared_acc = mean(Accuracy[which(IsUnique == 0)], na.rm = TRUE),
            Unique_acc = mean(Accuracy[which(IsUnique == 1)], na.rm = TRUE),) %>%
  group_by(SubNum, SessionNum)%>%
  summarise(slope = round(lm(Shared_acc ~ Unique_acc)$coefficients[2],2),
            intercept = round(lm(Shared_acc ~ Unique_acc)$coefficients[1],2))
Fig_2C_data_together$SessionNum<-as.factor(Fig_2C_data_together$SessionNum)

slopes_combo<-ggplot(data = Fig_2C_data_together, aes(x = SessionNum, y = slope, fill = SessionNum))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_bar(stat = "summary", fun = "mean", alpha = 1) +
  scale_fill_manual(values = c("#54A3D9","#C75D5D"))+
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.1, color = "black", size = 0.7) +
  geom_line(aes(group=SubNum), position = position_dodge(0.2), alpha = .3, col = "black") +
  geom_point(aes(fill=SessionNum,group=SubNum), position = position_dodge(0.2), alpha = .3, size = 3)+
  theme_classic() + theme(legend.position="none")+
  scale_x_discrete(labels = c("Prenap", "Postnap"))+
  ylim(-1,1)+
  xlab("Session") + ylab("Slope") + labs(title = "Slope Δ across test sessions")
slopes_combo

Fig_2C<-grid.arrange(prenapgg,postnapgg,slopes_combo,ncol=3)
Fig_2C

#############################################################
#################### Figure 4  ##############################
#############################################################

### Figure 4A: effect of cueing on unique and shared feature accuracy
model_4A <- lmer(acc_diff ~ Cued*TestType+num_cues3 + prenap + (1|SubNum), data = Data_once)
Anova(model_4A)
summary(model_4A)
resid_panel(model_4A, smoother = TRUE, qqbands = TRUE)

emmeans(model_4A, pairwise ~ TestType|Cued)
emmeans(model_4A, pairwise ~ Cued|TestType)

plot_cued_uncued <- function(model_to_plot) {
  xx<- ggpredict(model_to_plot, terms = c('Cued','TestType'))
  the_plot <- ggplot(xx, aes(x = as.factor(x), y = predicted, color = group)) +
    geom_hline(yintercept=0, color = "grey")+
    geom_point(size = 9,position = position_dodge(.7)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width=0,
                  lwd = 3,
                  position = position_dodge(.7)) +
    scale_color_manual(values = c("#8C59A4","#F68C21")) + 
    scale_x_discrete(labels = c("Uncued", "Cued"))+
    scale_y_continuous(breaks = c(-.3, -.2, -.1, 0, .1, .2, .3), limits = c(-.3, .3)) + theme_classic() + 
    theme(axis.text = element_text(size = 25),axis.title.x = element_blank(), axis.title = element_text(size = 25))+
    labs(y= "Model Estimates of Accuracy Δ")
  return(the_plot)
}
Fig_4A<-plot_cued_uncued(model_4A)
Fig_4A

### novel items

Data_new_novel <- Siefert2024_data %>%  
  group_by(SubNum, SatNum, TestType)  %>% 
  summarise(prenap = mean(Accuracy[which(SessionNum == 1)],na.rm = TRUE),
            postnap = mean(Accuracy[which(SessionNum == 2)],na.rm = TRUE),
            prenapC = mean(Confidence[which(SessionNum == 1)],na.rm = TRUE),
            postnapC = mean(Confidence[which(SessionNum == 2)],na.rm = TRUE),
            CueStyle = CueStyle[1],
            Isnovel = IsNovel[1],
            Cued = Cued[1],
            num_times_cued = NumberOfTimesCued[1]) %>% 
  mutate(acc_diff = postnap-prenap, conf_diff = postnapC-prenapC)

#novel items are all uncued, but we want a measure of the number of times their categories were cued
#and a marker for if it was in interleaved or blocked
for (i in 1:nrow(Data_new_novel)) { 
  subject_number = Data_new_novel$SubNum[i]
  total_cue_num <- sum(Data_new_novel$num_times_cued[Data_new_novel$SubNum==subject_number])
  number_interleaved_cues <- sum(Data_new_novel$num_times_cued[Data_new_novel$SubNum==subject_number & Data_new_novel$CueStyle=='interleaved' & Data_new_novel$Isnovel==0 ])
  number_blocked_cues <- sum(Data_new_novel$num_times_cued[Data_new_novel$SubNum==subject_number & Data_new_novel$CueStyle=='blocked' & Data_new_novel$Isnovel==0 ])
  
  if (Data_new_novel$Isnovel[i]==1 & Data_new_novel$CueStyle[i]=='interleaved') { # if its novel
    Data_new_novel$num_times_cued[i] = number_interleaved_cues
  } else if(Data_new_novel$Isnovel[i]==1 & Data_new_novel$CueStyle[i]=='blocked') {
    Data_new_novel$num_times_cued[i] = number_blocked_cues
  }
}

#only items who were in a category that were cued at least once, or those in uncued categories
Data_new_novel <- filter(Data_new_novel, (TestType=='novel' & num_times_cued>0) | (TestType=='novel' & CueStyle == 'uncued'))
Data_new_novel['num_cues3'] <- scale(Data_new_novel$num_times_cued) #scale as we did with the non-novel satellites

model_novel <- lmer(acc_diff ~ Cued+num_cues3 + prenap + (1 |SubNum), data = Data_new_novel)
summary(model_novel)

### Figure 4B: subset of items whose shared feature accuracy is greater than or equal to unique feature accuracy

Dat_subset <- Data_new
Dat_subset$Include <- 0
for (i in 1:nrow(Dat_subset)) { 
  subnum = Dat_subset$SubNum[i]
  Sat = Dat_subset$SatNum[i]
  how_many = length(Dat_subset$SubNum[Dat_subset$SubNum==subnum & Dat_subset$SatNum==Sat])
  
  if (how_many == 2){ 
    shared_acc = Dat_subset$prenap[Dat_subset$SubNum == subnum & Dat_subset$SatNum == Sat & Dat_subset$TestType == 'shared']
    unique_acc = Dat_subset$prenap[Dat_subset$SubNum == subnum & Dat_subset$SatNum == Sat & Dat_subset$TestType == 'unique']
    the_diff = unique_acc - shared_acc
    
    if (the_diff <= 0){ # if shared is greater than unique, should be less than 0
      Dat_subset$Include[i] <- 1
    }
  }
}
Dat_subset_sharedbetter <- filter(Dat_subset, Include == 1)

raw_data_4B_plot <- Dat_subset_sharedbetter %>%  
  group_by(SubNum, TestType)  %>% 
  summarise(Sess1Acc = mean(prenap,na.rm = TRUE),
            Sess2Acc = mean(postnap,na.rm = TRUE)) %>%
  mutate(AccDiff = Sess2Acc-Sess1Acc, propDiff = ((Sess2Acc+1)-(Sess1Acc+1))/(Sess1Acc+1))
raw_data_4B_plot<-filter(raw_data_4B_plot, TestType !='novel')

subset_plot<-ggplot(raw_data_4B_plot, aes(x=TestType, y=Sess1Acc)) +
  stat_summary(fun=mean,position=position_dodge(width=0.65),geom='col',width = 0.9, color = c("#8C59A4","#F68C21"),fill = c("#8C59A4","#F68C21")) +
  geom_jitter(data=raw_data_4B_plot,
              aes(x=TestType, y=Sess1Acc),alpha = 0.23,size=3, width = .2) +theme_classic(base_size = 25)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = 'black', width = 0.15, linewidth = 0.8, position = position_dodge(width=0.65))+
  geom_hline(yintercept=.16, color = "black", linetype = 'dashed', alpha = .5, linewidth = 1) +ylim(-.001,1.001)+
  theme(axis.title.x = element_blank())
subset_plot

x <- filter(raw_data_4B_plot, TestType=="unique")
y <- filter(raw_data_4B_plot, TestType=="shared")

t.test(x$Sess1Acc, mu = 1/6) 
t.test(y$Sess1Acc, mu = 1/6) 
t.test(y$Sess1Acc, x$Sess1Acc, paired = TRUE, alternative = "two.sided")

### run the model on the subset
Data_once_subset <- items_cued_once(Dat_subset_sharedbetter) 
Data_once_subset$Cued<-as.factor(Data_once_subset$Cued)

model_4B <- lmer(acc_diff ~ Cued*TestType+num_cues3 + prenap + (1 |SubNum), data = Data_once_subset)
Anova(model_4B)
summary(model_4B)

emmeans(model_4B, pairwise ~ TestType|Cued)
emmeans(model_4B, pairwise ~ Cued|TestType)

xx<- ggpredict(model_4B, terms = c('Cued','TestType'))
Fig_4_B <- ggplot(xx, aes(x = as.factor(x), y = predicted, color = group)) +
  geom_hline(yintercept=0, color = "grey")+
  geom_point(size = 9,position = position_dodge(.7)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width=0,
                lwd = 3,
                position = position_dodge(.7)) +
  scale_color_manual(values = c("#8C59A4","#F68C21")) + 
  scale_x_discrete(labels = c("Uncued", "Cued"))+
  scale_y_continuous(breaks = c(-.3, -.2, -.1, 0, .1, .2, .3), limits = c(-.35, .35)) + theme_classic() + 
  theme(axis.text = element_text(size = 25),axis.title.x = element_blank(), axis.title = element_text(size = 25))+
  labs(y= "Model Estimates of Accuracy Δ")
Fig_4_B 

### Figure 4C

# look at uncued items in cued categories
Data_fig4C <- Data_no_novel %>% mutate(leftout_cued=ifelse(num_times_cued== 0, 1,-1))
Data_fig4C$CatCued<-0 #creating new variable, CatCued
Data_fig4C$CatCued[Data_fig4C$CueStyle == 'uncued']<- 1 #uncued-uncued
Data_fig4C$CatCued[Data_fig4C$CueStyle == 'interleaved' & Data_fig4C$leftout_cued == 1]<- 2 #uncued-cued
Data_fig4C$CatCued[Data_fig4C$CueStyle == 'blocked' & Data_fig4C$leftout_cued == 1]<- 2 #uncued-cued
Data_fig4C$CatCued[Data_fig4C$CueStyle == 'interleaved' & Data_fig4C$num_times_cued > 0]<- 3 #cued-cued
Data_fig4C$CatCued[Data_fig4C$CueStyle == 'blocked' & Data_fig4C$num_times_cued > 0]<- 3 #cued-cued
Data_fig4C<- filter(Data_fig4C, CatCued >0) 
Data_fig4C$CatCued<-as.factor(Data_fig4C$CatCued)

model_4C<- lmer(acc_diff ~ CatCued*TestType+prenap+num_cues3+(1|SubNum), data = Data_fig4C)
Anova(model_4C)
summary(model_4C)

emmeans(model_4C, pairwise ~ CatCued|TestType)
emmeans(model_4C, pairwise ~ TestType|CatCued)

x <- ggpredict(model_4C, terms = c('CatCued','TestType'))
Fig_4_C <- ggplot(x, aes(x = as.factor(x), y = predicted, color = group)) +
  geom_hline(yintercept=0, color = "grey")+
  geom_point(size = 9,position = position_dodge(.7)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width=0,
                lwd = 3,
                position = position_dodge(.7)) +
  coord_cartesian(ylim = c(-.3, .3))+ theme(legend.position = 'none') +
  scale_x_discrete(breaks=c("1","2", "3"),labels=c("Uncued Category", "Uncued in Cued Category", "Cued in Cued Category")) +
  scale_y_continuous(breaks = c(-.3, -.2, -.1, 0, .1, .2, .3), limits = c(-.35, .35)) + theme_classic() + 
  scale_color_manual(values = c("#8C59A4","#F68C21")) + theme(axis.text = element_text(size = 15),axis.title.x = element_blank())+
  labs(y= "Model Estimates of Accuracy Δ")+ theme(axis.title = element_text(size = 15))
Fig_4_C

### sleep stage information

dat_sleep_times <- sleep_information %>%
  mutate(prop_N1 = N1/TotalSleep, prop_N2 = N2/TotalSleep, prop_N3 = N3/TotalSleep, prop_REM = REM/TotalSleep)

dat_sleep2 <- cueing_stages %>%
  group_by(SubNum) %>%
  summarise(total_cues = sum(N2, N3, N1, W),
            N2 = sum(N2),
            N3 = sum(N3),
            N1 = sum(N1),
            W = sum(W))%>%
  mutate(W_prop = W/total_cues, N1_prop = N1/total_cues, N2_prop = N2/total_cues, N3_prop = N3/total_cues)

# sleep times information is in epochs. Each epoch is 30s. To convert to minutes:
sleep_mins = (dat_sleep_times$TotalSleep*30)/60

#integrate sleep information in for analyses - add to behavior data
Data_once$N2_cues <-NaN
Data_once$N3_cues <-NaN

for (i in 1:nrow(Data_once)) { # for each row in the data structure
  sub_num = Data_once$SubNum[i]
  item_num = Data_once$SatNum[i]
  
  if (length(cueing_stages$SubNum[cueing_stages$SubNum == sub_num & cueing_stages$SatNum == item_num])>0){
    Data_once$N2_cues[i] = cueing_stages$N2[cueing_stages$SubNum==sub_num & cueing_stages$SatNum == item_num]
    Data_once$N3_cues[i] = cueing_stages$N3[cueing_stages$SubNum==sub_num & cueing_stages$SatNum == item_num]
  }
}
Data_once$N2_cues[Data_once$Cued == -1] = 0
Data_once$N3_cues[Data_once$Cued == -1] = 0

Data_sleep_int<-Data_once

# generate CueTime, which represents which stages that item was cued in
Data_sleep_int$CueTime <- NaN
Data_sleep_int$CueTime[Data_sleep_int$CueStyle == 'uncued'] <- 1
Data_sleep_int$CueTime[Data_sleep_int$N2_cues > 0 & Data_sleep_int$N3_cues == 0]<- 2 #cued in N2 and not N3
Data_sleep_int$CueTime[Data_sleep_int$N3_cues > 0 & Data_sleep_int$N2_cues == 0]<- 3 #cued in N3 and not N2
Data_sleep_int$CueTime[Data_sleep_int$N3_cues > 0 & Data_sleep_int$N2_cues > 0]<- 4 #cued in N2 and N3
Data_sleep_int$CueTime[is.nan(Data_sleep_int$CueTime)]<-0
Data_new6 <- filter(Data_sleep_int, CueTime > 0)
Data_new6$CueTime <- as.factor(Data_new6$CueTime)

sleep_stage_model<- lmer(acc_diff ~ CueTime*TestType + num_times_cued + prenap + (1|SubNum), data = Data_new6)
summary(sleep_stage_model)
Anova(sleep_stage_model)
emmeans(sleep_stage_model, pairwise ~ TestType|CueTime)
emmeans(sleep_stage_model, pairwise ~ CueTime|TestType)

# N2 cues vs N3 cues directly
Data_sleep_int2<-filter(Data_once, TestType != "novel")
Data_sleep_int2$CueTime <- NaN
Data_sleep_int2$CueTime[Data_sleep_int2$CueStyle == 'uncued'] <- 0
Data_sleep_int2$CueTime[Data_sleep_int2$N2_cues > 0 & Data_sleep_int2$N3_cues == 0]<- 2
Data_sleep_int2$CueTime[Data_sleep_int2$N3_cues > 0 & Data_sleep_int2$N2_cues == 0]<- 3
Data_sleep_int2$CueTime[Data_sleep_int2$N3_cues > 0 & Data_sleep_int2$N2_cues > 0]<- 0 #want to remove these this time
Data_sleep_int2$CueTime[is.nan(Data_sleep_int2$CueTime)]<-0
Data_new6_2 <- filter(Data_sleep_int2, CueTime > 0)
Data_new6_2$CueTime <- as.factor(Data_new6_2$CueTime)

n3vsn3_model<- lmer(acc_diff ~ CueTime*TestType + num_times_cued + prenap + (1|SubNum), data = Data_new6_2)
summary(n3vsn3_model)
Anova(n3vsn3_model)

################################################################################
#################### Figure 5 ##################################################
################################################################################

# want to look at blocked vs interleaved
Data_morethan3 <- filter(Data_no_novel, SubNum!=3) #this person had an issue with the cueing orders, where the blocked list repeated meaning items were not truely blocked

Data_morethan3$Include <- NaN
Data_morethan3$Include[Data_morethan3$CueStyle == 'interleaved' & Data_morethan3$num_times_cued > 3]<-1 #4 or more items!
Data_morethan3$Include[Data_morethan3$CueStyle == 'blocked' & Data_morethan3$num_times_cued > 3]<-1
Data_morethan3$Include[Data_morethan3$CueStyle == 'uncued']<-1
Data_morethan3<- filter(Data_morethan3, Include == 1)
Data_morethan3$CueStyle<-as.factor(Data_morethan3$CueStyle)
Data_morethan3$CueStyle <- relevel(Data_morethan3$CueStyle, ref ="uncued") 

# run direct contrast of blocked and interleaved - remove uncued items
Data_morethan3_nouncued <- filter(Data_morethan3, CueStyle != 'uncued')

model_5A <- lmer(acc_diff ~ CueStyle*TestType + num_cues3+ prenap + (1 |SubNum), data = Data_morethan3_nouncued)
Anova(model_5A)
summary(model_5A) 

emmeans(model_5A, pairwise ~ TestType|CueStyle)
emmeans(model_5A, pairwise ~ CueStyle|TestType)
resid_panel(model_5A, smoother = TRUE, qqbands = TRUE)

# can run model with the uncued items to confirm the existence of interactions across blocked-uncued, interleaved-uncued
model_5A_2 <- lmer(acc_diff ~ CueStyle*TestType + prenap + (1 |SubNum), data = Data_morethan3)
summary(model_5A_2)

# lets make this plot!
plot_interleaved_blocked <- function(model_to_plot){
  xx<- ggpredict(model_to_plot, terms = c('CueStyle','TestType'))
  the_plot <- ggplot(xx, aes(x = as.factor(x), y = predicted, color = group)) +
    geom_hline(yintercept=0, color = "grey")+
    geom_point(size = 8,position = position_dodge(.7)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width=0,
                  lwd = 3,
                  position = position_dodge(.7)) +
    scale_y_continuous(breaks = c(-.3, -.2, -.1, 0, .1, .2, .3), limits = c(-.3, .3)) + 
    theme_classic()+
    scale_color_manual(values = c("#8C59A4","#F68C21")) + theme(axis.text = element_text(size = 25),axis.title.x = element_blank())+
    labs(y= "Model Estimates of Accuracy Δ")+ theme(axis.title = element_text(size = 25))
  return(the_plot)
}

Fig_5A<-plot_interleaved_blocked(model_5A)
Fig_5A

### Figure 5B, blocked sequence analysis
blocked_fullycued <- filter(Data_no_novel, SubNum!=3, (CueStyle == 'blocked' & num_times_cued >=8)) #participant 3 had an issue with cueing order
unique_subnums <- c(unique(blocked_fullycued$SubNum)) 
blocked_fullycued2 <- filter(Data_no_novel, SubNum %in% unique_subnums)
Data_seq_analysis <- filter(blocked_fullycued2, (CueStyle == 'blocked' & num_times_cued >=8) | CueStyle == 'uncued')
blocked_fullycued<-Data_seq_analysis
blocked_fullycued$Serial <- blocked_fullycued$SatNum
blocked_fullycued$Serial[blocked_fullycued$CueStyle == 'uncued'] <- 0
blocked_fullycued$revSerial <- NaN
blocked_fullycued$revSerial[blocked_fullycued$CueStyle == 'uncued'] <- 0

for (i in 1:nrow(blocked_fullycued)) { # step through each row of the array
  if (blocked_fullycued$CueStyle[i] != 'uncued') {
    subject_number = blocked_fullycued$SubNum[i]# find the subject number
    if (i == 1) { # if the first row
      #find all the unique SatNum for that person
      the_items <- unique(blocked_fullycued$SatNum[blocked_fullycued$SubNum==subject_number])
      # order them!
      the_order <- order(the_items, decreasing = T)
    } else if (subject_number != blocked_fullycued$SubNum[i-1]) {
      # do same thing as if first line
      the_items <- unique(blocked_fullycued$SatNum[blocked_fullycued$SubNum==subject_number])
      the_order <- order(the_items, decreasing = T)
    } 
    # find your current Sat number
    my_SatNum <- blocked_fullycued$SatNum[i]
    # get the index in order:
    order_index <- which(the_order == my_SatNum)
    # put that in new column: revSerial
    blocked_fullycued$revSerial[i] <- order_index
  }
}

blocked_fullycued$revSerial <- blocked_fullycued$revSerial-5 #normalize for uncued items
blocked_fullycued$revSerial2 <- abs(blocked_fullycued$revSerial - 6) #flip 
blocked_fullycued$revSerial2[blocked_fullycued$CueStyle=='uncued']<-0 # fix uncued items
blocked_fullycued4 <- filter(blocked_fullycued, revSerial < 5) #remove two edge cases
blocked_fullycued4$revSerial2 <- as.factor(blocked_fullycued4$revSerial2) #to recalibrate the levels

# run model as numeric first
blocked_fullycued4$revSerial2<-as.numeric(blocked_fullycued4$revSerial2)
model_5B_numeric <- lmer(acc_diff~revSerial2*TestType + prenap + (1|SubNum), data =blocked_fullycued4)
summary(model_5B_numeric) 
emtrends(model_5B_numeric,pairwise ~ TestType, var= "revSerial2") 

# then run model as categorical
blocked_fullycued4$revSerial2 <- as.factor(blocked_fullycued4$revSerial2)
model_5B_cat<-lmer(acc_diff~TestType*revSerial2 + prenap +(1|SubNum), data =blocked_fullycued4)
summary(model_5B_cat) #2 is position 1, 3 is position 2, 4 is position 3, 5 is position 4. 1 is the contrast, which is uncued.
Anova(model_5B_cat)
emmeans(model_5B_cat, pairwise ~ TestType | revSerial2)
emmeans(model_5B_cat, pairwise ~ revSerial2 | TestType)

#make actual plot
blocked_fullycued4$revSerial2<-as.factor(blocked_fullycued4$revSerial2)
xx<- ggpredict(model_5B_cat, terms = c('revSerial2','TestType'))
Fig_5B <- ggplot(xx, aes(x = x, y = predicted, color = group, group =group )) +
  geom_hline(yintercept=0, color = "grey") +
  geom_smooth(method = "lm", aes(fill = group, alpha = .15), linetype = "dotted", alpha = .15)+
  geom_point(size = 8,position = position_dodge(.45)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width=0,
                lwd = 3,
                position = position_dodge(.45)) +
  theme_classic()+
  scale_y_continuous(breaks = c(-.4,-.3, -.2, -.1, 0, .1, .2, .3,.4), limits = c(-.5, .5)) +
  scale_fill_manual(values=c("#8C59A4","#F68C21"), name="fill")+
  theme_classic() + 
  scale_color_manual(values = c("#8C59A4","#F68C21")) + theme(axis.text = element_text(size = 25),axis.title.x = element_text(size = 25))+
  labs(y= "Model Estimates of Accuracy Δ", x = "Sequence Position")+ theme(axis.title = element_text(size = 25))
Fig_5B

##################################################################################################
#################### Correlations between behavior and neural data  ##############################
##################################################################################################

Data_no_novel_neural<-Data_no_novel 

Data_no_novel_neural$Clust1<-NaN
Data_no_novel_neural$Clust2<-NaN

for (i in 1:nrow(Data_no_novel_neural)) { # for each row in the data structure
  Sub_num <- Data_no_novel_neural$SubNum[i]
  Item_num <- Data_no_novel_neural$SatNum[i]
  if (any(cluster_array$SubNum==Sub_num & cluster_array$SatNum==Item_num)){
    Data_no_novel_neural$Clust1[i] <- cluster_array$Clust1[cluster_array$SubNum==Sub_num & cluster_array$SatNum==Item_num]
    Data_no_novel_neural$Clust2[i] <- cluster_array$Clust2[cluster_array$SubNum==Sub_num & cluster_array$SatNum==Item_num]
  } 
}
Data_no_novel_neural<-na.omit(Data_no_novel_neural)

# make factors
Data_no_novel_neural$TestType<-as.factor(Data_no_novel_neural$TestType)
Data_no_novel_neural$CueStyle<-as.factor(Data_no_novel_neural$CueStyle)

#use in model instead of cued/uncued to see if there is any predictive factor
model_clust2 <- lmer(acc_diff ~ Clust2*TestType + prenap + num_times_cued + (1 |SubNum), data = Data_no_novel_neural)
summary(model_clust2) 

model_clust1 <- lmer(acc_diff ~ Clust1*TestType + prenap + num_times_cued + (1 |SubNum), data = Data_no_novel_neural)
summary(model_clust1) 

# analyses of differences across N2 vs N3 cues, interleaved vs. blocked cues were done using cluster based permutation testing, to see if any clusters existed when comparing those to each other
# these analyses were performed in matlab and generated non-significant clusters


##################################################################################################
#################### confirm basic analyses in confidence data   ##############################
##################################################################################################

Data_new_confidence <- Siefert2024_data %>%  
  group_by(SubNum, SatNum, TestType)  %>% 
  summarise(prenap = mean(Confidence[which(SessionNum == 1)],na.rm = TRUE),
            postnap = mean(Confidence[which(SessionNum == 2)],na.rm = TRUE),
            CueStyle = CueStyle[1],
            Cued = Cued[1],
            num_times_cued = NumberOfTimesCued[1]) %>% 
  mutate(conf_diff = postnap-prenap)
Data_new_confidence$CueStyle3 = relevel(as.factor(Data_new_confidence$CueStyle), ref = 'uncued') # contrast to uncued
Data_new_confidence$Cued<-as.factor(Data_new_confidence$Cued)
Data_new_confidence$TestType<-as.factor(Data_new_confidence$TestType)
Data_new_confidence = filter(Data_new_confidence, prenap != 0)
Data_new_confidence['num_cues3'] <- scale(Data_new_confidence$num_times_cued) #scale 

# data no novel
Data_new_confidence_no_novel <- filter(Data_new_confidence, TestType != 'novel')

# data once
Data_new_confidence_no_novel_once <- items_cued_once(Data_new_confidence_no_novel)

model_conf <- lmer(conf_diff ~ Cued*TestType+num_cues3 + prenap + (1 |SubNum), data = Data_new_confidence_no_novel_once)
summary(model_conf)
Anova(model_conf)
plot(ggpredict(model_conf, terms = c('Cued', 'TestType')))

