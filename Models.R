
setwd("/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes")
library(R2jags)
library(rjags)
library(coda)
library(ggpubr)
library(runjags)
library(CalvinBayes)
library(sm)
library(ggdist)

#descriptive data analysis
se <- function(x) sqrt(var(x) / length(x))

highfull <- big_data%>%
  filter(Magnitude == "high")

highfull <- highfull%>%
  mutate(RISK = RISK * -1)


high_sum_full <- highfull%>%
  group_by(RISK)%>%
  summarise(mean = mean(choice), se = se(choice))

g_high<- ggplot(high_sum_full, aes(x=RISK, y=mean)) + 
  ylim(-0.0, 1.0) +
  geom_line(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("EV of loss(dkk)", labels = as.character(high_sum_full$RISK), breaks = high_sum_full$RISK)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("P(risky choice)")+
  xlab("EV of loss(dkk)")+ theme_minimal()

g_high

highfull$RISK <- as.double(highfull$RISK)

high_p <- highfull%>%
  group_by(ParID, RISK)%>%
  summarise(mean = mean(choice))

ggplot(high_p, aes(x = factor(RISK), y = mean)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, fill = "blue", alpha = 0.2,
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill = "blue", alpha = 0.3
  ) + geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  theme_minimal()

medfull <- big_data%>%
  filter(Magnitude == "medium")

medfull <- medfull%>%
  mutate(RISK = RISK * -1)


med_sum_full <- medfull%>%
  group_by(RISK)%>%
  summarise(mean = mean(choice), se = se(choice))

g_med<- ggplot(med_sum_full , aes(x=RISK, y=mean)) + 
  ylim(-0.0, 1.0) +
  geom_line(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("EV of loss(dkk)", labels = as.character(med_sum_full$RISK), breaks = med_sum_full$RISK)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("P(risky choice)")+
  xlab("EV of loss(dkk)") + theme_minimal()

g_med

med_p <- medfull%>%
  group_by(ParID, RISK)%>%
  summarise(mean = mean(choice))

ggplot(med_p, aes(x = factor(RISK), y = mean)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, fill = "blue", alpha = 0.2,
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill = "blue", alpha = 0.3
  ) + geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  theme_minimal()

lowfull <- big_data%>%
  filter(Magnitude == "low")

lowfull <- lowfull%>%
  mutate(RISK = RISK * -1)


low_sum_full <- lowfull%>%
  group_by(RISK)%>%
  summarise(mean = mean(choice), se = se(choice))

g_low<- ggplot(low_sum_full, aes(x=RISK, y=mean)) + 
  ylim(-0.0, 1.0) +
  geom_line(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("EV of loss(dkk)", labels = as.character(low_sum_full$RISK), breaks = low_sum_full$RISK)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("reaction time(s)")+
  xlab("EV of loss(dkk)") + theme_minimal()

g_low
low_p <- lowfull%>%
  group_by(ParID, RISK)%>%
  summarise(mean = mean(choice))

ggplot(lowfull, aes(x = factor(RISK), y = rt)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, fill = "blue", alpha = 0.2,
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill = "blue", alpha = 0.3
  ) + #geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  theme_minimal()

low_normalized <- lowfull%>%
  mutate(RISK = RISK * 100)

low_normalized$RISK<-replace(low_normalized$RISK, low_normalized$RISK == -1.26, -1.27)

all_normalised$RISK<-replace(all_normalised$RISK, all_normalised$RISK == -111, -110)

med_normalized <- medfull %>%
  mutate(RISK = RISK * 10)

high_normalized <- highfull 

all_normalised <- rbind(low_normalized, med_normalized, high_normalized)


sum_normalised <- all_normalised%>%
  group_by(RISK)%>%
  summarise(mean = mean(rt, na.rm = TRUE), se = sd(rt, na.rm = TRUE)/sqrt(length((rt))))



g_all<- ggplot(sum_normalised, aes(x=RISK, y=mean)) + 
  ylim(-0.0, 3.0) +
  geom_line(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.9,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("EV of loss(dkk)", labels = as.character(sum_normalised$RISK), breaks = sum_normalised$RISK)+
  #geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("reaction time(s)")+
  xlab("EV of loss(dkk)") + theme_minimal()

g_all

sum_p <- all_normalised%>%
  group_by(ParID, RISK)%>%
  summarise(mean = mean(choice))

ggplot(sum_p, aes(x = factor(RISK), y = mean)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, fill = "blue", alpha = 0.2,
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill = "blue", alpha = 0.3
  ) + geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  theme_minimal()

middle_dat <- all_normalised%>%
  filter(RISK == - 110)

write.csv(middle_sum, file = "middle_level_sum.csv")

middle_sum <- middle_dat%>%
  group_by(ParID, RISK)%>%
  summarise(mean = mean(choice))

low_ev <- list(arr[,1,])

low_y <- list(yy[,1,])

low_ev_unlist <- unlist(low_ev)

low_y_unlist <- unlist(low_y)

low_graph <- cbind(low_ev_unlist, low_y_unlist)

low_graph <- as.data.frame(low_graph)


names(low_graph) <- c("ev", "pRisky")
low_graph <- low_graph%>%
  mutate(ev = ev * -1)

low_sum <- low_graph%>%
  group_by(ev)



by_ev <- low_sum%>%summarise(mean = mean(pRisky), se = se(pRisky))

g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low


med_ev <- list(arr[,2,])

med_y <- list(yy[,2,])

med_ev_unlist <- unlist(med_ev)

med_y_unlist <- unlist(med_y)

med_graph <- cbind(med_ev_unlist, med_y_unlist)

med_graph <- as.data.frame(med_graph)


names(med_graph) <- c("ev", "pRisky")
med_graph <- med_graph%>%
  mutate(ev = ev * -1)

med_sum <-med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), se = se(pRisky))


g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev), breaks = by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in medium magnitude over five levels")

g_med



low_ev <- list(arr[,1,])

low_y <- list(yy[,1,])

low_ev_unlist <- unlist(low_ev)

low_y_unlist <- unlist(low_y)

low_graph <- cbind(low_ev_unlist, low_y_unlist)

low_graph <- as.data.frame(low_graph)


names(low_graph) <- c("ev", "pRisky")
low_graph <- low_graph%>%
  mutate(ev = ev * -1)

low_sum <- low_graph%>%
  group_by(ev)



by_ev <- low_sum%>%summarise(mean = mean(pRisky), se = se(pRisky))

g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low


high_ev <- list(arr[,3,])

high_y <- list(yy[,3,])

high_ev_unlist <- unlist(high_ev)
high_y_unlist <- unlist(high_y)

high_graph <- cbind(high_ev_unlist, high_y_unlist)

high_graph <- as.data.frame(high_graph)


names(high_graph) <- c("ev", "pRisky")
high_graph <- high_graph%>%
  mutate(ev = ev * -1)

high_sum <-high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), se = se(pRisky))


g_med<- ggplot(by_ev_high , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")

g_med


moneyLeft <- big_data%>%
  filter(totalTrialCount == 255)

moneyLeft$Endowment <-replace(moneyLeft$Endowment, moneyLeft$Endowment< 0, 0)

ggplot(moneyLeft, aes(x = Endowment)) +
  geom_freqpoly(
    color = 4,
    lwd = 1,
    linetype = 2
  )+ ylab("Count of subjects") +xlim(0, 5000) +
  xlab("Residual wealth(dkk)")+ theme_minimal()

#data formatting

for (i in 1:10455){
  if (bigTwo[i,]$Endowment < 0){
    bigTwo[i,] <- NA
  }
}

#stop the wealth when it hit 0
low <- big_data%>%
  filter(Magnitude == "low")%>%
  select(Endowment, RISK, CE, choice)

for (i in 1:3485){
  if (low[i,]$Endowment < 0){
    low[i,] <- NA
  }
}

high <- big_data%>%
  filter(Magnitude == "high")%>%
  select(Endowment, RISK, CE, choice)

for (i in 1:3485){
  if (high [i,]$Endowment < 0){
    high [i,] <- NA
  }
}


med <- big_data%>%
  filter(Magnitude == "medium")%>%
  select(Endowment, RISK, CE, choice)

for (i in 1:3485){
  if (med[i,]$Endowment < 0){
    med[i,] <- NA
  }
}

someData <- rep(NaN, 41*3*85)
wealth <- array(someData, c(41, 3, 85))
ar <- array(someData, c(41, 3, 85))
safe <- array(someData, c(41, 3, 85))
y < -array(someData, c(41, 3, 85))


for (j in 0:40){
  
  for (m in 1:85){
    
    index <- j*85 + m
    print(j)
    print(index)
    
    wealth[j+1,1,m] <- low$Endowment[index]
    wealth[j+1,2,m] <- med$Endowment[index]
    wealth[j+1,3,m] <- high$Endowment[index]
    
    ar[j+1,1,m] <- low$RISK[index]
    ar[j+1,2,m] <- med$RISK[index]
    ar[j+1,3,m] <- high$RISK[index]
    
    safe[j+1,1,m] <- low$CE[index]
    safe[j+1,2,m] <- med$CE[index]
    safe[j+1,3,m] <- high$CE[index]
    
    y[j+1,1,m] <- low$choice[index]
    y[j+1,2,m] <- med$choice[index]
    y[j+1,3,m] <- high$choice[index]
    
  }
  
}



arr <- ar[,,1:68]
saff <- saff[,,1:68]
yy <- y[,,1:68]
wwalth <- wealth[,,1:68]

#condition dependent model
dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "y" = yy)
params <- c("theta", "beta", "y", "deu", "eu_risk", "mu_b", "sigma_b")

samples <- jags(dat, parameters.to.save = params,
                model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Null_Model_Condition_Dependent.txt")

# r hat value between 1 to 1.1 conversion check
theta <- samples$BUGSoutput$sims.list$theta
beta <- samples$BUGSoutput$sims.list$beta

dev <- samples$BUGSoutput$sims.list$deviance

low<- hist(log(beta[,,1]), breaks = 20) 

med <- hist(log(beta[,,2]), breaks = 20)

high <- hist(log(beta[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(theta[,1,1,1])

hist(theta[,,,])



#prediction generations
Nulldat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff)

pred <- jags(Nulldat, parameters.to.save = params,
             model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Null_Model_Condition_Dependent.txt", DIC = FALSE)
thetaPred <- pred$BUGSoutput$sims.list$theta
betaPred <- pred$BUGSoutput$sims.list$beta

devPred <- pred$BUGSoutput$sims.list$deviance

low<- hist(log(betaPred[,,1]), breaks = 20) 

med <- hist(log(betaPred[,,2]), breaks = 20)

high <- hist(log(betaPred[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(thetaPred [,1,1,1])

hist(thetaPred[,,,])


out_risk <- pred$BUGSoutput$sims.list$eu_risk

out_theta <- pred$BUGSoutput$sims.list$theta

low_risk <- out_risk[,,1,]

med_risk <- out_risk[,,2,]

high_risk <- out_risk[,,3,]

low_theta <- out_theta[,,1,]

med_theta <- out_theta[,,2,]

high_theta <- out_theta[,,3,]

low_risk_unlist <- unlist(low_risk)

low_theta_unlist <- unlist(low_theta)

low_graph <- cbind(low_risk_unlist, low_theta_unlist)

low_graph <- as.data.frame(low_graph)

names(low_graph) <- c("ev", "pRisky")

low_sum <- low_graph%>%
  group_by(ev)

by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev * 2), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low

med_risk_unlist <- unlist(med_risk)

med_theta_unlist <- unlist(med_theta)

med_graph <- cbind(med_risk_unlist, med_theta_unlist)

med_graph <- as.data.frame(med_graph)

names(med_graph) <- c("ev", "pRisky")

med_sum <- med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev * 2), breaks = by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in medium magnitude over five levels")

g_med


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

has.neg <- apply(high_theta_unlist, 1, function(row) any(row < 0))

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(-0.1, 1.2) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev * 2), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky hoice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")

g_high
  

out_risk <- samples$BUGSoutput$sims.list$eu_risk

out_theta <- samples$BUGSoutput$sims.list$theta

low_risk <- out_risk[,,1,]

med_risk <- out_risk[,,2,]

high_risk <- out_risk[,,3,]

low_theta <- out_theta[,,1,]

med_theta <- out_theta[,,2,]

high_theta <- out_theta[,,3,]

low_risk_unlist <- unlist(low_risk)

low_theta_unlist <- unlist(low_theta)

low_graph <- cbind(low_risk_unlist, low_theta_unlist)

low_graph <- as.data.frame(low_graph)

names(low_graph) <- c("ev", "pRisky")

low_sum <- low_graph%>%
  group_by(ev)



by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev * 2), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low

med_risk_unlist <- unlist(med_risk)

med_theta_unlist <- unlist(med_theta)

med_graph <- cbind(med_risk_unlist, med_theta_unlist)

med_graph <- as.data.frame(med_graph)

names(med_graph) <- c("ev", "pRisky")

med_sum <- med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.05,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev * 2), breaks =by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in medium magnitude over five levels")

g_med


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev * 2), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky hoice probablity")+
  xlab("Expected value of loss(kr))")+
  ggtitle("Risky choice probablity in high magnitude over five levels")

g_high


#utility function model
saff <- saff*-1

arr <- arr*-1
dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "y" = y, "wealth" = wwalth)
params <- c("p_risk", "beta", "y", "sdeuiso", "deuiso", "ev_risk", "mu_b", "sigma_b", "tau_eta", "mu_eta", "eta", "uwealth", "edusafe", "edurisk")

utilitySam <- jags(dat, parameters.to.save = params,
                model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Utility_Model_Condition_Dependent.txt")

theta <- utilitySam$BUGSoutput$sims.list$p_risk
beta <- utilitySam$BUGSoutput$sims.list$beta


low<- hist(log(beta[,,1]), breaks = 20) 

med <- hist(log(beta[,,2]), breaks = 20)

high <- hist(log(beta[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(theta[,1,1,1])

hist(theta[,,,])

utility_risk <- utilitySam$BUGSoutput$sims.list$ev_risk

utility_theta <- utilitySam$BUGSoutput$sims.list$p_risk

utility_wsafe <- utilitySam$BUGSoutput$sims.list$wsafe
utility_wrisk <- utilitySam$BUGSoutput$sims.list$wrisk1
utility_zero <- utilitySam$BUGSoutput$sims.list$wrisk2

low_risk <- utility_risk[,,1,]

med_risk <- utility_risk[,,2,]

high_risk <- utility_risk[,,3,]

low_theta <- utility_theta [,,1,]

med_theta <- utility_theta [,,2,]

high_theta <- utility_theta [,,3,]

low_risk_unlist <- unlist(low_risk)

low_theta_unlist <- unlist(low_theta)

low_graph <- cbind(low_risk_unlist, low_theta_unlist)

low_graph <- as.data.frame(low_graph)

names(low_graph) <- c("ev", "pRisky")

low_sum <- low_graph%>%
  group_by(ev)

by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))



g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low

med_risk_unlist <- unlist(med_risk)

med_theta_unlist <- unlist(med_theta)

med_graph <- cbind(med_risk_unlist, med_theta_unlist)

med_graph <- as.data.frame(med_graph)

names(med_graph) <- c("ev", "pRisky")

med_sum <- med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))


g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev), breaks = by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in medium magnitude over five levels")

g_med


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")


g_high

#utility model prediction
dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "wealth" = wwalth)
params <- c("theta", "beta", "y", "mu_b", "sigma_b", "tau_eta", "mu_eta", "eta", "uwealth", "deuiso", "sdeuiso", "eu_risk", "wsafe","wrisk1", "wrisk2")

utilityPred <- jags(dat, parameters.to.save = params,
                   model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Utility_Model_Condition_Dependent.txt", DIC = FALSE)

theta <- utilitySam$BUGSoutput$sims.list$theta
beta <- utilitySam$BUGSoutput$sims.list$beta


low<- hist(log(beta[,,1]), breaks = 20) 

med <- hist(log(beta[,,2]), breaks = 20)

high <- hist(log(beta[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(theta[,1,1,1])

hist(theta[,,,])

utility_risk_pred <- utilityPred $BUGSoutput$sims.list$ev_risk

utility_theta_pred <- utilityPred $BUGSoutput$sims.list$p_risk

low_risk <- utility_risk_pred[,,1,]

med_risk <- utility_risk_pred[,,2,]

high_risk <- utility_risk_pred[,,3,]

low_theta <- utility_theta_pred[,,1,]

med_theta <- utility_theta_pred[,,2,]

high_theta <- utility_theta_pred[,,3,]

low_risk_unlist <- unlist(low_risk)

low_theta_unlist <- unlist(low_theta)

low_graph <- cbind(low_risk_unlist, low_theta_unlist)

low_graph <- as.data.frame(low_graph)

names(low_graph) <- c("ev", "pRisky")

low_sum <- low_graph%>%
  group_by(ev)

by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))



g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low

med_risk_unlist <- unlist(med_risk)

med_theta_unlist <- unlist(med_theta)

med_graph <- cbind(med_risk_unlist, med_theta_unlist)

med_graph <- as.data.frame(med_graph)

names(med_graph) <- c("ev", "pRisky")

med_sum <- med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))



g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev), breaks = by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in medium magnitude over five levels")

g_med


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))


g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(-0.1, 1.1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")

g_high


eta_util <- utilitySam$BUGSoutput$sims.list$eta

beta_util <- utilitySam$BUGSoutput$sims.list$beta

for (i in 1:41){
  
  onepar <- eta_util[,i,]
  parNo <- i
  title<- paste0("eta density graph for participant ", parNo)
  jpeg(file=paste0("/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/graph/etaPar", parNo))
  plot(density(onepar[,1]), xlim = c(-1, 5), ylim = c(-0, 2), main= title)                  # Plot density of x
  lines(density(onepar[,2]), col = "red")                      # Overlay density of y
  lines(density(onepar[,3]), col = "green")  
  legend("topright",                                  # Add legend to density
         legend = c("low", "medium", "high"),
         col = c("black", "red", "green"),
         lty = 1)
  dev.off()

}

for (i in 1:41){
  onepar <- beta_util[,i,]
  parNo <- i
  title<- paste0("beta density graph for participant ", parNo)
  jpeg(file=paste0("/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/beta/betaPar", parNo))
  plot(density(onepar[,1]), main= title)                  # Plot density of x
  lines(density(onepar[,2]), col = "red")                      # Overlay density of y
  lines(density(onepar[,3]), col = "green")  
  legend("topright",                                  # Add legend to density
         legend = c("low", "medium", "high"),
         col = c("black", "red", "green"),
         lty = 1)
  dev.off()
  
}
someData <- rep(NaN, 41)
low_beta <- array(someData, c(41))
med_beta <- array(someData, c(41))
high_beta <- array(someData, c(41))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:41){
  onepar <- beta_util[,i,]
  mode_low <- getmode(onepar[,1])
  mode_med <- getmode(onepar[,2])
  mode_high <- getmode(onepar[,3])
  
  low_beta[i] <- mode_low
  med_beta[i] <- mode_med
  high_beta[i] <- mode_high
  
  
}


plot(low_beta, type = 'l', main= "Distribution of beta per magnitude", xlab="Participants",ylab="Beta",
     ylim=c(0,100),xlim=c(1,41))                  # Plot density of x
lines(med_beta, col = "red")                      # Overlay density of y
lines(high_beta, col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)

someData <- rep(NaN, 41)
low_eta <- array(someData, c(41))
med_eta <- array(someData, c(41))
high_eta <- array(someData, c(41))



for (i in 1:41){
  onepar <- eta_util[,i,]
  mean_low <- mean(onepar[,1])
  mean_med <- mean(onepar[,2])
  mean_high <- mean(onepar[,3])
  
  low_eta[i] <- mean_low
  med_eta[i] <- mean_med
  high_eta[i] <- mean_high
  
  
}


plot(low_eta, type = 'l', main= "Distribution of eta per magnitude", xlab="Participants",ylab="Eta",
     ylim=c(-1,1.5),xlim=c(1,41))                  # Plot density of x
lines(med_eta, col = "red")                      # Overlay density of y
lines(high_eta, col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)

someData <- rep("2", 41)
low_labs <- array(someData, c(41))
med_labs <- array(someData, c(41))
high_labs <- array(someData, c(41))

low_ETA <- cbind(low_labs,low_eta)
low_ETA <- as.data.frame(low_ETA)
names(low_ETA) <- c("order", "eta")

med_ETA <- cbind(med_labs,med_eta)
med_ETA <- as.data.frame(med_ETA)
names(med_ETA) <- c("order", "eta")

high_ETA <- cbind(high_labs,high_eta)
high_ETA <- as.data.frame(high_ETA)
names(high_ETA) <- c("order", "eta")

eta_sum <- bind_rows(low_ETA, med_ETA, high_ETA)
low_beta <- log10(as.numeric(low_beta))

low_Beta <- cbind(low_labs,low_beta)
low_Beta <- as.data.frame(low_Beta)
names(low_Beta) <- c("order", "beta")


med_Beta <- cbind(med_labs,med_beta)
med_Beta <- as.data.frame(med_Beta)
names(med_Beta) <- c("order", "beta")


high_Beta <- cbind(high_labs,high_beta)
high_Beta <- as.data.frame(high_Beta)
names(high_Beta) <- c("order", "beta")
as.double.factor <- function(x) {as.numeric(levels(x))[x]}
beta_sum <- bind_rows(low_Beta, med_Beta, high_Beta)



beta_sum <- beta_sum%>%
  mutate(Logbeta = log(beta))


ggplot(beta_sum, aes(x = factor(order), y = Logbeta, fill = factor(order))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Log value of Beta")+
  ggtitle("Beta distribution over three magnitude")

eta_sum <- eta_sum%>%
  mutate(eta = as.double.factor(eta))

ggplot(eta_sum, aes(x = factor(order), y = eta, fill = factor(order))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) + 
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Value of Eta")+
  ggtitle("Eta distribution over three magnitude")

#Ratio utility function model

dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "y" = y, "wealth" = wwalth)
params <- c("p_risk", "beta", "y", "mu_b", "sigma_b", "tau_eta", "mu_eta", "eta", "deuiso", "sdeuiso", "ev_risk", "ratio")

ratioSam <- jags(dat, parameters.to.save = params,
                   model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Ratio_Utility_Model_Condition_Dependent.txt")

theta <- utilitySam$BUGSoutput$sims.list$theta
beta <- utilitySam$BUGSoutput$sims.list$beta


low<- hist(log(beta[,,1]), breaks = 20) 

med <- hist(log(beta[,,2]), breaks = 20)

high <- hist(log(beta[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(theta[,1,1,1])

hist(theta[,,,])

ratio_risk <- ratioSam$BUGSoutput$sims.list$ev_risk

ratio_theta <- ratioSam$BUGSoutput$sims.list$p_risk

low_risk <- ratio_risk[,,1,]

med_risk <- ratio_risk[,,2,]

high_risk <- ratio_risk[,,3,]

low_theta <- ratio_theta [,,1,]

med_theta <- ratio_theta [,,2,]

high_theta <- ratio_theta [,,3,]

risk_ratio <- c(low_risk,med_risk, high_risk)
risk_ratio_unlist <- unlist(risk_ratio)

theta_ratio <- c(low_theta,med_theta, high_theta)
theta_ratio_unlist <- unlist(theta_ratio)

ratio_graph <- cbind(risk_ratio_unlist, theta_ratio_unlist)

names(ratio_graph) <- c("ev", "pRisky")

low_sum <- ratio_graph%>%
  group_by(ev)

by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))


low_risk_unlist <- unlist(low_risk)

low_theta_unlist <- unlist(low_theta)

low_graph <- cbind(low_risk_unlist, low_theta_unlist)

low_graph <- as.data.frame(low_graph)

names(low_graph) <- c("ev", "pRisky")

low_sum <- low_graph%>%
  group_by(ev)

by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))


g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1.05) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity over five levels")


g_low

med_risk_unlist <- unlist(med_risk)

med_theta_unlist <- unlist(med_theta)

med_graph <- cbind(med_risk_unlist, med_theta_unlist)

med_graph <- as.data.frame(med_graph)

names(med_graph) <- c("ev", "pRisky")

med_sum <- med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))



g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev), breaks = by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in over five levels")

g_med


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))


g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")


g_high

#ratio model prediction
dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "wealth" = wwalth)
params <- c("p_risk", "beta", "y", "mu_b", "sigma_b", "tau_eta", "mu_eta", "eta", "deuiso", "sdeuiso", "ev_risk", "ratio")


params <- c( "beta", "mu_b", "sigma_b", "tau_eta", "mu_eta", "eta","edusafe", "edurisk", "dusafe", "durisk1", "durisk2", "eu_risk", "eu_safe")

ratioPred <- jags(dat, parameters.to.save = params,
                    model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Ratio_Utility_Model_Condition_Dependent.txt", DIC = FALSE)

theta <- utilitySam$BUGSoutput$sims.list$theta
beta <- utilitySam$BUGSoutput$sims.list$beta

ratio <- ratioPred$BUGSoutput$sims.list$ratio
edusafe <- ratioPred$BUGSoutput$sims.list$edusafe
edurisk <- ratioPred$BUGSoutput$sims.list$edurisk

low<- hist(log(beta[,,1]), breaks = 20) 

med <- hist(log(beta[,,2]), breaks = 20)

high <- hist(log(beta[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(theta[,1,1,1])

hist(theta[,,,])

ratio_risk <- ratioPred$BUGSoutput$sims.list$ev_risk

ratio_theta <- ratioPred$BUGSoutput$sims.list$p_risk

low_risk <- ratio_risk[,,1,]

med_risk <- ratio_risk[,,2,]

high_risk <- ratio_risk[,,3,]

low_theta <- ratio_theta[,,1,]

med_theta <- ratio_theta[,,2,]

high_theta <- ratio_theta[,,3,]

low_risk_unlist <- unlist(low_risk)

low_theta_unlist <- unlist(low_theta)

low_graph <- cbind(low_risk_unlist, low_theta_unlist)

low_graph <- as.data.frame(low_graph)

names(low_graph) <- c("ev", "pRisky")

low_sum <- low_graph%>%
  group_by(ev)

by_ev <- low_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))


g_low <- ggplot(by_ev, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev$ev), breaks = by_ev$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in low magnitude over five levels")


g_low

med_risk_unlist <- unlist(med_risk)

med_theta_unlist <- unlist(med_theta)

med_graph <- cbind(med_risk_unlist, med_theta_unlist)

med_graph <- as.data.frame(med_graph)

names(med_graph) <- c("ev", "pRisky")

med_sum <- med_graph%>%
  group_by(ev)

by_ev_med <- med_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_med<- ggplot(by_ev_med , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_med$ev), breaks = by_ev_med$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in medium magnitude over five levels")

g_med


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(-0.1, 1.1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=1,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")

g_high

ratio<- ratioPred$BUGSoutput$sims.list$ratio

eta_ratio <- ratioSam$BUGSoutput$sims.list$eta

beta_ratio <- ratioSam$BUGSoutput$sims.list$beta

for (i in 1:41){
  
  onepar <- eta_util[,i,]
  parNo <- i
  title<- paste0("eta density graph for participant ", parNo)
  jpeg(file=paste0("/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/graph/etaPar", parNo))
  plot(density(onepar[,1]), xlim = c(-1, 5), ylim = c(-0, 2), main= title)                  # Plot density of x
  lines(density(onepar[,2]), col = "red")                      # Overlay density of y
  lines(density(onepar[,3]), col = "green")  
  legend("topright",                                  # Add legend to density
         legend = c("low", "medium", "high"),
         col = c("black", "red", "green"),
         lty = 1)
  dev.off()
  
}

for (i in 1:41){
  onepar <- beta_util[,i,]
  parNo <- i
  title<- paste0("beta density graph for participant ", parNo)
  jpeg(file=paste0("/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/beta/betaPar", parNo))
  plot(density(onepar[,1]), main= title)                  # Plot density of x
  lines(density(onepar[,2]), col = "red")                      # Overlay density of y
  lines(density(onepar[,3]), col = "green")  
  legend("topright",                                  # Add legend to density
         legend = c("low", "medium", "high"),
         col = c("black", "red", "green"),
         lty = 1)
  dev.off()
  
}
someData <- rep(NaN, 41)
low_beta <- array(someData, c(41))
med_beta <- array(someData, c(41))
high_beta <- array(someData, c(41))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:41){
  onepar <- beta_ratio[,i,]
  mode_low <- getmode(onepar[,1])
  mode_med <- getmode(onepar[,2])
  mode_high <- getmode(onepar[,3])
  
  low_beta[i] <- mode_low
  med_beta[i] <- mode_med
  high_beta[i] <- mode_high
  
  
}


plot(low_beta, type = 'l', main= "Distribution of beta per magnitude", xlab="Participants",ylab="Beta",
     ylim=c(0,3000),xlim=c(1,41))                  # Plot density of x
lines(med_beta, col = "red")                      # Overlay density of y
lines(high_beta, col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)

someData <- rep(NaN, 41)
low_eta <- array(someData, c(41))
med_eta <- array(someData, c(41))
high_eta <- array(someData, c(41))



for (i in 1:41){
  onepar <- eta_ratio[,i,]
  mean_low <- mean(onepar[,1])
  mean_med <- mean(onepar[,2])
  mean_high <- mean(onepar[,3])
  
  low_eta[i] <- mean_low
  med_eta[i] <- mean_med
  high_eta[i] <- mean_high
  
  
}


plot(low_eta, type = 'l', main= "Distribution of eta per magnitude", xlab="Participants",ylab="Eta",
     ylim=c(-0.2,2),xlim=c(1,41))                  # Plot density of x
lines(med_eta, col = "red")                      # Overlay density of y
lines(high_eta, col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)


low_ETA <- cbind(low_labs,low_eta)
low_ETA <- as.data.frame(low_ETA)
names(low_ETA) <- c("order", "eta")

med_ETA <- cbind(med_labs,med_eta)
med_ETA <- as.data.frame(med_ETA)
names(med_ETA) <- c("order", "eta")

high_ETA <- cbind(high_labs,high_eta)
high_ETA <- as.data.frame(high_ETA)
names(high_ETA) <- c("order", "eta")

eta_sum <- bind_rows(low_ETA, med_ETA, high_ETA)
low_beta <- log10(as.numeric(low_beta))

low_Beta <- cbind(low_labs,low_beta)
low_Beta <- as.data.frame(low_Beta)
names(low_Beta) <- c("order", "beta")


med_Beta <- cbind(med_labs,med_beta)
med_Beta <- as.data.frame(med_Beta)
names(med_Beta) <- c("order", "beta")


high_Beta <- cbind(high_labs,high_beta)
high_Beta <- as.data.frame(high_Beta)
names(high_Beta) <- c("order", "beta")
as.double.factor <- function(x) {as.numeric(levels(x))[x]}
beta_sum <- bind_rows(low_Beta, med_Beta, high_Beta)



beta_sum <- beta_sum%>%
  mutate(beta = as.double.factor(beta))

beta_sum <- beta_sum%>%
  mutate(betaLog = log(beta))

ggplot(beta_sum, aes(x = factor(order), y = betaLog, fill = factor(order))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Log value of Beta")+
  ggtitle("Beta distribution over three magnitude")

eta_sum <- eta_sum%>%
  mutate(eta = as.double.factor(eta))

ggplot(eta_sum, aes(x = factor(order), y = eta, fill = factor(order))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) + 
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Value of Eta")+
  ggtitle("Eta distribution over three magnitude")


#Ratio condition dependent model
dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "y" = yy)
params <- c("theta", "beta", "y", "deu", "eu_risk", "mu_b", "sigma_b")

ratioFit <- jags(dat, parameters.to.save = params,
                model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Null_Model_Ratio_Condition_Dependent.txt")

# r hat value between 1 to 1.1 conversion check
theta <- samples$BUGSoutput$sims.list$theta
beta <- samples$BUGSoutput$sims.list$beta

dev <- samples$BUGSoutput$sims.list$deviance

low<- hist(log(beta[,,1]), breaks = 20) 

med <- hist(log(beta[,,2]), breaks = 20)

high <- hist(log(beta[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(theta[,1,1,1])

hist(theta[,,,])



#prediction generations
Nulldat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff)

ratioPred <- jags(Nulldat, parameters.to.save = params,
             model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/LosingMoney_JAGS_Null_Model_Ratio_Condition_Dependent.txt", DIC = FALSE)
thetaPred <- pred$BUGSoutput$sims.list$theta
betaPred <- pred$BUGSoutput$sims.list$beta

devPred <- pred$BUGSoutput$sims.list$deviance

low<- hist(log(betaPred[,,1]), breaks = 20) 

med <- hist(log(betaPred[,,2]), breaks = 20)

high <- hist(log(betaPred[,,3]) , breaks = 20)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(34,139,34, max = 255, alpha = 100, names = "It.green")

plot(low, col = c1)

plot(med, col= c2, add = TRUE)

plot(high, col = c3, add = TRUE)

hist(thetaPred [,1,1,1])

hist(thetaPred[,,,])


out_risk <- ratioFit$BUGSoutput$sims.list$eu_risk

out_theta <- ratioFit$BUGSoutput$sims.list$theta

low_risk <- out_risk[,,1,]

med_risk <- out_risk[,,2,]

high_risk <- out_risk[,,3,]

low_theta <- out_theta[,,1,]

med_theta <- out_theta[,,2,]

high_theta <- out_theta[,,3,]


high_risk_unlist <- unlist(high_risk)

high_theta_unlist <- unlist(high_theta)

has.neg <- apply(high_theta_unlist, 1, function(row) any(row < 0))

high_graph <- cbind(high_risk_unlist, high_theta_unlist)

high_graph <- as.data.frame(high_graph)

names(high_graph) <- c("ev", "pRisky")

high_sum <- high_graph%>%
  group_by(ev)

by_ev_high <- high_sum%>%summarise(mean = mean(pRisky), sd = sd(pRisky))

by_ev_high <- by_ev_high %>%
  mutate(evOne = ev/-55)

by_ev_high <- by_ev_high%>%
  mutate(evOne = round(evOne, digits=2))

g_high<- ggplot(by_ev_high, aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Ratio of risky to safe loss choice", labels = as.character(by_ev_high$evOne * -1), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity over five levels")

g_high


beta_ratio <- ratioFit$BUGSoutput$sims.list$beta
someData <- rep(NaN, 41)
low_beta <- array(someData, c(41))
med_beta <- array(someData, c(41))
high_beta <- array(someData, c(41))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:41){
  onepar <- beta_ratio[,i,]
  mode_low <- getmode(onepar[,1])
  mode_med <- getmode(onepar[,2])
  mode_high <- getmode(onepar[,3])
  
  low_beta[i] <- mode_low
  med_beta[i] <- mode_med
  high_beta[i] <- mode_high
  
  
}


plot(low_beta, type = 'l', main= "Distribution of beta per magnitude", xlab="Participants",ylab="Beta",
     ylim=c(0,3000),xlim=c(1,41))                  # Plot density of x
lines(med_beta, col = "red")                      # Overlay density of y
lines(high_beta, col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)


low_beta <- log10(as.numeric(low_beta))

low_Beta <- cbind(low_labs,low_beta)
low_Beta <- as.data.frame(low_Beta)
names(low_Beta) <- c("order", "beta")


med_Beta <- cbind(med_labs,med_beta)
med_Beta <- as.data.frame(med_Beta)
names(med_Beta) <- c("order", "beta")


high_Beta <- cbind(high_labs,high_beta)
high_Beta <- as.data.frame(high_Beta)
names(high_Beta) <- c("order", "beta")
as.double.factor <- function(x) {as.numeric(levels(x))[x]}
beta_sum <- bind_rows(low_Beta, med_Beta, high_Beta)



beta_sum <- beta_sum%>%
  mutate(beta = as.double.factor(beta))

beta_sum <- beta_sum%>%
  mutate(betaLog = log(beta))

ggplot(beta_sum, aes(x = factor(order), y = betaLog, fill = factor(order))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Log value of Beta")+
  ggtitle("Beta distribution over three magnitude")


#mixture model
dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "wealth" = wwalth, 'y' = yy)
params <- c("p_risk", "beta", "y", "mu_b", "sigma_b", "tau_eta", "mu_eta", "eta", "deuiso", "sdeuiso", "ev_risk", "z")

mixSam <- jags(dat, parameters.to.save = params,
                  model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/archives_model/LosingMoney_JAGS_Compare_Model_Condition_Dependent.txt")
mix_risk <- mixPred$BUGSoutput$sims.list$ev_risk

z <- mixSam$BUGSoutput$sims.list$z

denz <- density(z)
freqz <- table(z)
barplot(freqz, main= "z posterior distribution", names.arg=c("0eta_1ratio", "1eta_0ratio", "0eta_1ratio","1eta_0ratio"))

dims <- dim(arr)
arr2 <- arr
low <- arr2[,1,]
med <- arr2[,2,]
high <- arr2[,3,]
arr2 <- cbind(low,med,high)
low <- saff[,1,]
med <- saff[,2,]
high <- saff[,3,]
saff2 <- cbind(low,med,high)
low <- wwalth[,1,]
med <- wwalth[,2,]
high <- wwalth[,3,]
wwalth2 <- cbind(low,med,high)
low <- yy[,1,]
med <- yy[,2,]
high <- yy[,3,]
yy2 <- cbind(low,med,high)

dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr2, "safe" = saff2, "wealth" = wwalth2, 'y' = yy)
params <- c("p_risk", "beta", "y", "eta2", "ev_risk", "z", "mu_eta2", "tau_eta2", "sigma_eta2", "oneminEta2", "eta")

oconSam <- jags(dat, parameters.to.save = params,
               model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/1eta_0ratio_3beta.txt")


dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "wealth" = wwalth,"risk2" = arr2, "safe2" = saff2, "wealth2" = wwalth2, 'y' = yy)

allSam2 <- jags(dat, parameters.to.save = params,
                model.file ="/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/supermodel.txt", n.iter = 1500, n.thin = 1)
z <-allSam$BUGSoutput$sims.list$z

denz <- density(z)
freqz <- table(z)
barplot(freqz, main= "z posterior distribution")

z <- c(1,2,3,4,5,6,7,8,9,10,11,12)


fre <- c(0,0,0,0,0,0,0,0,0,0,0,0)

df <- as.data.frame(cbind(z, fre))

names(df) <- c("z", "freq")

someData <- rep(NaN, 6*41)
zAll <- array(someData, c(6,41))


for (i in 1:41){
  
  onepar <- as.data.frame(table(z[,i]))
  
  names(onepar) <- c("z", "freq")
  
  onepar$freq <- as.numeric(onepar$freq)
  
  df1 <- rbind(df, onepar)
  
  df1 <- aggregate(.~z,data=df1,FUN=sum)
  
  df1$freq <- df1$freq / 2250
  
  df1$z <- as.numeric(df1$z)
  
  df2 <- df1[order(df1$z),]
  
  m <- c(1,2,3,4,5,6)
  prob <-c(df2$freq[1] + df2$freq[3], df2$freq[2] + df2$freq[4], df2$freq[5] + df2$freq[7], df2$freq[6] + df2$freq[8], df2$freq[9] + df2$freq[11], df2$freq[10] + df2$freq[12])
  
  df3 <- as.data.frame(cbind(m, prob))
  
  names(df3) <- c("z", "freq")
  
  df3$freq <- log(df3$freq + 1)
  
  zAll[, i] <- df3$freq
  
  
}

zAll <- as.data.frame(zAll)
library("readr")
write_tsv(zAll, path = "/Users/anniehu/Desktop/Phenotype_losers/Bayesian/Code/prototypes/z6.txt")



freqz <- as.data.frame(freqz)

models <- c("0e1r", "3e0r", "0e0r", "3e1r", "1e1r", "1e0r")
nums <- c(15441, 11614, 11965, 18344, 34869, 17)

models <- c("0", "1", "3")
nums <- c(27406, 34886, 29958)

nums <- as.numeric(nums)

post <- cbind(models,nums)

post <- as.data.frame(post)


sum <- sum(as.numeric(post$nums))

names(post) <- c("models", "sample")

as.double.factor <- function(x) {as.numeric(levels(x))[x]}

post <- post%>%
     mutate(sample = as.double.factor(sample))

post <- post%>%
  mutate(probability = sample /  92250)

ggplot(post, aes(x=models, y=probability)) +
  geom_bar(stat="identity", fill="grey") + ylim(0,1)+theme_minimal()

models <- c("ratio", "non-ratio")
nums <- c(68654, 23596)


post <- cbind(models,nums)

post <- as.data.frame(post)



names(post) <- c("model", "sample")

as.double.factor <- function(x) {as.numeric(levels(x))[x]}

post <- post%>%
  mutate(sample = as.double.factor(sample))

post <- post%>%
  mutate(Ratio = sample / 1438)

ggplot(post, aes(x=model, y=Ratio, color=model)) +
  geom_bar(stat="identity", fill="white")


models <- c("ratio", "non-ratio")
sums <- c(4719, 1438)


post <- cbind(models,sums)

post <- as.data.frame(post)



names(post) <- c("model", "sample")

as.double.factor <- function(x) {as.numeric(levels(x))[x]}

post <- post%>%
  mutate(sample = as.double.factor(sample))

post <- post%>%
  mutate(prob = sample / 6157)

ggplot(post, aes(x=model, y=prob, color=model)) +
  geom_bar(stat="identity", fill="white") + ylim(c(0, 1))

postall <- post

models <- c("1", "2", "3", "4", "5", "6")
sums <- c(752, 1107, 0, 2320, 686, 1292)


post <- cbind(models,sums)

post <- as.data.frame(post)



names(post) <- c("model", "sample")

as.double.factor <- function(x) {as.numeric(levels(x))[x]}

post <- post%>%
  mutate(sample = as.double.factor(sample))

post <- post%>%
  mutate(probability = sample / 6157)

ggplot(post, aes(x=`utility parameter`, y=probability, color=`utility parameter`)) +
  geom_bar(stat="identity", fill="white") + ylim(0,1) + ylab("Marginal Model Probability")+ xlab("`number of utility parameter`")+
theme_minimal()


beta_all <- allSam$BUGSoutput$sims.list$beta
someData <- rep(NaN, 41)
low_beta <- array(someData, c(41))
med_beta <- array(someData, c(41))
high_beta <- array(someData, c(41))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:41){
  onepar <- beta_all[,i,]
  mode_low <- getmode(onepar[,1])
  mode_med <- getmode(onepar[,2])
  mode_high <- getmode(onepar[,3])
  
  low_beta[i] <- mode_low
  med_beta[i] <- mode_med
  high_beta[i] <- mode_high
  
  
}


plot(density(low_beta), type = 'l', main= "Distribution of beta per magnitude", xlab = "beta", ylab = "density", ylim = c(0.0, 0.055))                  # Plot density of x
lines(density(med_beta), col = "red")                      # Overlay density of y
lines(density(high_beta), col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)


low_Beta <- cbind(low_labs,low_beta)
low_Beta <- as.data.frame(low_Beta)
names(low_Beta) <- c("order", "beta")


med_Beta <- cbind(med_labs,med_beta)
med_Beta <- as.data.frame(med_Beta)
names(med_Beta) <- c("order", "beta")


high_Beta <- cbind(high_labs,high_beta)
high_Beta <- as.data.frame(high_Beta)
names(high_Beta) <- c("order", "beta")
as.double.factor <- function(x) {as.numeric(levels(x))[x]}
beta_sum <- bind_rows(low_Beta, med_Beta, high_Beta)



beta_sum <- beta_sum%>%
  mutate(beta = as.double.factor(beta))

beta_sum <- beta_sum%>%
  mutate(betaLog = log(beta))

ggplot(beta_sum, aes(x = order, y = betaLog, fill = order)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Log value of Beta")+
  ggtitle("Beta distribution over three magnitude") + theme_minimal()



z <- allSam$BUGSoutput$sims.list$z
eta <- allSam$BUGSoutput$sims.list$eta2
tau <- allSam$BUGSoutput$sims.list$tau_eta2
mu <- allSam$BUGSoutput$sims.list$mu_eta2
sigma <- allSam$BUGSoutput$sims.list$sigma_eta2
beta <- allSam$BUGSoutput$sims.list$beta

etaModel4 = array(NA, dim=c(34254,41))
m <- 1
for (k in 1:2250) {
  for (l in 1:41) {
    if(z[k,l] == 10 | z[k,l] == 12){
      etaModel4[m,l] <- eta[k,l]
      print(etaModel4[m,l] )
      m <- m+1
    }
  }
}

averageETA <- list()
m <- 1
for (l in 1 : 41){
  
  Model4ETA = na.omit(etaModel4[,l])
  meanPar = mean(Model4ETA)
  averageETA[m] <- meanPar
  m <- m+1
  
}



eta0 <- list()
m <- 1
for (k in 1:2250) {
  for (l in 1:41) {
    if(z[k,l] == 1 | z[k,l] == 3){
      eta0[m] <- eta[k,l]
      m <- m+1
    }
  }
}

eta003 <- list()
m <- 1
for (k in 1:2250) {
  for (l in 1:41) {
    if(zAgain[k,l] == 6 | zAgain[k,l] == 8){
      eta003[m] <- eta3params[k,l,3]
      m <- m+1
    }
  }
}

eta6 <- as.data.frame(eta6)
eta6 <- as.numeric(eta6)
eta0 <- as.data.frame(eta0)
eta0 <- as.numeric(eta0)
eta003 <- as.data.frame(eta003)
eta003 <- as.numeric(eta003)

averageETA = na.omit(averageETA)
averageETA <- as.data.frame(averageETA)
averageETA <- as.numeric(averageETA)



plot(density(averageETA), type = 'l', xlab = "eta", main= ' ', ylab = "Density", xlim = c(-1.0, 1.0), col = "blue")                  # Plot density of x

zAgain <- allSam2$BUGSoutput$sims.list$z

eta3params <- allSam2$BUGSoutput$sims.list$eta

plot(density(eta001), type = 'l', main= " ", xlab="eta",ylab="Density",
     xlim=c(-1.0,1.0), ylim = c(0,1.8), col = "red")                  # Plot density of x
lines(density(eta002), col = "blue")                      # Overlay density of y
lines(density(eta003), col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("red", "blue", "green"),
       lty = 1)

plot(density(eta003))


tau6 <- list()
m <- 1
for (k in 1:2250) {
  for (l in 1:41) {
    if(z[k,l] == 10 | z[k,l] == 12){
      tau6[m] <- tau[k]
      m <- m+1
    }
  }
}
plot(density(as.numeric(tau6)), type = 'l', main= "Distribution of tau in model 6", xlab = "tau_eta", ylab = "density", col = "green")                  # Plot density of x

mu6 <- list()
m <- 1
for (k in 1:2250) {
  for (l in 1:41) {
    if(z[k,l] == 10 | z[k,l] == 12){
      mu6[m] <- mu[k]
      m <- m+1
    }
  }
}
plot(density(as.numeric(mu6)), type = 'l', main= "Distribution of mu in model 6", xlab = "mu_eta", ylab = "density", col = "green")                  # Plot density of x

sigma6 <- list()
m <- 1
for (k in 1:2250) {
  for (l in 1:41) {
    if(z[k,l] == 10 | z[k,l] == 12){
      sigma6[m] <- sigma[k]
      m <- m+1
    }
  }
}
plot(density(as.numeric(sigma6)), type = 'l', main= "Distribution of sigma in model 6", xlab = "sigma_eta", ylab = "density", col = "green")                  # Plot density of x





someData <- rep("eta", 34254)
eta_labs <- array(someData, c(34254))
etamodel <- cbind(eta_labs,eta6)
etamodel <- as.data.frame(etamodel)
names(etamodel) <- c("order", "eta")
etamodel <- etamodel%>%
  mutate(eta = as.numeric(eta))


ggplot(etamodel, aes(x = factor(order), y = eta, fill = order)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) + 
  coord_cartesian(xlim = c(0.5, NA), clip = "off")+ xlab("Eta")+
  ylab("Value of Eta")+
  ggtitle("Eta distribution in model 6")

someData <- rep(NaN, 34254)
low_beta <- array(someData, c(34254))
med_beta <- array(someData, c(34254))
high_beta <- array(someData, c(34254))

m <- 1
s <- beta[,,1]
e <- beta[,,2]
b <- beta[,,3]
for (k in 1:2250) {
  for (l in 1:41) {
    if(z[k,l] == 10 | z[k,l] == 12){
      low_beta[m] <- s[k,l]
      med_beta[m] <- e[k,l]
      high_beta[m] <- b[k,l]
      m <- m+1
    }
  }
}

plot(density(low_beta), type = 'l', main= "Distribution of beta per magnitude", xlab = "beta", ylab = "density", ylim = c(0.0, 0.4))                  # Plot density of x
lines(density(med_beta), col = "red")                      # Overlay density of y
lines(density(high_beta), col = "green")  
legend("topright",                                  # Add legend to density
       legend = c("low", "medium", "high"),
       col = c("black", "red", "green"),
       lty = 1)

someData <- rep("2", 34869)
low_labs <- array(someData, c(34869))
med_labs <- array(someData, c(34869))
high_labs <- array(someData, c(34869))


low_Beta <- cbind(low_labs,low_beta)
low_Beta <- as.data.frame(low_Beta)
names(low_Beta) <- c("order", "beta")


med_Beta <- cbind(med_labs,med_beta)
med_Beta <- as.data.frame(med_Beta)
names(med_Beta) <- c("order", "beta")


high_Beta <- cbind(high_labs,high_beta)
high_Beta <- as.data.frame(high_Beta)
names(high_Beta) <- c("order", "beta")
as.double.factor <- function(x) {as.numeric(levels(x))[x]}
beta_sum <- bind_rows(low_Beta, med_Beta, high_Beta)



beta_sum <- beta_sum%>%
  mutate(beta = as.double.factor(beta))

beta_sum <- beta_sum%>%
  mutate(betaLog = log(beta))

ggplot(beta_sum, aes(x = factor(order), y = betaLog, fill = order)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA)+ 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+ xlab("Order of Magnitude")+
  ylab("Log value of Beta")+
  ggtitle("Beta distribution over three magnitude")
