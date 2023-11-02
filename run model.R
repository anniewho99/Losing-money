
library(R2jags)
library(rjags)
library(coda)
library(ggpubr)
library(runjags)
library(CalvinBayes)
library(sm)
library(ggdist)


#read in sumdata.txt as big_data

#descriptive data analysis
se <- function(x) sqrt(var(x) / length(x))
as.double.factor <- function(x) {as.numeric(levels(x))[x]}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#normalizing data
all_normalised <- #read in allnormaliseddata.txt as all_normalised


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


g_high<- ggplot(by_ev_high , aes(x=ev, y=mean)) + 
  ylim(0, 1) +
  geom_point(colour = "blue")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5,colour = "blue",
                position=position_dodge(0.01)) +
  scale_x_continuous("Expected value of loss(kr)", labels = as.character(by_ev_high$ev), breaks = by_ev_high$ev)+
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted")+
  ylab("Risky choice probablity")+
  xlab("Expected value of loss(kr)")+
  ggtitle("Risky choice probablity in high magnitude over five levels")

g_high


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

#data formatting here is the 

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
saff <- safe[,,1:68]
yy <- y[,,1:68]
wwalth <- wealth[,,1:68]


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


#latenet-mixture model (supermodel.txt)
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


params <- c("p_risk", "beta", "y", "eta2", "ev_risk", "z", "mu_eta2", "tau_eta2", "sigma_eta2", "oneminEta2", "eta")

dat <- list("nSubjects" = 41, "ntrials" = 68, "conditions" = 3,"risk" = arr, "safe" = saff, "wealth" = wwalth,"risk2" = arr2, "safe2" = saff2, "wealth2" = wwalth2, 'y' = yy)

allSam <- jags(dat, parameters.to.save = params,
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

zAll <- as.data.frame(zAll) #this is z6.txt


beta_all <- allSam$BUGSoutput$sims.list$beta
someData <- rep(NaN, 41)
low_beta <- array(someData, c(41))
med_beta <- array(someData, c(41))
high_beta <- array(someData, c(41))


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

zAgain <- allSam$BUGSoutput$sims.list$z

eta3params <- allSam$BUGSoutput$sims.list$eta

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
