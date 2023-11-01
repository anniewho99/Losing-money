library(readr)
library(dplyr)
library(ggplot2)

#here is the script to loop through all raw data and generate a datamframe for model prediction

datalist = list()
folders <- dir(path="path to raw data")
j <- 0
for (i in folders){
  parID <- substr(i, 1, 3)
  data_path <-paste0(folders, i)
  df <- read.table(data_path, fill = TRUE, header = TRUE, row.names = NULL)
  df <- df %>%
    select(RISK, CE, Endowment, Loss, TotalLoss, totalTrialCount, trisk, tchoice)
  df$ParID <- parID
  df <- df%>%
    mutate(rt = tchoice - trisk)
  
  df <- df %>%
    mutate(choice = if_else(Loss == CE, 0,1))
  
  j <- j + 1
  
  datalist[[j]] <- df
}

big_data = do.call(rbind,datalist)

big_data <- big_data %>%
  select(RISK, CE, Endowment, Loss, TotalLoss, totalTrialCount, rt, ParID, choice)

sumAtt <- big_data%>%
  group_by(Magnitude)%>%
  summarise(riskAttitude = mean(choice)) #error bar for this one

rtdata <- big_data%>%
  mutate(risky = if_else(choice == 1, "risky","safe"))%>%
  group_by(Magnitude, risky)%>%
  summarise(RT = mean(rt, na.rm = TRUE))

big_data <- big_data%>%
  mutate(Magnitude = case_when(
  CE == 5.50 ~ "medium",
  CE < 5.50 ~ "low",
  CE > 5.50 ~ "high"
))
t_test <- t.test(RT ~ risky, data=rtdata)
t_test

write.table(big_data, file = "sumdata.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
select(big_data, -1)


#descriptive data plotting

ggplot(data=rtdata, aes(x=Magnitude, y=RT, fill=risky)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=risky), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

ggplot(sumAtt, aes(x=Magnitude, y=riskAttitude, fill=Magnitude)) +
  geom_bar(stat="identity")+scale_fill_brewer(palette="Dark2")+theme_minimal()



