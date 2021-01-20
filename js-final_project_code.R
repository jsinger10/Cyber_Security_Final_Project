library(ggplot2)
library(dplyr)
#Julian Singer and Don Osipov
###########################################################################
#setting up the vectors that will operate the data set
responders <- c(1:1000)
result_of_flip <- vector(mode="integer", length=1000)
response <- vector(mode="character", length=1000)
result_coin <- vector(mode="character", length=1000)
###########################################################################
#initialize the randomization and the responses
for (i in 1:1000){
  result_of_flip[i] = 0
  result_of_flip[i] = result_of_flip[i] + rbinom(1,1,.5) + rbinom(1,1,.5)
  if (result_of_flip[i]==0){
    result_coin[i]="HH"
  } else if (result_of_flip[i]==2){
    result_coin[i]="TT"
  } else {
    result_coin[i]="HT/TH"
  }
}
for (i in 1:1000){
  if (result_of_flip[i]==0){
    response[i] = "yes"} else if (result_of_flip[i]==2)
    {
      response[i] = "no"
    } else {
      if (rbinom(1,1,.80) == 0){
        response[i] = "yes"
      } else {
        response[i] = "no"
      }
    }
}
##############################################################################
#initialize the different data sets necessary for analysis
data <- data.frame(responders, result_coin, response, result_of_flip)
print(data)
filtered_yes = data %>% filter(response=="yes")
filtered_no = data %>% filter(response=="no")

data_filtered_1 <- filtered_yes[sample(1:nrow(filtered_yes), .75*nrow(filtered_yes)),]
data_filtered_2 <- filtered_no[sample(1:nrow(filtered_no), .75*nrow(filtered_no)),]

data_filtered_final <- rbind(data_filtered_1, data_filtered_2)

data_filtered_final <- data_filtered_final %>% arrange(responders)
##############################################################################
#The data plots are below
ggplot(data, aes(fill=response, y=1000, x=result_coin)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Percentage") +
  xlab("Coin Toss Results") +
  ggtitle("Percentage of Each type of Answer Unflitered")

ggplot(data, aes(fill=response, x=result_coin)) + 
  geom_bar(position="stack", stat="count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  ylab("Count") +
  xlab("Coin Toss Results") +
  ggtitle("Count of Each Response Unfiltered")

ggplot(data_filtered_final, aes(fill=response, y=nrow(data_filtered_final), x=result_coin)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=c("#7851A9", "#50C878")) +
  ylab("Percentage") +
  xlab("Coin Toss Results") +
  ggtitle("Percentage of Each Type of Answer Filtered")

ggplot(data_filtered_final, aes(fill=response, x=result_coin)) + 
  geom_bar(position="stack", stat="count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  scale_fill_manual(values=c("#7851A9", "#50C878")) +
  ylab("Count") +
  xlab("Coin Toss Results") +
  ggtitle("Count of Each Response Filtered")
  
  
