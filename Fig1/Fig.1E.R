library(dplyr)
library(ggplot2)
library(reshape2)
data <- read.table('29486genome.length',header = T)
len_grad <- seq(10000,400000,5000)
data_vlp <- data[data$method == "VLPs",]
data_bulk <- data[data$method == "Bulk",]

len_num <- matrix(0,nrow = length(len_grad)-1,ncol = 3)
for (i in 1:(length(len_grad)-1)){
  ranges <- paste(len_grad[i], "-" , len_grad[i+1])
  num_vlp <- sum(between(data_vlp$len,len_grad[i],len_grad[i+1]))
  num_bulk <- sum(between(data_bulk$len,len_grad[i],len_grad[i+1]))
  len_num[i,] <- c(ranges,num_vlp,num_bulk)
}

colnames(len_num) <- c("ranges","VLPs","Bulk")
len_num <- data.frame(len_num)
ranges <- len_num$ranges

len_num <- melt(len_num,id.vars = c("ranges"))
len_num$ranges <- factor(len_num$ranges,levels = ranges)
len_num$value <- as.integer(len_num$value)
ggplot(len_num)+
  geom_bar(aes(ranges,value,fill=variable),stat = "identity",width = 1,position = "dodge")+
  labs(x="Genome length",y="Number")+
  scale_fill_manual(values=c(rgb(225,135,39,max=255),rgb(32,133,76,max=255)))+
  scale_y_continuous(limits = c(0,5000),breaks=seq(0,5000,1000))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 90,size=4.5,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')
