library(ggplot2)
library(ggpubr)
library(reshape2)

data <- read.table("a2b2.Lachnospiraceae.rela",header = T)
a2_rela <- data[data$individual=="A2",]
b2_rela <- data[data$individual=="B2",]

#a2
data <- a2_rela[,1:3]
summary(lm(data$Lacho.rela~data$Fruit))
summary(lm(data$Lacho.rela~data$Leaf))
data <- melt(data,id.vars = c("Lacho.rela"))
a2 <- ggplot(data)+
  geom_point(aes(value,Lacho.rela,color = variable),size=3)+
  geom_smooth(aes(value,Lacho.rela,color = variable),method='lm',fill=NA)+
  labs(x="Dietary proportion",y="Relative abundance (A2)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0)+
  scale_color_manual(values=c(rgb(92,35,102,max=255),rgb(6,223,6,max=255)))+
  scale_x_continuous(breaks = seq(0,1,0.2))+
  scale_y_continuous(limits = c(0,0.25),breaks = seq(0,0.25,0.05)) 

#b2
data <- b2_rela[,1:3]
summary(lm(data$Lacho.rela~data$Fruit))
summary(lm(data$Lacho.rela~data$Leaf))
data <- melt(data,id.vars = c("Lacho.rela"))
b2 <- ggplot(data)+
  geom_point(aes(value,Lacho.rela,color = variable),size=3)+
  geom_smooth(aes(value,Lacho.rela,color = variable),method='lm',fill=NA)+
  labs(x="Dietary proportion",y="Relative abundance (B2)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0)+
  scale_color_manual(values=c(rgb(92,35,102,max=255),rgb(6,223,6,max=255)))+
  scale_x_continuous(breaks = seq(0,1,0.2))+
  scale_y_continuous(limits = c(0,0.6),breaks = seq(0,0.6,0.15)) 

ggarrange(a2,b2,nrow = 2,ncol = 1)
