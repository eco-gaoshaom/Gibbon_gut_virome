data<-read.table('a2.pop.crispr_spacer',header = T)
library(ggplot2)
library(ggpubr)
summary(lm(data$pop_criratio~data$Fruit))
summary(lm(data$pop_criratio~data$Leaf))

top <- ggplot(data)+
  geom_point(aes(Fruit,pop_criratio),size=3,color = rgb(92,35,102,max=255))+
  geom_smooth(aes(Fruit,pop_criratio),method='lm',color = rgb(92,35,102,max=255))+
  geom_point(aes(Leaf,pop_criratio),size=3,color = rgb(6,223,6,max=255))+
  geom_smooth(aes(Leaf,pop_criratio),method='lm',color = rgb(6,223,6,max=255))+
  labs(x="Dietary proporation",y="Prevalence CRISPR systems")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0.5)+
  scale_x_continuous(breaks = seq(0,1,0.2))+
  scale_y_continuous(breaks = seq(0.5,0.7,0.05))  



summary(lm(data$pop_spacernum~data$Fruit))
summary(lm(data$pop_spacernum~data$Leaf))
bottom <- ggplot(data)+
  geom_point(aes(Fruit,pop_spacernum),size=3,color = rgb(92,35,102,max=255))+
  geom_smooth(aes(Fruit,pop_spacernum),method='lm',color = rgb(92,35,102,max=255))+
  geom_point(aes(Leaf,pop_spacernum),size=3,color = rgb(6,223,6,max=255))+
  geom_smooth(aes(Leaf,pop_spacernum),method='lm',color = rgb(6,223,6,max=255))+
  labs(x="Dietary proporation",y="Number of CRISPR-spacers")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=10)+
  scale_x_continuous(breaks = seq(0,1,0.2))+
  scale_y_continuous(breaks = seq(10.0,18.0,2.0))

ggarrange(top,bottom,ncol = 1, nrow = 2)
