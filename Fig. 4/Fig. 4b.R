library(ggplot2)

data<-read.table('b2.v2tvotu.abun_ratio',header = T)
ggplot(data)+
  geom_point(aes(Leaf,v2tabun),size=3,color = rgb(6,223,6,max=255))+
  geom_smooth(aes(Leaf,v2tabun),method='lm',color = rgb(6,223,6,max=255))+
  geom_point(aes(Fruit,v2tabun),size=3,color = rgb(92,35,102,max=255))+
  geom_smooth(aes(Fruit,v2tabun),method='lm',color = rgb(92,35,102,max=255))+
  labs(x="Dietary proporation",y="Virulent/Temperate abundance ratio (B2)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0)+
  scale_x_continuous(breaks = seq(0,1,0.2))+
  scale_y_continuous(limits = c(0,6),breaks = seq(0,6,1))  
