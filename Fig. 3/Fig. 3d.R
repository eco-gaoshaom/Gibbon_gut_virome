library(ggplot2)
library(vegan)

data<-read.table('a2.f2l.viral_range.variation',header = T)
ggplot(data)+
  geom_bar(aes(num,a2_f2l,fill=group),stat = 'identity',width = 1)+
  scale_fill_manual(values = c(rgb(129,184,223,max=255),rgb(250,192,15,max=255)))+
  geom_line(aes(num,rela))+
  labs(x="MAGs",y="Viral range variation (F to L)/Relative abundance (%)")+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
        axis.text.x = element_text(hjust = 0.5,size=12,colour = 'black',angle = 0),
        axis.text.y=element_text(size=12,colour = 'black'))+
  theme(legend.position = 'bottom')+geom_hline(yintercept=0,linetype=1,color="grey",size=1)+
  expand_limits(y=c(-7,9))+scale_y_continuous(breaks = seq(-7,9,4))

cor.test(data$a2_f2l,data$rela)
