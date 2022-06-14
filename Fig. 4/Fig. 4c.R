library(ggplot2)
data<-read.table('b2.f2l.vvotu_tvotu.abun_variation',header = T)

ggplot(data)+
  geom_bar(aes(num,abun_variation,fill=family),stat = 'identity',width = 1)+
  scale_fill_manual(values = c(rgb(195,19,27,max=255),rgb(224,110,18,max=255),rgb(6,223,6,max=255),
                               rgb(13,67,146,max=255),rgb(112,59,147,max=255),rgb(0,105,180,max=255),
                               rgb(102,159,72,max=255),rgb(125,153,183,max=255),rgb(255,59,59,max=255),
                               rgb(15,153,183,max=255),rgb(198,113,113,max=255),rgb(250,192,15,max=255),
                               rgb(92,35,102,max=255),rgb(197,19,27,max=255)))+
  labs(x="MAGs",y="Viral abundance variation (F to L)")+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
        axis.text.x = element_text(hjust = 0.5,size=12,colour = 'black',angle = 0),
        axis.text.y=element_text(size=12,colour = 'black'))+
  theme(legend.position = 'right')+geom_hline(yintercept=0,linetype=1,color="grey",size=1)+
  expand_limits(y=c(-8000,32000))+scale_y_continuous(breaks = seq(-8000,32000,5000))
