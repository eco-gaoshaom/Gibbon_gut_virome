library(vegan)
library(ggplot2)

data<-read.table('site_vvotu.abun',header = T)
group <- data$group
data <- data[,1:(ncol(data)-1)]
data<-decostand(data,method='hellinger')
data<-data.frame(data,group)
adonis(data[,1:2933]~data[,2934],data=data,permutations = 999,method='bray')

data <- data[,1:(ncol(data)-1)]
pcoa.data<-capscale(data~1,distance = 'bray')

sumd<-summary(pcoa.data)
sites<-as.data.frame(sumd$sites[,1:2])

shape <- c("A2-f" =16,"A2-l" =16,"B2-f" =16,"B2-l" =16)
color <- c("A2-f" =rgb(255,59,59,max=255),"A2-l" =rgb(255,59,59,max=255),
           "B2-f" =rgb(15,153,183,max=255),"B2-l" =rgb(15,153,183,max=255))
texts<-rownames(data)
sites$group<-group
pc1<-round(sumd$cont$importance[2,1]*100,2)
pc2<-round(sumd$cont$importance[2,2]*100,2)
label1<-paste("PCo1 (",pc1,"% of total variation)",sep = "")
label2<-paste("PCo2 (",pc2,"% of total variation)",sep = "")
#par(oma=c(2,2,2,2))
ggplot(sites,aes(MDS1,MDS2))+geom_point(size=4,aes(color=group,shape=group))+labs(x=label1,y=label2)+
  #geom_text_repel(aes(MDS1,MDS2,label=texts,color=factor(group)),size=5)+
  scale_shape_manual(values=shape)+scale_color_manual(values=color)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid = element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=1,colour="black"),
        legend.text = element_text(size=15))+expand_limits(x=0,y=0)+theme(legend.position = 'none')+
  geom_hline(yintercept=0,linetype=4,color="grey",size=1)+ 
  geom_vline(xintercept=0,linetype=4,color="grey",size=1)
