vir_byhost <- read.table("virabun_by_hostpop.txt")
tem_byhost <- read.table("temabun_by_hostpop.txt")
hostv <- read.table("mag_89sample.abun")
hostt <- read.table("mag_100sample.abun")

vira2 <- vir_byhost[,1:39]
hosta2 <- hostv[,1:39]

vira2 <- t(vira2)
hosta2 <- t(hosta2)

vir2host_a2 <- c()
for (i in 1:ncol(vira2)){
  abun_vir <- vira2[,i]
  hostname = colnames(vira2)[i]
  abun_host <- hosta2[,c(hostname)]
  corr <- cor.test(abun_vir,abun_host)
  vir2host_a2[i] <- corr$estimate
}


virb2 <- vir_byhost[,40:89]
hostb2 <- hostv[,40:89]

virb2 <- t(virb2)
hostb2 <- t(hostb2)

vir2host_b2 <- c()
for (i in 1:ncol(virb2)){
  abun_vir <- virb2[,i]
  hostname = colnames(virb2)[i]
  abun_host <- hostb2[,c(hostname)]
  corr <- cor.test(abun_vir,abun_host)
  vir2host_b2[i] <- corr$estimate
}


tema2 <- tem_byhost[,1:46]
hosta2 <- hostt[,1:46]

tema2 <- t(tema2)
hosta2 <- t(hosta2)

tem2host_a2 <- c()
for (i in 1:ncol(tema2)){
  abun_tem <- tema2[,i]
  hostname = colnames(tema2)[i]
  abun_host <- hosta2[,c(hostname)]
  corr <- cor.test(abun_tem,abun_host)
  tem2host_a2[i] <- corr$estimate
}


temb2 <- tem_byhost[,47:100]
hostb2 <- hostt[,47:100]

temb2 <- t(temb2)
hostb2 <- t(hostb2)

tem2host_b2 <- c()
for (i in 1:ncol(temb2)){
  abun_tem <- temb2[,i]
  hostname = colnames(temb2)[i]
  abun_host <- hostb2[,c(hostname)]
  corr <- cor.test(abun_tem,abun_host)
  tem2host_b2[i] <- corr$estimate
}

vir2host_a2_num <- na.omit(vir2host_a2)
vir2host_b2_num <- na.omit(vir2host_b2)
tem2host_a2_num <- na.omit(tem2host_a2)
tem2host_b2_num <- na.omit(tem2host_b2)


cora2 <- c(vir2host_a2_num,tem2host_a2_num)
ga2 <- c(replicate(length(vir2host_a2_num),"vir2host_a2"),replicate(length(tem2host_a2_num),"tem2host_a2"))
dataa2 <- data.frame(cora2,ga2)
dataa2$ga2 <- factor(dataa2$ga2,levels = c("vir2host_a2","tem2host_a2"))
ggplot(dataa2,aes(x=cora2))+geom_density(aes(color=ga2,fill=ga2),alpha=0.5)+
  labs(x="Virus-host correlations",y="Density")+
  #geom_text_repel(aes(MDS1,MDS2,label=texts,color=factor(group)),size=5)+
  #scale_shape_manual(values=shape)+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255)))+
  scale_fill_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=-5,y=0)+
  scale_x_continuous(limits = c(-0.5,1),breaks=seq(-0.5,1,0.3))+
  scale_y_continuous(limits = c(0,3.5),breaks=seq(0,3.5,0.5))+
  geom_vline(xintercept=0.019,linetype=4,color=rgb(210,15,38,max=255),size=1)+
  geom_vline(xintercept=0.283,linetype=4,color=rgb(25,115,232,max=255),size=1)



corb2 <- c(vir2host_b2_num,tem2host_b2_num)
gb2 <- c(replicate(length(vir2host_b2_num),"vir2host_b2"),replicate(length(tem2host_b2_num),"tem2host_b2"))
datab2 <- data.frame(corb2,gb2)
datab2$gb2 <- factor(datab2$gb2,levels = c("vir2host_b2","tem2host_b2"))
ggplot(datab2,aes(x=corb2))+geom_density(aes(color=gb2,fill=gb2),alpha=0.5)+
  labs(x="Virus-host correlations",y="Density")+
  #geom_text_repel(aes(MDS1,MDS2,label=texts,color=factor(group)),size=5)+
  #scale_shape_manual(values=shape)+
  scale_color_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255)))+
  scale_fill_manual(values=c(rgb(210,15,38,max=255),rgb(25,115,232,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=-5,y=0)+
  scale_x_continuous(limits = c(-0.5,1),breaks=seq(-0.5,1,0.3))+
  scale_y_continuous(limits = c(0,3.5),breaks=seq(0,3.5,0.5))+
  geom_vline(xintercept=0.036,linetype=4,color=rgb(210,15,38,max=255),size=1)+
  geom_vline(xintercept=0.273,linetype=4,color=rgb(25,115,232,max=255),size=1)
