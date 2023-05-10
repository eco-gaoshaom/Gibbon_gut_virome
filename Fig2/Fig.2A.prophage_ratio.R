data <- read.table('genome.family_checkv_lifestyle',header = T)

new <- aggregate(data$seq,by=list(data$sample,data$method,data$type),FUN=length)
new$x <- as.numeric(as.character(new$x))
group <- paste(new$Group.3,new$Group.2)

new$group <- group
new$group <- factor(new$group,levels = c(unique(new$group)))

new1 <- new[new$Group.3=="Prophage",]

seq_num <- aggregate(data$seq,by=list(data$sample,data$method),FUN=length)
seq_num$x <- as.numeric(as.character(seq_num$x))

d048 <- c("D048","VLPs","Prophage",0,"Prophage VLPs")
new2 <- rbind(new1[1:164,],d048,new1[165:209,])

df <- data.frame(new2,seq_num$x)
colnames(df) <- c("sample","method","type","prophage_num","group","total_seq_num")
prophage_ratio <- as.numeric(df$prophage_num)/as.numeric(df$total_seq_num)
df <- data.frame(df,prophage_ratio)
df <- df[order(df$method),]
write.table(df,"sample.prophage_ratio.replicate",sep="\t")

df <- read.table("sample.prophage_ratio.noreplicate",header = T)
df$method <- factor(df$method,levels=c("VLPs","Bulk"))
ggplot(df)+
  stat_boxplot(geom = "errorbar",width=0.3,aes(method,prophage_ratio,color=method),position = position_dodge(1))+
  geom_boxplot(aes(method,prophage_ratio,color=method,fill=method), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  #geom_jitter(aes(method,completeness,color=method),shape = 16,width = 0.2,alpha = 1)+
  scale_fill_manual(values=c(rgb(225,135,39,max=255),rgb(32,133,76,max=255)))+
  scale_color_manual(values=c(rgb(0,0,0,max=255),rgb(0,0,0,max=255)))+
  #geom_point(data=c4,aes(x=group,y=value,color=group),shape=15,size=1)+
  labs(y="Genome completeness")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(limits = c(0,0.35),breaks=seq(0,0.35,0.05))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,0.35))
