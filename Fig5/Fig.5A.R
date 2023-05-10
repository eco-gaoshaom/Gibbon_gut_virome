data <- read.table('hostpop.virrange_by_family',header = T)
new <- aggregate(data$virrange,by=list(data$family,data$group),mean)
new <- new[order(new$x,decreasing = T),]

new$Group.1 <- factor(new$Group.1,levels = unique(new$Group.1))
new$Group.2 <- factor(new$Group.2,levels = c("virulent","temperate"))
ggplot(new)+
  geom_bar(aes(Group.1,x,fill=Group.2),stat = "identity",width = 0.5,position = "dodge")+
  labs(x="Average viral range",y="Family")+
  scale_fill_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  scale_y_continuous(limits = c(0,75),breaks=seq(0,75,15))+
  theme_bw()+coord_flip()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=5,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')
