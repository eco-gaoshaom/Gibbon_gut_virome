data <- read.table('genome.family_checkv_lifestyle',header = T)

new <- aggregate(data$seq,by=list(data$type,data$method,data$family),FUN=length)
new$x <- as.integer(as.character(new$x))

family_total <- aggregate(data$seq,by=list(data$family),FUN=length)
family_total <- family_total[order(family_total$x,decreasing = T),]

new_data <- new %>%
  arrange_at(c(2,4,1), desc) %>%
  arrange(match(Group.3, family_total$Group.1))

group <- paste(new_data$Group.3,new_data$Group.2)
new_data$group <- group
new_data$group <- factor(new_data$group,levels = unique(new_data$group))

ggplot(new_data)+
  geom_bar(aes(group,x,fill=Group.1),stat = "identity",width = 0.5,position = "stack")+
  labs(x="Families",y="Genome number")+
  scale_fill_manual(values=c(rgb(250,192,15,max=255),rgb(220,220,220,max=255),rgb(10,10,10,max=255)))+
  scale_y_continuous(limits = c(0,5000),breaks=seq(0,5000,1000))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 1,angle = 45,size=8,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')
