a2_vir_rate <- read.table('a2.vir.evo_rate',header = T)
a2_vir_rate_mean <- aggregate(a2_vir_rate$Sub_rate,by=list(a2_vir_rate$Seq),mean)
a2_tem_rate <- read.table('a2.tem.evo_rate',header = T)
a2_tem_rate_mean <- aggregate(a2_tem_rate$Sub_rate,by=list(a2_tem_rate$Seq),mean)

a2_virtem <- rbind(a2_vir_rate_mean,a2_tem_rate_mean)
group <- c(replicate(nrow(a2_vir_rate_mean),"a2_vir"),replicate(nrow(a2_tem_rate_mean),"a2_tem"))
a2_virtem$group <- group
sub_rate_log <- log10(a2_virtem$x)
a2_virtem$sub_rate_log <- sub_rate_log
colnames(a2_tem_rate) <- c("seq","sub_rate","group","log_sub_rate")

a2_virtem$group <- factor(a2_virtem$group,levels = c("a2_vir","a2_tem"))
ggplot(a2_virtem, aes(group,sub_rate_log)) + 
  ggdist::stat_halfeye(aes(color=group,fill=group),adjust = 0.2, width = .3, .width = 0, justification = -.9, point_colour = NA) + 
  geom_jitter(aes(color=group),width = .2) +
  scale_fill_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  scale_color_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  labs(y="Substitution rate (A2)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+theme(legend.position = 'none')+
  scale_y_continuous(limits = c(-10,-5),breaks=seq(-10,-5,1))

b2_vir_rate <- read.table('b2.vir.evo_rate',header = T)
b2_vir_rate_mean <- aggregate(b2_vir_rate$Sub_rate,by=list(b2_vir_rate$Seq),mean)
b2_tem_rate <- read.table('b2.tem.evo_rate',header = T)
b2_tem_rate_mean <- aggregate(b2_tem_rate$Sub_rate,by=list(b2_tem_rate$Seq),mean)

b2_virtem <- rbind(b2_vir_rate_mean,b2_tem_rate_mean)
group <- c(replicate(nrow(b2_vir_rate_mean),"b2_vir"),replicate(nrow(b2_tem_rate_mean),"b2_tem"))
b2_virtem$group <- group
sub_rate_log <- log10(b2_virtem$x)
b2_virtem$sub_rate_log <- sub_rate_log
colnames(b2_tem_rate) <- c("seq","sub_rate","group","log_sub_rate")

b2_virtem$group <- factor(b2_virtem$group,levels = c("b2_vir","b2_tem"))
ggplot(b2_virtem, aes(group,sub_rate_log)) + 
  ggdist::stat_halfeye(aes(color=group,fill=group),adjust = 0.2, width = .3, .width = 0, justification = -.9, point_colour = NA) + 
  geom_jitter(aes(color=group),width = .2) +
  scale_fill_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  scale_color_manual(values=c(rgb(250,192,15,max=255),rgb(129,184,223,max=255)))+
  labs(y="Substitution rate (b2)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=20,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+theme(legend.position = 'none')+
  scale_y_continuous(limits = c(-10,-5),breaks=seq(-10,-5,1))
