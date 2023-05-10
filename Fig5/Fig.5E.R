a2_vir_rate <- read.table('a2.vir.evo_rate',header = T)
a2_vir_rate <- aggregate(a2_vir_rate$Sub_rate,by=list(a2_vir_rate$Days),mean)
sub_rate_log <- log10(a2_vir_rate$x)
a2_vir_rate$sub_rate_log <- sub_rate_log
colnames(a2_vir_rate) <- c("Days","sub_rate","sub_rate_log")
ggplot(a2_vir_rate)+
  geom_point(aes(Days,sub_rate_log),shape=16,size=2,color=rgb(220,220,220,max=255))+
  labs(x = "Time (days)",y="Substitution rate (log10)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_x_continuous(limits = c(0,400),breaks = seq(0,400,100))+
  scale_y_continuous(limits = c(-9,-4),breaks=seq(-9,-4,1))+
  coord_cartesian(ylim = c(-9,-4))

a2_tem_rate <- read.table('a2.tem.evo_rate',header = T)
a2_tem_rate <- aggregate(a2_tem_rate$Sub_rate,by=list(a2_tem_rate$Days),mean)
sub_rate_log <- log10(a2_tem_rate$x)
a2_tem_rate$sub_rate_log <- sub_rate_log
colnames(a2_tem_rate) <- c("Days","sub_rate","sub_rate_log")
ggplot(a2_tem_rate)+
  geom_point(aes(Days,sub_rate_log),shape=16,size=2,color=rgb(220,220,220,max=255))+
  labs(x = "Time (days)",y="Substitution rate (log10)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_x_continuous(limits = c(0,400),breaks = seq(0,400,100))+
  scale_y_continuous(limits = c(-9,-4),breaks=seq(-9,-4,1))+
  coord_cartesian(ylim = c(-9,-4))

b2_vir_rate <- read.table('b2.vir.evo_rate',header = T)
b2_vir_rate <- aggregate(b2_vir_rate$Sub_rate,by=list(b2_vir_rate$Days),mean)
sub_rate_log <- log10(b2_vir_rate$x)
b2_vir_rate$sub_rate_log <- sub_rate_log
colnames(b2_vir_rate) <- c("Days","sub_rate","sub_rate_log")
ggplot(b2_vir_rate)+
  geom_point(aes(Days,sub_rate_log),shape=16,size=2,color=rgb(220,220,220,max=255))+
  labs(x = "Time (days)",y="Substitution rate (log10)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_x_continuous(limits = c(0,450),breaks = seq(0,450,150))+
  scale_y_continuous(limits = c(-9,-4),breaks=seq(-9,-4,1))+
  coord_cartesian(ylim = c(-9,-4))

b2_tem_rate <- read.table('b2.tem.evo_rate',header = T)
b2_tem_rate <- aggregate(b2_tem_rate$Sub_rate,by=list(b2_tem_rate$Days),mean)
sub_rate_log <- log10(b2_tem_rate$x)
b2_tem_rate$sub_rate_log <- sub_rate_log
colnames(b2_tem_rate) <- c("Days","sub_rate","sub_rate_log")
ggplot(b2_tem_rate)+
  geom_point(aes(Days,sub_rate_log),shape=16,size=2,color=rgb(220,220,220,max=255))+
  labs(x = "Time (days)",y="Substitution rate (log10)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_x_continuous(limits = c(0,450),breaks = seq(0,450,150))+
  scale_y_continuous(limits = c(-9,-4),breaks=seq(-9,-4,1))+
  coord_cartesian(ylim = c(-9,-4))
