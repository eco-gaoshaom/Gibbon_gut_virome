data <- read.table('VLPs-vOTU.shared_specific.rela_preva',header = T)
library(ggplot2)
library(ggExtra)
data$category <- factor(data$category,levels = c("Shared","VLPs-specific"))
pv <- ggplot(data,aes(rela.log,prevalence))+geom_point(size=2,aes(color=category))+
  labs(x="Relative abundance",y="Prevalence")+
  scale_color_manual(values=c(rgb(220,220,220,max=255),rgb(225,135,39,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=-5,y=0)+
  scale_x_continuous(limits = c(-5,1),breaks=seq(-5,1,1))+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))

ggMarginal(pv,type = 'density',groupColour = TRUE,groupFill = TRUE)


data <- read.table('Bulk-vOTU.shared_specific.rela_preva',header = T)
library(ggplot2)
library(ggExtra)
data$category <- factor(data$category,levels = c("Shared","Bulk-specific"))
pm <- ggplot(data,aes(rela.log,prevalence))+geom_point(size=2,aes(color=category))+
  labs(x="Relative abundance",y="Prevalence")+
  scale_color_manual(values=c(rgb(220,220,220,max=255),rgb(32,133,76,max=255)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'bottom')+
  expand_limits(x=-5,y=0)+
  scale_x_continuous(limits = c(-5,1),breaks=seq(-5,1,1))+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))

ggMarginal(pm,type = 'density',groupColour = TRUE,groupFill = TRUE,margins = "both")
