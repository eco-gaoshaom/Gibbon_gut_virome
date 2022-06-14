library(ggplot2)
library(ggpubr)

data<-read.table('virulent_genome.feature',header = T)

mean <- log10(tapply(data[,4],data$family,mean))
mins <- log10(tapply(data[,4], data$family, min))
maxs <- log10(tapply(data[,4], data$family, max))
Freq <- table(data$family)
cir <- round((tapply(data[,3], data$family, sum)/Freq)*100,1)
family <- unique(data$family)
num <- c(1:9)
new1 <- data.frame(cbind(mean,mins,maxs,Freq,num,cir))
new1$sizes <- ifelse(new1$Freq < 10, "1.5",ifelse(new1$Freq < 100, "3",ifelse(new1$Freq < 1000, "6","12")))
new1$sizes <- as.numeric(new1$sizes)
new1$family <- rownames(new1)


virulent <- ggplot(new1)+geom_segment(aes(x = num,xend = num,y = mins, yend = maxs))+
  geom_segment(aes(x = num-0.2,y = mins,xend = num+0.2,yend=mins))+
  geom_segment(aes(x = num-0.2,y = maxs,xend = num+0.2,yend=maxs))+
  geom_point(aes(num,mean,size = sizes),color = rgb(190,190,190,max=255))+
  geom_text(aes(num,y=6,label=cir))+
  #scale_size("Number",limits = c(1,12))+
  theme_classic(base_line_size = 1)+scale_size_area(max_size = 12)+
  labs(x="Family",y="Contig length (kb)")+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=20),
        axis.text.x = element_text(hjust = 0.5,vjust = 0.5,size=14,colour = 'black',angle=0),
        axis.text.y=element_text(size=18,colour = 'black'),legend.position = "none")+
  coord_flip()+
  expand_limits(x=c(1,10))+
  scale_x_continuous(breaks = seq(1,10,1),labels = c((new1$family),"Inoviridae"))+
  expand_limits(y=c(3,6))+scale_y_continuous(limits = c(3,6),breaks = seq(3,6,1))


data<-read.table('temperate_genome.feature',header = T)

mean <- log10(tapply(data[,4],data$family,mean))
mins <- log10(tapply(data[,4], data$family, min))
maxs <- log10(tapply(data[,4], data$family, max))
Freq <- table(data$family)
cir <- round((tapply(data[,3], data$family, sum)/Freq)*100,1)
family <- unique(data$family)
new2 <- data.frame(cbind(mean,mins,maxs,Freq,cir))
new2$sizes <- ifelse(new2$Freq < 10, "1.5",ifelse(new2$Freq < 100, "3",ifelse(new2$Freq < 1000, "6","12")))
new2$sizes <- as.numeric(new2$sizes)
new2$family <- rownames(new2)
new2 <- rbind(new2[1,],new2[3:7,],new2[2,])
num <- c(4:10)
new2$num <- num


temperate <- ggplot(new2)+geom_segment(aes(x = num,xend = num,y = mins, yend = maxs))+
  geom_segment(aes(x = num-0.2,y = mins,xend = num+0.2,yend=mins))+
  geom_segment(aes(x = num-0.2,y = maxs,xend = num+0.2,yend=maxs))+
  geom_point(aes(num,mean,size = sizes),color = rgb(190,190,190,max=255))+
  geom_text(aes(num,y=6,label=cir))+
  #scale_size("Number",limits = c(1,12))+
  theme_classic(base_line_size = 1)+scale_size_area(max_size = 12)+
  labs(x=NULL,y="Contig length (kb)")+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=20),
        axis.text.x = element_text(hjust = 0.5,vjust = 0.5,size=14,colour = 'black',angle=0),
        axis.text.y=element_text(size=18,colour = 'black'))+
  coord_flip()+
  expand_limits(x=c(1,10))+
  scale_x_continuous(breaks = seq(1,10,1),labels = c("Autographiviridae","Chaseviridae","Genomoviridae",(new2$family)))+
  expand_limits(y=c(3,6))+scale_y_continuous(limits = c(3,6),breaks = seq(3,6,1))

ggarrange(virulent,temperate,ncol=2,nrow=1)

