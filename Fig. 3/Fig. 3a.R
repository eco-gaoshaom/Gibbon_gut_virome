library(ggdendro)
library(ggplot2)
library(reshape2)
library(ggpubr)

data<-read.table('a2.diet_com',header = T)
dist<-vegdist(data[,1:5],method = 'bray')
hc <- hclust(dist)
tree <- ggdendrogram(hc)

label_order <- dendro_data(hc, type="rectangle")$labels$label

adonis(data[,1:5]~data[,7],data=data,permutations = 999,method='bray')
#SIMPER Test
library(vegan)
contri<-simper(data[,1:5],data[,7],permutations = 999)
summary(contri)


data2 <- data[,1:6]
dataf <- melt(data2,id.vars = c("Dates"))
dataf$variable <- factor(dataf$variable,levels = c("Others","Animal","Flower","Leaf","Fruit"))
diet <- ggplot(dataf) + geom_area(aes(Dates,value,fill = variable),alpha = 0.5,position = "fill")+
  scale_fill_manual(values=c(rgb(220,220,220,max=255),rgb(15,153,183,max=255),
                             rgb(14,96,107,max=255),rgb(6,223,6,max=255),rgb(92,35,102,max=255)
  ))+
  labs(x="Dates",y="Proportion")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=12,angle = 0, colour = 'black'),
        axis.text.y=element_text(size=12,colour = 'black'),
        axis.line = element_line(size=0.5,colour="black"),
        legend.text = element_text(size=18),legend.position = 
          "bottom")+
  #scale_x_continuous(limits = c(0,54),breaks=seq(0,54,1))+
  scale_x_continuous(limits = c(0,47),breaks=seq(0,47,1))+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))

diet_label <- rownames(data)
Dates <- data$Dates
pairs <- data.frame(label_order,diet_label,Dates)

label_order_num <- pairs[,c(1,3)][order(pairs[,c(1,3)]$label_order),]
diet_label_num <- pairs[,c(2,3)][order(pairs[,c(2,3)]$diet_label),]

new_pair <- cbind(label_order_num,diet_label_num)
new_pair <- new_pair[order(new_pair[,4]),]
group <- c(replicate(47,"tree_label"),replicate(47,"diet_label"))
orders <- c(new_pair[,2],new_pair[,4])
final <- data.frame(group,orders)

ggpaired(final,x="group",y="orders")

ggarrange(tree,diet,ncol = 1,nrow = 2)


library(ggdendro)
library(ggplot2)
library(reshape2)

data<-read.table('b2.diet_com',header = T)
dist<-vegdist(data[,1:5],method = 'bray')
hc <- hclust(dist)
tree <- ggdendrogram(hc)

label_order <- dendro_data(hc, type="rectangle")$labels$label

adonis(data[,1:5]~data[,7],data=data,permutations = 999,method='bray')
#SIMPER Test
library(vegan)
contri<-simper(data[,1:5],data[,7],permutations = 999)
summary(contri)


data2 <- data[,1:6]
dataf <- melt(data2,id.vars = c("Dates"))
dataf$variable <- factor(dataf$variable,levels = c("Others","Animal","Flower","Leaf","Fruit"))
diet <- ggplot(dataf) + geom_area(aes(Dates,value,fill = variable),alpha = 0.5,position = "fill")+
  scale_fill_manual(values=c(rgb(220,220,220,max=255),rgb(15,153,183,max=255),
                             rgb(14,96,107,max=255),rgb(6,223,6,max=255),rgb(92,35,102,max=255)
  ))+
  labs(x="Dates",y="Proportion")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=12,angle = 0, colour = 'black'),
        axis.text.y=element_text(size=12,colour = 'black'),
        axis.line = element_line(size=0.5,colour="black"),
        legend.text = element_text(size=18),legend.position = 
          "bottom")+
  scale_x_continuous(limits = c(0,54),breaks=seq(0,54,1))+
  #scale_x_continuous(limits = c(0,47),breaks=seq(0,47,1))+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))

diet_label <- rownames(data)
Dates <- data$Dates
pairs <- data.frame(label_order,diet_label,Dates)

label_order_num <- pairs[,c(1,3)][order(pairs[,c(1,3)]$label_order),]
diet_label_num <- pairs[,c(2,3)][order(pairs[,c(2,3)]$diet_label),]

new_pair <- cbind(label_order_num,diet_label_num)
new_pair <- new_pair[order(new_pair[,4]),]
group <- c(replicate(54,"tree_label"),replicate(54,"diet_label"))
orders <- c(new_pair[,2],new_pair[,4])
final <- data.frame(group,orders)

ggpaired(final,x="group",y="orders")

ggarrange(tree,diet,ncol = 1,nrow = 2)
