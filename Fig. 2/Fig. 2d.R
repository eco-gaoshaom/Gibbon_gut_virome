data<-read.table('a2b2.vvotu_tvotu.rela',header = T)
group <- data$group

data <- t(data[,1:4])

site<-rownames(data)[1:length(rownames(data))]
family<-colnames(data)
sites<-as.character(replicate(site,n=length(family)))
familys<-as.character(t(replicate(family,n=length(site))))
ra<-as.numeric(as.matrix(data))
groups <- as.character(t(replicate(group,n=length(site))))
new_group <- paste(sites,groups)
dataf<-data.frame(familys,ra,new_group)
dataf$new_group <- factor(dataf$new_group,levels=c("A2.vir viral_family",  "A2.vir host_family",
                                                   "B2.vir viral_family",  "B2.vir host_family",
                                                   "A2.tem viral_family",  "A2.tem host_family",
                                                   "B2.tem viral_family",  "B2.tem host_family"))
dataf$familys <- factor(dataf$familys,levels=colnames(data))
ggplot(dataf,aes(new_group,ra,fill=familys))+
  geom_histogram(stat = 'identity',position = 'fill',width =0.4)+
  scale_fill_manual(values = c(rgb(129,184,223,max=255),rgb(255,59,59,max=255),rgb(15,153,183,max=255),
                               rgb(6,223,6,max=255),rgb(105,139,105,max=255),rgb(195,19,27,max=255),
                               rgb(250,192,15,max=255),rgb(92,35,102,max=255),rgb(198,113,113,max=255),
                               rgb(190,192,195,max=255),
                               rgb(15,153,183,max=255),rgb(250,192,15,max=255),rgb(6,223,6,max=255),
                               rgb(255,59,59,max=255),rgb(105,139,105,max=255),rgb(195,19,27,max=255),
                               rgb(250,192,15,max=255),rgb(92,35,102,max=255),rgb(198,113,113,max=255),
                               rgb(190,192,195,max=255)
  ))+
  labs(x="Samples",y='Relative abundance',fill='')+
  theme_bw()+#coord_fixed(ratio=50)+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 1,angle = 45,size=12,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),legend.position = 'right')

