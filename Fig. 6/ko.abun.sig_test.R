#significance test of abundances of carbon-related ko on temperate genomes
ko <- read.table('temperate.carbon.site_ko.abun',header = T)

ko <- ko[order(ko$group),]
colname<-colnames(ko)
leng<-length(colname)

result_a2fl <- c()
result_b2fl <- c()
for (i in 1:(leng-1)){
  print(i)
  group<-ko[,leng]
  ko_abun<-ko[,i]
  ko.new<-data.frame(ko_abun,group)
  ko.new
  aovs_a2<-wilcox.test(ko_abun~group,data=ko.new[1:46,],paired=FALSE)
  summary.p_a2<-aovs_a2$p.value
  result_a2fl[i]<-summary.p_a2
  
  aovs_b2<-wilcox.test(ko_abun~group,data=ko.new[47:100,],paired=FALSE)
  summary.p_b2<-aovs_b2$p.value
  result_b2fl[i]<-summary.p_b2
}

result_a2fl<-as.numeric(result_a2fl)
result_b2fl<-as.numeric(result_b2fl)
result_a2fl_adj<-p.adjust(result_a2fl,method = "BH")
result_b2fl_adj<-p.adjust(result_b2fl,method = "BH")

result <- data.frame(result_a2fl_adj,result_b2fl_adj)
row.names(result) <- colname[1:(leng-1)]
write.table(result,"tem.ko.sig.result_wilcox-t_BH",sep='\t')

#significance test of abundances of carbon-related ko on virulent genomes
ko <- read.table('virulent.carbon.site_ko.abun',header = T)

ko <- ko[order(ko$group),]
colname<-colnames(ko)
leng<-length(colname)

result_a2fl <- c()
result_b2fl <- c()
for (i in 1:(leng-1)){
  print(i)
  group<-ko[,leng]
  ko_abun<-ko[,i]
  ko.new<-data.frame(ko_abun,group)
  ko.new
  aovs_a2<-wilcox.test(ko_abun~group,data=ko.new[1:40,],paired=FALSE)
  summary.p_a2<-aovs_a2$p.value
  result_a2fl[i]<-summary.p_a2
  
  aovs_b2<-wilcox.test(ko_abun~group,data=ko.new[41:90,],paired=FALSE)
  summary.p_b2<-aovs_b2$p.value
  result_b2fl[i]<-summary.p_b2
}

result_a2fl<-as.numeric(result_a2fl)
result_b2fl<-as.numeric(result_b2fl)
result_a2fl_adj<-p.adjust(result_a2fl,method = "BH")
result_b2fl_adj<-p.adjust(result_b2fl,method = "BH")

result <- data.frame(result_a2fl_adj,result_b2fl_adj)
row.names(result) <- colname[1:(leng-1)]
write.table(result,"vir.ko.sig.result_wilcox-t_BH",sep='\t')