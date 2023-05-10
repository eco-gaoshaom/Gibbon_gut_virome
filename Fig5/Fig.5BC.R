v2h_adj <- read.table('virpop2hpop.rmr',header = T)

library(tidyverse)
vh_mat <- spread(v2h_adj,key=pop,value=value)
vh_mat[is.na(vh_mat)] <- 0
row.names(vh_mat)<-vh_mat$votu
vh_mat <- vh_mat[,-1]

votu_mat <- read.table("vir_89site.abun",header = T)
host_mat <- read.table("host_89site.abun",header = T)

site_otuname <- list()
site_hostname <- list()
site_mat <- list()
site_hostrange <- c()
site_virrange <- c()

library(igraph)
for (i in 1:89){
  print(i)
  site_otuname[[i]] <- intersect(c(rownames(votu_mat)[votu_mat[,i]>0]),rownames(vh_mat))
  site_hostname[[i]] <- intersect(c(rownames(host_mat)[host_mat[,i]>0]),colnames(vh_mat))
  site_mat[[i]] <- vh_mat[site_otuname[[i]],site_hostname[[i]]]
  votuname <- as.character(t(replicate(ncol(site_mat[[i]]),rownames(site_mat[[i]]))))
  hostname <- as.character(replicate(nrow(site_mat[[i]]),colnames(site_mat[[i]])))
  links <- as.numeric(t(site_mat[[i]]))
  site_virrange[i] <- sum(site_mat[[i]]>0)/length(colSums(site_mat[[i]])[colSums(site_mat[[i]])>0])
  site_hostrange[i] <- sum(site_mat[[i]]>0)/length(rowSums(site_mat[[i]])[rowSums(site_mat[[i]])>0])
}
vir_net_char <- data.frame(site_gd,site_avgk,site_hostrange,site_mod,site_virrange)

for (i in 1:89){
  name = paste("vir.vh_pair",i,sep="")
  write.table(site_adj[[i]],name,sep="\t",row.names = FALSE, col.names = FALSE,quote = FALSE)
}




v2h_adj <- read.table('tempop2hpop.rmr',header = T)

library(tidyverse)
vh_mat <- spread(v2h_adj,key=pop,value=value)
vh_mat[is.na(vh_mat)] <- 0
row.names(vh_mat)<-vh_mat$votu
vh_mat <- vh_mat[,-1]

votu_mat <- read.table("tem_100site.abun",header = T)
host_mat <- read.table("host_100site.abun",header = T)

site_otuname <- list()
site_hostname <- list()
site_mat <- list()
site_hostrange <- c()
site_virrange <- c()

library(igraph)
for (i in 1:100){
  print(i)
  site_otuname[[i]] <- intersect(c(rownames(votu_mat)[votu_mat[,i]>0]),rownames(vh_mat))
  site_hostname[[i]] <- intersect(c(rownames(host_mat)[host_mat[,i]>0]),colnames(vh_mat))
  site_mat[[i]] <- vh_mat[site_otuname[[i]],site_hostname[[i]]]
  
  votuname <- as.character(t(replicate(ncol(site_mat[[i]]),rownames(site_mat[[i]]))))
  hostname <- as.character(replicate(nrow(site_mat[[i]]),colnames(site_mat[[i]])))
  links <- as.numeric(t(site_mat[[i]]))
  site_virrange[i] <- sum(site_mat[[i]]>0)/length(colSums(site_mat[[i]])[colSums(site_mat[[i]])>0])
  site_hostrange[i] <- sum(site_mat[[i]]>0)/length(rowSums(site_mat[[i]])[rowSums(site_mat[[i]])>0])
}
tem_net_char <- data.frame(site_gd,site_avgk,site_hostrange,site_mod,site_virrange)

for (i in 1:100){
  name = paste("tem.vh_pair",i,sep="")
  write.table(site_adj[[i]],name,sep="\t",row.names = FALSE, col.names = FALSE,quote = FALSE)
}