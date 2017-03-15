EnergyScan1 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_1_2.txt", sep=";", comment.char="#")
EnergyScan2 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_2_2.txt", sep=";", comment.char="#")
EnergyScan3 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_3_1.txt", sep=";", comment.char="#")
EnergyScan4 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_4_1.txt", sep=";", comment.char="#")
EnergyScan5 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_5_1.txt", sep=";", comment.char="#")
EnergyScan6 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_6_1.txt", sep=";", comment.char="#")
EnergyScan7 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_7_1.txt", sep=";", comment.char="#")
EnergyScan8 <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/EnergyScan_8_1.txt", sep=";", comment.char="#")
EnergyScan.list <- list(EnergyScan1,EnergyScan2,EnergyScan3,EnergyScan4,EnergyScan5,EnergyScan6,EnergyScan7,EnergyScan8)
rm(EnergyScan1,EnergyScan2,EnergyScan3,EnergyScan4,EnergyScan5,EnergyScan6,EnergyScan7,EnergyScan8)

EnergyScan <- data.frame()
for (i in 1:8){
  temp <- EnergyScan.list[[i]]
  temp$node <- rep(i,nrow(temp))
  EnergyScan <- rbind(EnergyScan,temp)
}
rm(EnergyScan.list, temp, i)

par(mfrow=c(2,2))
grid <- matrix(c(1,2,3,3), nrow = 2,  ncol = 2, byrow = TRUE)
layout(grid)
rm(grid)

radios <- c("2400OQPSK","433MSK","915FSK")
nodes <- unique(EnergyScan$node)
for (r in radios){
  radio.set <- subset(EnergyScan,radio==r,select=c(channel,rssi,node))
  plot(radio.set$channel,radio.set$rssi,type="n",ylab = "RSSI",xlab = "CHANNEL", main = r)
  for (i in 1:8){
    node.set <- subset(radio.set,node==i,select=c(channel,rssi))
    node.set <- merge(
      data.frame(channel=1:max(node.set$channel)),
      #aggregate(.~channel,data=node.set,mean), #Use mean to compute rssi.
      aggregate(rssi~channel,data=node.set,quantile,probs=0.5), #Use quantile with 50% probability to compute rssi.
      all.x = TRUE
    )
    lines(node.set$channel,node.set$rssi,type="s",col=i)
    #points(node.set$channel,node.set$rssi, pch = 19, cex = 1.2, col=i)
  }
  legend("topleft" ,legend=1:8, col=1:8, horiz = TRUE, pch = 15, pt.cex=1.5, cex=0.8)
}
rm(node.set, radio.set, i, r, nodes, radios)
