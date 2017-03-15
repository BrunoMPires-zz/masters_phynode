library (zoo)

#setwd("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/LinkQA_v2")

radios <- unique(LinkQA[,"radio"])
samples <- 7000
window <- 50

LinkQA.sucess <- subset(LinkQA, crc == T)
LinkQA.sucess$snr <- LinkQA.sucess$rssi - LinkQA.sucess$floor


#################################################################################
# Normalize LQI values for all interfaces, since metric is hardware dependent.
tmp <- data.frame()
for (r in radios){
  link <- subset(LinkQA.sucess, radio == r, select = c("radio","seq","snr","lqi"))
  link$lqi <- (link$lqi-min(LinkQA.sucess$lqi))/(max(LinkQA.sucess$lqi)-min(LinkQA.sucess$lqi))
  #link$snr <- (link$snr-min(LinkQA.sucess$snr))/(max(LinkQA.sucess$snr)-min(LinkQA.sucess$snr))
  tmp <- rbind(tmp,link)
}
LinkQA.sucess$lqi <- NULL
#LinkQA.sucess$snr <- NULL
LinkQA.sucess <- merge(LinkQA.sucess, tmp, sort = F)
#################################################################################

#################################################################################
########################### Triangle LQE Metric #################################
#################################################################################
# Nº of lost packets between each received packet
tmp <- data.frame()
for (r in radios){
  link <- subset(LinkQA.sucess, radio == r, select = c("radio","seq"))
  link$lost <- c(0, diff(link$seq) - 1)
  tmp <- rbind(tmp,link)
}
LinkQA.sucess <- merge(LinkQA.sucess, tmp, sort = F)
rm( tmp, r, link)

# Triangle LQE metric for each received packet
tmp <- data.frame()
for (r in radios){
  link <- subset(LinkQA.sucess, radio == r)
  wlqi <- rollapplyr(link[,"lqi"], width = window, partial = 1, sum)
  wsnr <- rollapplyr(link[,"snr"], width = window, partial = 1, sum)
  # Nº of sent packets per window
  wtotal <- rollapplyr(link[,"lost"], width = window, partial = 1, sum)
  for (i in 1:nrow(link)){
    if (i < window){
      wtotal[i] <- wtotal[i] + i
    } else {
      wtotal[i] <- wtotal[i] + window
    }
  }
  # WIndowed LQI & SNR
  wlqi <- wlqi / wtotal
  wsnr <- wsnr / wtotal
  # Triangle LQE
  link$lqe <- sqrt((wsnr ^ 2) + (wlqi ^ 2))
  tmp <- rbind(tmp, link)
}
LinkQA.sucess <- merge (LinkQA.sucess, tmp, sort =F)
rm (tmp, r, link, wlqi, wsnr, wtotal, i)
#write.csv(LinkQA.sucess, "LinkQA2_LQE.csv", row.names = F)
##################################################################################

################################## << PLOT >> ####################################
# LQE vs. lost packet count
for (r in radios){
  lost <- subset(LinkQA.sucess, radio == r)[,"lost"]
  lost <- (lost-min(lost))/(max(lost)-min(lost))
  lqe <- subset(LinkQA.sucess, radio == r)[,"lqe"]
  lqe <- (lqe-min(lqe))/(max(lqe)-min(lqe))
  plot(lost, ylim = c(0,1), type ="l", main = r, ylab = "")
  lines(smooth.spline(lqe), col = "red")
  legend("topright", legend = c("Perda","LQE"), fill = c("black","red"), cex = 0.8 )
}
rm (r, lost, lqe)
#################################################################################

#################################################################################
######################### Triangle LQE Metric - Modified ########################
#################################################################################
# Compute full samples matrix
LinkQA.total <- data.frame()
for (r in radios){
  link <- subset (LinkQA.sucess, radio == r)
  link <- merge (link, data.frame(radio=r,seq=1:samples), all.y = T)
  link$lqe <- na.locf(link$lqe)
  link[is.na(link$crc),"crc"] <- FALSE
  link[is.na(link$lqi),"lqi"] <- 0
  link[is.na(link$snr),"snr"] <- 0
  LinkQA.total <- rbind(LinkQA.total, link)
}
LinkQA.total <- LinkQA.total[order(LinkQA.total$seq),]
rm(r, link)
# Triangle LQE based on packets received by any interface
tmp <- data.frame()
for (r in radios){
  link <-  subset(LinkQA.total, radio == r)
  wlqi <- rollapplyr(link[,"lqi"], width = window, partial = 1, sum)
  wsnr <- rollapplyr(link[,"snr"], width = window, partial = 1, sum)
  wtotal <- pmin (1:nrow(link),window)
  # WIndowed LQI & SNR
  wlqi <- wlqi / wtotal
  wsnr <- wsnr / wtotal
  # Modified Triangle LQE
  link$lqemod <- sqrt((wsnr ^ 2) + (wlqi ^ 2))
  tmp <- rbind(tmp, link)
}
LinkQA.total <- merge (LinkQA.total, tmp, sort =F)
rm (tmp, r, link, wlqi, wsnr, wtotal)
#################################################################################

#################################################################################
# Compute real PRR
tmp <- data.frame()
for (r in radios){
  link <- subset(LinkQA.total, radio == r, select = c("radio","seq","crc"))
  link$prr <- rollapplyr(as.numeric(link$crc), width = window, partial = 1, sum)
  link$prr <- link$prr / pmin(1:nrow(link),window)
  tmp <- rbind(tmp,link)
}
LinkQA.total <- merge (LinkQA.total, tmp, sort =F)
rm (tmp, r, link)
write.csv(LinkQA.total, "LinkQA2_RProcessed.csv", row.names = F)
#################################################################################

#*******************************************************************************#
#******************************** Data Analysis ********************************#
#*******************************************************************************#

################################## << PLOT >> ###################################
# PRR vs Triangle  vs Modified Triangle
for (r in radios){
  link <- subset (LinkQA.total, radio == r)
  lqemod <- (link$lqemod-min(link$lqemod))/(max(link$lqemod)-min(link$lqemod))
  lqe <- (link$lqe-min(link$lqe))/(max(link$lqe)-min(link$lqe))
  plot (smooth.spline(lqemod), col="darkblue", type="l", ylab="", xlab="Index", ylim=c(0,1.3), main = r)
  lines(smooth.spline(lqe), col="red")
  lines(smooth.spline(link$prr), col="grey")
  legend("topright", inset = c(0.015,0.03), legend = c("Triangle","Triangle MOD","PRR"), fill = c("darkblue","red","grey"), cex = 0.8 )
}
rm (r, link, lqemod, lqe)
################################################################################

################################## << PLOT >> ###################################
# LQE(MOD) for all interfaces
plot (LinkQA.total$lqemod, type = "n", ylab = "", xlim=c(1,samples), main = "Triangle Metric")
i <- 1
for (r in radios){
  link <- subset(LinkQA.total, radio == r)
  lines(smooth.spline(link$lqemod), col=rainbow(3)[i])
  i <- i + 1
}
rm (i, r, link)
#################################################################################