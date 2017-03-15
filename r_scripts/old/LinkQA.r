#LinkQA <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/LinkQA/LinkQA_2_1.txt", sep=";")
#LinkQA <- read.csv("G:/LinkQuality/logg.txt", sep=";")

#LQI vs RSSI
radios <- unique(LinkQA$radio)
samples <- 72000
for(r in radios){
  radio.ok <- subset(LinkQA,radio==r & crc==TRUE)
  radio.fail <- subset(LinkQA,radio==r & crc==FALSE)
  plot(LinkQA$rssi,LinkQA$lqi,type="n", xlab = "RSSI", ylab = "LQI", main = r)
  points(radio.ok$rssi,radio.ok$lqi, pch=20, col="blue")
  points(radio.fail$rssi,radio.fail$lqi, pch=20, col="red")
}
rm (r, radio.ok, radio.fail)

global.ok <- subset(LinkQA,crc==TRUE)
global.fail <- subset(LinkQA,crc==FALSE)
global.sucess.count <- as.data.frame(table(global.ok$seq))
global.sucess.distr <- as.data.frame(table(global.sucess.count$Freq))
fail.count <- samples - sum(global.sucess.distr$Freq)
global.sucess.distr <- rbind(
  data.frame(Var1="0",Freq = samples - sum(global.sucess.distr$Freq)),
  global.sucess.distr
)
barplot(global.sucess.distr$Freq, xlab = "Nº de Interfaces", ylab ="Frequência",
        names.arg = global.sucess.distr$Var1, col=c("red","darkblue","darkorange","darkorange"))

seq=global.fail.distr <- merge(as.data.frame(table(global.ok$radio)),as.data.frame(table(global.fail$radio)),by="Var1")
barplot(
  t(as.matrix(subset(global.fail.distr,select = c(Freq.x,Freq.y)))),
  col=c("darkblue","red"),
  names.arg = global.fail.distr$Var1
)

error.rate <- data.frame(
  Radio="PhyNode",
  Rate=((fail.count/ samples) * 100)
)

for (r in radios){
  fail.count.r <- samples - nrow(subset(global.ok,radio==r))
  fail.rate <- (fail.count.r / samples) * 100
  error.rate <- rbind(error.rate,data.frame(Radio=r,Rate=fail.rate))
  print(fail.rate)
}
barplot(error.rate$Rate, ylab = "Taxa de falha", xlab = "Interface",
        names.arg = error.rate$Radio, col=c("darkblue","darkorange","darkorange","darkorange"))
rm(r, fail.count, fail.count.r, fail.rate)
