library(zoo)

radios <- unique(LinkQA$radio)
samples <- 8000
block.size <- 10
good <- 90
bad <- 40

LinkQA.sucess = subset(LinkQA,crc==TRUE)
LinkQA.failure = subset(LinkQA,crc==FALSE)

# Frequência de recebimento vs Número de interfaces
LinkQA.pkt.count <- as.data.frame(table(LinkQA.sucess$seq))
colnames(LinkQA.pkt.count) <- c("seq","count")
LinkQA.if.cumul <- as.data.frame(table(LinkQA.pkt.count$count))
colnames(LinkQA.if.cumul) <- c("cnt","freq")
Total.fail <- samples - sum(LinkQA.if.cumul$freq)
LinkQA.if.cumul <- rbind(data.frame(cnt="0", freq=Total.fail), LinkQA.if.cumul)
barplot(LinkQA.if.cumul$freq, names.arg = LinkQA.if.cumul$cnt,
        xlab = "Nº de Interfaces", ylab ="Frequência",
        col=c("red","darkblue","darkorange","darkorange"))

# Error rates
LinkQA.per <- data.frame(
  radio="PhyNode",
  rate=((Total.fail/ samples) * 100)
)

for (r in radios){
  fail.count.r <- samples - nrow(subset(LinkQA.sucess,radio==r))
  fail.rate <- (fail.count.r / samples) * 100
  LinkQA.per <- rbind(LinkQA.per,data.frame(radio=r,rate=fail.rate))
}
barplot(LinkQA.per$rate, ylab = "Taxa de falha", xlab = "Interface",
        ylim=c(0,100),
        names.arg = LinkQA.per$radio,
        col=c("darkblue","darkorange","darkorange","darkorange"))
rm(r, fail.count.r, fail.rate)

# Cria dataframe contendo todos os elementos da sequência, marcando pacotes faltantes como crc=FALSE
LinkQA.total <- data.frame();
for (r in radios){
  parc <- data.frame(seq=setdiff(1:samples,subset(LinkQA.sucess,radio==r)[,"seq"]),crc=FALSE)
  parc <- merge(data.frame(seq=1:samples), parc, by = "seq", all.x = TRUE)
  parc[is.na(parc)] <- TRUE
  parc$radio <- rep (r,nrow(parc))
  LinkQA.total <- rbind (LinkQA.total,parc)
}
LinkQA.total <- merge(LinkQA.total, LinkQA, all.x = T)
rm (r, parc)

# Split BoxPlots do tamanho dos blocos
par(mfrow = c(1,3))
for (r in radios){
  blocks <- rle(subset(LinkQA.total,radio==r)[,"crc"])
  blocks <-  data.frame(crc=blocks$values,size=blocks$lengths)
  boxplot(size~crc, data = blocks, 
          col=c("red","darkgreen"),
          main = r, xlab = "CRC", ylab = "Nº de ocorrências consecutivas",
          names=c("FALHA","SUCESSO"),
          range = 0.2, outline = FALSE)
}
rm(r, blocks)

# History
colors = c("purple","darkgrey","blue")
par(mfrow=c(1,1))
plot(0:1, type = "n", xlim=c(1,samples), ylim=c(0,100),
     ylab = "Packet Reception Ratio", xlab = "Sequência",
     col=1:3)
abline(h = good, col="darkgreen")
abline(h = bad, col="red")
for (i in 1:length(radios)){
  r = radios[i]
  LinkQA.r <- subset(LinkQA.total, radio==r);
  status <- rollapply(as.numeric(LinkQA.r[,"crc"]), width = block.size, sum);
  status <-  (status / block.size) * 100
  lines(smooth.spline(status), type = "l", col = colors[i])
}
legend("bottomright", legend = radios, fill = colors, cex = 0.8 )

rm (i, r, bad, good, colors, status, LinkQA.r)

###################################################################################################
#########                            Análise pós otimização                              ##########
###################################################################################################

# An Ideal Path
# LinkQA.ideal <- LinkQA.sucess[match(unique(LinkQA.sucess$seq), LinkQA.sucess$seq),]

LinkOPT$en <- rep(TRUE,nrow(LinkOPT))
LinkOPT.total <- merge(LinkQA.total, LinkOPT, all.x = TRUE)
LinkOPT.total[is.na(LinkOPT.total)] <- FALSE

# Optmized error rates
LinkOPT.per <- data.frame();
enabled <- nrow(subset(LinkOPT.total, en == TRUE));
Opt.fail <- nrow(subset(LinkOPT.total, en == TRUE & crc == FALSE));
Opt.sucess <- nrow(subset(LinkOPT.total, en == TRUE & crc == TRUE));
rate <- (fail / enabled) * 100
LinkOPT.per <- rbind(LinkOPT.per, data.frame(radio="PhyNode", per = rate, en = enabled, err = Opt.fail, ok = Opt.sucess))
for (r in radios){
  enabled <- nrow(subset(LinkOPT.total, radio == r & en == TRUE));
  fail <- nrow(subset(LinkOPT.total, radio == r & en == TRUE & crc == FALSE));
  sucess <- nrow(subset(LinkOPT.total, radio == r & en == TRUE & crc == TRUE));
  rate <- (fail / enabled) * 100
  LinkOPT.per <- rbind(LinkOPT.per, data.frame(radio=r,per=rate, en = enabled, err = fail, ok = sucess))
}

# Optmized Frequência de recebimento vs Número de interfaces
LinkOPT.pkt.count <- as.data.frame(table(subset(LinkOPT.total, en == TRUE & crc == TRUE)[,"seq"]))
colnames(LinkOPT.pkt.count) <- c("seq","count")
LinkOPT.if.cumul <- as.data.frame(table(LinkOPT.pkt.count$count))
colnames(LinkOPT.if.cumul) <- c("cnt","freq")
Opt.fail <- Opt.sucess - sum(LinkOPT.if.cumul$freq)
LinkOPT.if.cumul <- rbind(data.frame(cnt="0", freq=Opt.fail), LinkOPT.if.cumul)
barplot(LinkOPT.if.cumul$freq, names.arg = LinkOPT.if.cumul$cnt,
        xlab = "Nº de Interfaces", ylab ="Frequência",
        col=c("red","darkblue","darkorange","darkorange"))

# Backup prr / lqi
prr <- rollapplyr(as.numeric(subset(LinkQA.total, radio == "915FSK")[,"crc"]), width = 100, partial = 1, sum)
lqi <- rollapplyr(subset(LinkQA.total, radio == "915FSK")[,"rssi"], width = 100, partial = 1, mean, fill = NA)

# Windowed PRR
lost <- rep(0,block.size)
last <- 0
wprr <- data.frame()
Link <- subset(LinkQA.sucess, radio == "915FSK")
for (i in 1:nrow(Link)){
  seq <- Link[i,"seq"]
  if (i < block.size){
    lost[i] <- seq - (last + 1)
    wprr <- rbind(wprr, data.frame(seq=seq, prr=(i/(i+sum(lost)))*100))
  }else{
    lost <- c(lost[2:block.size], seq - (last + 1))
    wprr <- rbind(wprr, data.frame(seq=seq, prr=(block.size / (block.size + sum(lost)))*100))
  }
  last <- seq
}
lqi <- rollapplyr(Link[,"lqi"], width = block.size, partial = 1, mean)
snr <- rollapplyr(Link[,"snr"], width = block.size, partial = 1, mean)
plot(lqi,wprr[,"prr"])
abline(lm(wprr[,"prr"] ~ lqi))

#Triangle LQE
Tri.w <- 100
Tri.link <- subset(LinkQA.sucess, radio == "2400OQPSK")
Tri.snr <- rollapplyr(Tri.link[,"snr"], width = Tri.w, partial = 1, sum)
Tri.lqi <- rollapplyr(Tri.link[,"lqi"], width = Tri.w, partial = 1, sum)
Tri.lost <- rep(0,nrow(Tri.link))
Tri.prr <-  rep(0,nrow(Tri.link))
last <- 1999
for (i in 1:nrow(Tri.link)){
  seq <- Tri.link[i,"seq"]
  Tri.lost[i] <- seq - (last + 1)
  block <- min(i,Tri.w)
  Tri.prr[i] <- block / (block + Tri.lost[i])
  last <- seq;
}
Tri.lost <- rollapplyr(Tri.lost, width = Tri.w, partial = 1, sum)
n <- 0
for (i in 1:nrow(Tri.link)){
  if (i < Tri.w){
    n = i + Tri.lost[i]
  } else {
    n = Tri.w + Tri.lost[i]
  }
  Tri.snr[i] <- Tri.snr[i] / n
  Tri.lqi[i] <- Tri.lqi[i] / n
}
Tri <- rep(0,nrow(Tri.link))
for(i in 1:nrow(Tri.link)){
  Tri[i] <- sqrt((Tri.snr[i] ^ 2) + (Tri.lqi[i] ^ 2))
}
Tri.norm <- (Tri-min(Tri))/(max(Tri)-min(Tri))
plot(Tri,Tri.prr)
abline(lm(Tri.prr ~ Tri))