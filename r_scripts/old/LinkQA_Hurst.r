LinkQA.failure = subset(LinkQA,crc==FALSE)
LinkQA.sucess = subset(LinkQA,crc==TRUE)

radios <- unique(LinkQA$radio)
samples <- 10000

# Cria dataframe contendo todos os elementos da sequência, marcando pacotes faltantes como crc=FALSE
pkt.total <- data.frame();
pkt.hurst <- data.frame();
for (r in radios){
  parc <- data.frame(seq=setdiff(1:samples,subset(LinkQA.sucess,radio==r)[,"seq"]),crc=FALSE)
  parc <- merge(data.frame(seq=1:samples), parc, by = "seq", all.x = TRUE)
  parc[is.na(parc)] <- TRUE
  parc$radio <- rep (r,nrow(parc))
  pkt.total <- rbind (pkt.total,parc)
  #Compute hurst exponent for each radio
  hurst <- hurstBlock(as.numeric(parc[,"crc"]), method = "aggVar")
  hurst <- data.frame(radio=r,H=hurst[1])
  pkt.hurst <- rbind(pkt.hurst,hurst)
}
rm (r, parc, hurst)

