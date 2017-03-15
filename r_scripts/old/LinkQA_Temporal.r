#LinkQA <- read.csv("~/UNESP-SerendipTec/Projetos/PhyNode_Data/SDLogg/LinkQA/LinkQA_2_1.txt", sep=";")
library("fArma")
library("fractal")

LinkQA.failure = subset(LinkQA,crc==FALSE)
LinkQA.sucess = subset(LinkQA,crc==TRUE)

radios <- unique(LinkQA$radio)
samples <- 7000

# Cria dataframe contendo todos os elementos da sequência, marcando pacotes faltantes como crc=FALSE
LinkQA.total <- data.frame();
LinkQA.hurst <- data.frame();
for (r in radios){
  parc <- data.frame(seq=setdiff(1:samples,subset(LinkQA.sucess,radio==r)[,"seq"]),crc=FALSE)
  parc <- merge(data.frame(seq=1:samples), parc, by = "seq", all.x = TRUE)
  parc[is.na(parc)] <- TRUE
  parc$radio <- rep (r,nrow(parc))
  LinkQA.total <- rbind (LinkQA.total,parc)
  #Compute hurst exponent for each radio
  hurst <- hurstBlock(as.numeric(parc[,"crc"]), method = "aggVar")
  hurst <- data.frame(radio=r,H=hurst[1])
  LinkQA.hurst <- rbind(LinkQA.hurst,hurst)
}
rm (r, parc, hurst)

# Stripchart p/ visualização linear das falhas de transmissão
#par(mfrow=c(3,1))
#for (r in radios){
#  stripchart(subset(LinkQA.total,radio==r & crc==FALSE)[,"seq"], pch = 20, col = "red", xlab = "Tempo", main = r)
#  stripchart(subset(LinkQA.total,radio==r & crc==TRUE)[,"seq"], pch = 20, col = "blue", xlab = "Tempo", main = r, add = TRUE)
#}
#rm (r)

# Densidade kernal do tamanho dos blocos de pacotes perdidos
#par(mfrow=c(3,2))
#for (r in radios){
#  block.sizes <- rle(subset(LinkQA.total,radio==r)[,"crc"])
#  plot(density(block.sizes$lengths[block.sizes$values==FALSE]), xlab = "Falhas consecutivas", main = r, col = "red")
#  plot(density(block.sizes$lengths[block.sizes$values==TRUE]), xlab = "Sucessos consecutivos", main = r, col = "darkblue")
#  #legend("topright" ,legend=c("Sucesso","Falha"), col=c("blue","red"), horiz = TRUE, pch = 15, pt.cex=1.5, cex=0.8)
#}
#rm (r, block.sizes)

# Histograma do tamanho dos blocos
#for (r in radios){
#  block.sizes <- rle(subset(LinkQA.total,radio==r)[,"crc"])
#  hist(block.sizes$lengths[block.sizes$values==FALSE], breaks = 50,  xlab = "Falhas consecutivas", main = r, col = "red")
#  hist(block.sizes$lengths[block.sizes$values==TRUE], breaks = 50, xlab = "Sucessoss consecutivos", main = r, col = "darkblue")
#}
#rm (r, block.sizes)

# Barplot do tamanho dos blocos
#par(mfrow=c(3,1))
#for (r in radios){
#  block <- rle(subset(LinkQA.total,radio==r)[,"crc"])
#  block.ok <- block$lengths[block$values==TRUE]
#  block.err <- block$lengths[block$values==FALSE]
#  block <- merge(
#    data.frame(ok=block.ok),
#    data.frame(err=block.err),
#    by = 0, all = TRUE, sort = FALSE
#  )
#  block <- subset(block, select=c("ok","err"))
#  block[is.na(block)] <- 0
#  block <- do.call(rbind, subset(block, select=c("ok","err")))
#  barplot(block, border = NA, col=c("darkgreen","grey"), beside = TRUE, ylim = c(0,100),
#          legend.text = rownames(block), main  = r)
#}
#rm(r, block, block.ok, block.err)

# Boxplot do tamanho dos blocos
#par(mfrow = c(1,1))
#blocks <- rle(subset(LinkQA.total,radio==radios[1])[,"crc"])
#blocks <- data.frame(crc=blocks$values,size=blocks$lengths)
#boxplot(size~crc, data = blocks, col=c("red", "darkgreen"), 2:1, add=FALSE, xlim = c(1,10), ylim = c(-1,3000), at=2:3, outline = FALSE)
#for (i in 2:length(radios)){
#  blocks <- rle(subset(LinkQA.total,radio==radios[i])[,"crc"])
#  blocks <- data.frame(crc=blocks$values,size=blocks$lengths)
#  boxplot(size~crc, data = blocks, col=c("red", "darkgreen"), add = TRUE, at = ((i*3)-1):(i*3) , outline = FALSE)
#}
#rm (i, blocks)

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
