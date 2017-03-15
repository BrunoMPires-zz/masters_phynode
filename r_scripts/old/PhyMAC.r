#****************************** OPTIMIZATION ******************************#
cat.verygood <- 25
cat.good <- 8
cat.average <- 5
cat.bad <- 1

tmp <- subset(LinkQA.total, select = c("radio","seq"))
tmp[which(tmp$seq == 1),"en"] <- T
for (i in 1:(samples-1)){
  current <- subset(LinkQA.total, seq == i)
  best <- max(current$lqemod)
  if (best >= cat.verygood){ # If at least 1 radio is 100% reliable, use only one radio.
    enabled <- current[which(current$lqemod >= best),"radio"]
    tmp[which((tmp$radio == enabled[1]) & (tmp$seq == (i + 1))),"en"] <- T
  }else if (best >= cat.average){ # If multiple radios are good/average sum them up
    enabled <- current[which(current$lqemod >= cat.average),"radio"] 
    tmp[which((tmp$radio %in% enabled) & (tmp$seq == (i + 1))),"en"] <- T
  }else { # If no good link is available, use all
    tmp[which(tmp$seq == (i + 1)),"en"] <- T
  }
}
tmp[is.na(tmp)] <- F
LinkQA.total <- merge(LinkQA.total, tmp, all.x = T, sort = F)
rm (tmp, i, current, best, enabled, cat.verygood, cat.good, cat.average, cat.bad)
#LinkQA.total <- merge(LinkQA.total, tmp, all.x = T, sort = F)

plot(LinkQA.total$en, type = "n", ylab = "", xlim = c(1,samples), ylim = c(0,4), main = "PhyMac")
i <- 1;
for (r in radios){
  en <- as.numeric(subset(LinkQA.total, radio == r)$en)
  points(en * i, col = rainbow(3)[i], cex = 0.05)
  points(en * 4, cex = 0.01)
  i <- i + 1
}
rm (i, r, en)

count <- c()
for (i in 1:samples){
  count <- c(count,nrow(subset(LinkQA.total, seq == i & en == T)))
}
plot(count, type="p", cex = 0.01)

