library(zoo)

block.size <- 10
good <- 90
bad <- 40
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

# Filled plot
#plot(0:1, type = "n", xlim=c(1,samples), ylim=c(0,100),
#     ylab = "Packet Reception Ratio", xlab = "Sequência",
#     col=1:3)
#abline(h = good, col="orange")
#abline(h = bad, col="red")
#for (i in 1:length(radios)){
#  r = radios[i]
#  LinkQA.r <- subset(LinkQA.total, radio==r);
#  status <- rollapply(as.numeric(LinkQA.r[,"crc"]), width = block.size, sum);
#  status <-  (status / block.size) * 100
#  lines(smooth.spline(status), type = "n")#l", col = colors[i])
#  polygon(
#    c(1:length(status),length(status):1),
#    c(rep(good,length(status)),pmax(good,rev(smooth.spline(status)$y))),
#    col= "darkgreen", border = NA)
#  polygon(
#    c(1:length(status),length(status):1),
#    c(rep(good,length(status)),pmax(bad,pmin(good,rev(smooth.spline(status)$y)))),
#    col= "orange", border = NA)
#  polygon(
#    c(1:length(status),length(status):1),
#    c(rep(bad,length(status)),pmin(bad,rev(smooth.spline(status)$y))),
#    col= "red", border = NA)
#}
#legend("bottomright", legend = c("GOOD","INTERM","BAD"), fill = c("darkgreen","orange","red"), cex = 0.8 )
#rm (i, r, LinkQA.r, status, colors)
