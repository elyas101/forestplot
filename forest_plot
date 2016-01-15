
load(data)
names(data) <- c("study","beta","se","p")

library(metafor)

data$pnum <- 0
data$pnum <-  signif(data$p, digits = 3)

data$author <- as.character(data$author)

# Assign values for plotting
labs <- data$group
yi   <- data$beta
sei  <- data$se
 
data$pnum <- format(data$pnum, scientific=TRUE,digits=3)


# Combine data into summary estimate

res  <- rma(yi=yi, sei=sei, method="DL",slab=author,data=data)
summary(res)


png(file="forestplot.png", units="in", width=9, height=6.5, res=800)
 
par(mar=c(4,4,1,2))
 
op <- par(cex=.9, font=1)
par(op)
par(mar=c(4,4,1,2))

### set up forest plot (with 2x2 table counts added; rows argument is used
### to specify exactly in which rows the outcomes will be plotted)

par(cex=.9, font=1)
forest(res, xlim=c(-3.5,3.5), at=log(c(0.5,1,2.5)), atransf=exp,
       ilab=cbind(data$pnum),
       ilab.xpos=c(2), cex=.90, ylim=c(-1, 27),
       rows=c(21,18:16,12:10,6:4),
       xlab="Odds Ratio", mlab=paste("RE Model, all strata ", "[",  expression(I2),"=",round(summary(res)$I2,2),"%, ","Heterogeneity p=",round(summary(res)$QEp,2),"]",sep=""))
     
       
### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'

op <- par(cex=.9, font=2)
 
 
 par(font=4)
### add text for the subgroups
text(-3.5, c(22,19,13,7), pos=4, c("EXAMPLE1","EXAMPLE2","EXAMPLE3","EXAMPLE4"))
                             
 
### switch to bold font
par(font=2)
 
### add column headings to the plot
text(c(2), 26, c("p"))

text(-3.5,26, "EXAMPLE, Study",pos=4)
text(3.5,26, "Odds Ratio [95% CI]",pos=2)
 
### set par back to the original settings

 
### fit random-effects model in the three subgroups

yi1 <- yi[2:4]
yi2 <- yi[5:7]
yi3 <- yi[8:10]


sei1 <- sei[2:4]
sei2 <- sei[5:7]
sei3 <- sei[8:10]


res.s  <- rma(yi=yi1, sei=sei1, method="DL")
res.r  <- rma(yi=yi2, sei=sei2, method="DL")
res.a  <- rma(yi=yi3, sei=sei3, method="DL")



par(font=4)
 
### add summary polygons for the three subgroups
addpoly(res.s, row=14.5, cex=.9, atransf=exp, mlab="RE Model for stratum")
addpoly(res.r, row= 8.5, cex=.9, atransf=exp, mlab="RE Model for stratum")
addpoly(res.a, row= 2.5, cex=.9, atransf=exp, mlab="RE Model for stratum")


par(font=3)

text(2,-1.7, paste( signif(summary(res)$pval,digits = 3)),pos=3,cex=.9)
text(2,13.9, signif(summary(res.s)$pval,digits = 3),pos=3,cex=.9)
text(2,7.9,signif(summary(res.r)$pval,digits = 3),pos=3,cex=.9)
text(2,1.9,format(summary(res.a)$pval,scientific=TRUE,digits=3),pos=3,cex=.9)
dev.off()

