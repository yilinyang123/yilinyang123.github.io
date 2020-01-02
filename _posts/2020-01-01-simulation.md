---
layout: post
title:  "simulation"
date:   2020-01-01
excerpt: "Minimal, one column Jekyll theme for your blog."
project: true
tag:
- jekyll 
- moon
- blog
- about
- theme
comments: false
---

```r
x_axis=c("4","8","16","32","64","128","256","512","1024","2048","4096","8192","16384","32768")
prob=c("0.01","0.05","0.10","0.25","0.5")
prob_label=paste0("p=",prob)
plot(as.vector(abs_error[,1]),xaxt='n',type="b",col="red",lwd=3,pch=20,xlim=c(0,14),ylim=c(0,0.2),xlab="N(log2 scale)",ylab="Absolute Error")
axis(1,at=1:14,labels=x_axis,las=2)
lines(as.vector(abs_error[,2]),type="b",col="blue",lwd=3,pch=20)
lines(as.vector(abs_error[,3]),type="b",col="green",lwd=3,pch=20)
lines(as.vector(abs_error[,4]),type="b",col="purple",lwd=3,pch=20)
lines(as.vector(abs_error[,5]),type="b",col="orange",lwd=3,pch=20)
text(1,abs_error[1,],prob_label,pos=2,cex=0.7)
```
