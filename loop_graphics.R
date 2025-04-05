setwd("F:/Gilson/Programacao/R")
library(ggplot2)
library(readxl)

d<- read_excel("F:/Gilson/Programacao/R/dados/teste.xlsx")
d<-as.data.frame(d)
d[,1]<-as.factor(d[,1])

for(i in 2:length(d))
{
  ggplot(d, aes(d[,1],d[,i])) + geom_boxplot(outlier.shape = NA) +  
    labs(x = names(d)[1], y = names(d)[i], fill = NULL, 
         title = paste(names(d)[1],"x",names(d)[i])) +
    scale_x_discrete(labels=levels(d[,1])) + 
    geom_jitter(width = 0.1, color="black") + 
    annotate("text",x=1:length(levels(d[,1])),
             y=max(d[,i])+sd(d[,i]),
    label = paste("n =",table(d[,1])))
  ggsave(paste(names(d)[i],".jpg"),dpi = 300)
}

for(i in 2:length(d))
{
  png(paste("teste",i,".png"))
  boxplot(d[,i] ~ d[,1], data = d, outpch = NA,
          ylab=names(d)[i],xlab=names(d)[1],
          ylim=c(round(min(d[,i]),0),round(max(d[,i])+sd(d[,i]),0)))
  text(1:length(levels(d[,1])),round(max(d[,i])+sd(d[,i]),0),
       paste("n =",table(d[,1])))
  stripchart(d[,i] ~ d[,1], data = d, vertical = TRUE,
             method = "jitter", pch = 21, col = "black",
             bg = "black", add = TRUE)
  dev.off()
}