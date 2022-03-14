library(readxl)
library(reshape2)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/selim.unal/Documents/Private/OzU/Project")
workdata=as.data.frame(read_excel("data_new.xlsx"))
tickers=unique(workdata$ticker)

graphs_mcap=list()
graphs_vol=list()

for (x in 1:24){
  temp=workdata[workdata$ticker==tickers[x],]
  t_temp=temp[,c(1,5,6,9)]
  t_temp=melt(t_temp, value.name="Values", id="date")
  t_temp=t_temp[order(t_temp$date),]
  t_temp$date=as.Date(t_temp$date)
  graphs_mcap[[x]] <- ggplot(data=t_temp, aes(fill=variable, y=Values/1000, x=date, label=paste(round(Values/1000,2))))  +
    geom_bar(position="stack", stat="identity",width=30) + ylab("billions TL") + 
    geom_text(size = 3, position = position_stack(vjust = 0.5))+
    labs(title = paste("Asset, Equity and Debt for", tickers[x])) 
  graphs_vol[[x]]<-ggplot(data=temp, aes(y=probability_of_default*10000, x=date, label=paste(round(probability_of_default*10000,1))))  +
    geom_line(size=2, colour="red") + ylab("Basis Points") + 
    geom_text(size = 3, position = position_stack(vjust = 1.05))+
        labs(title = paste("Default Probability Basis Points for", tickers[x])) 
    
}

ggsave(
  filename = "MCap.pdf", 
  plot = marrangeGrob(graphs_mcap, nrow=1, ncol=1), 
  width = 15, height = 9
)
for (x in 1:24){
  png(paste0(tickers[x],".png"), width = 960, height = 480, units = "px", pointsize = 12,)
  print(graphs_mcap[[x]])
  dev.off()
}

for (x in 1:24){
  png(paste0(tickers[x],"p.png"), width = 960, height = 480, units = "px", pointsize = 12,)
  print(graphs_vol[[x]])
  dev.off()
}

ggsave(
  filename = "Vol.pdf", 
  plot = marrangeGrob(graphs_vol, nrow=1, ncol=1), 
  width = 15, height = 9
)
dates=unique(workdata$date)
graphs_dist=list()
quarter=c("2016-4",
           "2017-1","2017-2","2017-3","2017-4",
           "2018-1","2018-2","2018-3","2018-4",
           "2019-1","2019-2","2019-3","2019-4",
           "2020-1","2020-2","2020-3","2020-4",
           "2021-1","2021-2","2021-3"
                      )

for (x in 1:20){
  temp=workdata[workdata$date==dates[x],]
  temp$ratio=temp$debt / temp$asset_value*100
#  temp=temp[temp$probability_of_default*10000>10,]
  graphs_dist[[x]] <- ggplot(data=temp, aes(x=ratio, y=probability_of_default*10000))  +
    geom_point(size=1) + ylab("Default Probability Basis Points") + xlab("Debt/Asset Ratio %") + 
    #geom_text(size = 3,position = position_dodge(width = 1), vjust=-0.5)+ 
    geom_text_repel(aes(label=ticker),max.overlaps = Inf) +
    labs(title = paste("Debt/Asset Ratio  vs Default Probability for ", quarter[x]," Reporting Period")) 
  
}

ggsave(
  filename = "Com.pdf", 
  plot = marrangeGrob(graphs_dist, nrow=1, ncol=1), 
  width = 15, height = 9
)

for (x in 1:20){
  png(paste0(x,".png"), width = 960, height = 480, units = "px", pointsize = 12,)
  print(graphs_dist[[x]])
  dev.off()
}
