
#===================================================================
#graph preparation stuff
dfRes <- as.data.frame( t(matRes))
expNum <- row.names(dfRes)
dfRes <- cbind(expNum, dfRes)
rowNum <- 1:nrow(dfRes)
row.names(dfRes) <- rowNum
library(reshape2)
numCols <- length(colnames(dfRes))
colsToDisp <- colnames(dfRes)[2:numCols]
dfResLong <-melt(dfRes, id = "expNum", measure = colsToDisp)
#===================================================================
#graph stuff

theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
  legend.position="none",
  panel.grid.minor=element_blank(),
  panel.grid.major=element_blank(),
  plot.background = element_rect(fill="black",color = NA), 
  panel.background=element_rect(fill="black",color = NA),   
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  panel.border=element_blank()
)


p <- ggplot(dfResLong, aes(variable, value, group = expNum)) 
jitr <- position_jitter(width = 0.04, height = 0)
p <- p + geom_line(colour = "yellow", alpha = 0.06, position = jitr)  
p
#=============================================================

scaledDFRes <- as.data.frame(scale(dfRes[,-1]))
clustNumber <- kmeans(scaledDFRes, 3)$cluster
dfRes$cluster <- clustNumber 
#re-orders the cluster according to the highest scores
dfRes$cluster <- reorder(factor(clustNumber), dfRes$sex)
levels(dfRes$cluster) <- seq_along(levels(dfRes$cluster))



numCols <- length(colnames(dfRes))
colsToDisp <- colnames(dfRes)[2:numCols]
#don't need to set "measure" argument to anything
dfResLongNew <-melt(dfRes, id = c("expNum", "cluster"))
dfResLongNew$cluster <- factor(dfResLongNew$cluster)

#pos 3 = middle,  pos 2 = highest, pos 1 - lowest


colV <- c("#0099ff", "#ff9900", "#ff0000")

p <- ggplot(dfResLongNew, aes(variable, value, group = expNum, colour = cluster)) 
jitr <- position_jitter(width = 0.04, height = 0)
p <- p + geom_line( alpha = 0.08, position = jitr)  
p <- p + scale_color_manual(values = colV)
p












 
