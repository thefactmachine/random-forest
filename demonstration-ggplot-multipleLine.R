library(ggplot2)
library(reshape2)
popular <- subset(movies, votes > 1e4)
ratings <- popular[,7:16]
ratings$.row <- rownames(ratings)
#cluster into six groups on the first 10 columns
cl <- kmeans(ratings[1:10],6)

#reorders 1st argument, based on second argument
#cl$cluster is the cluster number.  popular$rating has 54 unqiue values
#reorder must
ratings$cluster <- reorder(factor(cl$cluster), popular$rating)
# levels(ratings$cluster) ==>  returns characters (1, 5, 6, 2, 3, 4)
# seq_along(levels(ratings$cluster)) returns numeric sequence 1:6
levels(ratings$cluster) <- seq_along(levels(ratings$cluster))
#now returns  levels(ratings$cluster) ==>  returns characters (1, 2, 3, 4, 5, 6)

molten <- melt(ratings, id = c(".row", "cluster"))
#returns 4 rows: .row, cluster, variable, value

#molten <- melt(ratings, id = ".row")

pcp <- ggplot(molten, aes(variable, value, group =  .row, colour = cluster))
jit <- position_jitter(width = 0.25, height = 2.5)
pcp <- pcp + geom_line( alpha = 0.05,  
                       position = jit)
pcp





