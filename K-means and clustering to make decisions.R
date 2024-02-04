#Neemias Moreira
#Date 11/27/2023

# This project has the goal to answer 3 questions: 

#1. Conduct a K-means clustering analysis on mtcars. Use the wss elbow plot method to identify the best
#value of k. Run the clustering at this value of k and one other values of k. Compare the results

####
#2. Conduct a Hierarchical Cluster analysis of the mtcars data using both AGNES and DIANA and the
#“average” linking function. Compare the two dendrograms noting any differences.

####
#3. You are in the market for a Camero Z28 but production of this car has ceased. You use HC cluster
#analysis to identify the car that are most similar to the Z28. According to your AGNES HC analysis (pick
#one linkage), what car(s) are most similar to the Z28? If those cares are not available, which are the next
#most similar?



## loading lib
library(stats)
library(ggplot2)
library(factoextra)
library(corrplot)
library(cluster)
library(cluster)
library(dendextend)

# Defining data-set
carshw3 <- mtcars
head(carshw3)
dfhw3 <- scale(carshw3)

#1-Conduct a K-means clustering analysis on mtcars. Use the wss elbow plot method to identify the best
#value of k. Run the clustering at this value of k and one other values of k. Compare the results:


set.seed(123)
carsr <- sample(1:30, 5)
carsr1 <- dfhw3[carsr,]
head(carsr)

distE <- dist(carsr1, method='euclidean')
head(distE)
round(as.matrix(distE)[1:5, 1:5], 1)

fviz_dist(distE)

wss <- sapply(1:carsr, function(k){kmeans(mtcars, k, nstart=10, iter.max=5)$tot.withinss})
plot(1:carsr, wss, type="b", pch =19, frame=FALSE, xlab="Number of Clusters K", ylab="Total within-clusters sum of squares")

fviz_nbclust(mtcars, kmeans, method="wss") + geom_vline(xintercept =4, linetype= 5, col= "darkred")

km.res <- kmeans(dfhw3, 4, nstart = 25)
km.res

km.res$totss
km.res$betweenss

km.res$betweenss/km.res$totss


dfhw3_member <- cbind(mtcars, cluster = km.res$cluster)
head(dfhw3_member)

fviz_cluster(km.res, data=mtcars)

fviz_cluster(km.res, data = mtcars,
             palette=c("red", "blue", "black","brown"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())

## Testing with 5 clusters

km.res <- kmeans(dfhw3, 5, nstart = 25)
km.res

km.res$totss
km.res$betweenss

km.res$betweenss/km.res$totss

dfhw3_member <- cbind(mtcars, cluster = km.res$cluster)
head(dfhw3_member)

fviz_cluster(km.res, data=mtcars)

fviz_cluster(km.res, data = mtcars,
             palette=c("red", "blue", "black","brown","pink"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())

###########################################################################
#On elbow test the best was K- Means 4 and I tested with K-means 5.
#After further analysis the best number of K is with 4 clusters.
############################################################################


#2. Conduct a Hierarchical Cluster analysis of the mtcars data using both AGNES and DIANA and the
#“average” linking function. Compare the two dendrograms noting any differences.


library(cluster)
library(dendextend)

res.agnes <- agnes(x=mtcars, stand = TRUE, 
                   metric = "euclidean", method="ward")

res.agnes

res.diana <- diana(x=mtcars, stand = TRUE, 
                   metric = "euclidean")
res.diana

fviz_dend(res.agnes, cex=0.6, k=4)
fviz_dend(res.diana, cex=0.6, k=4)

set.seed(113)
ss <- sample (1:30, 5)
df1 <- dfhw3[ss,]


res.dist1 <- dist(df1, method="euclidean")

hcl1 <- hclust(res.dist1, method="average")
hcl2 <- hclust(res.dist1, method="ward.D2")
dend1 <- as.dendrogram(hcl1)
dend2 <- as.dendrogram(hcl2)

dend_list <- dendlist(dend1, dend2)

dendlist(dend1, dend2) %>% 
  untangle(method="step1side") %>%
  tanglegram()

dendlist(dend1, dend2) %>% 
  untangle(method="step1side") %>%
  entanglement()


dendlist(dend1, dend2) %>% 
  untangle(method="step1side") %>%
  tanglegram(highlight_distinct_edges=FALSE, 
             common_subtrees_color_lines = FALSE, 
             common_subtrees_color_branches = TRUE)

cor.dendlist(dend_list, method="cophenetic")
cor_cophenetic(dend1, dend2)


############################################################################
#There are some differences between DIANA and AGNES.
#For example: In the DIANA method for example, the Porsche 914-2 is on the  
# 1 cluster. And on the AGNES it's on the 3 cluster.
############################################################################


#3-You are in the market for a Camaro Z28 but production of this car has ceased.
#You use HC cluster
#analysis to identify the car that are most similar to the Z28. 
#According to your AGNES HC analysis (pick one linkage),
#what car(s) are most similar to the Z28? If those cares are not available, which are the next
#most similar?

fviz_dend(res.agnes, cex=0.6, k=4)

###############################################################################
# According to HC Agnes the car Duster 360 is the most similar to Camaro Z28
# After Duster the most similar is the Chrysler Imperial.
###############################################################################
