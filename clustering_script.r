library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(data.table)

retail.df <- raw.data[!is.na(raw.data$AmountSpent),]


clustering.df <- cor.data #make sure you have the current correlation_script before you run this line
dim(clustering.df)[2] # make sure that you get 9 after running this line


# Choosing optimal number of clusters ----------------------------
#first before we run k-Means, let's decide how many clusters we want to generate 
#we can do it in many various ways, I will start with the elbow method: 


########### explanation about WSS ########### 

#let's decide the maximum K to cluster. Say 10: 
k.max <- 10

#we will create a vector of the total within sum of squars, in order to visulize it 
wss <- sapply(1:k.max, function(k){kmeans(clustering.df, k, nstart=50,iter.max = 1000 )$tot.withinss})

options("scipen"=999)
ggplot()+ aes(x = 1:k.max, y = wss) + geom_point() + geom_line()+
  labs(x = "Number of clusters K", y = "Total within-clusters sum of squares")+
  scale_x_continuous(breaks = seq(0,10,1))+
  ggtitle("The Elbow Method")

#We can use the built in function persented to us in class, fviz_nbclust: 
fviz_nbclust(clustering.df, FUN = kmeans,method = "wss" ,nstart = 50)


#When looking at the Elbow Method, one cannot tell for sure what's the optimal 
# number of clusters K. could be either 3 or 4 
#(some would say only 2), therefore we shall look into the silhouette score 
#using the built-in function Optimal_Clusters_KMeans:


########### explanation about silhouette Score ########### 

opt.k.sil<- Optimal_Clusters_KMeans(clustering.df, max_clusters=10, plot_clusters=TRUE, 
                                    criterion="silhouette")
#both 2 and 4 number of clusters generated a high silhouette score of 5.9 
#combining that with the WSS output we can conclude that the optimal number of clusters would be 4. 


########### explanation about Calinski-Harabasz index ########### 

#the final nail in the coffin would be Calinski-Harabasz index between 2 and 4 clusters
km_2k <- kmeans(clustering.df, 2) 
km_4k <- kmeans(clustering.df, 4) 

round(calinhara(clustering.df,km_2k$cluster),digits=1)
round(calinhara(clustering.df,km_4k$cluster),digits=1)
#It is obvious now that 4 clusters would be best and we can move on

# Custering ---------------------------------------------------
#We can start our clustering 
retail.df$History <- NULL
retail.df <- raw.data[!is.na(raw.data$AmountSpent),]

KMC <- kmeans(clustering.df,centers = 4,iter.max = 999, nstart=50) 

retail.clustered <- (cbind(retail.df, cluster= KMC$cluster)) 
# Create new DF, # consisted with the original DF 
# with the cluster number for each observation

table_of_cluster_distribution <- table(retail.clustered$cluster) # the result:
# 1   2   3   4 
# 157 285 283 269 

barplot(table_of_cluster_distribution, xlab="Clusters", 
        ylab="# of customers", main="# of customers in each cluster",
        col="#69b3a2")

retail.clustered <- data.table(retail.clustered)
retail.clustered[, avg_AmountSpent_in_cluster := mean(AmountSpent),by=list(cluster)]
retail.clustered[, avg_SalarySpent_in_cluster := mean(Salary),by=list(cluster)]

retail.clustered  <-  retail.clustered[, c("Age", "Gender", "OwnHome", "Married",                   
         "Location", "Children", "Catalogs", "Salary","AmountSpent", 
         "avg_AmountSpent_in_cluster", "avg_SalarySpent_in_cluster", "cluster" )]

cluster_1 <- retail.clustered[retail.clustered$cluster==1,]
cluster_2 <- retail.clustered[retail.clustered$cluster==2,]
cluster_3 <- retail.clustered[retail.clustered$cluster==3,]
cluster_4 <- retail.clustered[retail.clustered$cluster==4,]

#View(cluster_1)
lapply(retail.clustered[,1:7],table)

data.with.clustering <- cbind(clustering.df, retail.clustered)
#View(data.with.clustering)


clustering.df.n<-normalize(clustering.df)

normalize(x, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")

#View(clustering.df.n)

dis<-dist(clustering.df.n) # dissimilarity matrix
dis2<-sim2diss(dis, method=1, to.dist = TRUE)

#mantel.test(as.matrix(dis), as.matrix(dis2))

mds_fit <-mds(dis2, ndim=3,  type="ordinal") # We use the type of "ordinal", 
#since most of the vars in these Dataset are categorical, redcued 5 vars into 2
