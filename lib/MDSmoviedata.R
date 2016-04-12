library(MASS)
library(plotly)
setwd("~/Documents/stat4249/project4")
dta<-read.csv("final.data.csv")
movie_info<-read.csv("final_movie.csv")
movie_info<-movie_info[which(movie_info$summary >100),]
movie_info$recommendation<-0
recom<-c("B000J10ER4","B001UV4XFG","B00003CX95","B00005JNTX","B00005JPI2","B000KKQNRO",  
         "B00005JKDQ","B00000G3BX","B004DTLK80" ,"B001HN6940") 
for(i in 1:length(recom)){
  index=which(movie_info$product_productid ==recom[i])
  movie_info[index,]$recommendation<-1  
}  

data<-merge(movie_info,dta,by.x="product_productid",by.y="product_productid")


table<-xtabs(data$review_score~droplevels(data)$product_productid + data$user_id,
             exclude=NULL,na.action=na.pass)

table[table == 0] <- NA
m<-mean(data$review_score)
table.scale<-scale(table)
table.scale[is.na(table.scale)] <- m

dist_matrix<-dist(table.scale, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
non_metric<-cmdscale(dist_matrix,eig=TRUE,k=2)
id_2<-data.frame(id=rownames(non_metric$points))
id_info<-merge(id_2,movie_info,by.x="id",by.y="product_productid",all.x=TRUE)
x1<-non_metric$points[,1]
x2<-non_metric$points[,2]
km<-kmeans(data2d[,1:2],30)
id_km<-names(km$cluster)
km_result<-data.frame(id_km,km$cluster)
data_2<-data.frame(x1,x2,id_info)
data_2<-merge(data_2,km_result,by.x="id",by.y="id_km")
data_2<-data_2[order(data_2$summary,decreasing=TRUE),]
saveRDS(data_2,"2ddata.rds")
saveRDS(table,"tablescore.rds")
