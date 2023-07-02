rm(list=c())
if (!require("RecordLinkage", quietly = TRUE))
  install.packages("RecordLinkage")
if (!require("lava", quietly = TRUE))
install.packages('lava')
library(RecordLinkage)
# -------------------------------------------------------------------------
data=read.csv(file.choose())
# Replace "data=read.csv(choose.files())" if you are using windows.
n=nrow(data);c=ncol(data)

best_order<-function(){
id=1
distance<-matrix(NA,nrow=n*(n-1)/2,ncol=c)
pair=t(combn(1:nrow(data),2))
var=matrix("", nrow(pair), 2)
similarity=matrix(,nrow=nrow(data),ncol=nrow(data))
dimnames(similarity) <- list(data[,id], data[,id])
## normalize
df=data
for(i in 1:c)
  if(is.numeric(data[,i])) {
    x=data[,i]
    df[,i]=(x-min(x))/(max(x)-min(x))
  }

## calculate distance
for(k in 1:c){
  for(i in 1:nrow(pair)){
  t=pair[i,]
  if(is.numeric(df[,k])) distance[i,k]=abs(df[t[1],k]-df[t[2],k]) else
 distance[i,k]=1-levenshteinSim(df[t[1],k],df[t[2],k])  
  var[i,]=df[t,1]
   }
}
## least score most similarity
distance
## The first way
mean_distance=rowMeans(distance)
mean_distance
out <- data.frame(var,mean_distance)
out
for(i in 1:nrow(pair))
similarity[var[i,, drop=FALSE]]<-1-mean_distance[i]

k=nrow(df)-1
km=kmeans(1-as.dist(t(similarity)), k)
cluster=hclust(1-as.dist(t(similarity)))
#plot(cluster)
ord=cluster$order
output=data.frame(ord,data[,])
return(output[order(output$ord,decreasing = TRUE),])
}
res1=best_order()
res1
write.table(res1,file=paste("Recomendation1",".csv"),sep=",",row.names =FALSE )

# part 2 ------------------------------------------------------------------
install.packages('stringr')
library(stringr)
propose<-function(){
  item=readline(prompt="Enter item search with respect to :")
  if(str_count(item, "\\w+")==0)  stop("Enter correct input")
  ans=readline(prompt="Is that integer: yes or no?")
  if(ans!="yes" & ans!="no")   stop("Enter correct input") 
  if(ans=="yes") item=as.integer(item)
  col.ind=as.integer(readline(prompt="Enter its column index:"))
  distance=c()
  for(i in 1:n)
    if(is.numeric(data[,col.ind])) distance[i]=abs(data[i,col.ind]-item) else
      distance[i]=1-levenshteinSim(data[i,col.ind],item)  
   return(data[order(distance),])  
}
propose()
