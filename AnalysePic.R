#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html


library(jpeg)
library(ggplot2)
library(psych)
library(img.compression)
#####Functions##############################


############################################MAIN################################################
img <- readJPEG("C:/Users/maximilian.lacher/Downloads/test2.jpg")

#img <- readJPEG("E:/Users/lacher/Documents/GitHub/Photo-and-Web-Mining/sunset.jpg")
x_Pixel<-img[1,,1]
y_Pixel<-img[,1,1]
x_Size<-length(x_Pixel)
y_Size<-length(y_Pixel)
x_Grid<-Div_Raster(x_Size, 40)
y_Grid<-Div_Raster(y_Size, 22)

img_c<-Clustered_Pic(img,y_Size,x_Size)

Cluster_result1<-describeBy(as.numeric(img_c[,,1]), group = img_c[,,4],mat=TRUE)
Cluster_result2<-describeBy(as.numeric(img_c[,,2]), group = img_c[,,4],mat=TRUE)
Cluster_result3<-describeBy(as.numeric(img_c[,,3]), group = img_c[,,4],mat=TRUE)
RowCol<-matrix(unlist(strsplit(as.character( Cluster_result3$group1 ), "/")),ncol=2, byrow=TRUE)
RowCol<- as.data.frame(RowCol)
RowCol1<- cbind(as.numeric(levels(RowCol$V1))[RowCol$V1],
            as.numeric(levels(RowCol$V2))[RowCol$V2])
Cluster_result <- cbind.data.frame(Cluster_result1$median,
                                   Cluster_result2$median,
                                   Cluster_result3$median,
                                   Cluster_result1$sd,
                                   RowCol1
                                   )
colnames(Cluster_result)<-c("med_red","med_green","med_blue","sd","Xaxis","Yaxis")
Cluster_result$Yaxis<-Cluster_result$Yaxis*-1

ggplot(Cluster_result , aes(x=Xaxis,y=Yaxis))+
  geom_tile(aes(fill=rgb(med_red,med_green,med_blue)))+
  scale_fill_identity()+
  theme_minimal()

ggplot(Cluster_result, aes(x=Xaxis,y=Yaxis))+
  geom_point(aes(size = sd, color = sd))


#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}

