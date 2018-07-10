#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html


library(jpeg)
library(ggplot2)
library(psych)
library(img.compression)
library(fmsb)
#####Functions##############################
#1. Pic Format
#2. Pic Structure
#3. Color Scheme

############################################MAIN################################################
img <- readJPEG("C:/Users/maximilian.lacher/Downloads/sunset.jpg")

#img <- readJPEG("E:/Users/lacher/Documents/GitHub/Photo-and-Web-Mining/sunset.jpg")
x_Pixel<-img[1,,1]
y_Pixel<-img[,1,1]
x_Size<-length(x_Pixel)
y_Size<-length(y_Pixel)

x_Grid<-Div_Raster(x_Size, 20)
y_Grid<-Div_Raster(y_Size, 18)

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
                                   Cluster_result2$sd,
                                   Cluster_result3$sd,
                                   RowCol1
                                   )
colnames(Cluster_result)<-c("med_red","med_green","med_blue","sd_red","sd_green","sd_blue","Xaxis","Yaxis")
Cluster_result$Yaxis<-Cluster_result$Yaxis*-1

ggplot(Cluster_result , aes(x=Xaxis,y=Yaxis))+
  geom_tile(aes(fill=rgb(med_red,med_green,med_blue)))+
  scale_fill_identity()+
  theme_minimal()

ggplot(Cluster_result, aes(x=(med_red+med_green+med_blue)/3))+
  geom_histogram(aes(fill=rgb(med_red,med_green,med_blue)))+
  scale_fill_identity()+
  theme_minimal()

ggplot(Cluster_result, aes(x= Yaxis, y = (sd_red+sd_blue+sd_green)/3))+
  geom_smooth()+
  theme_minimal()


#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}

  rgb(Cluster_result$med_blue, Cluster_result$med_red, Cluster_result$med_green, maxColorValue=1)

