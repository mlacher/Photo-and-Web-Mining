#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html
#https://www.kandooadventures.com/news-and-views/top-10-instagram-landscape-photographers/

library(jpeg)
library(ggplot2)
library(psych)
library(img.compression)
library(fmsb)
library(gridExtra)
#####Functions##############################
#1. Pic Format
#2. Pic Structure
#3. Color Scheme

############################################MAIN################################################
img <- readJPEG("C:/Users/maximilian.lacher/Downloads/test.jpg")
#
#img <- readJPEG("E:/Users/lacher/Documents/GitHub/Photo-and-Web-Mining/sunset.jpg")
x_Pixel<-img[1,,1]
y_Pixel<-img[,1,1]
x_Size<-length(x_Pixel)
y_Size<-length(y_Pixel)

x_Grid<-Div_Raster(x_Size, 20)
y_Grid<-Div_Raster(y_Size, 18)

img_c<-Clustered_Pic(img,y_Size,x_Size)
Cluster_result<-Analyse_Pic(img_c)

ggplot(Cluster_result , aes(x=Xaxis,y=Yaxis))+
  geom_tile(aes(fill=rgb(med_red,med_green,med_blue)))+
  scale_fill_identity()+
  theme_minimal()

test <-Cluster_result[((Cluster_result$sd_mean/max(Cluster_result$sd_mean))>0.35),]
ggplot(test, aes(x= Xaxis, y = Yaxis))+
  geom_point()+
  ylim(-250,0)+
  theme_minimal()



combo.box <- grid.arrange(p1,p2, nrow = 2)

#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}

  rgb(Cluster_result$med_blue, Cluster_result$med_red, Cluster_result$med_green, maxColorValue=1)


