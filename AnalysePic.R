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
---------------------------

files = list.files(path = "C:/Users/maximilian.lacher/Documents/GitHub/Photo-and-Web-Mining", pattern="*.jpg")
Path <- "C:/Users/maximilian.lacher/Documents/GitHub/Photo-and-Web-Mining"
#Cluster_result <- needs to be defined
img <- readJPEG("C:/Users/maximilian.lacher/Downloads/test.jpg")
i=1
#img <- readJPEG("E:/Users/lacher/Documents/GitHub/Photo-and-Web-Mining/sunset.jpg")
while (i <= length(files)){
location <- paste(Path,files[i],sep= "/")
img <- readJPEG(location)
x_Pixel<-img[1,,1]
y_Pixel<-img[,1,1]
x_Size<-length(x_Pixel)
y_Size<-length(y_Pixel)
x_Grid<-Div_Raster(x_Size, 20)
y_Grid<-Div_Raster(y_Size, 18)

img_c<-Clustered_Pic(img,y_Size,x_Size)
Pic_result<-Analyse_Pic(img_c)
Pic_result<- cbind.data.frame(Pic_result,(Pic_result$sd_mean/max(Pic_result$sd_mean)) ,files[i],x_Size/y_Size)

Cluster_result <- rbind.data.frame(Cluster_result,Pic_result)
i = i+1
}

Cluster_result <- na.omit(Cluster_result)
ggplot(Cluster_result , aes(x=Xaxis,y=Yaxis))+
  geom_tile(aes(fill=rgb(med_red,med_green,med_blue)))+
  scale_fill_identity()+
  theme_minimal()


test <-Cluster_result[(Cluster_result$`(Pic_result$sd_mean/max(Pic_result$sd_mean))`> 0.40),]
ggplot(test, aes(x= Xaxis, y = Yaxis, group = `files[i]`))+
  geom_tile(aes(fill=`files[i]`))+
  ylim(-18,0)+
  theme_minimal()



combo.box <- grid.arrange(p1,p2, nrow = 2)

#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}


