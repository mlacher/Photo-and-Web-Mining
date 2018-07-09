#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html


library(jpeg)
library(ggplot2)
library(psych)
#####Functions##############################
#Create Mask for Divider Frames
Div_Raster<- function (Size, Div_const){
  i=0
  Div_count<- Size/Div_const
  Div_Frame<-c(0)
  while (i < Div_const){
    i=i+1;
    Div_Frame=rbind(Div_Frame,Div_count+Div_Frame[i-1]);
  }
  return (Div_Frame)
}

img <- readJPEG("C:/Users/maximilian.lacher/Downloads/sunset.jpg")
x_Pixel<-img[1,,1]
y_Pixel<-img[,1,1]
x_Size<-length(x_Pixel)
y_Size<-length(y_Pixel)
x_Grid<-Div_Raster(x_Size, 20)
y_Grid<-Div_Raster(y_Size, 16)


#next function
newArray <- c(0)
Cluster_img<-array(c(img,newArray),dim = c(853,1280,4))
i_piy<-1
i_pix<-1

while(i_piy<= y_Size){ 'move on y axis'
  i_pix = 1;
  while (i_pix<= x_Size){ 'move on x axis'
    i_grid = 1;
    while(i_grid< length(x_Grid)){ 'check which raster in X'
      if((i_pix >= (x_Grid[i_grid]))&&(i_pix < (x_Grid[i_grid+1]))){ 'part of a x raster'
      x_CharPos<- x_Grid[i_grid];
      }
      i_grid = i_grid+1;'increment raster x value'
    }
    i_grid = 1;
    while(i_grid< length(y_Grid)){ 'check which raster in y'
      if((i_piy >= (y_Grid[i_grid]))&&(i_piy < (y_Grid[i_grid+1]))){ 'part of a y raster'
      y_CharPos<-  y_Grid[i_grid];
      }
      i_grid = i_grid+1;'increment raster y value'
    }
  Cluster_img[i_piy,i_pix,4]<- paste(x_CharPos,y_CharPos,sep="/");
  i_pix = i_pix +1;
  }
i_piy = i_piy+1;
}

Cluster_result<-describeBy(as.numeric(Cluster_img[,,3]), group = Cluster_img[,,4],mat=TRUE)
unlist(strsplit(as.character( Cluster_result$group1), "/"))
ggplot(Cluster_result, aes(x=group1,y=median))+
  geom_point()


#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}

