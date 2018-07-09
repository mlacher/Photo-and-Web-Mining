#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html


library(jpeg)
library(ggplot2)
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


while(y_Pixel<y_Size){ 'move on y axis'
  while (x_Pixel[i_pix]<x_Size){ 'move on x axis'
    while(i_count< length(x_Grid)){ 'check which raster in X'
      if((x_Pixel[i_pix] >= (x_Grid[i_grid]))&&(x_Pixel < (x_Grid[i_grid+1]))){ 'part of a x raster'
      img_grid<- rbind(img_grid,) 
      }
      i_grid = i_grid+1;'increment raster x value'
    }
    &&((pixel >= (Div_Framey[test-1]))&&(pixel < (Div_Framey[test])))){
      #2D Arrary benötigt
      #name(Div_Frame[0],Div_Frame[1],...)
      #value1(1,1,...)
      #value2(1,2,...)
      #.....
    }
    i_pix = i_pix +1;
  }
  pixel_x = pixel_x+1;
}
pixel_y = pixel_y+1;  
}








img_BLUE<-img
img_RED <-img
img_GREEN<-img
img_BLUE[,,1:2]<-0
img_RED[,,2:3]<-0
img_GREEN[,,c(1,3)]<-0

#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}


plot(img[,,2])

plot(density(img[,,2]),col="darkgreen")
lines(density(img[,,1]),col="red")
lines(density(img[,,3]),col="blue")