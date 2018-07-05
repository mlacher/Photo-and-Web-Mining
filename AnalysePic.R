#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html
Image <- readImage('C:/Users/aximilian.lacher/Downloads/sunset.JPG')

library(jpeg)
library(ggplot2)

img <- readJPEG("C:/Users/maximilian.lacher/Downloads/sunset.jpg")
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