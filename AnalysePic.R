#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html
#https://www.kandooadventures.com/news-and-views/top-10-instagram-landscape-photographers/

library(jpeg)
library(ggplot2)
library(psych)
library(img.compression)
library(fmsb)
library(gridExtra)
library(plyr)
#library(tidyverse)

#####Functions##############################
#1. Pic Format
#2. Pic Structure
#3. Color Scheme

############################################MAIN################################################
---------------------------


pb<-winProgressBar(title="Example progress bar", label="progress bar",min=0,max=100, initial = 0, width = 300)

Path <- "C:/Users/maximilian.lacher/Documents/GitHub/Photo-and-Web-Mining/Pics"
#Path <- "E:/Users/lacher/Documents/GitHub/Photo-and-Web-Mining/Pics"

files = list.files(path = Path, pattern="*.jpg")

#img <- readJPEG("C:/Users/maximilian.lacher/Downloads/test.jpg")
#img <- readJPEG("E:/Users/lacher/Documents/GitHub/Photo-and-Web-Mining/InstaSave[2].jpg")


Cluster_result  <- data.frame(File=character())


i=1
while (i <= length(files)){
#read pic
  location <- paste(Path,files[i],sep= "/")
  img <- readJPEG(location)
  x_Size<-length(img[1,,1])
  y_Size<-length(img[,1,1])
#create mesh
  Mesh <-Div_Raster(x_Size, y_Size,20,24)
  sc_Mesh<- Scale_Mesh(x_Size,y_Size,20,24,Mesh)
#convert rgb to hsv
  img_hsv<- pic_HSV(x_Size,y_Size,img)
  img_hsv[,,4]<- sc_Mesh
#pic analyse
  Pic_result<-Analyse_Pic(img_hsv)
  Pic_result<- cbind.data.frame(Pic_result,(Pic_result$sd_tbd/max(Pic_result$sd_tbd)) ,files[i],x_Size/y_Size)
  Cluster_result <- rbind.data.frame(Cluster_result,Pic_result)
  setWinProgressBar(pb,i/(length(files))*100)
i = i+1
Pic_result<-""
}
close(pb)


##test
ggplot(Cluster_result, aes (x= `x_Size/y_Size`))+
  geom_histogram()



##color distribution
ggplot(Cluster_result, aes(x=hue))+
  geom_histogram(aes(fill=hsv(Cluster_result$hue,0.5, 0.5)))+
  #geom_histogram(aes(fill = "Red"))+
  scale_fill_identity()+
  #ylim(-1000,1800) +
  theme_minimal()
  #+coord_polar(start = 0)

##pic brigthness distribution
Val <-Cluster_result[(Cluster_result$val > 0.5),]
Val.counts <- ddply(Val, .(Val$Xaxis, Val$Yaxis), nrow)
names(Val.counts) <- c("xaxe", "yaxe", "Freq")
ggplot(Val.counts, aes(x= xaxe, y = yaxe))+
  geom_tile(aes(fill=Freq))+
  scale_fill_distiller(palette = "Spectral")+
  ylim(-24,0)+
  theme_minimal()

##pic xy size

Size.counts <- ddply(Cluster_result, .(Cluster_result$`x_Size/y_Size`, nrow))
names(Val.counts) <- c("xaxe", "yaxe", "Freq")
ggplot(Cluster_result)+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_blank())+
  annotate("rect", xmin=0, xmax=10, ymin= 0,
          ymax= 10/(as.numeric(levels(factor(Cluster_result$`x_Size/y_Size`)))),
          alpha=0.1, color = "BLUE",fill=levels(factor(Cluster_result$`x_Size/y_Size`)))+
  ylim(0,15)+
  xlim(0,10.5)




combo.box <- grid.arrange(p1,p2, nrow = 2)

#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}


