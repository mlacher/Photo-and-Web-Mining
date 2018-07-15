# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#
###################This Function defines the Raster Size
#
library(psych)
Div_Raster<- function (Size_x,Size_y, Div_count_x,Div_count_y){
  i=1
  Div_const_x<- Size_x/Div_count_x
  Div_Frame_x<- i
  while (i < Div_count_x){
    i=i+1;
    Div_Frame_x=rbind(Div_Frame_x,i);
  }
  i=1
  Div_const_y<- Size_y/Div_count_y
  Div_Frame_y<- i
  while (i < Div_count_y){
    i=i+1;
    Div_Frame_y=rbind(Div_Frame_y,i);
  }

  mesh <- array(1,dim = c(Div_count_y,Div_count_x))
  i = 0
  while(i<= length(Div_Frame_y)){
    mesh[i,] <- paste(Div_Frame_x,Div_Frame_y[i],sep="/");
    i=i+1;
  }
  return (mesh)
}

#
#####################Scale Mesh
#
Scale_Mesh<- function(Size_x, Size_y,Div_count_x,Div_count_y, mesh){
a<-(Size_x)/Div_count_x
m_a<-Size_x%%Div_count_x
r_a<-ceiling(a)
sc_mesh <- mesh[,rep(1:ncol(mesh),each=r_a)]
  if(m_a){
    sc_mesh<-sc_mesh[,-c((Size_x+1):length(sc_mesh[1,])) ]
  }
b<- (Size_y)/Div_count_y
m_b<-Size_y%%Div_count_y
r_b<-ceiling(b)
sc_mesh <- sc_mesh[rep(1:nrow(sc_mesh),each=r_b),]
  if(m_b){
   sc_mesh<-sc_mesh[-c((Size_y+1):length(sc_mesh[,1])),]
  }
return(sc_mesh)
}



#
############################Change RBG to HSV
#
pic_HSV<- function(Size_x,Size_y,img){

img_h <- array(1,dim = c(Size_y,Size_x,4))
i=1
  while (i<= Size_y){
    img_hsv<-t(rgb2hsv(img[i,,1],img[i,,2],img[i,,3],1))
    img_h[i,,1]<- img_hsv[,1]
    img_h[i,,2]<- img_hsv[,2]
    img_h[i,,3]<- img_hsv[,3]
    i = i +1;
  }
return(img_h)
}


#
#do the math#########################################
#
Analyse_Pic <- function (Pic){
  library(psych)
  Cluster_result1<-describeBy(as.numeric(Pic[,,1]), group = Pic[,,4],mat=TRUE)
  Cluster_result2<-describeBy(as.numeric(Pic[,,2]), group = Pic[,,4],mat=TRUE)
  Cluster_result3<-describeBy(as.numeric(Pic[,,3]), group = Pic[,,4],mat=TRUE)
  RowCol<-matrix(unlist(strsplit(as.character( Cluster_result3$group1 ), "/")),ncol=2, byrow=TRUE)
  RowCol<- as.data.frame(RowCol)
  RowCol1<- cbind(as.numeric(levels(RowCol$V1))[RowCol$V1],
                as.numeric(levels(RowCol$V2))[RowCol$V2])
  Cluster_result <- cbind.data.frame(Cluster_result1$median,
                                   Cluster_result2$median,
                                   Cluster_result3$median,
                                   Cluster_result1$sd/Cluster_result1$median,
                                   Cluster_result2$sd/Cluster_result2$median,
                                   Cluster_result3$sd/Cluster_result3$median,
                                   RowCol1
  )
  colnames(Cluster_result)<-c("hue","sat","val","sd_hue","sd_sat","sd_val","Xaxis","Yaxis")
  Cluster_result$Yaxis<-Cluster_result$Yaxis*-1
  Cluster_result<-cbind.data.frame(Cluster_result,Cluster_result$sd_sat)
  colnames(Cluster_result)<-c("hue","sat","val","sd_hue","sd_sat","sd_val","Xaxis","Yaxis","sd_tbd")
  Cluster_result$sd_tbd[is.infinite(Cluster_result$sd_tbd)]<-0
  return(Cluster_result)
}
#
##########################################
#
