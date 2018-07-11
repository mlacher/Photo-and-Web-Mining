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

#This Function defines the Raster Size
library(psych)
Div_Raster<- function (Size, Div_const){
  i=0
  Div_count<- Size/Div_const
  Div_Frame<-c(0)
  while (i <= Div_const){
    i=i+1;
    Div_Frame=rbind(Div_Frame,Div_count+Div_Frame[i-1]);
  }
  return (Div_Frame)
}

#Create 4th Dimension including Raster
Clustered_Pic<- function(Pic,y_Size,x_Size){
  newArray <- c(0)
  Raster_Pic<-array(c(Pic,newArray),dim = c(y_Size,x_Size,4))
  i_piy<-0
  while(i_piy<= y_Size){ 'move on y axis'
    i_pix = 0;
    while (i_pix<= x_Size){ 'move on x axis'
      i_grid = 0;
      while(i_grid< length(x_Grid)){ 'check which raster in X'
        if((i_pix >= (x_Grid[i_grid]))&&(i_pix < (x_Grid[i_grid+1]))){ 'part of a x raster'
          x_CharPos<- x_Grid[i_grid];
        }
        i_grid = i_grid+1;'increment raster x value'
      }
      i_grid = 0;
      while(i_grid< length(y_Grid)){ 'check which raster in y'
        if((i_piy >= (y_Grid[i_grid]))&&(i_piy < (y_Grid[i_grid+1]))){ 'part of a y raster'
          y_CharPos<-  y_Grid[i_grid];
        }
        i_grid = i_grid+1;'increment raster y value'
      }
      Raster_Pic[i_piy,i_pix,4]<- paste(x_CharPos,y_CharPos,sep="/");
      i_pix = i_pix +1;
    }
    i_piy = i_piy+1;
  }
  return(Raster_Pic)
}

#to the math
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
  colnames(Cluster_result)<-c("med_red","med_green","med_blue","sd_red","sd_green","sd_blue","Xaxis","Yaxis")
  Cluster_result$Yaxis<-Cluster_result$Yaxis*-1
  Cluster_result<-cbind.data.frame(Cluster_result,(Cluster_result$sd_red+
                                                   Cluster_result$sd_green+
                                                   Cluster_result$sd_blue)/3)
  colnames(Cluster_result)<-c("med_red","med_green","med_blue",
                            "sd_red","sd_green","sd_blue",
                            "Xaxis","Yaxis","sd_mean")
  Cluster_result$sd_mean[is.infinite(Cluster_result$sd_mean)]<-0
  return(Cluster_result)
}
