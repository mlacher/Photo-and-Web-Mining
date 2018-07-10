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
