#https://alstatr.blogspot.com/2014/09/r-image-analysis-using-ebimage.html
source("https://bioconductor.org/biocLite.R")
biocLite()
biocLite()
biocLite("EBImage")

# Reading Image
Image <- readImage('C:/Users/aximilian.lacher/Downloads/sunset.JPG')
display(Image)