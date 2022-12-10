## Load libraries
library(e1071)
library(png)
library(abind)
library(fields)

## Load in data
data <- read.csv("DMC1.csv")
data$DMC <- factor(data$DMC)

## Tests
#
# data[which(data$DMC=="3890"),]
# data[19,]
#
# which(data$DMC=="496")
# num <- c("3371","310","B5200","168","169","318","644","646")
# num <- c("168","169","300","301","307","310","317","318","322","336",
#          "400","402","413","414","444","554","606","644","646","699",
#          "701","702","728","747","779","780","783","791","801","809",
#          "815","826","895","898","919","921","938","939","963","975",
#          "977","995","996","3371","3743","3750","3752","3776","3777","3799",
#          "3826","3843","B5200")
# num <- c("939","598","816","817","972","3818","B5200")
# ## Selecting certain colors 
# ind <- rep(0,length(num))
# for(k in 1:length(num)){
#   ind[k] <- which(data$DMC==num[k])
# }
# data <- data[ind,]      

# Code to select colors
fit <- svm(DMC ~ RED + GRN + BLU ,data=data,kernel="linear",cost=1)

## Don't mess with this!!!

## Tests for predictions
# predict(fit,newdata=data.frame(RED=151,GRN=11,BLU=35))
# 
# predict(fit,newdata=data.frame(RED=255,GRN=214,BLU=0))
# 
# predict(fit,newdata=data.frame(RED=27, GRN=40, BLU=83))

data$DMC
which(data$DMC=="3829")
data[c(309,313,351),]
data[which(data$DMC=="3371"),]
###

## Code for importing .png file

img <- readPNG(file.choose())
img_color <- img*255
newdata <- data.frame(RED=c(t(img_color[,,1])),GRN=c(t(img_color[,,2])),BLU=c(t(img_color[,,3])))
rows <- dim(img)[1]
columns <- dim(img)[2]

image <- array(0,dim=c(rows,columns,3))
bigw <- matrix(0, rows, columns)
for(i in 1:rows){
  pred <- predict(fit,newdata=newdata[1:columns + (i-1)*columns,])
  w <- rep(0,columns)
  for(j in 1:columns){
    w[j] <- which(data$DMC==pred[j])
  }
  bigw[i,] <- w 
  image[i,,] <- cbind(data$RED[w],data$GRN[w],data$BLU[w])
  if(i%%20==0){cat(i)}
}
writePNG(image/255,target="sgt2.png")

### Code for writting pattern 
## Not complete yet



uni_col <- unique(c(bigw))
len_uni <- length(uni_col)

bigw_w <- matrix(as.numeric(factor(bigw)),rows,columns)

dim(p1)
dim(p3)
p1 <- readPNG("C:/Users/jonba/Documents/Pattern/1.png")
p2 <- readPNG("C:/Users/jonba/Documents/Pattern/2.png")
p3 <- readPNG("C:/Users/jonba/Documents/Pattern/3.png")
p4 <- readPNG("C:/Users/jonba/Documents/Pattern/4.png")
p5 <- readPNG("C:/Users/jonba/Documents/Pattern/5.png")
p6 <- readPNG("C:/Users/jonba/Documents/Pattern/6.png")
p7 <- readPNG("C:/Users/jonba/Documents/Pattern/7.png")
p8 <- readPNG("C:/Users/jonba/Documents/Pattern/8.png")
p9 <- readPNG("C:/Users/jonba/Documents/Pattern/9.png")
a <- abind(p1,p2,p3,p4,p5,p6,p7,p8,p9,rev.along=0)
png_size <- dim(p1)[1]

big_grid <- array(0,c(png_size*rows,png_size*columns,3))
for(i in 1:rows){
  for(j in 1:columns){
    temp <- bigw_w[i,j]
    big_grid[1:png_size+(i-1)*png_size,1:png_size+(j-1)*png_size,] <- a[,,,temp]
  }
}

par(mar=c(0,0,0,0))
plot(-1,-1,ylim=c(0,len_uni+1),xlim=c(0,10),axes=FALSE,xlab="",ylab="")
text(1,len_uni+1,"Symbol")
text(3,len_uni+1,"DMC Number")
text(5,len_uni+1,"Color")
text(7,len_uni+1,"Number of Stitches")

for(i in 1:len_uni){
  add.image(1,len_uni-i,t(a[,,1,i]),col=c(1,0),image.height = 0.05,image.width = 0.05)
  temp_data <- subset(data,DMC==data$DMC[uni_col[i]])
  text(3,len_uni-i,temp_data$DMC)
  text(5,len_uni-i,temp_data$Description)
  text(7,len_uni-i,table(bigw_w)[[i]])
}


