# Installation

You can install the development version of nolap from [GitHub](https://github.com/) with:

``` r
#install.packages("remotes")
remotes::install_github("ksatohds/nolap")
library(nolap)
```

Package Archive File (.tar.gz) is also available on [Dropbox](https://www.dropbox.com/scl/fo/1jxzzfqz9xsl3wlzwm3pj/ACYDNzn-h54VqgAngTKVyc0?rlkey=i5bcup2qxzwqgles27ke0f9ab&st=mdbkongw&dl=0)

# Function

The function **nolap** divides a scatterplot of x,y vectors into a grid and moves the data points so that one sample fits on each grid. The finer the grid, the fewer the moves.

# Example 1. nolap with scatter plot
``` r
#install.packages("remotes")
#remotes::install_github("ksatohds/nolap")
library(nolap)

x <- cars$speed
y <- cars$dist
res <- nolap(x=x,y=y,xdiv=10,ydiv=10,nseed=10)
print(res$seed)
plot(res$x,res$y,col=2,xlim=range(res$xlims),ylim=range(res$ylims))
abline(h=res$ylims,col=8)
abline(v=res$xlims,col=8)

# check movements
plot(x,y,xlim=range(res$xlims),ylim=range(res$ylims),type="n")
text(x,y,1:50)
abline(h=res$ylims,col=8)
abline(v=res$xlims,col=8)
arrows(x,y,res$x,res$y,length=0.1,col=ifelse(res$is.moved,2,4))
legend("topleft",legend=c("moved","stay"),fill=c(2,4))
```

# Example 2. nolap with text
``` r
#install.packages("remotes")
#remotes::install_github("ksatohds/nolap")
library(nolap)

loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2]
res <- nolap(x=x,y=y,xdiv=8,ydiv=8,seed=77)
plot(x,y,type="n")
for(i in 1:nrow(loc)){
  if(res$is.moved[i]) arrows(x[i],y[i],res$x[i],res$y[i],col=2,code=0)
  text(res$x[i],res$y[i],rownames(loc)[i],cex=1)
}
```

# Example 3. nolap with image
``` r
# install.packages("remotes")
# remotes::install_github("ksatohds/nolap")
# install.packages("dslabs")
# install.packages("umap")
# install.packages("RColorBrewer")

library(dslabs)
d <- read_mnist()
y <- d$train$labels[1:100]
x <- d$train$images[1:100,]

library(umap)
set.seed(1)
res <- umap(x)
u <- res$layout[,1]
v <- res$layout[,2]

myimage <- function(x,col){
  f <- matrix(as.matrix(x),28,28,byrow=T)
  f <- (255-f[,1:28])/255
  f <- as.raster(f)
  f[f=="#FFFFFF"] <- col
  return(f)
}

library(RColorBrewer)
mycol <- brewer.pal(12,"Paired")
library(nolap)
resxy <- nolap(u,v,xdiv=21,ydiv=15)

plot(u,v,type="n",xlim=range(u),ylim=range(v))
for(i in 1:length(u)){
  img <- myimage(x[i,],mycol[y[i]+1])
  rasterImage(as.raster(img),
              resxy$xleft[i],
              resxy$ybottom[i],
              resxy$xright[i],
              resxy$ytop[i])    
}
legend("topright",legend=0:9,fill=mycol[0:9+1])
```

