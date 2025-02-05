# Installation

You can install the development version of nolap from [GitHub](https://github.com/) with:

``` r
#install.packages("remotes")
remotes::install_github("ksatohds/nolap")
library(nolap)
```

# Functions

The function **nolap** divides a scatterplot of x,y vectors into a grid and moves the data points so that one sample fits on each grid. The finer the grid, the fewer the moves.

# Example 1.
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

# Example 2.
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
