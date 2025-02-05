# Installation

You can install the development version of nolap from [GitHub](https://github.com/) with:

``` r
#install.packages("remotes")
remotes::install_github("ksatohds/nolap")
library(nolap)
```

# Functions

The function is **nolap**.

## Example

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

