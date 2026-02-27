.onAttach <- function(...) {
  packageStartupMessage("Last update on 5 FEB 2025")
  packageStartupMessage("https://github.com/ksatohds/nolap")
}

#' @title Arrange data points so that they do not overlap in a scatter plot.
#' @description \code{nolap} This package aims to optimize arrangement by minimizing the movement of data points so that they do not overlap.
#' @param x vector of x-coordinates
#' @param y vector of y-coordinates
#' @param xdiv number of x-axis divisions
#' @param ydiv number of y-axis divisions
#' @param xlim range of x-axis to be divided
#' @param ylim range of y-axis to be divided
#' @param seed initial value of the random number to be specified
#' @param nseed number of trials when initial random number is not specified
#' @return xleft: a vector of left x positions.
#' @return ybottom: a vector of bottom y positions.
#' @return xright: a vector of right x positions.
#' @return ytop: a vector of top y positions.
#' @return x: x-coordinate of the center of the rectangular region
#' @return y: y-coordinate of the center of the rectangular region
#' @return xlims: x-axis dividing interval
#' @return ylims: y-axis dividing interval
#' @return ssd: total distance moved (sum of Euclidean distances)
#' @return seed: The initial value of the specified random number or the best initial value obtained by trial
#' @return is.moved: logical vector indicating whether each data point was moved from its original cell
#' @export
#' @examples
#' x <- cars$speed
#' y <- cars$dist
#' res <- nolap(x=x,y=y,xdiv=10,ydiv=10,nseed=10)
#' print(res$seed)
#' plot(res$x,res$y,col=2,xlim=range(res$xlims),ylim=range(res$ylims))
#' abline(h=res$ylims,col=8)
#' abline(v=res$xlims,col=8)
#' # check movements
#' plot(x,y,xlim=range(res$xlims),ylim=range(res$ylims),type="n")
#' text(x,y,1:50)
#' abline(h=res$ylims,col=8)
#' abline(v=res$xlims,col=8)
#' arrows(x,y,res$x,res$y,length=0.1,col=ifelse(res$is.moved,2,4))
#' legend("topleft",legend=c("moved","stay"),fill=c(2,4))

nolap <- function(x,y,xdiv=70,ydiv=50,xlim=range(x),ylim=range(y),seed=NULL,nseed=10){
  makedxy <- function(x,y,xdiv,ydiv,xlim,ylim){
    xr <- xlim
    yr <- ylim
    xbreaks <- seq(xr[1],xr[2],length.out=xdiv+1)
    ybreaks <- seq(yr[1],yr[2],length.out=ydiv+1)
    xlims <- cbind(xbreaks[1:xdiv],xbreaks[2:(xdiv+1)])
    ylims <- cbind(ybreaks[1:ydiv],ybreaks[2:(ydiv+1)])
    xi <- findInterval(x,xbreaks,rightmost.closed=TRUE)
    yj <- findInterval(y,ybreaks,rightmost.closed=TRUE)
    dxy <- matrix(0,nrow=xdiv,ncol=ydiv)
    for(k in seq_along(x)) dxy[xi[k],yj[k]] <- dxy[xi[k],yj[k]]+1
    dij <- data.frame(i0=xi,j0=yj,i=xi,j=yj,id=seq_along(x))
    return(list(dij=dij,dxy=dxy,xlims=xlims,ylims=ylims))
  }
  roundfreq <- function(d,ij){
    ic <- ij[1]
    jc <- ij[2]
    freq <- NULL
    for(i in max(ic-1,1):min(ic+1,dim(d)[1]))
      for(j in max(jc-1,1):min(jc+1,dim(d)[2])){
        if(!(i==ic&j==jc))freq <- rbind(freq,c(i,j,d[i,j]))
      }
    freq <- as.data.frame(freq)
    colnames(freq) <- c("i","j","n")
    return(freq)
  }
  distij <- function(ij1,ij2) as.numeric(((ij1[1]-ij2[1])^2+(ij1[2]-ij2[2])^2)^0.5)
  searchspace <- function(d,dij,seed=1){
    set.seed(seed)
    repeat{
      if(max(d)==1) break
      ij <- which(d==max(d),arr.ind=T)[1,]
      ijr <- roundfreq(d,ij)
      index <- which(ijr$n==min(ijr$n))
      index <- index[sample.int(length(index),1)]
      ijt <- ijr[index,1:2] # target cell to move to
      ijt <- unlist(as.vector(ijt))
      dijc <- dij[dij$i==ij[1]&dij$j==ij[2],]
      distances <- 0*(1:nrow(dijc))
      for(i in 1:nrow(dijc)) distances[i] <- distij(dijc[i,1:2],ijt)
      i <- which(distances==min(distances))
      idm <- dijc$id[i[1]]
      dij[dij$id==idm,]$i <- ijt[1]
      dij[dij$id==idm,]$j <- ijt[2]
      d[ij[1],ij[2]] <- d[ij[1],ij[2]]-1
      d[ijt[1],ijt[2]] <- d[ijt[1],ijt[2]]+1
    }
    totaldist <-0
    for(i in 1:nrow(dij))totaldist <- totaldist+distij(dij[i,1:2],dij[i,3:4])
    return(list(dxy=d,dij=dij,totaldist=totaldist))
  }
  resxy <- makedxy(x=x,y=y,xdiv=xdiv,ydiv=ydiv,xlim=xlim,ylim=ylim)
  xi <- resxy$xlims
  yi <- resxy$ylims
  if(xdiv*ydiv<length(x)){
    stop("Not enough space! Set larger xdiv and ydiv.")
  }
  if(is.null(seed)){
    totaldists <- 0*1:nseed
    for(i in 1:nseed){
      ressp <- searchspace(resxy$dxy,resxy$dij,seed=i)
      totaldists[i] <- ressp$totaldist
    }
    bestseed <- which(totaldists==min(totaldists))[1]
  }else{
    bestseed <- seed
  }
  ressp <- searchspace(resxy$dxy,resxy$dij,seed=bestseed)
  oij <- resxy$dij
  dij <- ressp$dij
  index <- ifelse(oij$i0==dij$i & oij$j0==dij$j,TRUE,FALSE)
  return(list(xleft=xi[dij$i,1],
              ybottom=yi[dij$j,1],
              xright=xi[dij$i,2],
              ytop=yi[dij$j,2],
              x=(xi[dij$i,1]+xi[dij$i,2])/2,
              y=(yi[dij$j,1]+yi[dij$j,2])/2,
              xlims=xi,
              ylims=yi,
              is.moved=!index,
              ssd=ressp$totaldist,
              seed=bestseed)
  )
}

