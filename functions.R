addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

handplot <- function(g, lout, prob=0.01, 
                     cex=V(g)$size, col="blue", pch=1){
  
  plot(lout, type='n', axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
  esample <- sample(E(g), prob*ecount(g), prob=E(g)$weight)
  for(e in esample){
    v <- ends(g,e, names=FALSE)
    segments(lout[v[1],1], lout[v[1],2],  lout[v[2],1], lout[v[2],2], col='grey')
  }
  points(lout, pch=pch, cex=cex, col=col)
}


handsubplot <- function(g, lout, col="grey"){
  
  #palette(heat.colors(40))
  transparency <- 120
  par(mai=c(0,0,0,0))
  plot(lout, type='n', axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
  for(e in E(g)){
    v <- ends(g,e, names=FALSE)
    Arrows(lout[v[1],1], lout[v[1],2],  lout[v[2],1], lout[v[2],2], 
           col=addTrans("darkgrey", transparency))
  }
  points(lout, pch=15, cex=9*V(g)$size, col=addTrans(col, transparency))
  points(lout, pch=22, cex=9.3*V(g)$size, col=addTrans("darkblue", transparency))
  xtext <- min(lout[,1]) + 0.15*(max(lout[,1]) - min(lout[,1]))
  ytext <- min(lout[,2]) + 0.01*(max(lout[,2]) - min(lout[,2]))
  text(xtext, ytext, labels=sprintf("%d vertices, %d edges", vcount(g), ecount(g)), 
       col='darkblue')
}
