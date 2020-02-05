library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

##the following function is used to define the mood of each song
mood_count<-function(lyric.list){
  lyric.list=lyric.list%>%mutate(mood="unknown")
  for (i in 1:nrow(lyric.list))
  {basic<-lyric.list[i,9]
  output_mood<-colnames(lyric.list[9])
  for (j in 10:16)
  {
    if (lyric.list[i,j]>=basic)
    {basic=lyric.list[i,j]
    output_mood<-colnames(lyric.list[j])}
  }
  lyric.list[i,18]=output_mood
  }
  return(lyric.list)
}

#the following function is used to calculate the mood proportion in each kind of song
plot_list<-function(mood.list){
  n=length(names(table(factor(lyrics.pop[,18]))))
  count=rep(0,n)
  for (i in 1:n)
  {
   count[i]=table(factor(mood.list[,18]))[[i]]
  }
  return(tibble(mood=names(table(factor(mood.list[,18]))),count=count))
}

#the following function is used to plot the graph for visualization.
plot_fun<-function(names,song_type){
  g1<-ggplot(data=song_type)+geom_histogram(aes(x=mood),stat="count")+theme_light()+labs(title=paste("The mood distribution of ",names))
  return(g1)
}
#count the word
f.word_count=function(str){
  library(stringr)
  return(str_count(str, '\\w+'))
}

f.speechlinks=function(html.page, node.type=".ver12 a"){
  urls <- html.page %>% # feed `main.page` to the next step
    html_nodes(node.type) %>% # get the CSS nodes
    html_attr("href") # extract the URLs
  # Get link text
  links <- main.page %>% # feed `main.page` to the next step
    html_nodes(node.type) %>% # get the CSS nodes
    html_text() # extract the link text
  # Combine `links` and `urls` into a data.frame
  out <- data.frame(links = links, urls = urls, stringsAsFactors = FALSE)
  
  return(out)
  
}

f.plotsent.len=function(In.list, InFile, InType, InTerm, President){
  
  #"anticipation" "joy"          "surprise"     "trust"       
  #"anger"        "disgust"      "fear"         "sadness"
  
  col.use=c("light grey", "darkgoldenrod1", "darkgoldenrod1", "darkgoldenrod1", "darkgoldenrod1",
            "red2", "chartreuse3", "blueviolet","dodgerblue3")
  
  In.list$topemotion=apply(select(In.list, 
                                  anticipation:sadness), 
                           1, which.max)
  In.list$topemotion.v=apply(select(In.list,
                                    anticipation:sadness), 
                             1, max)
  In.list$topemotion[In.list$topemotion.v<0.01]=0
  In.list$topemotion=In.list$topemotion+1
  
  temp=In.list$topemotion.v
  In.list$topemotion.v[temp<0.05]=1
  
  df=In.list%>%filter(File==InFile, 
                      type==InType, Term==InTerm)%>%
    select(sent.id, word.count, 
           topemotion, topemotion.v)
  
  ptcol.use=alpha(col.use[df$topemotion], sqrt(sqrt(df$topemotion.v)))
  
  plot(df$sent.id, df$word.count, 
       col=ptcol.use,
       type="h", #ylim=c(-10, max(In.list$word.count)),
       main=President)
}

f.smooth.topic=function(x, y.mat){
  y.out=y.mat
  for(i in 1:ncol(y.mat)){
    y.out[,i]=pmax(smooth.spline(x, y.mat[,i])$y, 0)
  }
  return(y.out)
}
function(
  x, y, 
  order.method = "as.is",
  ylab="", xlab="", 
  border = NULL, lwd=1, 
  col=rainbow(length(y[1,])),
  ylim=NULL, stack.lab=colnames(y),
  ...
){
  
  if(sum(y < 0) > 0) error("y cannot contain negative numbers")
  
  if(is.null(border)) border <- par("fg")
  border <- as.vector(matrix(border, nrow=ncol(y), ncol=1))
  col <- as.vector(matrix(col, nrow=ncol(y), ncol=1))
  lwd <- as.vector(matrix(lwd, nrow=ncol(y), ncol=1))
  
  if(order.method == "max") {
    ord <- order(colSums(y), decreasing = T)
    y <- y[, ord]
    col <- col[ord]
    border <- border[ord]
    stack.lab <- stack.lab[ord]
  }
  
  if(order.method == "first") {
    ord <- order(apply(y, 2, function(x) min(which(x>0))))
    y <- y[, ord]
    col <- col[ord]
    border <- border[ord]
    stack.lab <- stack.lab[ord]
  }
  
  top.old <- x*0
  lab.y=0
  polys <- vector(mode="list", ncol(y))
  for(i in seq(polys)){
    top.new <- top.old + y[,i]
    polys[[i]] <- list(x=c(x, rev(x)), y=c(top.old, rev(top.new)))
    top.old <- top.new
    lab.y=c(lab.y, top.new[length(x)])
  }
  
  if(is.null(ylim)) ylim <- range(sapply(polys, function(x) range(x$y, na.rm=TRUE)), na.rm=TRUE)
  plot(x,y[,1], ylab=ylab, xlab=xlab, xlim=c(min(x), max(x)+diff(range(x))*0.1),
       ylim=ylim, t="n", ...)
  for(i in seq(polys)){
    polygon(polys[[i]], border=border[i], col=col[i], lwd=lwd[i])
  }
  
  lab.y.0=lab.y[-1]
  lab.y.1=(seq(0, max(rowSums(y))*0.95, len=ncol(y)+1))[-1]
  print(lab.y.1)
  text(rep(max(x)+diff(range(x))*0.03, ncol(y)),
       lab.y.1, cex=0.8, font=2,
       stack.lab, col=col, adj=0)
  segments(rep(max(x), ncol(y)),
           lab.y.0,
           rep(max(x)+diff(range(x))*0.027, ncol(y)),
           lab.y.1,
           col=col)
  
}
