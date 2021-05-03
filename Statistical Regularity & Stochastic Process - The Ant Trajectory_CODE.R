rm(list=ls())

library(ggplot2)
library(ggthemes)
library(hrbrthemes)

library(gganimate)

f = function(n)
{
  x=0:n
  y=0:n

  for(i in 1:n)
  {
	  r=sample(1:4,1,replace=TRUE)
	  if(r==1){
		  x[i+1]=(0.5*x[i])+0.25
		  y[i+1]=(0.5*y[i])+0.4
	  }
	  else if(r==2){
		  x[i+1]=(0.8*x[i])+0.1
	  	y[i+1]=(0.8*y[i])+0.04
	  }
	  else if(r==3){
		  x[i+1]=(0.355*x[i])-(0.355*y[i])+0.266
		  y[i+1]=(0.355*x[i])+(0.355*y[i])+0.078
	  }
	  else{
		  x[i+1]=(0.355*x[i])+(0.355*y[i])+0.378
		  y[i+1]=-(0.355*x[i])+(0.355*y[i])+0.434
	  }
  }
  d = data.frame(ts=1:(n+1),x.axis=x,y.axis=y)
  
  ggplot(d, aes(x = x.axis, y = y.axis, group=1)) + geom_point(color="#229954",size=1) + 
  theme_ipsum() + labs(title = "The Ant Trajectory after 10000 steps",subtitle = "A Markov Chain")
}

f(100000)
