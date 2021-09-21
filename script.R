main = function()
{ 
  a=0.1
  b=2
  h=0.001
  
  E=0.01
  
  f = function(x){
    return(x-log(x, exp(1)))
  }
  
  fprime = function(x){
    return(1-1/x)
  }
  
  iter=0
  textpos = 2.4
  x=seq(a,b,h)
  ans=0
  while(1){
    iter = iter + 1
    plot(x,f(x), type="l", col="red", lwd="2",
         ylim=c(-0.5,2.5), xlim=c(0,2))
    grid()
    aprime = fprime(a)
    bprime = fprime(b)
    
    a1=f(a)-aprime*a
    a2=f(b)-bprime*b
    
    abline(b=aprime,a=a1)
    abline(b=bprime,a=a2)
    
    xinter = (a2-a1)/(aprime-bprime)
    
    points(xinter, y=f(xinter), lwd="2", pch=19)
    lines(c(xinter,xinter),c(aprime*xinter + a1, f(xinter)),lty="dotted", lwd="3", col="blue")
    text(0.8,textpos,paste("Iteration number = ", iter), adj = c(0,0))
    text(0.8,textpos-0.2,paste("A = ", a), adj = c(0,0))
    text(0.8,textpos-0.4,paste("B = ", b), adj = c(0,0))
    text(0.8,textpos-0.6,paste("x = ", xinter), adj = c(0,0))
    text(0.8,textpos-0.8,paste("f'(x) = ", fprime(xinter)), adj = c(0,0))
    text(0.8,textpos-1.0,paste("f(x) = ", f(xinter)), adj = c(0,0))
    
    
    if(fprime(xinter) > E){
      b=xinter
    }
    else if(fprime(xinter) < -E){
      a=xinter
    }
    else{
      break
    }
    
   
    browser()
    #Sys.sleep(1)
  }
  finter = f(xinter)
  plot(x,f(x), type="l", col="red", lwd="2",
       ylim=c(-0.5,2.5), xlim=c(0,2))
  grid()
  points(xinter, y=finter, lwd="3", pch=19)
  lines(c(xinter,xinter,xinter, -1000),c(-1000, finter, finter, finter),lty="dotted", lwd="3", col="blue")
  
  text(0.8,textpos,paste("Iteration number = ", iter), adj = c(0,0))
  text(0.8,textpos-0.2,paste("A = ", a), adj = c(0,0))
  text(0.8,textpos-0.4,paste("B = ", b), adj = c(0,0))
  text(0.8,textpos-0.6,paste("x = ", xinter), adj = c(0,0))
  text(0.8,textpos-0.8,paste("f'(x) = ", fprime(xinter)), adj = c(0,0))
  text(0.8,textpos-1.0,paste("f(x) = ", f(xinter)), adj = c(0,0))
  text(1.5,textpos,paste("xmin = ", xinter), adj = c(0,0))
  text(1.5,textpos-0.2,paste("ymin = ", finter), adj = c(0,0))

}
main()