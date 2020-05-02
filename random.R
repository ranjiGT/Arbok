N<-100000  #input 
counter<-1
for(i in rnorm(N)){
  if(i > -1 & i < 1){
    counter <- counter + 1
  }
 
}
counter/100000
ans <- counter / N
ans
