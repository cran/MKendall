MRTS=function(X,k,r){
  T=dim(X)[1]
  p1=dim(X)[2]
  p2=dim(X)[3]
  Fhat=array(0,c(T,k,r))
  Rhat=sqrt(p1)*eigen(MSK(X,1))$vectors[,1:k]
  Chat=sqrt(p2)*eigen(MSK(X,2))$vectors[,1:r]
  for(t in 1:T){
    Fhat[t,,]=t(Rhat)%*%X[t,,]%*%Chat/(p1*p2)
  }
  result=list()
  result$Rloading=Rhat
  result$Cloading=Chat
  result$Fhat=Fhat
  return(result)
}