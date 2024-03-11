
sumRow<-function(X,c1,c2){
  T1=matrix(X,c1,c2)
  TT=T1%*%t(T1)/(norm(T1,"F")^2)
  return(TT)
}
sumCol<-function(X,c1,c2){
  T2=matrix(X,c1,c2)
  TT=t(T2)%*%T2/(norm(T2,"F")^2)
  return(TT)
}
SKR<-function(X){
  T=dim(X)[1]
  p1=dim(X)[2]
  p2=dim(X)[3]
  TK1=matrix(0,p1,p1)
  for(i in 1:(T-2)){
    TT1=apply(array(rep(X[i,,],T-i),c(p1,p2,T-i)),3,as.vector)-apply(X,1,as.vector)[,(i+1):T]
    TT=apply(TT1,2,sumRow,c1=p1,c2=p2)
    TK1=TK1+matrix(apply(TT,1,sum),p1)

  }
  TT=X[T-1,,]-X[T,,]
  TK1=TK1+TT%*%t(TT)/(norm(TT,"F")^2)
  TK1=2/(T*(T-1))*TK1
  return(TK1)
}
SKC<-function(X){
  T=dim(X)[1]
  p1=dim(X)[2]
  p2=dim(X)[3]
  TK2=matrix(0,p2,p2)
  for(i in 1:(T-2)){
    TT2=apply(array(rep(X[i,,],T-i),c(p1,p2,T-i)),3,as.vector)-apply(X,1,as.vector)[,(i+1):T]
    TT=apply(TT2,2,sumCol,c1=p1,c2=p2)
    TK2=TK2+matrix(apply(TT,1,sum),p2)

  }
  TT=X[T-1,,]-X[T,,]
  TK2=TK2+t(TT)%*%TT/(norm(TT,"F")^2)
  TK2=2/(T*(T-1))*TK2
  return(TK2)
}

MSK=function(X,type = "1"){
  if (type==1){
    return(SKR(X))
  }
  if(type==2){
    return(SKC(X))
  }
}



