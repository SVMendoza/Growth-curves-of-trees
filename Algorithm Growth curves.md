# Growth-curves-of-trees
Algorithm 
###############################################################################################
tray<-function(DAP1, DAP2, perm, X, X.1)
{
cre=as.data.frame(matrix(nrow=300,ncol=perm))
colnames(cre)<-c(paste("Tiem", 1:perm, sep = ""))

for(j in 1:perm)
{
 NF=1
 inicio<-sample(DAP1[DAP1<X.1],1)
 i=1

 esmenor<-(inicio<X)

while (esmenor)
      {
      X1=c()
      X1[i]=inicio
               if (X1[i]<X)
               {
               X2=c()
               X2[i]=(min(DAP2[X1[i]<DAP2]))
               cre[NF,j]<-X1[i]+(X2[i]-X1[i])
               NF=NF+1

               inicio<- X2[i]
               esmenor<-(X2[i]<X)
                                 }
                                 i=i+1
     }
}
return(cre)
}
