
heurst_pvc_class_3 <- function (n=100, sim=2, param1=0, param2=3, seed=1240, Matchoice=Mat_default){
  
  set.seed(seed)
  
  if (n%%3==0) {
    mlst=(n-3)/3
  }else if (n%%3==1) {
    mlst=(n-1)/3
  }else{
    mlst=(n-2)/3
  }
  ## mlst est approximativement les nombre d'étapes de voyage, une étape étant un parcours sur trois villes.
  
  villes=1:n
  
  if (sim==1) {
    M=matrix(rpois(n^2,param1),nrow=n,ncol=n,byrow=FALSE)
  }
  
  if (sim==2) {
    M=matrix(runif(n^2,param1,param2),nrow=n,ncol=n,byrow=FALSE)
  }
  if (sim==3) {
    M=matrix(rbinom(n^2,param1,param2),nrow=n,ncol=n,byrow=FALSE)
  }
  if (sim==4) {
    M=matrix(rexp(n^2,param1),nrow=n,ncol=n,byrow=FALSE)
  }
  if (sim==5) {
    M=matrix(rchisq(n^2,param1),nrow=n,ncol=n,byrow=FALSE)
  }
  
  if (sim==6) {
    M=matrix(rf(n^2,param1, param2),nrow=n,ncol=n,byrow=FALSE)
  }
  
  if (sim==7) {
    M=matrix(abs(rnorm(n^2,param1, param2)),nrow=n,ncol=n,byrow=FALSE)
  }
  
  
  if (sim==0) {
    Dst=Matchoice
  }else{
    Dst=floor(t(M)%*%M)
  }
  
  Dst=Dst-diag(diag(Dst),n,n)
  
  wl=matrix(0,mlst,n)
  suivant <- array(0, dim = c(mlst,n, 3))
  Dismat <- array(0, dim = c(n,n, n,n))
  Distot=1:n
  
  # Définir la fonction qui calcule la distance totale reliant le point u à deux autres points v et w
  Dist= function(x,v,w,y){ z= Dst[x,v] + Dst[v,w]+ Dst[w,y]; return(z)}
  
  for (j in 1:n) {
    for (l in 1:n) {
      for (q in 1:n) {
        for (r in 1:n) {
          if ((j==l) || (j==q) || (j==r) || (l==q) || (l==r) || (q==r)) {
            Dismat[j,l,q,r]=Inf
          }else{
            Dismat[j,l,q,r]=Dist(j,l,q,r)
          }
          
        }
        
      }
      
    }
    
  }
  
  for (u in 1:n) {
    # u est le point de départ
    
    # Calculer la plus petite distance qui relie le point u à trois autres points à visiter
    wl[1,u]<- min(Dismat[u,,,])
    # Identifier tous les triplets de points les plus proches de u
    suiv_1<-which(Dismat[u,,,]==wl[1,u],arr.ind = TRUE)
    # Choisir un des couples de points les plus proches de u et en faire les étape 1 et 2 du voyage
    suiv1 <- suiv_1[suiv_1[,1]!=suiv_1[,2] & suiv_1[,1]!=suiv_1[,3]
                    & suiv_1[,2]!=suiv_1[,3]
                    & suiv_1[,1]!=u & suiv_1[,2]!=u & suiv_1[,3]!=u,]
    
    if (is.vector(suiv1)) {
      suivant[1,u,1] = suiv1[1]
      suivant[1,u,2] = suiv1[2]
      suivant[1,u,3] = suiv1[3]
    }
    
    if (is.matrix(suiv1)) {
      suivant[1,u,1] = suiv1[1,][1]
      suivant[1,u,2] = suiv1[1,][2]
      suivant[1,u,3] = suiv1[1,][3]
    }
    
    i=2
    while (i<=mlst) {
      # Calculer la plus petite distance qui sépare le point de la i-1 eme étape des autres points non encore visités
      wl[i,u]<- min(Dismat[suivant[i-1,u,3],
                           c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3]), c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3]),
                           c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3]) ])
      # Identifier tous les couples de points non encore visités, les plus proches du second point de la i-1 eme étape
      suiv<- which(Dismat[suivant[i-1,u,3],,,]==wl[i,u],arr.ind = TRUE)
      
      # subset(m, m[,4] == 16)
      # Choisir un des couples de points les plus proches de la i-1 eme étape et en faire l'étape i
      
      if (is.matrix(suiv)) {
        suiv = suiv[!(suiv[,1]==u) & !(suiv[,2]==u) & !(suiv[,3]==u)
                    & !(suiv[,1]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,1]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,1]%in%suivant[1:(i-1),u,3])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,3])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,3]),] }
      
      if (is.vector(suiv)) {
        suivant[i,u,1] = suiv[1]
        suivant[i,u,2] = suiv[2]
        suivant[i,u,3] = suiv[3]}
      
      if (is.matrix(suiv)) {
        suivant[i,u,1] = suiv[1,][1]
        suivant[i,u,2] = suiv[1,][2]
        suivant[i,u,3] = suiv[1,][3]}
      
      i=i+1
    }
    # calcul du coût total ou de la distance totale en fonction de la vlle de départ
    chemp=c(u,c(t(suivant[1:mlst,u,])))
    if (n%%3==0) {
      Distot[u]=sum(wl[,u]) + min(Dst[suivant[mlst,u,3],villes[-chemp][1]]
                                  + Dst[villes[-chemp][1],villes[-chemp][2]]
                                  + Dst[villes[-chemp][2],u],
                                  Dst[suivant[mlst,u,3],villes[-chemp][2]]
                                  + Dst[villes[-chemp][2],villes[-chemp][1]]
                                  + Dst[villes[-chemp][1],u])
    }else if (n%%3==1) {
      Distot[u]=sum(wl[,u]) + Dst[suivant[mlst,u,3],u]
    }else{
      Distot[u]=sum(wl[,u]) + Dst[suivant[mlst,u,3],villes[-chemp]]
      + Dst[villes[-chemp],u]
    }
  }
  Distot
  # calcul du coût total minimal (ou distance totale minimal)
  DistMin = min(Distot)
  # Identifier les points de départ correspondant au minimum du coût total
  wv=which(Distot==DistMin,arr.ind = TRUE)
  nbr=length(wv)
  # Identifier le cycle le plus court ou le moins coûteux permettant de relier le n points considérés
  
  chempath = list()
  
  for (k in 1:nbr) {
    chemp=c(wv[k],c(t(suivant[1:mlst,wv[k],])))
    if (n%%3==0) {
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])), villes[-chemp],wv[k])
    }else if (n%%3==1) {
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])),wv[k])
    }else{
      chemp=c(wv[k],c(t(suivant[1:mlst,wv[k],])))
      
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])), villes[-chemp],wv[k])
    }
  }
  return(list(DistMin, chempath, Dst))
}

