
heurst_pvc_class_2 <- function (n=6, sim=2, param1=0, param2=3, seed=1240, Matchoice=Mat_default){
  
  set.seed(seed)
  
  if (n%%2==1) {
    mlst=floor(n/2)
  }else{
    mlst=(n-2)/2
  }
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
  
  ## Générer une matrice symétrique, qui joue ici le rôle de la matrice des distances entre les villes.
  
  if (sim==0) {
    Dst=Matchoice
  }else{
    Dst=floor(t(M)%*%M)
  }
  
  Dst=Dst-diag(diag(Dst),n,n)
  
  wl=matrix(0,mlst,n)
  suivant <- array(0, dim = c(mlst,n, 2))
  Dismat <- array(0, dim = c(n,n, n))
  Distot=1:n
  
  # Définir la fonction qui calcule la distance totale reliant le point u à deux autres points v et w
  Dist= function(x,v,w){ z= Dst[x,v] + Dst[v,w]; return(z)}
  
  for (j in 1:n) {
    for (l in 1:n) {
      for (q in 1:n) {
        if ((l==q || j==l) || j==q) {
          Dismat[j,l,q]=Inf
        }else{
          Dismat[j,l,q]=Dist(j,l,q)
        }
        q=q+1
      }
      l=l+1
    }
    j=j+1
  }
  
  for (u in 1:n) {
    # u est le point de départ
    
    # Calculer la plus petite distance qui relie le point u à deux autres points à visiter
    wl[1,u]<- min(Dismat[u,,])
    # Identifier tous les couples de points les plus proches de u
    suiv_1<-which(Dismat[u,,]==wl[1,u],arr.ind = TRUE)
    # Choisir un des couples de points les plus proches de u et en faire les étape 1 et 2 du voyage
    suiv1 <- suiv_1[suiv_1[,1]!=suiv_1[,2]& suiv_1[,1]!=u & suiv_1[,2]!=u,]
    
    if (is.vector(suiv1)) {
      suivant[1,u,1] = suiv1[1]
      suivant[1,u,2] = suiv1[2] 
    }
    
    if (is.matrix(suiv1)) {
      suivant[1,u,1] = suiv1[1,][1]
      suivant[1,u,2] = suiv1[1,][2]
    }
    
    i=2
    while (i<=mlst) {
      # Calculer la plus petite distance qui sépare le point de la i-1 eme étape des autres points non encore visités
      wl[i,u]<- min(Dismat[suivant[i-1,u,2],c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2]), c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2])])
      # Identifier tous les couples de points non encore visités, les plus proches du second point de la i-1 eme étape
      suiv<- which(Dismat[suivant[i-1,u,2],,]==wl[i,u],arr.ind = TRUE)
      
      # subset(m, m[,4] == 16)
      # Choisir un des couples de points les plus proches de la i-1 eme étape et en faire l'étape i
      
      if (is.matrix(suiv)) {
        suiv = suiv[!(suiv[,1]==u) & !(suiv[,2]==u) & !(suiv[,1]%in%suivant[1:(i-1),u,1]) & !(suiv[,2]%in%suivant[1:(i-1),u,1]) & !(suiv[,1]%in%suivant[1:(i-1),u,2]) & !(suiv[,2]%in%suivant[1:(i-1),u,2]),] }
      
      if (is.vector(suiv)) {
        suivant[i,u,1] = suiv[1]
        suivant[i,u,2] = suiv[2] }
      
      if (is.matrix(suiv)) {
        suivant[i,u,1] = suiv[1,][1]
        suivant[i,u,2] = suiv[1,][2] }
      
      i=i+1
    }
    # calcul du coût total ou de la distance totale en fonction de la vlle de départ
    
    if (n%%2==1) {
      Distot[u]=sum(wl[,u]) + Dst[suivant[mlst,u,2],u]
    }else{
      chemp=c(u,c(t(suivant[1:mlst,u,])))
      
      Distot[u]=sum(wl[,u]) + Dst[suivant[mlst,u,2],villes[-chemp]]
      + Dst[villes[-chemp],u]
    }
    
  }
  Distot
  # calcul du coût total minimal (ou distance totale minimal)
  DistMin = min(Distot)
  # Identifier les points de départ correspondant au minimum du coût total
  wv=which(Distot==DistMin, arr.ind = TRUE)
  nbr=length(wv)
  # Identifier le cycle le plus court ou le moins coûteux permettant de relier le n points considérés
  
  chempath = list()
  
  for (k in 1:nbr) {
    
    if (n%%2==1) {
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])),wv[k])
    }else{
      chemp=c(wv[k],c(t(suivant[1:mlst,wv[k],])))
      
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])), villes[-chemp],wv[k])
    }
  }
  return(list(DistMin, chempath, Dst))
}

