
heurst_pvc_class_1 <- function (n=20, sim=2, param1=0, param2=3, seed=1240, Matchoice=Mat_default){
  
  set.seed(seed)
  
  if (sim==0) {
    M=Matchoice
  }
  
  
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
  ## Initialiser quelques matrices qui seront utilisées dans le script
  wl=matrix(0,n-1,n)
  suivant=matrix(0,n-1,n)
  Distot=1:n
  
  for (u in 1:n) {
    # u est le point de départ
    # Calculer la plus petite distance entre le point u et un autre point à visiter
    wl[1,u]<- min(Dst[u,-u])
    # Identifier tous les points les plus proches de u
    suiv_1<-which(Dst[u,]==wl[1,u])
    # Choisir un des points les plus proches de u et en faire l'étape 1
    suivant[1,u] = suiv_1[suiv_1!=u][1]
    
    
    i=2
    while (i<=n-1) {
      # Calculer la plus petite distance qui sépare le point de la i-1 eme étape des autres points non encore visités
      wl[i,u]<- min(Dst[suivant[i-1,u],c(-u,-suivant[1:(i-1),u])])
      # Identifier tous les points non encore visités, les plus proches du point de la i-1 eme étape
      suiv<- which(Dst[suivant[i-1,u],]==wl[i,u])
      # Choisir un des points les plus proches de la i-1 eme étape et en faire l'étape i
      suivant[i,u] = suiv[suiv!=u & !(suiv %in% suivant[1:(i-1),u])][1]
      
      i=i+1
    }
    # calcul du coût total ou de la distance totale en fonction de la vlle de départ
    Distot[u]=sum(wl[,u]) + Dst[suivant[n-1,u],u]
  }
  
  Distot
  # calcul du coût total minimal (ou distance totale minimal)
  DistMin=min(Distot)
  # Identifier les points de départ correspondant au minimum du coût total
  wv=which(Distot==DistMin)
  nbr=length(wv)
  # Identifier le cycle le plus court ou le moins coûteux permettant de relier le n points considérés
  chempath = list()
  for (k in 1:nbr) {
    chempath[[k]]=c(wv[k],t(suivant[,wv[k]]),wv[k])
  }
  return(list(DistMin, chempath, Dst))
}

