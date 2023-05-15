
heurst_pvc_class_4 <- function (n=19, sim=2, param1=0, param2=3, seed=1240, Matchoice=Mat_default){
  
  set.seed(seed)
  
  if (n%%4==0) {
    mlst=(n-4)/4
  }else if (n%%4==1) {
    mlst=(n-1)/4
  }else if (n%%4==2) {
    mlst=(n-2)/4
  }else{
    mlst=(n-3)/4
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
  
  
  if (sim==0) {
    Dst=Matchoice
  }else{
    Dst=floor(t(M)%*%M)
  }
  
  Dst=Dst-diag(diag(Dst),n,n)
  
  wl=matrix(0,mlst,n)
  suivant <- array(0, dim = c(mlst,n, 4))
  Dismat <- array(0, dim = c(n,n,n,n,n))
  Distot=1:n
  
  # Définir la fonction qui calcule la distance totale reliant le point u à deux autres points v et w
  Dist= function(x,v,w,y,z){ tern= Dst[x,v] + Dst[v,w]+ Dst[w,y]+ Dst[y,z]; return(tern)}
  
  for (j in 1:n) {
    for (l in 1:n) {
      for (q in 1:n) {
        for (r in 1:n) {
          for (t in 1:n) {
            if ((j==l) || (j==q) || (j==r) || (j==t) || (l==q) || (l==r)  || (l==t) || (q==r) || (q==t) || (r==t)) {
              Dismat[j,l,q,r,t]=Inf
            }else{
              Dismat[j,l,q,r,t]=Dist(j,l,q,r,t)
            }
          }
          
        }
        
      }
      
    }
    
  }
  
  for (u in 1:n) {
    # u est le point de départ
    
    # Calculer la plus petite distance qui relie le point u à trois autres points à visiter
    wl[1,u]<- min(Dismat[u,,,,])
    # Identifier tous les triplets de points les plus proches de u
    suiv_1<-which(Dismat[u,,,,]==wl[1,u],arr.ind = TRUE)
    # Choisir un des couples de points les plus proches de u et en faire les étape 1 et 2 du voyage
    suiv1 <- suiv_1[suiv_1[,1]!=suiv_1[,2] & suiv_1[,1]!=suiv_1[,3] & suiv_1[,1]!=suiv_1[,4] & suiv_1[,2]!=suiv_1[,3] & suiv_1[,2]!=suiv_1[,4]
                    & suiv_1[,3]!=suiv_1[,4]
                    & suiv_1[,1]!=u & suiv_1[,2]!=u & suiv_1[,3]!=u & suiv_1[,4]!=u,]
    
    if (is.vector(suiv1)) {
      suivant[1,u,1] = suiv1[1]
      suivant[1,u,2] = suiv1[2]
      suivant[1,u,3] = suiv1[3]
      suivant[1,u,4] = suiv1[4]
    }
    
    if (is.matrix(suiv1)) {
      suivant[1,u,1] = suiv1[1,][1]
      suivant[1,u,2] = suiv1[1,][2]
      suivant[1,u,3] = suiv1[1,][3]
      suivant[1,u,4] = suiv1[1,][4]
    }
    
    i=2
    while (i<=mlst) {
      # Calculer la plus petite distance qui sépare le point de la i-1 eme étape des autres points non encore visités
      wl[i,u]<- min(Dismat[suivant[i-1,u,4],
                           c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3],-suivant[1:(i-1),u,4]),
                           c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3],-suivant[1:(i-1),u,4]),
                           c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3],-suivant[1:(i-1),u,4]),
                           c(-u,-suivant[1:(i-1),u,1],-suivant[1:(i-1),u,2],-suivant[1:(i-1),u,3],-suivant[1:(i-1),u,4])])
      # Identifier tous les couples de points non encore visités, les plus proches du second point de la i-1 eme étape
      suiv<- which(Dismat[suivant[i-1,u,4],,,,]==wl[i,u],arr.ind = TRUE)
      
      # subset(m, m[,4] == 16)
      # Choisir un des couples de points les plus proches de la i-1 eme étape et en faire l'étape i
      
      if (is.matrix(suiv)) {
        suiv = suiv[!(suiv[,1]==u) & !(suiv[,2]==u) & !(suiv[,3]==u) & !(suiv[,4]==u)
                    & !(suiv[,1]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,4]%in%suivant[1:(i-1),u,1])
                    & !(suiv[,1]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,4]%in%suivant[1:(i-1),u,2])
                    & !(suiv[,1]%in%suivant[1:(i-1),u,3])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,3])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,3])
                    & !(suiv[,4]%in%suivant[1:(i-1),u,3])
                    & !(suiv[,1]%in%suivant[1:(i-1),u,4])
                    & !(suiv[,2]%in%suivant[1:(i-1),u,4])
                    & !(suiv[,3]%in%suivant[1:(i-1),u,4])
                    & !(suiv[,4]%in%suivant[1:(i-1),u,4]),] }
      
      if (is.vector(suiv)) {
        suivant[i,u,1] = suiv[1]
        suivant[i,u,2] = suiv[2]
        suivant[i,u,3] = suiv[3]
        suivant[i,u,4] = suiv[4]}
      
      if (is.matrix(suiv)) {
        suivant[i,u,1] = suiv[1,][1]
        suivant[i,u,2] = suiv[1,][2]
        suivant[i,u,3] = suiv[1,][3]
        suivant[i,u,4] = suiv[1,][4]}
      
      i=i+1
    }
    # Chemin suivi du point u jusqu'à l'étape mlst
    chemp=c(u,c(t(suivant[1:mlst,u,])))
    
    # Définition d'une fonction de distance
    Drest = function (s,r) {
      vel=Dst[suivant[mlst,u,4],s] + Dst[s,r] + Dst[r,u]
      return(vel)}
    
    # Création d'une matrice carrée nxn
    Drit=matrix(1:n^2,n,n)
    for (s in 1:n) {
      for (r in 1:n) {
        Drit[s,r]=Drest(s,r)
      }
    }
    
    Drest2 = function (s,r,x) {
      vel2=Dst[suivant[mlst,u,4],s] + Dst[s,r] + Dst[r,x] + Dst[x,u]
      return(vel2)}
    
    # Création d'un array nxnxn
    Drit2=array(1:n,c(n,n,n))
    for (s in 1:n) {
      for (r in 1:n) {
        for (x in 1:n) {
          Drit2[s,r,x]=Drest2(s,r,x)
        }
      }
    }
    
    # calcul du coût total ou de la distance totale en fonction de la ville de départ
    if (n%%4==0) {
      # optimiser la fin de parcours
      Va=c(villes[-chemp][1],villes[-chemp][2],villes[-chemp][3])
      vict2=c(Drit2[Va[1],Va[2],Va[3]],  Drit2[Va[2],Va[1],Va[3]],  Drit2[Va[1],Va[3],Va[2]],  Drit2[Va[3],Va[1],Va[2]],  Drit2[Va[2],Va[3],Va[1]],  Drit2[Va[3],Va[2],Va[1]])
      dm2=min(vict2)
      # Calculer le coût total ou la distance totale
      Distot[u]=sum(wl[,u]) +dm2
    }else if (n%%4==1) {
      Distot[u]=sum(wl[,u]) + Dst[suivant[mlst,u,4],u]
    }else if (n%%4==2) {
      Distot[u]=sum(wl[,u]) + Dst[suivant[mlst,u,4],villes[-chemp]]
      + Dst[villes[-chemp],u]
    }else{
      # optimiser la fin de parcours
      Va=c(villes[-chemp][1],villes[-chemp][2])
      vict=c(Drit[Va[1],Va[2]],  Drit[Va[2],Va[1]])
      dm=min(vict)
      # Calculer le coût total ou la distance totale
      Distot[u]=sum(wl[,u]) +dm
    }
  }
  Distot
  # calcul du coût total minimal (ou distance totale minimal)
  DistMin = min(Distot)
  # Identifier les points de départ correspondant au minimum du coût total
  wv=which(Distot==DistMin, arr.ind = TRUE)
  nbr=length(wv)
  # Identifier le cycle le plus court ou le moins coûteux permettant de relier le n points considérés
  
  for (k in 1:nbr) {
    Drestf1 = function (s,r) {
      vel1=Dst[suivant[mlst,wv[k],3],s] + Dst[s,r] + Dst[r,wv[k]]
      return(vel1)}
    Dritf1=array(1:n,c(n,n))
    for (s in 1:n) {
      for (r in 1:n) {
        Dritf1[s,r]=Drestf1(s,r)
      }
    }
    
    Drestf2 = function (s,r,x) {
      vel2=Dst[suivant[mlst,wv[k],3],s] + Dst[s,r] + Dst[r,x] + Dst[x,wv[k]]
      return(vel2)}
    Dritf2=array(1:n,c(n,n,n))
    for (s in 1:n) {
      for (r in 1:n) {
        for (x in 1:n) {
          Dritf2[s,r,x]=Drestf2(s,r,x)
        }
      }
    }
    
    chempath = list()
    
    # for (k in 1:nbr) {
    chemp=c(wv[k],c(t(suivant[1:mlst,wv[k],])))
    
    if (n%%4==0) {
      # préciser la fin de parcours
      Va=c(villes[-chemp][1],villes[-chemp][2],villes[-chemp][3])
      victf2=c(Dritf2[Va[1],Va[2],Va[3]],  Dritf2[Va[2],Va[1],Va[3]],  Dritf2[Va[1],Va[3],Va[2]],  Dritf2[Va[3],Va[1],Va[2]],  Dritf2[Va[2],Va[3],Va[1]],  Dritf2[Va[3],Va[2],Va[1]])
      dmf2=min(victf2)
      indce=which(victf2==dmf2)
      
      vecst=matrix(1:18,6,3)
      vecst[1,]=c(1,2,3)
      vecst[2,]=c(2,1,3)
      vecst[3,]=c(1,3,2)
      vecst[4,]=c(3,1,2)
      vecst[5,]=c(2,3,1)
      vecst[6,]=c(3,2,1)
      
      # Ecrire le chemin trouvé sous la forme d'un circuit
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])), villes[-chemp][vecst[indce,1]], villes[-chemp][vecst[indce,2]],villes[-chemp][vecst[indce,3]],wv[k])
    }else if (n%%4==1) {
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])),wv[k])
    }else if (n%%4==2) {
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])), villes[-chemp],wv[k])
    }else{
      Va=c(villes[-chemp][1],villes[-chemp][2])
      victf1=c(Dritf1[Va[1],Va[2]],  Dritf1[Va[2],Va[1]])
      dmf=min(victf1)
      indf1=which(victf1==dmf)
      indf2=which(victf1!=dmf)
      chempath[[k]]=c(wv[k],c(t(suivant[1:mlst,wv[k],])), villes[-chemp][indf1], villes[-chemp][indf2],wv[k])
    }
  }
  return(list(DistMin, chempath, Dst))
}

