
decision_with_map <- function(towns_list, seed_value =1240) {

  numb <- length(towns_list)
  long_lat_cities <- read.csv(system.file("AppData/worldcities.csv",package = "Zilde"))
  head(long_lat_cities)

  long_lat_cities <- as.matrix(long_lat_cities)

  start <- c(long_lat_cities[, 3][long_lat_cities[, 1]==towns_list[1]], long_lat_cities[, 4][long_lat_cities[, 1]==towns_list[1]]) # Paris
  end <- c(long_lat_cities[, 3][long_lat_cities[, 1]==towns_list[1]], long_lat_cities[, 4][long_lat_cities[, 1]==towns_list[1]]) # Paris

  # Cities coordinates

  latus = c()
  longus = c()
  for (i in 1:numb) {

    lats = long_lat_cities[,3][long_lat_cities[,1]==towns_list[i]]
    latus[i]=lats[1]
    longs = long_lat_cities[,4][long_lat_cities[,1]==towns_list[i]]
    longus[i]=longs[1]
  }
  latus <- as.numeric(latus)
  longus <- as.numeric(longus)


  cities <- data.frame(towns_list, latus, longus)


  Mat_distance <- matrix(0,numb,numb)
  ptsmat <- t(cities[, c("longus", "latus")])

  for (i in 1:numb) {
    Mat_distance[i,] <- distHaversine(ptsmat[,i],t(ptsmat))

  }
  result = list()
  result1 = list()
  result2 = list()
  result3 = list()
  result4 = list()

  if (numb <= 40) {

    result1 = heurst_pvc_class_1(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result2 = heurst_pvc_class_2(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result3 = heurst_pvc_class_3(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result4 = heurst_pvc_class_4(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)

    result = list(result1,result2,result3,result4)

    vect_result = c(result1[[1]],result2[[1]],result3[[1]],result4[[1]])

  }

  if (numb >= 41 && numb <= 80) {

    result1 = heurst_pvc_class_1(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result2 = heurst_pvc_class_2(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result3 = heurst_pvc_class_3(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result = list(result1,result2,result3)
    vect_result = c(result1[[1]],result2[[1]],result3[[1]])

  }

  if (numb >= 81 && numb <= 200) {

    result1 = heurst_pvc_class_1(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result2 = heurst_pvc_class_2(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result = list(result1,result2)
    vect_result = c(result1[[1]],result2[[1]])

  }


  if (numb >= 201) {

    result1 = heurst_pvc_class_1(n=numb, sim=0, param1=parm1, param2=parm2, seed=seed_value, Matchoice=Mat_distance)
    result = list(result1)
    vect_result = c(result1[[1]])

  }
  mn = min(vect_result)

  h <- which(vect_result[]==mn, arr.ind = TRUE)

  parc_chif<- result[[h[1]]][[2]][[1]]

  solut = c()
  for (j in 1:(numb+1)) {
    solut[j] = towns_list[parc_chif[j]]
  }

  # data.frame corresponding to the retained solution

  latitude = c()
  longitude = c()
  for (i in 1:(numb+1)) {

    latits = long_lat_cities[,3][long_lat_cities[,1]==solut[i]]
    latitude[i]=latits[1]
    longits = long_lat_cities[,4][long_lat_cities[,1]==solut[i]]
    longitude[i]=longits[1]
  }
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)


  daf <- data.frame(latitude, longitude, solut, row.names = 1:length(latitude))

  return(list(solut, daf, mn, h, result[[h[1]]], Mat_distance))

}

