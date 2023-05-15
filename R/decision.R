
Decision <- function(Mat_distance = Mat_deflt, seed_value=1240) {
  
  Numb=dim(Mat_distance)[1]
  
  result = list()
  
  if (Numb <= 40) {

  result[[1]] = heurst_pvc_class_1(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  result[[2]] = heurst_pvc_class_2(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  result[[3]] = heurst_pvc_class_3(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  result[[4]] = heurst_pvc_class_4(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  
vect_result = c(result[[1]][[1]],result[[2]][[1]],result[[3]][[1]],result[[4]][[1]])

}

  if (Numb >= 41 && Numb <= 80) {

  result[[1]] = heurst_pvc_class_1(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  result[[2]] = heurst_pvc_class_2(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  result[[3]] = heurst_pvc_class_3(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  
vect_result = c(result[[1]][[1]],result[[2]][[1]],result[[3]][[1]])

  }
  
  if (Numb >= 81 && Numb <= 200) {

  result[[1]] = heurst_pvc_class_1(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  result[[2]] = heurst_pvc_class_2(n=Numb, sim=0, param1=parm1, param2=parm2, seed = seed_value, Matchoice=Mat_distance)
  
vect_result = c(result[[1]][[1]],result[[2]][[1]])

  }
  
   
  if (Numb >= 201) {

  result[[1]] = heurst_pvc_class_1(n=Numb, sim=0, param1=parm1, param2=parm2, Matchoice=Mat_distance)
  
vect_result = c(result[[1]][[1]])

}
mn = min(vect_result)
  
h <- which(vect_result[]==mn, arr.ind = TRUE)

return(list(mn, h, result[[h[1]]]))
  
}



