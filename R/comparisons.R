
# Create a function that compares the performance of heuristics

#' Title
#'
#' @param m
#' @param h
#' @param seed_value
#' @param sim_param
#'
#' @return
#' @export
#'
#' @examples
research <- function(m=8, h=0, seed_value=3333, sim_param=sim_param_default) {

## Initialize the tables that will receive the analysis results

perct=matrix(0,2,7)
perctm=matrix(0,2,7)
perct_21=matrix(0,2,7)
perct_43=matrix(0,2,7)
perct_41=matrix(0,2,7)
perct_32=matrix(0,2,7)
perct_31=matrix(0,2,7)
perct_42=matrix(0,2,7)

diff_21=matrix(0,7,m-h)
diff_43=matrix(0,7,m-h)
diff_41=matrix(0,7,m-h)
diff_32=matrix(0,7,m-h)
diff_31=matrix(0,7,m-h)
diff_42=matrix(0,7,m-h)

diff2_21=matrix(0,7,m-h)
diff2_43=matrix(0,7,m-h)
diff2_41=matrix(0,7,m-h)
diff2_32=matrix(0,7,m-h)
diff2_31=matrix(0,7,m-h)
diff2_42=matrix(0,7,m-h)

## Assign the different chosen parameters to the considered laws

for (s in 1:7) {

if (s==1) {
parm1 = sim_param[1,1]
}

if (s==2) {
parm1 = sim_param[1,2]
parm2 = sim_param[2,2]
}

if (s==3) {
parm1 = sim_param[1,3]
parm2 = sim_param[2,3]
}

if (s==4) {
parm1 = sim_param[1,4]
}

if (s==5) {
parm1 = sim_param[1,5]
}

if (s==6) {
parm1 = sim_param[1,6]
parm2 = sim_param[2,6]
}

if (s==7) {
parm1 = sim_param[1,7]
parm2 = sim_param[2,7]
}

odd_distmin = matrix(0,7,m-h)
even_distmin = matrix(0,7,m-h)
diff = matrix(0,7,m-h)

odd_distmin2 = matrix(0,7,m-h)
even_distmin2 = matrix(0,7,m-h)
diff2 = matrix(0,7,m-h)

distmin_34 = matrix(0,7,m-h)
distmin_12 = matrix(0,7,m-h)
diffm = matrix(0,7,m-h)

distmin2_34 = matrix(0,7,m-h)
distmin2_12 = matrix(0,7,m-h)
diffm2 = matrix(0,7,m-h)

for (k in (h+3):(m+2)) {

  ## The context of even occurrences

  result_1 = heurst_pvc_class_1(n=2*k, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)
  result_2 = heurst_pvc_class_2(n=2*k, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)
  result_3 = heurst_pvc_class_3(n=2*k, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)
  result_4 = heurst_pvc_class_4(n=2*k, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)

  odd_distmin[s,k-h-2] = min(result_1[[1]], result_3[[1]])
  even_distmin[s,k-h-2] = min(result_2[[1]], result_4[[1]])
  diff[s,k-h-2] = odd_distmin[s,k-h-2] - even_distmin[s,k-h-2]

  distmin_34[s,k-h-2] = min(result_3[[1]], result_4[[1]])
  distmin_12[s,k-h-2] = min(result_1[[1]], result_2[[1]])
  diffm[s,k-h-2] = distmin_34[s,k-h-2] - distmin_12[s,k-h-2]

  diff_21[s,k-h-2] = result_2[[1]]-result_1[[1]]
  diff_31[s,k-h-2] = result_3[[1]]-result_1[[1]]
  diff_41[s,k-h-2] = result_4[[1]]-result_1[[1]]
  diff_32[s,k-h-2] = result_3[[1]]-result_2[[1]]
  diff_42[s,k-h-2] = result_4[[1]]-result_2[[1]]
  diff_43[s,k-h-2] = result_4[[1]]-result_3[[1]]

## The context of odd occurrences

   result_5 = heurst_pvc_class_1(n=2*k+1, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)
  result_6 = heurst_pvc_class_2(n=2*k+1, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)
  result_7 = heurst_pvc_class_3(n=2*k+1, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)
  result_8 = heurst_pvc_class_4(n=2*k+1, sim=s, param1=parm1, param2=parm2, seed=1240, Matchoice=Mat_default)

  odd_distmin2[s,k-h-2] = min(result_5[[1]], result_7[[1]])
  even_distmin2[s,k-h-2] = min(result_6[[1]], result_8[[1]])
  diff2[s,k-h-2] = odd_distmin2[s,k-h-2] - even_distmin2[s,k-h-2]

  distmin2_34[s,k-h-2] = min(result_7[[1]], result_8[[1]])
  distmin2_12[s,k-h-2] = min(result_5[[1]], result_6[[1]])
  diffm2[s,k-h-2] = distmin2_34[s,k-h-2] - distmin2_12[s,k-h-2]

  diff2_21[s,k-h-2] = result_6[[1]]-result_5[[1]]
  diff2_31[s,k-h-2] = result_7[[1]]-result_5[[1]]
  diff2_41[s,k-h-2] = result_8[[1]]-result_5[[1]]
  diff2_32[s,k-h-2] = result_7[[1]]-result_6[[1]]
  diff2_42[s,k-h-2] = result_8[[1]]-result_6[[1]]
  diff2_43[s,k-h-2] = result_8[[1]]-result_7[[1]]
}
## Joint effects of parity and myopia

message1_parity <- print("The percentages indicated in the first line of the tables are those for which the heuristic of even class prevails over the heuristic of odd class. The tables affected are: perct, perct_21, perct_43, perct_41, perct_32.

The percentages indicated in the second line of the tables are those for which the heuristic of odd class prevails over the heuristic of even class. The tables affected are: perct, perct_21, perct_43, perct_41, perct_32.")

message2_parity <- print("In particular, the perct table compares the best of the even class 4 and 2 heuristics and the best of the odd class 1 and 3 heuristics.")

message3_parity <- print("All tests in this subsection are single-sample and use a 50% threshold. The p_value indicated at the end of a line relates to the comparison test of the sample made up of this line with the retained threshold of 50%.")

perct[1,s]=length(which(diff[s,] >= 0, arr.ind = TRUE))*100/(m-h)
perct[2,s]=length( which(diff2[s,] <= 0, arr.ind = TRUE))*100/(m-h)

ptest_1 <- t.test(jitter(perct[1,]), alternative = "two.sided", mu = 50)
ptest_2 <- t.test(jitter(perct[2,]), alternative = "two.sided", mu = 50)
p_value <- c(ptest_1[[3]], ptest_2[[3]])

dimnames(perct) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value[1] < 0.05) {
  message1_g <- print("The heuristics of even classes are significantly more efficient than those of odd classes when they are globally compared by applying them to even numbers of cities (p_value in the first row).")
}else{
  message1_g <- print("Even class heuristics do not perform significantly better than odd class heuristics when globally compared by applying them to even numbers of cities (p_value in the first row).")
}

if (p_value[2] < 0.05) {
  message2_g <- print("Odd class heuristics perform significantly better than even class heuristics when globally compared by applying them to odd numbers of cities (p_value in the second row).")
}else{
  message2_g <- print("Odd class heuristics do not perform significantly better than even class heuristics when globally compared by applying them to odd numbers of cities (p_value in the second row).")
}

perct_21[1,s]=length(which(diff_21[s,] <= 0, arr.ind = TRUE))*100/(m-h)
perct_21[2,s]=length( which(diff2_21[s,] >= 0, arr.ind = TRUE))*100/(m-h)

p21test_1 <- t.test(jitter(perct_21[1,]), alternative = "two.sided", mu = 50)
p21test_2 <- t.test(jitter(perct_21[2,]), alternative = "two.sided", mu = 50)
p_value_21 <- c(p21test_1[[3]], p21test_2[[3]])

dimnames(perct_21) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_21[1] < 0.05) {
  message1_21 <- print("The class 2 heuristic performs significantly better than the class 1 heuristic when applied with even numbers of cities (p_value in the first row).")
}else{
  message1_21 <- print("The class 2 heuristic does not perform significantly better than the class 1 one when applied with even numbers of cities (p_value in the first row).")
}

if (p_value_21[2] < 0.05) {
  message2_21 <- print("The class 1 heuristic performs significantly better than the class 2 heuristic when applied with odd numbers of cities (p_value in the second row).")
}else{
  message2_21 <- print("The class 1 heuristic does not perform significantly better than the class 2 heuristic when applied with odd numbers of cities (p_value in the second row).")
}

perct_43[1,s]=length(which(diff_43[s,] <= 0, arr.ind = TRUE))*100/(m-h)
perct_43[2,s]=length( which(diff2_43[s,] >= 0, arr.ind = TRUE))*100/(m-h)

p43test_1 <- t.test(jitter(perct_43[1,]), alternative = "two.sided", mu = 50)
p43test_2 <- t.test(jitter(perct_43[2,]), alternative = "two.sided", mu = 50)
p_value_43 <- c(p43test_1[[3]], p43test_2[[3]])

dimnames(perct_43) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_43[1] < 0.05) {
  message1_43 <- print("The class 4 heuristic performs significantly better than the class 3 heuristic when applied with even numbers of cities (p_value in the first row).")
}else{
  message1_43 <- print("The class 4 heuristic does not perform significantly better than the class 3 heuristic when applied with even numbers of cities (p_value in the first row).")
}

if (p_value_43[2] < 0.05) {
  message2_43 <- print("The class 3 heuristic performs significantly better than the class 4 heuristic when applied with odd numbers of cities (p_value in the second row).")
}else{
  message2_43 <- print("The class 3 heuristic does not perform significantly better than the class 4 heuristic when applied with odd numbers of cities (p_value in the second row).")
}

perct_41[1,s]=length(which(diff_41[s,] <= 0, arr.ind = TRUE))*100/(m-h)
perct_41[2,s]=length( which(diff2_41[s,] >= 0, arr.ind = TRUE))*100/(m-h)

p41test_1 <- t.test(jitter(perct_41[1,]), alternative = "two.sided", mu = 50)
p41test_2 <- t.test(jitter(perct_41[2,]), alternative = "two.sided", mu = 50)
p_value_41 <- c(p41test_1[[3]], p41test_2[[3]])

dimnames(perct_41) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_41[1] < 0.05) {
  message1_41 <- print("The class 4 heuristic performs significantly better than the class 1 heuristic when applied with even numbers of cities (p_value in the first row).")
}else{
  message1_41 <- print("The class 4 heuristic does not perform significantly better than the class 1 one when applied with even numbers of cities (p_value in the first row).")
}

if (p_value_41[2] < 0.05) {
  message2_41 <- print("The class 1 heuristic performs significantly better than the class 4 heuristic when applied with odd numbers of cities (p_value in the second row).")
}else{
  message2_41 <- print("The class 1 heuristic does not perform significantly better than the class 4 heuristic when applied with odd numbers of cities (p_value in the second row).")
}

perct_32[1,s]=length(which(diff_32[s,] >= 0, arr.ind = TRUE))*100/(m-h)
perct_32[2,s]=length( which(diff2_32[s,] <= 0, arr.ind = TRUE))*100/(m-h)

p32test_1 <- t.test(jitter(perct_32[1,]), alternative = "two.sided", mu = 50)
p32test_2 <- t.test(jitter(perct_32[2,]), alternative = "two.sided", mu = 50)
p_value_32 <- c(p32test_1[[3]], p32test_2[[3]])

dimnames(perct_32) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_32[1] < 0.05) {
  message1_32 <- print("The class 2 heuristic performs significantly better than the class 3 heuristic when applied with even numbers of cities (p_value in the first row).")
}else{
  message1_32 <- print("The class 2 heuristic does not perform significantly better than the class 3 heuristic when applied with even numbers of cities (p_value in the first row).")
}

if (p_value_32[2] < 0.05) {
  message2_32 <- print("The class 3 heuristic performs significantly better than the class 2 heuristic when applied with odd numbers of cities (p_value in the second row).")
}else{
  message2_32 <- print("The class 3 heuristic does not perform significantly better than the class 2 heuristic when applied with odd numbers of cities (p_value in the second row).")
}

## Effets de myopie pure

message1_myopia <- print("The percentages given in all the rows of the tables are those for which the higher class heuristic prevails over the lower class heuristic. The tables affected are: perct_31, perct_42 and perctm.")

message2_myopia <- print("In particular, the array perctm compares the best of the upper class 4 and 3 heristics and the best of the lower class 1 and 2 heuristics.")

message3_myopia <- print("The 50% threshold is used for all single-sample tests. The p_values of these tests are indicated in the first line of the tables of the myopia section.

The p_values indicated in the second line relate to comparison tests of two samples: the first and the second line. These tests make it possible to determine whether the myopia effect is more accentuated when the same parity heuristics are applied to odd numbers of cities, rather than to even numbers of cities.")

perct_31[1,s]=length(which(diff_31[s,] <= 0, arr.ind = TRUE))*100/(m-h)
perct_31[2,s]=length( which(diff2_31[s,] <= 0, arr.ind = TRUE))*100/(m-h)

p31test_1 <- t.test(c(jitter(perct_31[1,]), jitter(perct_31[2,])), alternative = "two.sided", mu = 50)
p31test_2 <- t.test(jitter(perct_31[1,]), jitter(perct_31[2,]), alternative = "two.sided", mu = 0, paired = TRUE)
p_value_31 <- c(p31test_1[[3]], p31test_2[[3]])

dimnames(perct_31) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_31[1] < 0.05) {
  message1_31 <- print("The myopia effect is significant when class 3 and class 1 heuristics are applied with arbitrary numbers of cities (p_value in the first row).")
}else{
  message1_31 <- print("The myopia effect is not significant when the class 3 and 1 heuristics are applied with arbitrary numbers of cities (p_value in the first row).")
}

if (p_value_31[2] < 0.05) {
  message2_31 <- print("The myopia effect is significantly higher when class 3 and 1 heuristics are applied with odd numbers of cities rather than with even numbers of cities (p_value in the second row).")
}else{
  message2_31 <- print("The myopia effect is not significantly higher when class 3 and 1 heuristics are applied with odd numbers of cities rather than with even numbers of cities (p_value in the second row).")
}

perct_42[1,s]=length(which(diff_42[s,] <= 0, arr.ind = TRUE))*100/(m-h)
perct_42[2,s]=length( which(diff2_42[s,] <= 0, arr.ind = TRUE))*100/(m-h)

p42test_1 <- t.test(c(jitter(perct_42[1,]), jitter(perct_42[2,])), alternative = "two.sided", mu = 50)
p42test_2 <- t.test(jitter(perct_42[1,]), jitter(perct_42[2,]), alternative = "two.sided", mu = 0, paired = TRUE)
p_value_42 <- c(p42test_1[[3]], p42test_2[[3]])

dimnames(perct_42) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_42[1] < 0.05) {
  message1_42 <- print("The myopia effect is significant when class 4 and 2 heuristics are applied with arbitrary numbers of cities (p_value in the first row).")
}else{
  message1_42 <- print("The myopia effect is not significant when class 4 and 2 heuristics are applied with arbitrary numbers of cities (p_value in the first row).")
}

if (p_value_42[2] < 0.05) {
  message2_42 <- print("The myopia effect is significantly higher when class 4 and 2 heuristics are applied with odd numbers of cities rather than with even numbers of cities (p_value in the second row).")
}else{
  message2_42 <- print("The myopia effect is not significantly higher when class 4 and 2 heuristics are applied with odd numbers of cities rather than with even numbers of cities (p_value in the second row).")
}

perctm[1,s]=length(which(diffm[s,] <= 0, arr.ind = TRUE))*100/(m-h)
perctm[2,s]=length( which(diffm2[s,] <= 0, arr.ind = TRUE))*100/(m-h)

ptestm_1 <- t.test(c(jitter(perctm[1,]), jitter(perctm[2,])), alternative = "two.sided", mu = 50)
ptestm_2 <- t.test(jitter(perctm[1,]), jitter(perctm[2,]), alternative = "two.sided", mu = 0, paired = TRUE)
p_value_m <- c(ptestm_1[[3]], ptestm_2[[3]])

dimnames(perctm) <- list(c("even", "odd"), c("rpois", "runif", "rbinom", "rexp", "rchisq", "rf", "|rnorm|"))

if (p_value_m[1] < 0.05) {
  message1_mg <- print("The myopia effect is significant when class 4 and 3 heuristics are globally compared to heuristics 1 and 2, applying them to any number of cities (p_value in the first row).")
}else{
  message1_mg <- print("The myopia effect is not significant when class 4 and 3 heuristics are globally compared to heuristics 1 and 2, by applying them to any number of cities (p_value in the first row).")
}

if (p_value_m[2] < 0.05) {
  message2_mg <- print("The myopia effect is significantly higher when class 4 and 3 heuristics are globally compared to class 1 and 2 heuristics, applying them to odd numbers of cities rather than even numbers of cities (p_value in the second row).")
}else{
  message2_mg <- print("The myopia effect is not significantly higher when class 4 and 3 heuristics are globally compared to class 1 and 2 heuristics, applying them to odd numbers of cities rather than to even numbers of cities (p_value in the second row).")
}
}
return(list(perct, perct_21, perct_43, perct_41, perct_32, perct_31, perct_42, perctm, p_value, p_value_21, p_value_43, p_value_41, p_value_32, p_value_31, p_value_42, p_value_m, message1_g, message2_g, message1_21, message2_21, message1_43, message2_43, message1_41, message2_41, message1_32, message2_32, message1_31, message2_31, message1_42, message2_42, message1_mg, message2_mg, message1_parity, message2_parity, message1_myopia, message2_myopia, message3_myopia, message3_parity))
}



