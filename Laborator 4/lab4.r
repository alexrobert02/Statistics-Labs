#ex_rezolvat_1
matrix_product = function(A, B, C) {
  n = nrow(A);
  r = matrix( nrow = n, ncol = 1);
  x = matrix( nrow = n, ncol = 1);
  y = matrix( nrow = n, ncol = 1);
  r = sample(0:1, n, replace = TRUE);
  for(i in 1:n) {# x = Br
    x[i] = 0;
    for(j in 1:nrow(B))
      x[i] = (x[i]+ B[i,j]*r[j])%%2;
  }
  for(i in 1:nrow(B)) {# y = Ax = ABr
    y[i] = 0;
    for(j in 1:n)
      y[i] = (y[i]+ A[i,j]*x[j])%%2;
  }
  for(i in 1:n) {# x = Cr
    x[i] = 0;
    for(j in 1:n)
      x[i] = (x[i]+ C[i,j]*r[j])%%2;
  }
  for(i in 1:n) {# verify if ABr==Cr
    if(y[i] !=x[i])
      return(FALSE);
  }
  return(TRUE);
}

x=c(5,4,2,6,8,9,2,3,1)
M=matrix(x,3,3)
N=matrix(x,3,3)
P=M*N
matrix_product(M,N,P)

#ex_rezolvat_2
tree_eval = function(i,leaves) {
  a = runif(1, 0, 1); len = length(leaves);
  if(log(i,2) >= log(len,2) - 1) { # copiii nodului i sunt frunze
    if(a <= 0.5) {
      if(leaves[2*i - len + 1] == 0)
        return(leaves[2*i +1 -len + 1]);
      return(1);}
    else {
      if(leaves[2*i + 1 -len + 1] == 0)
        return(leaves[2*i -len + 1]);
      return(1);
    }
  }
  if((floor(log(i,2))%% 2 == 0)){ # nodul i este de tip MIN
    if(a <= 0.5) {
      if(tree_eval (2*i, leaves) == 1)
        return(tree_eval (2*i + 1, leaves));
      return(0);
    }
    else{
      if(tree_eval (2*i +1, leaves) == 1)
        return(tree_eval (2*i, leaves));
      return (0);
    }
  }
}
game_tree_eval = function(leaves) {
  return(tree_eval(1, leaves));
}

leaves = c(0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0)
game_tree_eval(leaves)

#ex_1
ex1=function(){
  p=runif(1,100)
  x=sample.int(10,size=10,replase = FALSE, prob = NULL)
}