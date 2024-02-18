# exercitiu rezolvat
disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  cat("Estimare:",4*N_C/N,"val reala:",pi)
}
disc_area(10000)

#1.1
disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if(x*x + y*y +z*z<= 1)
      N_C = N_C + 1;
  }
  cat("Estimare:",8*N_C/N,"val reala:",4*pi/3)
}
disc_area(10000)

#1.2
disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(-2*x*x+5*x-2>=y)
      N_C = N_C + 1;
  }
  cat("Estimare:",4*N_C/N)
}
disc_area(10000)

#2.1-a
ex_2_1a = function(N) {
  sum=0
  for(i in 1:N)
  {
    x=runif(1,0,pi)
    sum=sum+sin(x)*sin(x)
  }
  cat("Estimare",pi*sum/N,"Val reala:",pi/2)
}
ex_2_1a(10)

#2.1-b
ex_2_1b = function(N) {
  sum=0
  for(i in 1:N)
  {
    x=runif(1,1,4)
    sum=sum+exp(x)
  }
  cat("Estimare",3*sum/N,"Val reala:51.87987")
}
ex_2_1b(10000)

#2.1-c
ex_2_1c = function(N) {
  sum=0
  for(i in 1:N)
  {
    x=runif(1,0,1)
    sum=sum+1/sqrt(1-x*x)
  }
  cat("Estimare",sum/N,"Val reala:",pi/2)
}
ex_2_1c(10000)

#2.1-d
ex_2_1d = function(N) {
  sum=0
  for(i in 1:N)
  {
    x=runif(1,1,1000)
    sum=sum+1/(4*x*x-1)
  }
  cat("Estimare",sum*999/N,"Val reala:",log(3)/4)
}
ex_2_1d(10000)

#2.2
ex_2_2_part1 = function(N) {
  sum=0
  for(i in 1:N){
    u=runif(1,0,1000)
    sum=sum+exp(-2*u*u)
  }
  cat("Estimare",sum*999/N,"Val reala:",sqrt(pi/8))
}
#ex_2_2(10000)

ex_2_2_part2 = function(k,N){
  estimates = 0;
  for(i in 1:k)
    estimates[i] = ex_2_2_part1(N);
  print(mean(estimates))
  print(sd(estimates))
}

ex_2_2_part2(3,50000)

#ex_3_1
Nr_days = function() {
  nr_days = 1;
  last_errors = c(9, 15, 13);
  nr_errors = 13;
  while(nr_errors > 0) {
    lambda = mean(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}
MC_nr_days = function(N) {
  s = 0;
  for(i in 1:N)
    s = s + Nr_days();
  return(s/N);
}

MC_nr_days(10000)

#ex_3_2
timpServire = function(N,lambda)
{
  timp = 0
  for(i in 1:N)
  {
    timp = timp + rexp(1,lambda)
  }
  return(timp*60/N)
}
timpServire(10000,12)
timpServire(10000,4)

#4 - exercitiu rezolvat
Nr_days = function() {
  nr_days = 2;
  last_errors = c(18, 22, 28);
  nr_errors = 18;
  while(nr_errors > 0) {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1:2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}
Nr_days();
MC_nr_days_21 = function(N) {
  s = 0;
  for(i in 1:N) {
    if(Nr_days() > 21)
    s = s + 1;
  }
  return(s/N);
}
MC_nr_days_21(5000)

#4-b
ex_4_b = function() {
alfa = 1 - 0.95
z = qnorm(alfa/2)
epsilon = 0.01
p = 0.35
N_min = p*(1 - p)*(z/epsilon)^2
return(N_min);
}
ex_4_b();
MC_nr_days_21(8739)

ex_4_1 = function(g1,g2,N){
  sum=0
  for(i in 1:N)
  {
    x=rgeom(1,g1)
    y=rgeom(1,g2)
    if(x<y*y)
      sum=sum+1
  }
  return(sum/N)
}
ex_4_1(0.3,0.5,5000)

#4.2
vect_initial=function(n){
  vect=vector(mode="logical",length = n)
  i=sample(n,1)
  vect[i]=TRUE
  return(vect)
}
vect_initial(40)


#functie care numara computerele infectate

comp_inf=function(vec,N){
  c=0
  for(i in 1:N){
    if(vec[i]==TRUE){
      c=c+1
    }
  }
  return(c)
}
comp_inf(vect_initial(40),40)
