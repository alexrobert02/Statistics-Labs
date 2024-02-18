ex1var1=function(x)
{
  a=max(x)
  b=min(x)
  c=sum(x)
  d=mean(x)
  e=min(x)/max(x)
  f=length(x[x>=40])
  g=length(x[x<40])/length(x)*100
  return(list(a,b,c,d,e,f,g))
}
x=c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)
ex1var1(x)


ex1var2=function(x)
{
  print(c("max=",max(x)))
  print(c("min=",min(x)))
  print(c("sum=",sum(x)))
  print(c("mean=",mean(x)))
  l=0
  for(i in 1:length(x))
    if (x[i]>=40)
      l=l+1
  print(l)
}
ex1var2(x)


ex2a=function(x)
{
  v=x/sum(x)
  return(v)
}
x=c(1, 2, 3, 4, 5)
ex2a(x)


ex2b=function(x)
{
  v=(x-min(x))/max(x)
  return (v)
}
x=c(1, 2, 3, 4, 5)
ex2b(x)


ex2c=function(x)
{
  y=vector()
  for(k in 1:length(x)-1)
    y[k]=sum(x[1:k])/sum(x[(k+1):length(x)])
  return(y)
}
x=c(1, 2, 3, 4, 5)
ex2c(x)


ex2d=function(x)
{
  y=vector()
  for(k in 1:length(x)-1)
    y[k]=max(x[1:k])/min(x[(k+1):length(x)])
  return(y)
}
x=c(1, 2, 3, 4, 5)
ex2d(x)


ex4=function(n,p,culoare)
{
  print(dbinom(0:n,n,p))
  barplot(dbinom(0:n,n,p),space=0,legend.text="Binomial dist",col=culoare)
}
ex4(18,0.25,"green")
ex4(40,0.5,"blue")
ex4(30,0.8,"yellow")


ex5=function(n,p)
{
  return (max(dbinom(0:n,n,p)))
}
ex5(30, 0.3)


ex5b=function(n,p,k)
{
  x=dbinom(0:n,n,p)
  print(sum(x[0:k]))
}
ex5b(30,0.3,10)


ex5c=function(n,p,k,m)
{
  x=dbinom(0:n,n,p)
  print(sum(x[k:m]))
}
ex5c(30,0.3,6,12)


ex6a=function(n,p)
{
  x=dgeom(0:n,p)
  print(sum(x))
}
ex6a(10,0.2)


ex6b=function(m,p)
{
  x=dgeom(0:m,p)
  print(sum(x[0:m]))
}
ex6b(10,0.2)


ex7=function(n,p,culoare)
{
  x=dgeom(0:n,p)
  barplot(dgeom(0:n,p),col=culoare)
}
ex7(10,0.2,"yellow")


ex8a=function(lambda,n)
{
  return(sum(dpois(0:n,lambda)))
}
ex8a(3,20)


ex8b=function(lambda,n)
{
  return(max(dpois(0:n,lambda)))
}
ex8b(3,20)


#ex8c=function(lambda,m)
#{
#  x=dpois(0:m,lambda)
#  print(sum(x[0:m]))
#}
#ex8c

ex6a = function(p, m) {
  x = dgeom(0:m, p)
  print(sum(x))
}
ex6a(0.2,10)

ex6b = function (n, p, m)
{
  x = dgeom (0: n, p)
  return (length(x[m:n])/length(x))
}
ex6b (10, 0.2, 4)

ex7 = function(p,n)
{
  return(barplot(dgeom(0:n,p)))
}
ex7(0.2,10)

ex8b = function(n, lambda) {
  print("Max:");
  print(max(dpois(0:(n-1), lambda)));
}
ex8b(3,20)

#AA BB
#12 13
#11 13
#13 14
#15 13
#14 13
#12 11
#11 15
#16 12
#13 14
#15 12
#13 15
#11 16
#13 14
#15 11
#16 14
#13 12
#13 15

ex9 = function(file,n,lambda) {
  y = read.table(file, header = T)
  print(dpois(0:(n-1), lambda));
  barplot(dpois(0:(n-1), lambda), space = 0, legend.text="Poisson distribution");
}
ex9("test.txt",20,3)

ex10a = function(file_name) {
  u = read.table(file_name, header = T);
  x = u[['AA']];
  y = u[['BB']];
  print(x);
  print(y);
  plot(x, y, type='p',col='darkred',lwd=5);
}
ex10a('test.txt')

ex10b= function(file_name)
{
  u = read.table(file_name, header = T);
  x = u[['AA']];
  y = u[['BB']];
  v=vector();
  for(k in 1 : length(x)-1)
  {
    v[k]=x[k]*y[k];
    print(v[k] );
  }
}
ex10b('test.txt')

ex10b = function(file_name) {
  u = read.table(file_name, header = T);
  x = u[['AA']];
  y = u[['BB']];
  
  return(x * y)
}

ex10d = function(file_name, p, q) {
  u = read.table(file_name, header = T);
  x = u[['AA']];
  y = u[['BB']];
  a = x[1]; b = y[1];
  dist_min = sqrt((a - p)^2 + (b - q)^2);
  for(i in 2:length(x))
    if(sqrt((x[i] - p)^2 + (y[i] - q)^2) < dist_min){
      a = x[i];
      b = y[i];
      dist_min = sqrt((x[i] - p)^2 + (y[i] - q)^2);
    }
  print("The closest point:");
  print(a);
  print(b);
}
ex10d('test.txt',11,16)


ex10c = function(file_name) {
  u = read.table(file_name, header = T);
  x = u[['AA']];
  y = u[['BB']];
  
  numitor = (x - y) ** 2
  return(abs(x - y) / numitor)
}

ex10c('test.txt')
