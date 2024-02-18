normal_density = function(limit,a) {
  t = seq(-limit, limit, length = a)
  f = 1/sqrt(2*pi)*exp(-t^2/2)
  plot(t, f, type = "l", lwd = 1)
}
normal_density(6,400)

ex1_1=function(miu,disp)
{
  sigma=sqrt(disp)
  t1=seq(miu-sigma, miu+sigma, length = 2*sigma)
  t2 = seq(miu-2*sigma, miu+2*sigma, length = 4*sigma)
  t3 = seq(miu-3*sigma, miu+3*sigma, length = 6*sigma)
  f1 = 1/sqrt(2*pi)*exp(-t1^2/2)
  f2 = 1/sqrt(2*pi)*exp(-t2^2/2)
  f3 = 1/sqrt(2*pi)*exp(-t3^2/2)
  plot(t1,f1,type="l",lwd=1)
  plot(t2,f2,type="l",lwd=1)
  plot(t3,f3,type="l",lwd=1)
}
ex1_1(0,1)
ex1_1(2,5)
ex1_1(1,9)

selection_mean = function(filename) {
  x = scan(filename);
  m = mean(x)
  return (m)
}
selection_mean("history.txt")

#3.1
zconfidence_interval = function(n,x,alfa,miu)
{
  sigma = sqrt(miu)
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = x - critical_z*sigma/sqrt(n)
  b = x + critical_z*sigma/sqrt(n)
  interval = c(a, b)
  interval
}
#3.2
zconfidence_interval(25,67.53,0.1,100)

#3.3
zconfidence_interval(50,5,0.05,0.25)

#3.6
zconfidence = function(filename,alfa)
{
  x=scan(filename)
  n=length(x)
  sample_mean=mean(x)
  sigma = sd(x)
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z*sigma/sqrt(n)
  b = sample_mean + critical_z*sigma/sqrt(n)
  interval = c(a, b)
  interval
}
zconfidence("history.txt",0.05)

#4.1
t_conf_interval = function(n,x,alfa,s)
{
  sample_mean=x
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a, b)
  interval
}
t_conf_interval(60,3.3,0.05,0.4)

#4.2
t_conf_interval(196,44.65,0.01,sqrt(2.25))

#5.2
#p0=0.1
#n=150
#succese=20
#alfa=0.05
#1.definim ipoteza nula h0
#p=p0
#2.definim ipoteza alternativa halfa
#p>p0=0.1(as dr)
#3
#alfa=0.05
#4.se calculeaza scorul testului(z)
#z=pprim-p0/sqrt(po*(1-p0)/n)=1.36
#pprim=succese/N
#5.se determina valoarea cut
#zstar=qnorm(1-alfa)=qnorm(0.95)
#6.z=1.36<qstar=1.65=>h0

ex=function(p0,n,succese,alfa,tiptest)
{
  pprim=succese/n
  z=(pprim-p0)/sqrt(p0(1-p0)/n)
  if(tiptest="dreapta")
  {
    zstar=qnorm(1-alfa)
    if(z<zstar){
      print("se accepta ipoteza nula")
    else
      print("se respinge ipoteza nula")
    }
  }
  if(tiptest=stanga)
    zstar=qnorm(alfa)
}
ex(0.1,150,20,0.05,"dreapta")
