#I.1
exI_1 = function(x)
{
  v=stem(x)
}
x=scan("sample1.txt")
exI_1(x)

#I.3
exI_3 = function(x,intervale_barbati,intervale_femei)
{
  speranta_barbati = x[['male']]
  speranta_femei = x[['female']]
  hist(speranta_barbati,breaks=intervale_barbati,right=F,col="green")
  hist(speranta_femei,breaks=intervale_femei,right=F,col="blue")
}
x=read.csv("life_expect.csv",sep=',',header=T)
exI_3(x,5,7)

#II.1
exII_1 = function(x)
{
  print("Media")
  print(mean(x))
  print("Mediana")
  print(median(x))
}
x=scan("sample1.txt")
exII_1(x)

#II.2
exII_2 = function(x){
  speranta_barbati=x[["male"]]
  speranta_femei=x[["female"]]
  print("Media - Speranta de viata barbati")
  print(round(mean(speranta_barbati)))
  print("Media - Speranta de viata femei")
  print(round(mean(speranta_femei)))
  print("Mediana - Speranta de viata barbati")
  print(round(median(speranta_barbati)))
  print("Mediana - Speranta de viata femei")
  print(round(median(speranta_femei)))
}
x=read.csv("life_expect.csv",sep=",",header=T)
exII_2(x)

#II_3
#tabulate = considera un vector cu valori intregi si numara nr de aparitii ale fiecarui numar intreg
exII_3 = function(x) {
  a = unique(x)
  a[which.max(tabulate(match(x, a)))]
}
x=scan("sample1.txt")
exII_3(x)

#III_1
#prima varianta, cea cu media
outliers_mean=function(x)
{
  j=0;#variabila in care se retine nr de valori aberante
  m=mean(x)
  s=sd(x)
  rez=vector() #variabila in care salvez valorile aberante
  for(i in 1:length(x))
  {
    if(x[i]<=m-2*s|x[i]>=m+2*s)
    {
      j=j+1
      rez[j]=x[i]
    }
  }
  rez
}
z=c(1,91,38,72,13,27,11,19,5,22,20,19,8,17,11,15,13,23,14,17)
summary(z)
outliers_mean(z)

#III_2
#a doua varianta, cea cu intervalul intercuartilic
outliers_iqr=function(x)
{
  j=0;
  y=summary(x)
  q1=y[2] #a 2-a valoare din sumar
  q3=y[5] #a 5-a valoare din sumar
  iqr=q3-q1
  rez=vector() #rezultatul se pune intr-un vector
  for(i in 1:length(x))
  {
    if(x[i]<=q1-3/2*iqr|x[i]>=q3+3/2*iqr)
    {
      j=j+1
      rez[j]=x[i]
    }
  }
  rez
}
z=c(1,91,38,72,13,27,11,19,5,22,20,19,8,17,11,15,13,23,14,17)
summary(z)
outliers_iqr(z)

#III_3
exIII_3 = function(x)
{
  a=quantile(x)
  b=outliers_mean(x) 
  c=outliers_iqr(x) 
  d=summary(x)
  return(list(a,b,c,d))
}
x=scan("sample2.txt")
exIII_3(x)
