#I1. Testul Z pentru medie
z_test = function(test_type, x, miu0, sigma, n, alpha) {
  z_score = (x - miu0) / (sigma / sqrt(n))
  print(paste("z_score = ", z_score))
  if (test_type == "stanga") {
    critical_z = qnorm(alpha, 0, 1)
    print(paste("critical_z = ", critical_z))
    if (z_score > critical_z) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
  if (test_type == "dreapta") {
    critical_z = qnorm(1 - alpha, 0, 1)
    print(paste("critical_z = ", critical_z))
    if (z_score < critical_z) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula. ")
    }
  }
}
#I.2
z_test("stanga", 88, 90, sqrt(144), 49, 0.05)
z_test("stanga", 88, 90, sqrt(144), 49, 0.01)

#I.3
#test_type, x, miu, sigma, n, alpha
z_test("simetrica", 85, 75, sqrt(17), 36, 0.01)

#I.4
z_test("stanga", 20.5, 21, 2.5, 100, 0.01) #se accepta
z_test("stanga", 20.5, 21, 2.5, 100, 0.05) #se respinge

#I.5
z_test("stanga", 970, 1000, 85, 100, 0.01)
z_test("stanga", 970, 1000, 85, 100, 0.05)

#I.6
z_test("simetrica", 20, 22, 3, 16, 0.05)


#II.1 Testul t pentru medie
#varianta in care media si deviatia standard (sau dispersia) se dau direct in problema
t_test = function(test_type, x, miu, s, n, alpha) {
  t_score = (x - miu) / (s / sqrt(n))
  print(paste("t_score = ", t_score))
  if (test_type == "stanga") {
    critical_t = qt(alpha, n - 1)
    print(paste("critical_t = ", critical_t))
    if (t_score > critical_t) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
  if (test_type == "dreapta") {
    critical_t = qt(1 - alpha, n - 1)
    print(paste("critical_t = ", critical_t))
    if (t_score < critical_t) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
  if (test_type == "simetrica") {
    critical_t = qt(1 - alpha / 2, n - 1)
    print(paste("critical_t = ", critical_t))
    if (abs(t_score) < abs(critical_t)) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
}

#varianta in care media, deviatia standard si dimensiunea esantionului sunt calculate din fisier

t_test_fisier = function(fisier, test_type, miu, alpha) {
  x=mean(fisier)
  s=sd(fisier)
  n=length(fisier)
  t_score = (x - miu) / (s / sqrt(n))
  print(paste("t_score = ", t_score))
  if (test_type == "stanga") {
    critical_t = qt(alpha, n - 1)
    print(paste("critical_t = ", critical_t))
    if (t_score > critical_t) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
  if (test_type == "dreapta") {
    critical_t = qt(1 - alpha, n - 1)
    print(paste("critical_t = ", critical_t))
    if (t_score < critical_t) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
if (test_type == "simetrica") {
    critical_t = qt(1 - alpha / 2, n - 1)
    print(paste("critical_t = ", critical_t))
    if (abs(t_score) < abs(critical_t)) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
}
#apeluri
#II.2
v=scan("date_prob_II2.txt")
t_test_fisier(v, "simetrica", 34, 0.01)

#test_type, x, miu, s, n, alpha

#II.3
t_test("dreapta", 11.9, 11.4, 0.25, 100, 0.01)
t_test("dreapta", 11.9, 11.4, 0.25, 100, 0.05)

#II.4
v=scan("history.txt")
t_test_fisier(v,"simetrica",80,0.01)

#II.5
t_test("simetrica",52,49,sqrt(89.5),64,0.05)

#II.6
t_test("stanga",29,30,sqrt(5),40,0.05)

####trebuie sa cream un fisier numit "date_prob_II2.txt" ca sa putem rula


#III.1 Testul Z pentru diferenta mediilor
z_test_means = function(test_type, x1, x2, n1, n2, sigma1, sigma2, m0, alpha) {
  z_score = (x1 - x2 - m0) / sqrt(sigma1 ^ 2 / n1 + sigma2 ^ 2 / n2)
  print(paste("z_score = ", z_score))
  if (test_type == "stanga") {
    critical_z = qnorm(alpha, 0, 1)
    print(paste("critical_z = ", critical_z))
    if (z_score > critical_z) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
  if (test_type == "dreapta") {
    critical_z = qnorm(1 - alpha, 0, 1)
    print(paste("critical_z = ", critical_z))
    if (z_score < critical_z) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
  if (test_type == "simetrica") {
    critical_z = qnorm(1 - alpha / 2, 0, 1)
    print(paste("critical_z = ", critical_z))
    if (abs(z_score) < abs(critical_z)) {
      print("Se accepta ipoteza nula.")
    }
    else {
      print("Se respinge ipoteza nula.")
    }
  }
}


#IV Testul F pentru raportul dispersiilor
#varianta in care datele se dau direct in problema
F_test = function(test_type,s1,s2,n1,n2,alpha)
{
  F_score = s1 ^ 2 / s2 ^ 2
  if (test_type == "simetric"){
    critical_F_s = qf(alpha/2, n1-1, n2-1)
    critical_F_d = qf(1-alpha/2, n1 - 1, n2 - 1)
    if (F_score > critical_F_s  || F_score < critical_F_d)
    {
      print("Se accepta ipoteza nula")
    }
    else
    {
      print("Se respinge ipoteza nula")
    }
  }
  if (test_type == "dreapta"){
    critical_F_d = qf(1-alpha, n1 - 1, n2 - 1)
    if (F_score < critical_F_d){
      print("Se accepta ipoteza nula")
    }
    else
    {
      print("Se respinge ipoteza nula")
    }
  }
  if (test_type == "stanga"){
    critical_F_s = qf(alpha, n1 - 1, n2 - 1)
    if (F_score > critical_F_s){
      print("Se accepta ipoteza nula")
    }
  else
  {
    print("Se respinge ipoteza nula")
  }
  }
}
#testare exemplu rezolvat
F_test("simetric",5.05,5.44,120,135,0.01)


#varianta cate datele se preiau din fisiere
#fisier, test_type, alpha
F_test = function(fisier, test_type, alpha) {
  x1 = read.table(fisier, header = TRUE)[["A"]] 
  x2 = read.table(fisier, header = TRUE)[["B"]] 
  n1 = length(x1)
  n2 = length(x2)
  s1 = sd(x1)  
  s2 = sd(x2)
  F_score = s1 ^ 2 / s2 ^ 2
  if (test_type == "simetric") {
    critical_F_s <- qf(alpha/2, n1 - 1, n2 - 1)
    critical_F_d <- qf(1 - alpha/2, n1 - 1, n2 - 1)
    if (F_score > critical_F_s & F_score < critical_F_d) {
      print("Se accepta ipoteza nula")
    } else {
      print("Se respinge ipoteza nula")
    }
  }
  if (test_type == "dreapta") {
    critical_F_d <- qf(1 - alpha, n1 - 1, n2 - 1)
    if (F_score < critical_F_d) {
      print("Se accepta ipoteza nula")
    } else {
      print("Se respinge ipoteza nula")
    }
  }
  if (test_type == "stanga") {
    critical_F_s <- qf(alpha, n1 - 1, n2 - 1)
    if (F_score > critical_F_s) {
      print("Se accepta ipoteza nula")
    } else {
      print("Se respinge ipoteza nula")
    }
  }
}
F_test("program.txt","simetric",0.01)
F_test("program.txt","simetric",0.05)

F_test = function(fisier,test_type,alpha)
{
  x1 = read.table(fisier, header = TRUE)[["A"]] 
  x2 = read.table(fisier, header = TRUE)[["B"]] 
  n1=length(x1)
  n2=length(x2)
  s1=sd(x1)  
  s2=sd(x2)
  F_score = s1 ^ 2 / s2 ^ 2
  if (test_type == "simetric"){
    critical_F_s = qf(alpha/2, n1-1, n2-1)
    critical_F_d = qf(1-alpha/2, n1 - 1, n2 - 1)
    if (F_score > critical_F_s & F_score < critical_F_d)
    {
      print("Se accepta ipoteza nula")
    }
    else
    {
      print("Se respinge ipoteza nula")
    }
  }
  if (test_type == "dreapta"){
    critical_F_d = qf(1-alpha, n1 - 1, n2 - 1)
    if (F_score < critical_F_d){
      print("Se accepta ipoteza nula")
    }
    else
    {
      print("Se respinge ipoteza nula")
    }
  }
  if (test_type == "stanga"){
    
    critical_F_s = qf(alpha, n1 - 1, n2 - 1)
  if (F_score > critical_F_s){
    print("Se accepta ipoteza nula")
  }
  else
  {
    print("Se respinge ipoteza nula")
  }
  }
}


