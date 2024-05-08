#Electricy use per-person effectively ends at 2014

for (i in list.files('BACKUPS')) {
  path1 <- file.path('Indicators',i)
  for (j in list.files(path1)){
    path2 <- file.path(path1, j)
    print(path2)
    data = read.csv(path2, header =TRUE, sep=',', check.names=FALSE)
    rownames(data) <- data[,1]
    data <- data[,-1]
    data <- data[,colnames(data)>=2000]
    onames = colnames(data)
    print(c(length(onames),min(onames),max(onames)))
    a = as.integer(max(onames))
    if (a < 2022){
      for (e in (a+1):2022){
        b = as.character(e)
        
        data[ ,b] <- NA
      }
    }
    write.csv(data, path2, row.names = TRUE)
  }
}
get_data <- function(){
  table1 = c()
  for (i in list.files('Indicators')) {
    path1 <- file.path('Indicators',i)
    for (j in list.files(path1)){
      path2 <- file.path(path1, j)
      data = read.csv(path2, header =TRUE, sep=',', check.names=FALSE)
      a = '2014' %in% colnames(data)
      b=length(row.names(data))
      d = sum(is.na(data[,'2014']))
      if (b-d != 0){
        print(path2)
        print(c(a,b,d))
        table1 = c(table1, path2)
      }
    }
  }
  return(table1)
}

table1 <- get_data()
table1


get_names <- function(path){
    data <- read.csv(path, header =TRUE, sep=',', check.names=FALSE)
    data <- data[, 1]
    data = c(data, rep(NA, 214-length(data)))
    return(data)
}

list_it <- function(paths){
  li <- list()
  for (i in paths){
    temp <- get_names(i)
    li <- append(li, list(temp))
  }
  return(li)
}

fulldiff <- function(x,y){
  za <- unlist(setdiff(x,y))
  zb <- unlist(setdiff(y,x))
  return(c(za, zb))
}

li <- list_it(table1)

x = as.vector(Reduce(union, li))
y = as.vector(Reduce(intersect, li))
z = as.vector(fulldiff(x,y))
z = sort(z)
x
y
z
length(z)
length(x)
length(y)
table1[1]
create_dataset <- function(paths, year, countries){
  df <- data.frame(countries)
  c = 0
  for (i in paths){
    c = c+1
    data = read.csv(i, header =TRUE, sep=',', check.names=FALSE)
    print(i)
    rownames(data) <- data[,1]
    data <- data[countries, ]
    df[, as.character(c)] <- data[,year]
  }
  return(df)
}

library(stringr)
x <- create_dataset(table1, "2014", y)
data_names <- x[,1]
length(data_names)
rownames(x) <- x[, 1]
x <- x[,-1]
x
names <- c("Exports (% of gdp)",
           "female employment rate (%)",
           "foreign investment (% of gdp)",
           "gnipercapita ppp (USD)",
           "cell phones (per 100 people)",
           "c02 emmissions per person (tonnes)",
           "electricity use per person",
           "children per women",
           "dtp3 immunization (% of 1 year olds)",
           "newborn mortality rate (per 1000)"
           )
colnames(x) <- names
x[,1]

conv_k <- function(arr){
  arr = lapply(arr, function(y) ifelse(endsWith(y, "k"), as.character(as.numeric(gsub("[^0-9.-]", "", y))*1000), y))
  return(unlist(arr))
}
x
x$`gnipercapita ppp (USD)` <- conv_k(x$`gnipercapita ppp (USD)`)
x$`electricity use per person` <- conv_k(x$`electricity use per person`)
x <- sapply(x, as.numeric )
x <- as.data.frame(x)
rownames(x) <- data_names
x <- na.omit(x)

data <- x
data
cor(data)
library("corrgram")
corrgram(data)

x <- rownames(data)
x <- tolower(x)
names = read.csv("2014Groups.csv", header =TRUE, sep=',', check.names=FALSE)
y <- names[,1]
y <- iconv(y,"WINDOWS-1252","UTF-8")
y <- tolower(y)

search_and_destroy <- function(y, pairs){
  n1 <- pairs[,1]
  n1 <- iconv(n1,"WINDOWS-1252","UTF-8")
  n2 <- pairs[,2]
  for (i in 1:length(y)){
    if (any(n1==y[i])){
      y[i] = n2[n1==y[i]]
    }
  }
  return(y)
}
pairs = read.csv("Conv.csv", header =TRUE, sep=',', check.names=FALSE)
y <- search_and_destroy(y, pairs)
names[,1] <- y
x <- search_and_destroy(x, pairs)
rownames(data) <- x
z <- fulldiff(x,y)
length(z)
length(x)
length(y)
intersect(x,y)
sort(z)
setdiff(x, y)
z = unlist(intersect(x,y))
rownames(names) <- names[,1]
names[z,]
zz <- names[z,]
zz <- merge(data, zz, by="row.names", all=TRUE)
rownames(zz) <- zz[,12]
zz <- zz[,-c(1,12)]
zz<-zz[order(row.names(zz)), ]
zz
write.csv(zz, "Data.csv", row.names = TRUE)
data <- zz
data <- data[,-11]

zz <- read.csv("Data.csv")
rownames(zz) <- zz[,1]
zz <- zz[,-1]
data <- zz[,-11]

#PCA
#Principle component analysis
data <- zz[,-11]

n = scale(data, scale=FALSE)
library(corrgram)
corrgram(data, upper.panel =panel.cor, lower.panel = panel.cor)
rownames(n) <- NULL
colnames(n) <- 
n
pp=princomp(data, cor=TRUE)
plot(pp) # Scree plot of eigenvalues.
plot(cumsum(pp$sdev^2 / sum(pp$sdev^2)), type='b', ylab="Proportion of Variance", xlab="Prin. Comp. Num.")
abline(h=0.75, col="red", lwd=2)
pp
pp$loadings
pp$scores
summary(pp)
plot(pp$scores) # Plot first two principal components.
p1=pp$scores[,1]
p2=pp$scores[,2]
plot(p1,p2) # Plot first two principal components.

points(p1[zz[,11]=="H"],p2[zz[,11]=="H"],pch=20,col="red")
points(p1[zz[,11]=="UM"],p2[zz[,11]=="UM"],pch=20,col="blue")
points(p1[zz[,11]=="L"],p2[zz[,11]=="L"],pch=20,col="green")
points(p1[zz[,11]=="LM"],p2[zz[,11]=="LM"],pch=20,col="yellow")

#Discriminant analysis
get_disc_function <- function(G1, G2){
  n1 <- nrow(G1)
  n2 <- nrow(G2)
  S <- (n1-1)*cov(G1) + (n2-1)*cov(G2)
  S <- S/(n1+n2-2)
  G1_bar <- colMeans(G1)
  G2_bar <- colMeans(G2)
  Sinv <- solve(S)
  a <- Sinv %*% (G1_bar-G2_bar)
  return(a/sqrt(diag(S)))
}

H <- data[zz$Group == c("H"),]
L <- data[zz$Group==c("L"),]
a <- get_disc_function(H,L)
Ha <- as.matrix(H) %*% a
La <- as.matrix(L) %*% a
z1 <- Ha
z2 <- La
z
z1_bar = mean(z1)
z2_bar = mean(z2)
z_sd <- sd(c(Ha, La))
thresh = (z1_bar + z2_bar)/2
n=4.25*z_sd
s <- seq(thresh-n, thresh+n)
plot(s, dnorm(s,z1_bar, z_sd), type="l", col="blue", lwd=2)
lines(s, dnorm(s,z2_bar, z_sd), col="red", lwd=2)
abline(v=thresh, lwd=2)

data$z <- t(a) %*% as.matrix(data)
#data$z <- data$z - min(data$z)
sort(data$z)
mean(data$z)
data$Group <- zz$Group

#Factor analysis
data <- zz[,-11]
d1 <- scale(data)
library(psych)
fa <- fa(r=d1, nfactors=4, rotate="varimax")
summary(fa)
