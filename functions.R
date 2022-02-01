pollutantmean <- function(directory,pollutant,id=1:332){
  if(pollutant=="sulfate"){
    coli=2
  }
  else if(pollutant=="nitrate"){
    coli=3
  }
  csum <- 0
  fsum <- 0
  for(i in id){
    if(i<10){
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/00",i,".csv",sep="")
    }
    else if(i>=10 && i<100){
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/0",i,".csv",sep="")
    }
    else{
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/",i,".csv",sep="")
    }
    f <- read.csv(f_name)
    good <- !is.na(f[,coli])
    csum <- csum + sum(good)
    fsum <- fsum + sum(f[,coli],na.rm=TRUE) 
  }
  m=fsum/csum
  m
}

complete<-function(directory,id=1:332){
  m <- matrix(nrow=length(id),ncol=2)
  mi <- 1
  for(i in id){
    if(i<10){
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/00",i,".csv",sep="")
    }
    else if(i>=10 && i<100){
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/0",i,".csv",sep="")
    }
    else{
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/",i,".csv",sep="")
    }
    f <- read.csv(f_name)
    good <- !is.na(f[,2])&!is.na(f[,3])
    m[mi,1] <- i
    m[mi,2] <- sum(good)
    mi<-mi+1
  }
  x <- data.frame(id=m[,1],nobs=m[,2])
  x
}

corr <- function(directory, threshold=0){
  corrl <- numeric()
  for(i in 1:332){
    if(i<10){
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/00",i,".csv",sep="")
    }
    else if(i>=10 && i<100){
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/0",i,".csv",sep="")
    }
    else{
      f_name <- paste("C:/Users/mahmo/Documents/R/R Programming/Assignment 1/",directory,"/",i,".csv",sep="")
    }
    f <- read.csv(f_name)
    good <- !is.na(f[,2])&!is.na(f[,3])
    if(sum(good)>threshold){
      corrl[i] <- cor(f[good,][,2],f[good,][,3])
    }
    else{
      next
    }
    }
  aaa<-na.omit(corrl)
  aaa
}