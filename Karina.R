library(foreign)
library(haven)
library(mice)
library(VIM)
library(ggplot2)
library(nnet)


KarinaDS <- read_sav("C:/Users/z047185/Desktop/Karina/Karina/NEW3 - exclusie - inclusief 2 pt GRainne 09122016.sav", user_na = F)
KarinaDS <- as.data.frame(KarinaDS)

ggplot() +  ggtitle("")
  geom_bar(aes(y = ..count..,x = as.factor(No_study)),colour="red", fill="darkgreen",data=KarinaDS) +theme_light()


ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(Age_binair)),colour="red", fill="blue",data=KarinaDS)


ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(Gender)),colour="red", fill="blue",data=KarinaDS)





ggplot() + geom_histogram(aes(x = Age),data=KarinaDS,colour = 'red',fill = 'blue') + theme_classic()
ggplot() +  geom_histogram(aes(y = ..count..,x = RBC_before),colour="red", fill="blue",data=KarinaDS)


ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(RBCbefore_binairy)),colour="red", fill="blue",data=KarinaDS)




a= matrix(ncol = dim(KarinaDS)[2], nrow = 2, 
          dimnames = list(c("Missing Percentage","Levels of variable"),names(KarinaDS)) )

for(i in 1:dim(KarinaDS)[2]){
  
  a[1,i]=paste(round(sum(is.na(KarinaDS[,i]))*100/length(KarinaDS[,i]),2), "%",sep="")
  a[2,i]= length(which(!is.na(unique(KarinaDS[,i]))))
  
}



for(i in colnames(a)[which(as.integer(a[2,])<10)] ){
  assign(paste("Plot", k,sep="") , eval(parse(text=paste("ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(", i ,")),
                                                         colour=red, fill=blue,data=KarinaDS") )))
  
}

multiplot(p1, p2, p3, p4, cols=2)


missing.percentage= paste(round(a*100, 2),"%",sep="")
names(missing.percentage)= names(KarinaDS)

missing.percentage[missing.percentage!="0%"]


### Univariate models with Un-imputed data
k=1

for(i in names(KarinaDS)){
  assign(paste("Uni.Fit", k,sep="") ,eval(parse(text=paste( "glm(data=KarinaDS, Bin_Response_50~", i ,", family=binomial)"))) )
    k=k+1
  
}

mlogit(KarinaDS$Klimova_response_3cat ~ Age, KarinaDS)
mod <- multinom(KarinaDS$Klimova_response_3cat ~ Age, KarinaDS)


for(i in (unique(KarinaDS$No_study))){
  
  assign(paste("karina",i,sep=""), value = KarinaDS[KarinaDS$No_study == i,])
  
}

md.pattern(karina1)

mice_plot <- aggr(karina1, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina1), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

mice_plot <- aggr(karina2, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina2), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

mice_plot <- aggr(karina3, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina3), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
mice_plot <- aggr(karina4, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina4), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

mice_plot <- aggr(karina5, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina5), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

mice_plot <- aggr(karina6, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina6), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

mice_plot <- aggr(karina7, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina7), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
mice_plot <- aggr(karina8, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(karina8), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


paterns<-md.pattern(KarinaDS)

pat <- md.pairs(KarinaDS)

pat$mm


pat$rr + pat$rm + pat$mr + pat$mm
# percentage of usable cases to impute row variable from column variable
round(100*(pat$mr+0.01)/(pat$mr+pat$mm+0.01))

imp<- mice(KarinaDS,m = 10 )

fit <- with(imp, polr(as.factor(Klimova_response_3cat) ~ Age , Hess=TRUE))
pool(fit)


