library(foreign, lib.loc = "c:/Program Files/R/R-3.3.2/library/")
library(haven,lib.loc = "c:/Program Files/R/R-3.3.2/library/")
library(mice,lib.loc = "c:/Program Files/R/R-3.3.2/library/")
library(VIM,lib.loc = "c:/Program Files/R/R-3.3.2/library/")
library(ggplot2,lib.loc = "c:/Program Files/R/R-3.3.2/library/")
library(nnet,lib.loc = "c:/Program Files/R/R-3.3.2/library/")
library(lme4)
library(npmlt)



KarinaDS <- read_sav("C:/Users/z047185/Desktop/Karina/NEW3 - exclusie - inclusief 2 pt GRainne 09122016.sav", user_na = F)
KarinaDS <- as.data.frame(KarinaDS)


ggplot() +  ggtitle('Barplot of Number of Studies') + 
  geom_bar(aes(y = ..count..,x = as.factor(No_study)),colour="red", fill="darkgreen",data=KarinaDS) +theme_light()

ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(Age_binair)),colour="red", fill="blue",data=KarinaDS)

ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(Gender)),colour="red", fill="blue",data=KarinaDS)

ggplot() + geom_histogram(aes(x = Age),data=KarinaDS,colour = 'red',fill = 'blue') + theme_classic()

ggplot() +  geom_histogram(aes(y = ..count..,x = RBC_before),colour="red", fill="blue",data=KarinaDS)


ggplot() +  geom_bar(aes(y = ..count..,x = as.factor(RBCbefore_binairy)),colour="red", fill="blue",data=KarinaDS)




k=1
for(i in colnames(a)[which(as.integer(a[2,])<10)] ){
  assign(paste("Plot", k,sep="") , eval(parse(text=paste("ggplot() +ggtitle('Barplot of",i," ') +   geom_bar(aes(y = ..count..,x = as.factor(", i ,")),colour='red', fill='darkgreen',data=KarinaDS) +theme_light()") )))
  k=k+1
}

multiplot(Plot1, Plot2, Plot3, Plot4, cols=2)
multiplot(Plot5, Plot6, Plot7, Plot8, cols=2)
multiplot(Plot9, Plot10, Plot11, Plot12, cols=2)
multiplot(Plot13, Plot14, Plot15, Plot16, cols=2)
multiplot(Plot17, Plot18, Plot19, Plot20, cols=2)
multiplot(Plot21, Plot22, Plot23, Plot24, cols=2)
multiplot(Plot25, Plot26, Plot27, Plot28, cols=2)
multiplot(Plot29, Plot30, Plot31, Plot32, cols=2)
multiplot(Plot33, Plot34, Plot35, Plot36, cols=2)
multiplot(Plot37, Plot38, Plot39, Plot40, cols=2)


a= matrix(ncol = dim(KarinaDS)[2], nrow = 2, 
          dimnames = list(c("Missing Percentage","Levels of variable"),names(KarinaDS)) )

for(i in 1:dim(KarinaDS)[2]){
  
  a[1,i]=paste(round(sum(is.na(KarinaDS[,i]))*100/length(KarinaDS[,i]),2), "%",sep="")
  a[2,i]= length(which(!is.na(unique(KarinaDS[,i]))))
  
}



a[1,][a[1,] =="0%"]


### Univariate models with Un-imputed data
k=1

for(i in names(KarinaDS)){
  assign(paste("Uni.Fit", k,sep="") ,eval(parse(text=paste( "glmer(data=KarinaDS, Bin_Response_50~", i ,"+(1|No_study), family=binomial)"))) )
    k=k+1
  
}

mlogit(KarinaDS$Klimova_response_3cat ~ Age, KarinaDS)

mod <- multinom(KarinaDS$Klimova_response_3cat ~ Age, KarinaDS)


npmlt

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


