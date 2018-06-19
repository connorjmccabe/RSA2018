m<-lm(negconc~age+gen+aa+ss+pre+ss:pre,data=cshs)

#extract variance-covariance matrix and parameter estimates of parameters
vcov<-vcov(m)
pes<-m$coefficients

focal.seq<-seq(min(cshs$ss,na.rm=T),max(cshs$ss, na.rm=T),.1)
xi<-as.data.frame(matrix(NA,nrow=2*length(focal.seq),ncol=length(pes)))
colnames(xi)<-names(pes)

xi$`(Intercept)`<-rep(1,nrow(xi))
xi$age<-rep(max(cshs$age,na.rm=T),nrow(xi))
xi$gen<-rep(0,nrow(xi))
xi$aa<-rep(0,nrow(xi))
xi$ss<-rep(focal.seq,2)
xi$pre<-c(rep(-1*sd(cshs$pre,na.rm=TRUE),nrow(xi)/2),rep(1*sd(cshs$pre,na.rm=TRUE),nrow(xi)/2))

matxi<-data.matrix(xi)
xi$`ss:pre`<-xi$ss*xi$pre
matxi<-data.matrix(xi)

sims <- 10000
require(MASS)

#simulate parameters from multivariate normal distribution
simparams<-as.data.frame(mvrnorm(sims,pes,vcov))

lower<-pe<-upper<-rep(NA,nrow(matxi))

res<-list()

for(i in 1:nrow(matxi)){
  simmu <- as.matrix(simparams)%*%matxi[i,]
  # if(m$family$family=="binomial"){simy <- 1/(1 + exp(-simmu))}
  # else if(m$family$link=="log"){simy <- exp(simmu)}
  simy<-simmu
  simy<-sort(simy)
  
  # lower[i]<-quantile(simy,.025)
  # pe[i]<-quantile(simy,.50)
  # upper[i]<-quantile(simy,.975)
  
  length.simy <- length(simy)
  low <- up <- NULL
  
  low <- c( low,simy[trunc((1-.95)/2*length.simy)] )
  up  <- c( up, simy[trunc((1-(1-.95)/2)*length.simy)])
  
  res$lower <- rbind(res$low,low)
  res$pe <- c(res$pe,mean(simy))
  res$upper <- rbind(res$up,up)
}

dfplot<-as.data.frame(cbind(res$lower,res$pe,res$upper,xi$ss,xi$pre),row.names=FALSE)
colnames(dfplot)<-c("lower","pe","upper","focal.seq","level.pre")


m<-lm(negconc~age+gen+aa+ss+pre+ss:pre,data=cshs)

#extract variance-covariance matrix and parameter estimates of parameters
vcov<-vcov(m)
pes<-m$coefficients

focal.seq<-seq(min(cshs$ss,na.rm=T),max(cshs$ss, na.rm=T),.1)
xi<-as.data.frame(matrix(NA,nrow=2*length(focal.seq),ncol=length(pes)))
colnames(xi)<-names(pes)

xi$`(Intercept)`<-rep(1,nrow(xi))
xi$age<-rep(min(cshs$age,na.rm=T),nrow(xi))
xi$gen<-rep(1,nrow(xi))
xi$aa<-rep(1,nrow(xi))
xi$ss<-rep(focal.seq,2)
xi$pre<-c(rep(-1*sd(cshs$pre,na.rm=TRUE),nrow(xi)/2),rep(1*sd(cshs$pre,na.rm=TRUE),nrow(xi)/2))

matxi<-data.matrix(xi)
xi$`ss:pre`<-xi$ss*xi$pre
matxi<-data.matrix(xi)

sims <- 10000
require(MASS)

#simulate parameters from multivariate normal distribution
simparams<-as.data.frame(mvrnorm(sims,pes,vcov))

lower<-pe<-upper<-rep(NA,nrow(matxi))

res<-list()

for(i in 1:nrow(matxi)){
  simmu <- as.matrix(simparams)%*%matxi[i,]
  # if(m$family$family=="binomial"){simy <- 1/(1 + exp(-simmu))}
  # else if(m$family$link=="log"){simy <- exp(simmu)}
  simy<-simmu
  simy<-sort(simy)
  
  # lower[i]<-quantile(simy,.025)
  # pe[i]<-quantile(simy,.50)
  # upper[i]<-quantile(simy,.975)
  
  length.simy <- length(simy)
  low <- up <- NULL
  
  low <- c( low,simy[trunc((1-.95)/2*length.simy)] )
  up  <- c( up, simy[trunc((1-(1-.95)/2)*length.simy)])
  
  res$lower <- rbind(res$low,low)
  res$pe <- c(res$pe,mean(simy))
  res$upper <- rbind(res$up,up)
}

dfplot.agemin<-as.data.frame(cbind(res$lower,res$pe,res$upper,xi$ss,xi$pre),row.names=FALSE)
colnames(dfplot.agemin)<-c("lower","pe","upper","focal.seq","level.pre")

dfplot$`Premeditation Level`[dfplot$level.pre<0]<- "-1 SD"
dfplot$`Premeditation Level`[dfplot$level.pre>0]<- "+1 SD"
dfplot$cond<-"Age 24 Caucasian Male"

dfplot.agemin$`Premeditation Level`[dfplot.agemin$level.pre<0]<- "-1 SD"
dfplot.agemin$`Premeditation Level`[dfplot.agemin$level.pre>0]<- "+1 SD"
dfplot.agemin$cond<-"Age 18 Asian-American Female"

dftot<-as.data.frame(rbind(dfplot,dfplot.agemin))

plot3<-ggplot() +
  geom_line(data=dftot,aes(x=focal.seq,y=pe,group=`Premeditation Level`,linetype=`Premeditation Level`)) +
  geom_ribbon(data=dftot, aes(x=focal.seq, ymin = lower, ymax = upper, group=`Premeditation Level`), alpha = .25) +
  facet_wrap(~cond) +
  xlab("Sensation Seeking") +
  ylab("Count of Negative Alcohol Consequences") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20))

setwd("~/Desktop")
ggsave("plot3.png",plot3, height=10,width=10, units = "in")

mpois<-glm(negconc~age+gen+aa+ss+pre+ss:pre,data=cshs, family="poisson")
table1<-cbind(summary(mpois)$coefficients,exp(mpois$coefficients),exp(confint(mpois)))

write.csv(table1, "~/Desktop/table1rsa.csv",row.names = F)
