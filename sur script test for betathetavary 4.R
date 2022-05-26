
library("purrr")
library("ggpubr")
library("cowplot")
#test1<-function(x){
#  f<-max(pluck(x,"time"))
  #stti<-sample(1:g)
  #de<-c(f,stti)
#}

#maxtimes<-map(s,test1)


datasurexperiment<-data#%>%filter(time<=50000)
timemaxex<-max(datasurexperiment$time)
s<-split(datasurexperiment,datasurexperiment$sim)

#stest1<-s[c(1:49000)]


funcsur<-function(i=NULL){

n<-seq(15,120,length.out = 7)
fre<-seq(15,120,length.out = 7)
datalist<-list()
indicator<-1
for (f in n){
  for (l in fre){
test4<-function(x,y){
  for(g in y){
    r5<-pluck(x)
    d<-r5[sample(nrow(r5),size=f,replace=FALSE),]
    print(d)
    print(match(g,y))
print(paste0("fre=",f,"n=",l))
    
    if(g>min(d$time)){
      m<-sum(d <= g)
      q<-mean(r5$time <= g)
      #t<-min(d$time)
      #d<-match(g,y)
      mylist<-list(q,m,timedet=g,theta=sample(r5$theta,1,replace = TRUE),beta=sample(r5$beta,1,replace = TRUE),sim=sample(r5$sim,1,replace = TRUE),truesim=indicator,frequency=l,samplesize=f)
      return(mylist)
    }
  }
}




stti<-sample(1:l,length(s),replace=TRUE)
samp.time <- lapply(stti, function(x) seq(from = x, to = timemaxex, by = l))


dftest4<-map2(s,samp.time,test4)
print("....................simulation set completed.............................")
dftestlistdocall<-data.frame(do.call(rbind,dftest4))
datalist[[indicator]]<-dftestlistdocall
      indicator<-indicator+1
  }
}

dfcompleteheatmap<-do.call(rbind,datalist)
}
cl <- makeCluster(mc <- getOption("cl.cores", 20))
clusterCall(cl,function() library("dplyr"))
clusterCall(cl,function() library("purrr"))
clusterExport(cl=cl, varlist=c("s","timemaxex"),envir = environment())
par_r1<-parLapply(1,fun=funcsur,cl=cl)
stopCluster(cl)
dfcompleteheatmap <- do.call("rbind", par_r1)


dfcompleteheatmap$q<-as.numeric(dfcompleteheatmap$V1)
sum.q<-dfcompleteheatmap%>%group_by(beta,theta,frequency,samplesize)%>%summarise_at(vars(q),list(q_mean = mean))
sum.q$anq<-((rdataset1$r_mean)*(as.numeric(sum.q$frequency)/as.numeric(sum.q$samplesize)))
sum.q$absdif<-abs(sum.q$anq-sum.q$q_mean)
sum.q$reldif<-sum.q$absdif/sum.q$q_mean
dfcompleteheatmap$t1<-as.numeric(dfcompleteheatmap$timedet)
#dfcompleteheatmap$d1<-as.numeric(dfcompleteheatmap$d)
time314<-dfcompleteheatmap%>%group_by(beta,theta,frequency,samplesize)%>%summarise_at(vars(t1),list(t_mean = mean))
#steps314<-dfcompleteheatmap%>%group_by(frequency,samplesize)%>%summarise_at(vars(d1),list(steps_mean = mean))


save(dfcompleteheatmap,file="dfheatmap4theta140beta150sursetcluster.rda")
save(sum.q,file="sum.qcompletetheta140beta150sursetcluster.rda")
save(rdataset1,file="rdataset1theta140beta150sursetcluster.Rda")



absdifgg<-ggplot(sum.q,aes(x=as.numeric(theta),y=as.numeric(beta),fill=absdif)) +
  geom_tile() + 
  scale_fill_gradient(low="lightblue",
                      high="darkblue",
                      name="Absolute Difference")+
  theme_minimal()+
  ylab("\U03B2")+
  xlab("\U03B8")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1)+ 
  geom_text(aes(label = round(absdif, 3)),colour="white")

#ggsave(file="absdifregbeta7theta7.pdf")



anqgg<-ggplot(sum.q,aes(x=as.numeric(theta),y=as.numeric(beta),fill=anq)) +
  geom_tile() + 
  scale_fill_gradient(low="#FF7F7F",
                      high="darkred",
                      name="rule of thumb prediction")+
  theme_minimal()+
  ylab("\U03B2")+
  xlab("\U03B8")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1)+ 
  geom_text(aes(label = round(anq, 3)),colour="white")

#ggsave(file="anqvaluesregbeta7theta7.pdf")


q_meangg<-ggplot(sum.q,aes(x=as.numeric(theta),y=as.numeric(beta),fill=q_mean)) +
  geom_tile() + 
  scale_fill_gradient(low="lightgreen",
                      high="darkgreen",
                      name="Simulated detection")+
  theme_minimal()+
  ylab("\U03B2")+
  xlab("\U03B8")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1)+ 
  geom_text(aes(label = round(q_mean, 3)),colour="blue")

#ggsave(file="simulatedvaluesregregbeta7theta7.pdf")



reldifgg<-ggplot(sum.q,aes(x=as.numeric(theta),y=as.numeric(beta),fill=reldif)) +
  geom_tile() + 
  scale_fill_gradient(low="lightyellow",
                      high="#CCCC00",
                      name="Relative Difference")+
  theme_minimal()+
  ylab("\U03B2")+
  xlab("\U03B8")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1) + 
  geom_text(aes(label = round(reldif, 3)),colour="black")

#ggsave(file="reldifclusterregregbeta7theta7.pdf")


collectedgraphs<-ggarrange(anqgg,q_meangg,absdifgg,reldifgg,labels = c("a","b","c","d"),ncol=2,nrow=2)
ggsave(collectedgraphs,width=35,height=35,units = "cm",file="testcollectbeta50theta50surset.png")


############################time test#################################################################
dfcompleteheatmap$timedet1<-as.numeric(dfcompleteheatmap$timedet)

time364<-data.frame(dfcompleteheatmap%>%group_by(beta,theta,frequency,samplesize)%>%summarise_at(vars(timedet1),list(t_mean = mean)))
sum.q$timepred<-log(sum.q$anq/(sum.q$int))/rdataset1$r_mean
sum.q$absdif1<-abs(sum.q$timepred-time364$t_mean)
sum.q$reldif1<-sum.q$absdif1/time364$t_mean
######################################################################################################


ggplot(sum.q,aes(x=as.numeric(frequency),y=as.numeric(samplesize),fill=reldif1)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="Relative time",
                      limits=c(0,1))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1) + 
  geom_text(aes(label = round(reldif1, 3)))

ggsave(file="reldif1regsurbeta150theta20.pdf")




ggplot(sum.q,aes(x=as.numeric(frequency),y=as.numeric(samplesize),fill=absdif1)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="abs time",
                      limits=c(0,50))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1)+ 
  geom_text(aes(label = round(absdif1, 3)))

ggsave(file="absdif1regsurbeta150theta20.pdf")


################################################################################
sum.q$int<-sum.q$anq/exp(rdataset1$r_mean*time364$t_mean)
sum.q$absdif2<-abs((infbegin/900)-sum.q$int)
sum.q$reldif2<-sum.q$absdif2/(infbegin/900)




ggplot(sum.q,aes(x=as.numeric(frequency),y=as.numeric(samplesize),fill=reldif2)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="Relative Difference",
                      limits=c(0,1))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1)+ 
  geom_text(aes(label = round(reldif2, 3)))

ggsave(file="heatmaprel2beta150theta20start10.pdf")

ggplot(sum.q,aes(x=as.numeric(frequency),y=as.numeric(samplesize),fill=absdif2)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="Absolute Difference initial inoculum",
                      limits=c(0,1))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1) + 
  geom_text(aes(label = round(absdif2, 3)))

ggsave(file="heatmapabs2beta150theta20start10.pdf")

###################################################################################################################

sum.q$anq1<-(log(sum.q$anq/sum.q$int)*as.numeric(sum.q$frequency))/(time364$t_mean*as.numeric(sum.q$samplesize))
sum.q$absdif3<-abs(sum.q$q_mean-sum.q$anq1)
sum.q$reldif3<-sum.q$absdif3/sum.q$q_mean

ggplot(sum.q,aes(x=as.numeric(frequency),y=as.numeric(samplesize),fill=absdif3)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="lunchtest",
                      limits=c(0,.25))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio = 1)+ 
  geom_text(aes(label = round(absdif3, 3)))
ggsave(file="heatmapabs3beta150theta20start1.pdf")
           
ggsave(file="heatmaprel3beta150theta20start1.pdf")
##################################################################################################################

