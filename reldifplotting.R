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