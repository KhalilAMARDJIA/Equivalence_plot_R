binomprp_plot<-function(x,n,Ref,Loweq,Higheq,conf.level){
     require(ggplot2)
  
  CI<-matrix(binom.test(x,n,Ref,conf.level = conf.level)$conf.int,nrow = 1) 
  CI.Low<-CI[1,1]
  CI.High<-CI[1,2]
  Test<-x/n
  p.value<-binom.test(x,n,Ref,conf.level = conf.level)$p.value
  data<-data.frame(CI.Low,CI.High,Test,p.value)
  
  plot<-ggplot()+
  
  geom_vline(xintercept = Ref,linetype = "dashed",col=3,lwd=1)+#ref value
  geom_text(aes(x = (Ref-(Ref/50)),y=0.55,label="Ref value"),col=3,size=4)+
  
  geom_vline(xintercept = Higheq,linetype = "dashed",lwd=1)+ #Uppereq 
  geom_vline(xintercept = Loweq,linetype = "dashed",lwd=1)+ #Lowereq
  
  geom_point(aes(x=Test,y=0.5),shape=15,size=7,, col=1)+
  geom_segment(aes(x = Test, y = 0.5, xend = CI.Low, yend = 0.5), col=1,lwd=2)+ #IClower
  geom_segment(aes(x = Test, y = 0.5, xend = CI.High, yend = 0.5), col=1,lwd=2)+ #ICupper
  geom_text(aes(x = Test,y=0.51,label="Test value"),col=1,size=4)+
  
  xlab(label = "Scale")+
  ylim(0.4,0.6)+
  
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
   
  print(data)
  plot
}
