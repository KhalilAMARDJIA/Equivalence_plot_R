TOST_binom_plot<-function(x,n,Ref,Loweq,Higheq,conf.level){
     require(ggplot2)
  
  CI.Low<-matrix(binom.test(x,n,Loweq,conf.level = conf.level,alternative = "greater")$conf.int,nrow = 1)[1,1]
  CI.High<-matrix(binom.test(x,n,Higheq,conf.level = conf.level,alternative = "less")$conf.int,nrow = 1)[1,2]
  Stat.diff<-binom.test(x,n,Ref,conf.level = conf.level,alternative = "two.sided")$p.value

  Test<-x/n
  p.Low<-binom.test(x,n,Loweq,conf.level = conf.level,alternative = "greater")$p.value
  p.High<-binom.test(x,n,Higheq,conf.level = conf.level,alternative = "less")$p.value
  data<-data.frame(Test,CI.Low,CI.High,p.Low,p.High)
  data$stat.diff.p_value<-Stat.diff
  
  plot<-ggplot()+
  
  geom_vline(xintercept = Ref,linetype = "dashed",col=3,lwd=1)+#ref value
  geom_text(aes(x = (Ref-(Ref/50)),y=0.55,label="Equivalent devices"),col=3,size=4)+
  
  geom_vline(xintercept = Higheq,linetype = "dashed",lwd=1)+ #Uppereq 
  geom_vline(xintercept = Loweq,linetype = "dashed",lwd=1)+ #Lowereq
  
  geom_point(aes(x=Test,y=0.5),shape=15,size=7, col=1)+
  geom_segment(aes(x = Test, y = 0.5, xend = CI.Low, yend = 0.5), col=1,lwd=2)+ #IClower
  geom_segment(aes(x = Test, y = 0.5, xend = CI.High, yend = 0.5), col=1,lwd=2)+ #ICupper
  geom_text(aes(x = Test,y=0.51,label="OS2-V"),col=1,size=4)+
  
  xlab(label = "Union rate")+
  ylim(0.4,0.6)+
  
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
    scale_x_continuous(labels = scales::percent)
   

if (CI.Low<=Loweq){data$Result<- "inconclusive"}
  else{if (CI.Low>Ref){data$Result<- "Superior"}
    else {if (CI.Low>Loweq & CI.High<Higheq){data$Result<- "Equivalent"} 
      else  {if (CI.Low>Loweq & CI.High>=Higheq){data$Result<- "Not_inferior"}}}}

  print(data)
  plot
}
