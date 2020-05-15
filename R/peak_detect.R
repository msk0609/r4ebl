#'peak detection with statistical difference
#'
#'statiscal comparison of raman intensity distribution by raman shift (more than 3 treatment)
#'@param g.name: columnname of groupname
#'@param bin: allow peak gap 
#'@param sig.level: criteria p.value to identify peak with significant difference 
#'@return Sp: Compared sp with control, if data not include control or compare add any column (ex: data$fac="none")  
#'@return PS: peak start variable
#'@return PE: peak start variable
#'@return no: temporal peak id while peak detection
#'@return gap: count variable between PS and PE
#'@examples
#'ex=anova_stat(mt,t.name = "Treatment", var.name = "variable")
#'ex$Trt="none"
#'
#'t=peak_detect(ex,g.name = "Trt",bin = 2, sig.level = 0.01)
#'t2=dcast(mt, Sam+Sp+Treatment~variable,mean) %>%  melt(id.var=c("Sam","Sp","Treatment"))
#'write.csv(t2, file = "test.csv",row.names = F)
#'t2=read.csv("test.csv")
#'ggplot()+
#'geom_rect(data=t, inherit.aes = F,aes(xmin=PS, xmax=PE),ymin=-Inf, ymax=Inf,fill="grey90" ,alpha=1)+
#'geom_line(data=t2,aes(x=variable, y=value, col=Treatment))
#'ggplot()+
#'
#'@export
peak_detect <- function(data=data, g.name=NULL, bin=2, sig.level=0.05 ){
  grp=as.vector(unique(data %>% select(g.name)))
  pda=data.frame()
  for (j in 1:nrow(grp)) {
    pds=data.frame()
    pde=data.frame()
    pd=data.frame()
    temp=subset(data,data[,which(colnames(data)==g.name)]==grp[j,1])
    init=temp[1,]
    init$variable=min(temp$variable)-1
    init$p=1
    end=temp[nrow(temp),]
    end$variable=max(temp$variable)+1
    end$p=1
    temp=rbind.data.frame(init,temp,end)
    temp=temp %>% mutate(SN=ifelse(temp$p<sig.level,"Sig","None"))
    head(temp)
    head(temp)
    for (i in 1:(nrow(temp)-1)) {
      t=temp[,which(colnames(temp)=="SN")]
      if(t[i]=="None"&&t[i+1]=="Sig"){
        ps=data.frame("Comp.Sp"=temp[i+1,which(colnames(data)==g.name)],"PS"=temp[i+1,which(colnames(data)=="variable")],"ii"=i+1)
        pds=rbind.data.frame(pds,ps)
        pds
      }else if(t[i]=="Sig"&t[i+1]=="None"){
        pe=data.frame("Comp.Sp"=temp[i,which(colnames(data)==g.name)],"PE"=temp[i,which(colnames(data)=="variable")],"ie"=i)
        pde=rbind.data.frame(pde,pe)
      }else{
        next
      }
    }
    
    pd=cbind.data.frame(pds,pde[,-1]) %>% `colnames<-`(c("Sp","PS","init","PE","ed"))
    pd=pd %>% mutate(no=row.names(pd))
    pda=rbind.data.frame(pda,pd)
  }
  pda$gap=pda$ed-pda$init
  pda=pda[,c(1,2,4,6,7)]
  pda=subset(pda,pda$gap>=bin)
  return(pda)
}