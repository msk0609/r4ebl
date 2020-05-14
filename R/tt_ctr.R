#'Comparison of raman spectrum intensity with control
#'
#'Compare intensity between sample and control according to raman shift
#'@param g.name: group name of comparison target
#'@param var.name: variable name (Raman shift)
#'@param ctr.name: control sp in group/ control species can be selected more than two
#'@return statiscal comparison of raman intensity distributionby raman shift
#'@examples
#'ft=ctr.tt(data = ex_rm,g.name ="Sp",var.name = "variable", ctr.name = c("a0","a12"))
#'
#'ggplot(ft, aes(x=variable, y=p,group=Comp.sp))+
#'geom_line()+
#'facet_wrap(.~Comp.sp, ncol = 1)
#
#'@export
ctr.tt <- function(data=data, g.name=NULL,var.name=NULL, ctr.name=NULL){


  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("You need to install package tidyverse to use this function")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("You need to install package dplyr to use this function")
  }

  grp=as.vector(unique(data %>% select(g.name)))
  var=as.vector(unique(data %>% select(var.name)))
  d=data.frame()
  for (i in 1:nrow(var)) {
    temp=subset(data,data[,which(colnames(data)==var.name)]==var[i,])
    ctemp=filter(temp, temp[,which(colnames(data)==g.name )] %in% ctr.name)
    head(ctemp2)
    for (j in 1:nrow(grp)) {
      ttemp=subset(temp,temp[,which(colnames(data)==g.name)]==grp[j,])
      tt=t.test(ctemp$value, ttemp$value, var.equal = T)
      new_d=data.frame(ctr.name,grp[j,],var[i,], tt$statistic,tt$stderr,tt$p.value)
      d=rbind.data.frame(d,new_d)
    }
  }
  d=d %>% `colnames<-`(c("Ctr.sp","Comp.sp","variable","statistic","stderr","p"))
  d=subset(d,d[,2]!=ctr.name) %>% droplevels()
  return(d)
}
