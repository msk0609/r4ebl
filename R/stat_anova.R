#'Comparison of raman intensity with multitreatment sample
#'
#'Compare intensity distribution by treatment using anova according to variable
#'@param t.name: columnname of multiple treatment
#'@param var.name: variable name (Raman shift in ex_data)
#'@return statiscal comparison of raman intensity distribution by raman shift (more than 3 treatment)
#'@examples
#'ex=anova_stat(mt,t.name = "Treatment", var.name = "variable")
#'
#'ggplot(ex)+
#'geom_line(aes(x=Var, y=F.value),col="royalblue")+
#'geom_line(aes(x=Var, y=P.value*10), col="red")+
#'theme_bw()
#
#'@export
anova_stat <- function(data, t.name=NULL, var.name=NULL){
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("You need to install package tidyverse to use this function")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("You need to install package dplyr to use this function")
  }
  var=as.vector(unique(data %>% select(var.name)))
  d=data.frame()
  for (i in 1:nrow(var)) {
    temp=subset(data,data[,which(colnames(data)==var.name)]==var[i,])

    stat=aov(temp$value~temp[,which(colnames(data)==t.name)])

    a=as.data.frame(matrix(unlist(summary(stat)),nrow=1))
    new_d=data.frame(variable=var[i,],F.value=a[,7],P.value=a[,9])
    d=rbind.data.frame(d,new_d)
  }
  return(d)
}
