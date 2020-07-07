
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
### Part 1: Subject attrition in a cohort design
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

f.var1=function(mm,kk,eta,rho,xi)   # no attrition
  {
  output= 4*(1-xi+(mm-1)*(rho-eta))/(2*kk*mm)
  return(output)
  }


f.var2=function(mm,kk,eta,rho,xi,pp.AB,pp.BA)   # attrition of subjects
{
  pp=pp.AB	# proportion attrition in second time period in each cluster in sequence AB
  nn=round(mm*pp-1)
  diag.mat=matrix(rho,mm,mm)
  diag(diag.mat)=1
  offdiag.mat=matrix(eta,mm,mm)
  diag(offdiag.mat)=xi
  
  VV.upper=cbind(diag.mat,offdiag.mat)
  VV.lower=cbind(offdiag.mat,diag.mat)
  VV=rbind(VV.upper,VV.lower)
  VV=VV[-((2*mm-nn):(2*mm)),-((2*mm-nn):(2*mm))]
  
  XXab=matrix(0,2*mm,3)
  XXab[,1]=1
  XXab[(mm+1):(2*mm),2:3]=1
  XXab=XXab[-((2*mm-nn):(2*mm)),]
  XVXab=t(XXab)%*%solve(VV)%*%XXab
  
  pp=pp.BA	# proportion attrition in second time period in each cluster in sequence BA
  nn=round(mm*pp-1)
  diag.mat=matrix(rho,mm,mm)
  diag(diag.mat)=1
  offdiag.mat=matrix(eta,mm,mm)
  diag(offdiag.mat)=xi
  VV.upper=cbind(diag.mat,offdiag.mat)
  VV.lower=cbind(offdiag.mat,diag.mat)
  VV=rbind(VV.upper,VV.lower)
  VV=VV[-((2*mm-nn):(2*mm)),-((2*mm-nn):(2*mm))]
  XXba=matrix(0,2*mm,3)
  XXba[,1]=1
  XXba[1:mm,3]=1
  XXba[(mm+1):(2*mm),2]=1
  XXba=XXba[-((2*mm-nn):(2*mm)),]
  XVXba=t(XXba)%*%solve(VV)%*%XXba
  
  var.beta=solve(0.5*kk*XVXab+0.5*kk*XVXba)
  var.beta
  return(var.beta[3,3])
}

#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
### Part 2: Cluster attrition in a cohort design
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

f.var3=function(mm,kk,eta,rho,xi,pp.AB,pp.BA)
{
  
  kk.AB=round(kk/2)
  kk.BA=kk-kk.AB
  
  ##################################
  ### sample sizes (yes=dropout; no=no dropout)
  ##################################
  
  kk.yes.AB=round(kk.AB*pp.AB)
  kk.no.AB=kk.AB-kk.yes.AB
  kk.yes.BA=round(kk.BA*pp.BA)
  kk.no.BA=kk.BA-kk.yes.BA
  
  ##################################
  ### clusters that do not drop out
  ##################################
  diag.mat=matrix(rho,mm,mm)
  diag(diag.mat)=1
  offdiag.mat=matrix(eta,mm,mm)
  diag(offdiag.mat)=xi
  
  VV.upper=cbind(diag.mat,offdiag.mat)
  VV.lower=cbind(offdiag.mat,diag.mat)
  VV=rbind(VV.upper,VV.lower)
  
  XXab=matrix(0,2*mm,3)
  XXab[,1]=1
  XXab[(mm+1):(2*mm),2:3]=1
  
  XXba=matrix(0,2*mm,3)
  XXba[,1]=1
  XXba[1:mm,3]=1
  XXba[(mm+1):(2*mm),2]=1
  
  XVXab=t(XXab)%*%solve(VV)%*%XXab
  XVXba=t(XXba)%*%solve(VV)%*%XXba
  
  ##################################
  ### clusters that do drop out
  ##################################
  diag.mat=matrix(rho,mm,mm)
  diag(diag.mat)=1
  VV=diag.mat
  
  XXab=matrix(0,mm,3)
  XXab[,1]=1
  
  XXba=matrix(0,mm,3)
  XXba[,1]=1
  XXba[,3]=1
  
  XVXab.dropout=t(XXab)%*%solve(VV)%*%XXab
  XVXba.dropout=t(XXba)%*%solve(VV)%*%XXba
  
  ##################################
  ### all clusters
  ##################################
  
  var.beta=solve(kk.no.AB*XVXab+kk.no.BA*XVXba+kk.yes.AB*XVXab.dropout+kk.yes.BA*XVXba.dropout)
  return(var.beta[3,3])
}



#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
### Part 3: Cluster attrition in a cross-sectional design
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

f.var4=function(mm,kk,eta,rho)   # no attrition
{
  output= 4*(1+(mm-1)*rho-mm*eta)/(2*kk*mm)
  return(output)
}


f.var5=function(mm,kk,eta,rho,pp.AB,pp.BA)
{
  
  kk.AB=round(kk/2)
  kk.BA=kk-kk.AB
  
  ##################################
  ### sample sizes (yes=dropout; no=no dropout)
  ##################################
  
  kk.yes.AB=round(kk.AB*pp.AB)
  kk.no.AB=kk.AB-kk.yes.AB
  kk.yes.BA=round(kk.BA*pp.BA)
  kk.no.BA=kk.BA-kk.yes.BA
  
  
  ##################################
  ### clusters that do not drop out
  ##################################
  diag.mat=matrix(rho,mm,mm)
  diag(diag.mat)=1
  offdiag.mat=matrix(eta,mm,mm)
  
  VV.upper=cbind(diag.mat,offdiag.mat)
  VV.lower=cbind(offdiag.mat,diag.mat)
  VV=rbind(VV.upper,VV.lower)
  
  XXab=matrix(0,2*mm,3)
  XXab[,1]=1
  XXab[(mm+1):(2*mm),2:3]=1
  
  XXba=matrix(0,2*mm,3)
  XXba[,1]=1
  XXba[1:mm,3]=1
  XXba[(mm+1):(2*mm),2]=1
  
  XVXab=t(XXab)%*%solve(VV)%*%XXab
  XVXba=t(XXba)%*%solve(VV)%*%XXba
  
  ##################################
  ### clusters that do drop out
  ##################################
  diag.mat=matrix(rho,mm,mm)
  diag(diag.mat)=1
  VV=diag.mat
  
  XXab=matrix(0,mm,3)
  XXab[,1]=1
  
  XXba=matrix(0,mm,3)
  XXba[,1]=1
  XXba[,3]=1
  
  XVXab.dropout=t(XXab)%*%solve(VV)%*%XXab
  XVXba.dropout=t(XXba)%*%solve(VV)%*%XXba
  
  ##################################
  ### all clusters
  ##################################
  
  var.beta=solve(kk.no.AB*XVXab+kk.no.BA*XVXba+kk.yes.AB*XVXab.dropout+kk.yes.BA*XVXba.dropout)
  return(var.beta[3,3])
}




