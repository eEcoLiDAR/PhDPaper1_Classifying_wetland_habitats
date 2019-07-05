"
@author: Zsofia Koma, UvA
Aim: Feature caculation functions (based on Tristan article)
"

EigenMetrics = function(X,Y,Z)
{
  xyz=cbind(X,Y,Z) 
  cov_m=cov(xyz) 
  
  if(sum(is.na(cov_m))==0) {
    
    eigen_m=eigen(cov_m)
    
    shapemetrics = list(
      curvature = eigen_m$values[3]/(eigen_m$values[1]+eigen_m$values[2]+eigen_m$values[3]),
      linearity = (eigen_m$values[1]-eigen_m$values[2])/eigen_m$values[1],
      planarity = (eigen_m$values[2]-eigen_m$values[3])/eigen_m$values[1],
      sphericity = eigen_m$values[3]/eigen_m$values[1],
      anisotrophy = (eigen_m$values[1]-eigen_m$values[3])/eigen_m$values[1]
    )
    return(shapemetrics)
  }
}

CoverageMetrics = function(z,classification)
{
  coveragemetrics = list(
    pulsepen_ground = (length(z[classification==2])/length(z))*100,
    cancov = (length(z[z>mean(z)])/length(z))*100,
    dens_perc_b2 = (length(z[classification==1 & z<2])/length(z))*100,
    dens_perc_b2_5 = (length(z[classification==1 & z>2 & z<5])/length(z))*100
  )
  return(coveragemetrics)
}

proportion = function(z, by = 1)
{
  # Normalize
  
  z_norm=z-min(z)
  
  # Define the number of x meters bins from 0 to 100 m
  bk = seq(0, ceiling(100/by)*by, by)
  
  # Compute the p for each bin
  hist = hist(z_norm,bk,plot=FALSE)
  
  # Proportion
  p=(hist$counts/length(z_norm))
  
  return(p)
}


VertDistr_Metrics = function(z)
{
  library("e1071")
  
  p=proportion(z, by = 1)
  p_whnull=p[p>0]
  
  vertdistr_metrics = list(
    zstd = sd(z),
    zvar = var(z),
    zskew = skewness(z),
    zkurto = kurtosis(z),
    canrelrat = (mean(z)-min(z))/max(z)-min(z),
    vertdenrat = (max(z)-median(z))/max(z),
    simpson = 1/sum(sqrt(p)),
    shannon = -sum(p_whnull*log(p_whnull))
  )
  return(vertdistr_metrics)
}

HeightMetrics = function(z)
{
  
  heightmetrics = list(
    zmax = max(z), 
    zmean = mean(z),
    zmedian = median(z),
    z025quantile = quantile(z, 0.25),
    z075quantile = quantile(z, 0.75),
    z090quantile = quantile(z, 0.90),
    zcoeffvar = sd(z)/mean(z),
    zmin = min(z)
  )
  return(heightmetrics)
}

HorizontalMetrics = function(dsm) {
  
  library("snow")
  
  rough_dsm=terrain(dsm,opt="roughness",neighbors=4)
  tpi_dsm=terrain(dsm,opt="TPI",neighbors=4)
  tri_dsm=terrain(dsm,opt="TRI",neighbors=4)
  
  beginCluster(18)
  
  sd_dsm=clusterR(dsm, focal, args=list(w=matrix(1,3,3), fun=sd, pad=TRUE,na.rm = TRUE))
  var_dsm=clusterR(dsm, focal, args=list(w=matrix(1,3,3), fun=var, pad=TRUE,na.rm = TRUE))
  
  endCluster()
  
  names(sd_dsm) <- "sd_dsm"
  names(var_dsm) <- "var_dsm"
  
  horizontal_metrics=stack(rough_dsm,tpi_dsm,tri_dsm,sd_dsm,var_dsm)
  
  return(horizontal_metrics)
}