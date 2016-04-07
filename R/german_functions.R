

calculateStandBasalArea_bitterlich <- function(dbh_m, site, k=4){
  ### Arguments
  ## dbh_m: Diameter at breat heigth in meters
  ## Site: Site ID
  ## k: Z?hlfaktor used for Bitterlich sampling in the field
  res <- aggregate(dbh_m, list(site=site), NROW)
  res$SBA_ha <- res$x * k
  res[,c("site", "SBA_ha")]
}


calculateBasalAreaTaller_bitterlich <- function(dbh_m, site, k=4){
  ### Arguments
  ## dbh_m: Diameter at breat heigth in meters
  ## Site: Site ID
  ## k: Z?hlfaktor used for Bitterlich sampling in the field
  ## Todo: This function is extremely slow!
  BAT_ha <- numeric(length(dbh_m))
  for(i in 1:length(dbh_m)){
    n <- NROW(dbh_m[dbh_m > dbh_m[i] & site==site[i]])
    BAT_ha[i] <- n * k
    if(i %in% seq(0,length(dbh_m), len=100)) print(i/length(dbh_m))
  }
  cbind.data.frame(BAT_ha=BAT_ha, site=site)
}

