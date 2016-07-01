caculate_species_basal_area_france <- function(dbh, species, site) {
  
  ba_tree <- pi * (dbh/2)^2 
  ba <- vector("numeric",length(dbh))
  ba[dbh > 23.5 & dbh <= 70.5] <- pi * 6^2
  ba[dbh > 70.5 & dbh <= 117.5] <- pi * 9^2
  ba[dbh > 117.5] <- pi * 15^2
  
  ba <- ba_tree/ba
  aggregate(ba, by=list(site=site, species=species), function(x) sum(x))
}


