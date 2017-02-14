calculate_TV <-  function(dat, ths){
  fpr <- fnr <- kap <- ths
  fpr[] <- NA
  fnr[] <- NA
  kap[] <- NA
  for(i in 1:length(ths)){
    dat$pa_th <- 0
    dat$pa_th[dat$fitted>ths[i]] <- 1
    tab <- table(dat$pa, dat$pa_th)/nrow(dat)
    #print(tab)
    if(ncol(tab)!=1 & nrow(tab)!=1) {
      fpr[i] <- tab[1,2]
    }
    if(ncol(tab)!=1 & nrow(tab)!=1) {
      fnr[i] <- tab[2,1]
    }
    kap[i] <- confusionMatrix(dat$pa_th, dat$pa)$overall["Kappa"]
  }
  hlpr <- which(rev(fpr) < 0.01)
  tv1 <- rev(ths)[hlpr[length(hlpr)]]
  hlpr <- which(fnr < 0.01)
  tv3 <- ths[hlpr[length(hlpr)]]
  
  hlpr <- which(rev(fpr) < 0.05)
  tv11 <- rev(ths)[hlpr[length(hlpr)]]
  hlpr <- which(fnr < 0.05)
  tv31 <- ths[hlpr[length(hlpr)]]
  
  tv2 <- ths[which.max(kap)]
  
  c(tv1=tv1, tv11=tv11, tv2=tv2, tv31=tv31, tv3=tv3)
}


happy_tree_index <- function(sdm, growth, ths_sdm=c(0.25, 0.5, 0.75), dat_growth, prob_growth=seq(0,1, len=10), write=FALSE){
  
  agg_func <- function(sdm, growth) {
    ### local function to combine growth and sdm information
    ### Old: sqrt(sdm^2 + growth^2)/sqrt(2)  
    (sdm + growth)/2 
  }
  
  if(class(sdm) == "RasterLayer") { 
    
    ### SDM
    brks <- ths_sdm
    ints <- findInterval(sdm[], brks)
    sdm[] <- ints / max(ints, na.rm=T) 
    
    ### GROWTH
    brks <- quantile(na.omit(dat_growth), prob=prob_growth) 
    #brks <- c(0, brks)
    #brks <- c(brks, max(growth[], na.rm=T))
    ints <- findInterval(growth[], brks)
    
    growth[] <- ints / max(ints, na.rm=T) 
  
    hti <- agg_func(sdm, growth)  
  
    ### Auf max = 1 normieren
    res <- stack(sdm, growth, hti)
    names(res) <- c("sdm", "growth", "happy_tree_index")    
  } else {
    
    sdm <- as.matrix(sdm)
    growth <- as.matrix(growth)
    
    ### SDM
    #brks <- sort(calculate_TV(dat_sdm, ths_sdm)  )  
    #brks <- c(0, brks, 1)
    brks <- ths_sdm
    ints <- findInterval(sdm, brks)
    sdm[] <- ints / max(ints, na.rm=T) 
    
    ### GROWTH
    brks <- quantile(na.omit(dat_growth), prob=prob_growth) 
    if (brks[1] > 0) brks <- c(0, brks)
    if (brks[length(brks)] < max(growth[], na.rm=T)) brks <- c(0, brks)
    ints <- findInterval(growth[], brks)
    
    growth[] <- ints / max(ints, na.rm=T) 
    
    
    hti <- agg_func(sdm, growth)   ### Sum index
    
    ### Auf max = 1 normieren
    res <- list(sdm, growth, hti)
    names(res) <- c("sdm", "growth", "happy_tree_index")
    
  }
  res  
}


create_hti_rgb_raster <- function(hti, red=1, green=2, blue=3, nclasses=10, set_zero=3, zero_value=0) {
  hti[[set_zero]][!is.na(hti[[set_zero]][])] <- zero_value
  hti_rgb <- stack(hti[[red]], hti[[green]], hti[[blue]])
  names(hti_rgb) <- c("red", "green", "blue")  
  hti_rgb
}


calculate_hti_change <- function(current_hti, future_hti){
  future_hti - current_hti 
}



create_basal_area_data <- function(growth_data, k=4){
  badata <- aggregate(growth_data$dbh2012, by=list(SiteID=growth_data$SiteID, species=growth_data$species), NROW)
  badata <- as.data.frame(badata)
  colnames(badata) <- c(colnames(badata)[-length(names(badata))], "BA")
  badata$BA <- badata$BA * k
  m <- match(badata$SiteID, growth_data$SiteID)
  badata$BA_share <- badata$BA/growth_data$SBA2012[m]
  badata$GKrechts <- growth_data$GKrechts[m]
  badata$GKhoch <- growth_data$GKhoch[m]
  badata
}


create_species_priority_regions <- function(basal_area_data, species="Picea_abies", hti_change, resolution=7000, negative_only=FALSE){
  ### Arguments
  # basal_area_data: Outpout of function create_basal_area_data. Column "species", "BA_share", "GKrechts" , "GKrechts".
  # resolotion: Resolution of output raster
  if (require(raster)){
    badata_species <-  basal_area_data[basal_area_data$species==species,]
    badata_species <-  badata_species[badata_species$BA_share > quantile(badata_species$BA_share, prob=0.75, na.rm=T),]
    
    species_actual <- rasterize(badata_species[, c("GKrechts", "GKhoch")], hti_change, badata_species$GKrechts, fun=median)
    
    species_priority <- mask(hti_change, species_actual)
    species_priority <- resample(species_priority, raster(ext=extent(species_priority), resolution=resolution))
    
    if(negative_only) species_priority[species_priority[]>=0] <- NA
    
    
  } else {
    print("Package raster not loaded. Please load or install and load.")
  }  
  species_priority
}


