library("raster")
library("rgdal")
library("foreign")

listPs <- c("OID","COUNT","FCID","MAP_SOURCE","ESLF_CODE","ESLF_NAME",
             "DATA_SOURC","ASSESSMENT","STATE","CNTY","PLOT","OCCASION_N",
             "BA_GE_3","BAC_GE_3","BAH_GE_3","BPH_GE_3_C","BPHC_GE_3_",
             "BPHH_GE_3_","TPH_GE_3","TPHC_GE_3","TPHH_GE_3","VPH_GE_3",
             "VPHC_GE_3","VPHH_GE_3","AGE_DOM","CANCOV","CANCOV_CON",
             "CANCOV_HDW","CANCOV_LAY","FORTYPBA","MNDBHBA","MNDBHBA_CO",
             "MNDBHBA_HD","QMD_DOM","QMDC_DOM","QMDH_DOM","QMD_HT25","LSOG",
             "OGSI","OGSI_80","OGSI_200","DDI","COVCL","SIZECL","STRUCCOND",
             "VEGCLASS","VC_MNDBHBA","STNDHGT","SDI_REINEK","TREEPLBA",
             "CONPLBA","HDWPLBA","SBPH_GE_25","STPH_GE_25","SVPH_GE_25",
             "DBPH_GE_25","DCOV_GE_25","DVPH_GE_25","ABAM_BA","ABBR_BA",
             "ABGRC_BA","ABLA_BA","ABPRSH_BA","ACCI_BA","ACGL_BA","ACGLD4_BA",
             "ACMA3_BA","ACNE2_BA","AECA_BA","AESCU_BA","AIAL_BA","ALRH2_BA",
             "ALRU2_BA","ARME_BA","BEOC2_BA","BEPA_BA","BEPAC_BA","CADE27_BA",
             "CELE3_BA","CHCH7_BA","CHLA_BA","CHNO_BA","CONU4_BA","CRATA_BA",
             "CUPRE_BA","CUSA3_BA","ELAN_BA","EUCAL_BA","EUGL_BA","FRAXI_BA",
             "FRLA_BA","FRPU7_BA","ILEX_BA","ILOP_BA","JUCA_BA","JUCA7_BA",
             "JUGLA_BA","JUHI_BA","JUOC_BA","JUOS_BA","JUSC2_BA","LALY_BA",
             "LAOC_BA","LIDE3_BA","LIST2_BA","MAFU_BA","MALUS_BA","NOTALY_BA",
             "OLTE_BA","PIAL_BA","PIAR_BA","PIAT_BA","PIBA_BA","PIBR_BA","PICO_BA",
             "PICO3_BA","PIEN_BA","PIFL2_BA","PIJE_BA","PILA_BA","PILO_BA","PIMO_BA",
             "PIMO3_BA","PIMU_BA","PIPO_BA","PIRA2_BA","PISA2_BA","PISI_BA","PISY_BA",
             "PIWA_BA","PLRA_BA","POBAT_BA","POFR2_BA","POTR5_BA","PRAV_BA","PREM_BA",
             "PRGLT_BA","PRPU_BA","PRUNU_BA","PRVI_BA","PSMA_BA","PSME_BA","QUAG_BA",
             "QUCH2_BA","QUDO_BA","QUEN_BA","QUERC_BA","QUGA4_BA","QUKE_BA","QULO_BA",
             "QUMU_BA","QUWI2_BA","SAAL2_BA","SALIX_BA","SALUL_BA","SANI_BA","SASC_BA",
             "SEGI2_BA","SESE3_BA","TABR2_BA","THPL_BA","TOCA_BA","TSHE_BA","TSME_BA","UMCA_BA")
gnn  <- raster("X:\\_Final\\Gnn_N83HARN.img")
chm <- raster("X:\\_Final\\WKRP_CHM10_UTMn83HARN.img")

tuffExist <- function(x, y, ...) {
  args <- list(...)
  exist <- "target" %in% names(args)
  exist
}


calcCorrCoeff <- function(target){
  
  library("raster")
  library("rgdal")
  library("foreign")
  
  gnn  <- raster("X:\\_Final\\Gnn_N83HARN.img")
  chm <- raster("X:\\_Final\\WKRP_CHM10_UTMn83HARN.img")
  
  gnn_wkrp <- crop(gnn, extent(chm))
  gnn.rat  <- read.dbf("X:\\_Final\\Gnn_N83HARN.img.vat.dbf")
  gnn <-  ratify(gnn)
  
  chm[chm == 0] <- NA
  
  if (! tuffExist(target)){
    target <- c("OGSI")
  }
  i = 1
  outList <- list()
    
  for (items in target){
    gnn.ogsi <- subs(gnn, gnn.rat, by="VALUE", which = items)
    gnn.ogsi.10 <- disaggregate(gnn.ogsi,fact = 3, fun = mean())    
    gnn.new <- crop(gnn.ogsi.10, chm)
    
    st <- stack(gnn.new, chm)
    
    gnn.chm.cov <- layerStats(st, "pearson", na.rm = TRUE)
    
    outList[i] <- gnn.chm.cov
    i <- i + 1
  }
outList
  
}

batchGNN <- function(){
  
  
  
  
}



gnn_wkrp <- crop(gnn, extent(chm))
extent(gnn)

gnn.rat  <- read.dbf("X:\\_Final\\Gnn_N83HARN.img.vat.dbf")
gnn <-  ratify(gnn)

chm[chm == 0] <- NA

gnn.ogsi <- subs(gnn, gnn.rat, by="VALUE", which = "OGSI")
gnn.ogsi.10 <- disaggregate(gnn.ogsi,fact = 3, fun = mean())

gnn.new <- crop(gnn.ogsi.10, chm)

st <- stack(gnn.new, chm)

gnn.chm.cov <- layerStats(st, "cov", na.rm = TRUE)


