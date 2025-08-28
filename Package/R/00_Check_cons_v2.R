Check_cons_v2 <- function(pts,chem,cons_data){

  cons <- na.omit(data.frame(country=c(unique(pts$rptMStateK[pts$Pt_type=="WWTP"|pts$Pt_type=="Agglomerations"]))))

  for (i in 1:nrow(chem)) {

    if (length(cons$country) == 0) {
      stop(cat("Prediction not possible due to absence of contaminant source in the domains", unique(pts$basin_id), "\n"))
    }
    else {

      cons$new <- NA

      colnames(cons)[colnames(cons)=="new"] <- chem$API[i]

      # Filling consumption dataframe with consumption data if available, otherwise end script and give message
      for (j in 1:nrow(cons)){
        cons[j,i+1] <- ifelse(cons$country[j]%in%cons_data$cnt & colnames(cons)[i+1]%in%colnames(cons_data),
                              cons_data[match(cons$country[j],cons_data$cnt),match(colnames(cons)[i+1],colnames(cons_data))],NA)
      }

      # try(if(any(is.na(cons))) stop(paste0("Prediction not possible due to insufficient consumption data for ",chem$API[i])))

      if(any(is.na(cons))) {
        stop(paste0("Prediction not possible due to insufficient consumption data for ",chem$API[i]))
      }

    }
    cons<-cons
  }
  return(cons)
}
