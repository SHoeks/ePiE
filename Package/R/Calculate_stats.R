CalculateWriteEnvStats = function(results,results_lakes,regime,pts,dir) {
  td <- paste0(dir,"/")
  pts2 <- split(pts,f=pts$basin_id)
  stats <- data.frame(stat=rep(c("P1","P2.5","P5","P10","P25","P50","P75","P90","P95","P97.5","P99"),2),
                      condition=rep(c("including zeroes","excluding zeroes"),each=11),
                      total=NA)
  for (i in 1:length(pts2)) {
    stats$new <- NA
    colnames(stats)[colnames(stats)=="new"] <- paste0("basin_",names(pts2)[i])
  }

  for (j in unique(results$API)) {
    rs_tot <- c()
    for (i in 1:length(pts2)) {
      sub_pts <- which(pts2[[i]]$Pt_type%in%c("node","START","MOUTH","JNCT"))
      rs <- data.frame(ID=pts2[[i]]$ID[sub_pts],HL_ID_new=pts2[[i]]$HL_ID_new[sub_pts])

      res_temp <- results[results$basin_ID==names(pts2)[i] & results$API==j,]
      rs$C_w <- res_temp$C_w[match(rs$ID,res_temp$ID)]
      res_lakes <- results_lakes[results_lakes$API==j,]
      rs$C_w[rs$HL_ID_new!=0] <- res_lakes$C_w[match(rs$HL_ID_new[rs$HL_ID_new!=0],res_lakes$Hylak_id)]

      stats[1:11,3+i] <- quantile(rs$C_w,probs=c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99),na.rm=T)
      stats[12:22,3+i] <- quantile(rs$C_w[rs$C_w!=0],probs=c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99),na.rm=T)
      rs_tot <- rbind(rs_tot,rs)
    }
    stats[1:11,3] <- quantile(rs_tot$C_w,probs=c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99),na.rm=T)
    stats[12:22,3] <- quantile(rs_tot$C_w[rs_tot$C_w!=0],probs=c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99),na.rm=T)
    options(warn=-1)
    write.csv(stats,file=paste0(td,Sys.Date(),"_Stats_flow_",regime,"_",j,".csv"),row.names = FALSE)
    options(warn=0)
  }
}
