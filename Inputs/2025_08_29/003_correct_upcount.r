library(ePiE)
library(zip)
library(mapview)
library(sf)
library(terra)
library(fst)
library(arrow)
sf_use_s2(FALSE)

# wd
wd = ("C:/Users/Selwyn Hoeks/Documents/GitHub/ePiE_Rpackage/Inputs/2025_08_29")
setwd(wd)

# read pts
ptsf = list.files("data_export_2025_08_15/csv/", pattern = ".csv", full.names = TRUE)
ptsf = grep("pts_", ptsf, value = TRUE)
p = lapply(ptsf, read.csv)
str(p,1)
pl = p

# check!
j_Tocorrect = c()
for(i in 1:length(pl)){
  print(paste("File",i,":",ptsf[i]))
  print(table(is.na(pl[[i]]$Freq)))
  j_Tocorrect[i] = ifelse(length(which(is.na(pl[[i]]$Freq)))==0, FALSE, TRUE)
}
j_Tocorrect = which(j_Tocorrect)
print(j_Tocorrect)

# correct
for(j in j_Tocorrect){
    p = pl[[j]]
    print(" ")
    print("-------------------")
    print(j)
    print(paste("n pts: ",length(pl[[j]]$Freq)))
    print(paste("n NAs: ",sum(is.na(pl[[j]]$Freq))))
    idx = which(is.na(p$Freq))
    p$IDb = paste0(p$ID,"_",p$basin_id)
    p$ID_nxtb = paste0(p$ID_nxt,"_",p$basin_id)
    head(p)
    for(i in seq_along(idx)){
        progress = round(i/length(idx)*100, 2)
        if(i %% 100 == 0 | i == 1 | i ==length(idx)) print(paste(i,"/",length(idx), " (",progress,"%)",sep=""))
        p$Freq[idx[i]] = length(which(p$IDb[idx[i]]==p$ID_nxtb))
    }
    idx = which(is.na(p$Freq))
    p$IDb = NULL
    p$ID_nxtb = NULL
    pl[[j]] = p
    print(paste("n pts: ",length(pl[[j]]$Freq)))
    print(paste("n NAs: ",sum(is.na(pl[[j]]$Freq))))
    print("-------------------")
    print(" ")
}

# check!
j_Tocorrect = c()
for(i in 1:length(pl)){
  print(paste("File",i,":",ptsf[i]))
  print(table(is.na(pl[[i]]$Freq)))
  j_Tocorrect[i] = ifelse(length(which(is.na(pl[[i]]$Freq)))==0, FALSE, TRUE)
}
j_Tocorrect = which(j_Tocorrect)
print(j_Tocorrect)

# write updated files
for(i in seq_along(pl)){
    write.csv(pl[[i]], file = ptsf[i], row.names = FALSE)
}





