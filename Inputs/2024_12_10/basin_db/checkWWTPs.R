library(fst)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
p = read.fst("pts_c75.fst")
head(p)
unique(p$Pt_type)
wwtp = p[p$Pt_type=="WWTP",]
head(wwtp)

