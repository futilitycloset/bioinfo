AOA_subset <- read.csv("~/Desktop/EECBData/Experiments/070318_get_homologues/subset_AOA_Nyel.csv")
genes_present_all <- AOA_subset[which(AOA_subset$X == 44),]
nyelORF_all <- as.data.frame(genes_present_all$Gene)

write.table(nyelORF_all,"clip",sep="\t")
install.packages("clipr")
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
}

wc(genes_present_all$Gene)

donne_fplc <- as.data.frame(read.table("donne_fplc.txt"))
liz_aane <- as.data.frame(read.table("liz_aane.txt"))
liz_aoa_exclusive <- as.data.frame(read.table("liz_aoa_exclusive.txt"))

nrow(donne_fplc)
nrow(liz_aane)
nrow(liz_aoa_exclusive)

length(intersect(donne_fplc$V1, liz_aoa_exclusive$V1))
length(intersect(liz_aane$V1, liz_aoa_exclusive$V1))
length(intersect(donne_fplc$V1, liz_aane$V1))
