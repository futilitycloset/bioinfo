AOA_subset <- read.csv("~/Desktop/EECBData/Experiments/070318_get_homologues/subset_AOA_Nyel.csv")
genes_present_all <- AOA_subset[which(AOA_subset$X == 44),]
nyelORF_all <- as.data.frame(genes_present_all$Gene)



write.table(nyelORF_all,"clip",sep="\t")
install.packages("clipr")
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
}

wc(genes_present_all$Gene)
