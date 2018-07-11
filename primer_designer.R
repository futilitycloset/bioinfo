forward_primer_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){
 gene_length <- nchar(gene)

 forward_nucs <- substr(gene, 1, primer_length)
 forward_primer <- ("")

 for (i in 1:nchar(forward_nucs)) {
   if (substr(forward_nucs, i, i) == "A"){
     forward_primer <- paste0(forward_primer, "T")
   }
   if (substr(forward_nucs, i, i) == "T"){
     forward_primer <- paste0(forward_primer, "A")
   }
   if (substr(forward_nucs, i, i) == "G"){
     forward_primer <- paste0(forward_primer, "C")
   }
   if (substr(forward_nucs, i, i) == "C"){
     forward_primer <- paste0(forward_primer, "G")
   }
 }

 for_melt_temp <- 0

 for (i in 1:nchar(forward_primer)) {
   if (substr(forward_primer, i, i) == "A"){
     for_melt_temp <- for_melt_temp + 2
   }
   if (substr(forward_primer, i, i) == "T"){
     for_melt_temp <- for_melt_temp + 2
   }
   if (substr(forward_primer, i, i) == "G"){
     for_melt_temp <- for_melt_temp + 4
   }
   if (substr(forward_primer, i, i) == "C"){
     for_melt_temp <- for_melt_temp + 4
   }
 }

 next_place <- nchar(forward_nucs) + 1

 while (for_melt_temp < low_melt_temp){

   if (substr(gene, next_place, next_place) == "A"){
     for_melt_temp <- for_melt_temp + 2
     forward_primer <- paste0(forward_primer, "T")
   }
   if (substr(gene, next_place, next_place) == "T"){
     for_melt_temp <- for_melt_temp + 2
     forward_primer <- paste0(forward_primer, "A")
   }
   if (substr(gene, next_place, next_place) == "G"){
     for_melt_temp <- for_melt_temp + 4
     forward_primer <- paste0(forward_primer, "C")
   }
   if (substr(gene, next_place, next_place) == "C"){
     for_melt_temp <- for_melt_temp + 4
     forward_primer <- paste0(forward_primer, "G")
   }
   next_place <- next_place + 1
 }
 print(forward_primer)
 print(for_melt_temp)
}








forward_primer_design("AAAGAGAGAGAGAGAGAGACCCCCCACACACACACCACATATTATAGA", 18, 55, 60)
nchar("AAAGAGAGAGAGAGAGAG")
gene <- "AAAGAGAGAGAGAGAGAGACCCCCCACACACACACCACATATTATAGA"
paste(gene, "t", sep = "")
