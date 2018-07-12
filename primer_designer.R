forward_primer_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){
 gene_length <- nchar(gene)

 forward_nucs <- substr(gene, 1, primer_length)
 forward_primer <- ("")

 for (i in 1:nchar(forward_nucs)) {
   if (substr(forward_nucs, i, i) == "A" | substr(forward_nucs, i, i) == "a"){
     forward_primer <- paste0(forward_primer, "T")
   }
   if (substr(forward_nucs, i, i) == "T" | substr(forward_nucs, i, i) == "t"){
     forward_primer <- paste0(forward_primer, "A")
   }
   if (substr(forward_nucs, i, i) == "G" | substr(forward_nucs, i, i) == "g"){
     forward_primer <- paste0(forward_primer, "C")
   }
   if (substr(forward_nucs, i, i) == "C" | substr(forward_nucs, i, i) == "c"){
     forward_primer <- paste0(forward_primer, "G")
   }
 }

 for_melt_temp <- 0

 for (i in 1:nchar(forward_primer)) {
   if (substr(forward_primer, i, i) == "A" | substr(forward_primer, i, i) == "a"){
     for_melt_temp <- for_melt_temp + 2
   }
   if (substr(forward_primer, i, i) == "T" | substr(forward_primer, i, i) == "t"){
     for_melt_temp <- for_melt_temp + 2
   }
   if (substr(forward_primer, i, i) == "G" | substr(forward_primer, i, i) == "g"){
     for_melt_temp <- for_melt_temp + 4
   }
   if (substr(forward_primer, i, i) == "C" | substr(forward_primer, i, i) == "c"){
     for_melt_temp <- for_melt_temp + 4
   }
 }

 next_place <- nchar(forward_nucs) + 1

 while (for_melt_temp < low_melt_temp){
   if (substr(gene, next_place, next_place) == "A" | substr(gene, next_place, next_place) == "a"){
     for_melt_temp <- for_melt_temp + 2
     forward_primer <- paste0(forward_primer, "T")
   }
   if (substr(gene, next_place, next_place) == "T" | substr(gene, next_place, next_place) == "t"){
     for_melt_temp <- for_melt_temp + 2
     forward_primer <- paste0(forward_primer, "A")
   }
   if (substr(gene, next_place, next_place) == "G" | substr(gene, next_place, next_place) == "g"){
     for_melt_temp <- for_melt_temp + 4
     forward_primer <- paste0(forward_primer, "C")
   }
   if (substr(gene, next_place, next_place) == "C" | substr(gene, next_place, next_place) == "c"){
     for_melt_temp <- for_melt_temp + 4
     forward_primer <- paste0(forward_primer, "G")
   }
   next_place <- next_place + 1
 }

 print(c("forward: ", forward_primer))
 print(nchar(forward_primer))
 print(for_melt_temp)

}

forward_primer_design("ttggttaagataagaacccatgtatatataaagggcaaggttcaaggtgtgtactttagacagaatatgcgtaatatagcaaggaagtacaatgtaaacggatgggttaagaaccttaaggatggaagagtagaagctgtacttgaaggtgatgaggatgctgtacatcaagtcatagagtggtgccatataggtcctgctggtgctagggttgatgacgttgatgttgtttatgaagagtacaagggtgagtttaactcatttgatataatatattaa", 18, 55, 60)

forward_primer_design("AAAGAGAGAGAGAGAGAGACCCCCCACACACACACCACATATTATAGA", 18, 55, 60)
nchar("AAAGAGAGAGAGAGAGAG")
gene <- "AAAGAGAGAGAGAGAGAGACCCCCCACACACACACCACATATTATAGA"
paste(gene, "t", sep = "")


reverse_primer_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){
  gene_length <- nchar(gene)

  reverse_nucs <- substr(gene, nchar(gene) - primer_length, nchar(gene))
  reverse_primer <- ("")

  for (i in 1:nchar(reverse_nucs)) {
    if (substr(reverse_nucs, i, i) == "A" | substr(reverse_nucs, i, i) == "a"){
      reverse_primer <- paste0(reverse_primer, "T")
    }
    if (substr(reverse_nucs, i, i) == "T" | substr(reverse_nucs, i, i) == "t"){
      reverse_primer <- paste0(reverse_primer, "A")
    }
    if (substr(reverse_nucs, i, i) == "G" | substr(reverse_nucs, i, i) == "g"){
      reverse_primer <- paste0(reverse_primer, "C")
    }
    if (substr(reverse_nucs, i, i) == "C" | substr(reverse_nucs, i, i) == "c"){
      reverse_primer <- paste0(reverse_primer, "G")
    }
  }

  rev_melt_temp <- 0

  for (i in 1:nchar(reverse_primer)) {
    if (substr(reverse_primer, i, i) == "A" | substr(reverse_primer, i, i) == "a"){
      rev_melt_temp <- rev_melt_temp + 2
    }
    if (substr(reverse_primer, i, i) == "T" | substr(reverse_primer, i, i) == "t"){
      rev_melt_temp <- rev_melt_temp + 2
    }
    if (substr(reverse_primer, i, i) == "G" | substr(reverse_primer, i, i) == "g"){
      rev_melt_temp <- rev_melt_temp + 4
    }
    if (substr(reverse_primer, i, i) == "C" | substr(reverse_primer, i, i) == "c"){
      rev_melt_temp <- rev_melt_temp + 4
    }
  }

  next_place <- nchar(reverse_nucs) - primer_length + 1

  while (rev_melt_temp < low_melt_temp){
    if (substr(gene, next_place, next_place) == "A" | substr(gene, next_place, next_place) == "a"){
      rev_melt_temp <- rev_melt_temp + 2
      reverse_primer <- paste0(reverse_primer, "T")
    }
    if (substr(gene, next_place, next_place) == "T" | substr(gene, next_place, next_place) == "t"){
      rev_melt_temp <- rev_melt_temp + 2
      reverse_primer <- paste0(reverse_primer, "A")
    }
    if (substr(gene, next_place, next_place) == "G" | substr(gene, next_place, next_place) == "g"){
      rev_melt_temp <- rev_melt_temp + 4
      reverse_primer <- paste0(reverse_primer, "C")
    }
    if (substr(gene, next_place, next_place) == "C" | substr(gene, next_place, next_place) == "c"){
      rev_melt_temp <- rev_melt_temp + 4
      reverse_primer <- paste0(reverse_primer, "G")
    }
    next_place <- next_place + 1
  }

  reverse_primer <- paste(rev(substring(reverse_primer,1:nchar(reverse_primer),1:nchar(reverse_primer))),collapse="")

  print(c("reverse: ", reverse_primer))
  print(nchar(reverse_primer))
  print(rev_melt_temp)
}

reverse_primer_design("ttggttaagataagaacccatgtatatataaagggcaaggttcaaggtgtgtactttagacagaatatgcgtaatatagcaaggaagtacaatgtaaacggatgggttaagaaccttaaggatggaagagtagaagctgtacttgaaggtgatgaggatgctgtacatcaagtcatagagtggtgccatataggtcctgctggtgctagggttgatgacgttgatgttgtttatgaagagtacaagggtgagtttaactcatttgatataatatattaa", 18, 55, 60)
