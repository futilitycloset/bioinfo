#returns complement of given base
complement <- function(nuc){
  nuc <- toupper(nuc)

  if (nuc == "A"){
    return("T")
  }
  if (nuc == "T"){
    return("A")
  }
  if (nuc == "G"){
    return("C")
  }
  if (nuc == "C"){
    return("G")
  }
  else {
    return("Invalid base. Enter A, G, C, or T.")
  }
}

#designs forward primer, and increases length if melting temperature is too low
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

 #print(c("forward: ", forward_primer))
 #print(nchar(forward_primer))
 #print(for_melt_temp)
 for_melt_temp <- as.numeric(for_melt_temp)
 for_results <- data.frame(c(forward_primer = forward_primer, melt_temp = for_melt_temp, primer_length = nchar(forward_primer)))
 return(for_results)

}

#designs reverse primer, and increases length if melting temperature is too low
reverse_primer_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){

  reversed_gene <- paste(rev(substring(gene,1:nchar(gene),1:nchar(gene))),collapse="")
  proto_reverse_results <- as.data.frame(forward_primer_design(reversed_gene, primer_length, low_melt_temp, high_melt_temp))

  reverse_temp <- as.character(proto_reverse_results[2,])
  proto_reverse_primer <- as.character(proto_reverse_results[1,])
  correct_reverse_primer <- paste(rev(substring(proto_reverse_primer,1:nchar(proto_reverse_primer),1:nchar(proto_reverse_primer))),collapse="")

  reverse_temp <- as.numeric(reverse_temp)
  rev_results <- data.frame(rbind(reverse_primer = as.character(correct_reverse_primer), melt_temp = reverse_temp, primer_length = nchar(correct_reverse_primer)))
  return(rev_results)
}

both_primers_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){
  forward_results <- forward_primer_design(gene, primer_length, low_melt_temp, high_melt_temp)
  reverse_results <- reverse_primer_design(gene, primer_length, low_melt_temp, high_melt_temp)

  both_results <- cbind(forward_results, reverse_results)
  row.names(both_results) <- c("primer", "melt_temp", "primer_length")
  colnames(both_results) <- c("forward", "reverse")
  return(both_results)
}

test_seq <- both_primers_design("atgattatgataaatagtaatgataataacataaataatattaaagatgcaagttatgcaatatgctatgcagataagatgctaaaggataatgatatagatcctatagttgctgatattcttaatagggcattatcgttcaaggatataggtgtgaaggatgctgtagaactatttgaatgcagcaaagattcacttaaggcattaattgcaacagcagatgcgttaaggaaggttagtgtaggtgatgtagtaacatatgtagttaatagaaatataaacttcacaaatgtgtgtataaagcgttgtggattctgtgcattctcaagggactttagagaggaagagggttatctattgcctatagaagagatagtaaggagagcaaaggaagcatacgtctttggggctactgaggtatgcatacaggcagggttgatgccaaagatggatggctatctttacattgatatatgcagagcaattaagaaggagttgccagacatgcatatacatgcattctcaccagaggaggtaatgtatggcgcattaagggcaaatatgagcatagaggattaccttaaggcattaaaggatgcagggttaaacagcatccctggtacagccgctgagattcttgtacagagggtaaggaacataatatccccaggtaggataaaggttaaggactggataagagttataaagactgcacataggttagggataccatctactgcaactataatgtatgggcatattgagaactctctacataaggcagtacatctaaaacttataagagatattcagatggagactcatgggttcacagagtttgtacccttaagctttgtatacagagaggcaccaatgtataagcatagactggttaatggtttaagagaaggtgcaaatagggaagaagtgcttaagatgcatgctatagcaagggtaatgttaaataactatataaagaatatccaagtctcttgggttaaagaaggcatagagtttgctaaggatttgcttaatgctggtgcaaatgatcttggtggtacattaataaatgagagcatatctactgctgcaggtgcaaggaatgggcaactaattagacctagggagttaaggagtgctataaggagcataggtaggatacctgctcaaagagacacattatacaagagtataagggtatttgagtcatatgagcctgaggatccattggataaggttactgatacatctatctttggttcatacaacgaacttattaggcttaagaggtttagatatagatctagttag", 18, 55, 60)
test_seq
