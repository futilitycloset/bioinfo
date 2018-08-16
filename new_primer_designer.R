#returns complement of given base
complement <- function(nuc){
  nuc <- toupper(nuc)

  if (nuc == "A"){
    return("t")
  }
  if (nuc == "T"){
    return("a")
  }
  if (nuc == "G"){
    return("c")
  }
  if (nuc == "C"){
    return("g")
  }
  else {
    return("Invalid base. Enter A, G, C, or T.")
  }
}

#designs forward primer, and increases length if melting temperature is too low
forward_primer_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){
  gene_length <- nchar(gene)

  forward_primer <- substr(gene, 1, primer_length)

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

  next_place <- nchar(forward_primer) + 1

  while (for_melt_temp < low_melt_temp){
    if (substr(gene, next_place, next_place) == "A" | substr(gene, next_place, next_place) == "a"){
      for_melt_temp <- for_melt_temp + 2
      forward_primer <- paste0(forward_primer, "a")
    }
    if (substr(gene, next_place, next_place) == "T" | substr(gene, next_place, next_place) == "t"){
      for_melt_temp <- for_melt_temp + 2
      forward_primer <- paste0(forward_primer, "t")
    }
    if (substr(gene, next_place, next_place) == "G" | substr(gene, next_place, next_place) == "g"){
      for_melt_temp <- for_melt_temp + 4
      forward_primer <- paste0(forward_primer, "g")
    }
    if (substr(gene, next_place, next_place) == "C" | substr(gene, next_place, next_place) == "c"){
      for_melt_temp <- for_melt_temp + 4
      forward_primer <- paste0(forward_primer, "c")
    }
    next_place <- next_place + 1
  }

  for_melt_temp <- as.numeric(for_melt_temp)
  for_results <- data.frame(c(forward_primer = forward_primer, melt_temp = for_melt_temp, primer_length = nchar(forward_primer)))
  return(for_results)

}

reverse_primer_design <- function(gene, primer_length, low_melt_temp, high_melt_temp){

  reversed_gene <- paste(rev(substring(gene,1:nchar(gene),1:nchar(gene))),collapse="")
  proto_reverse_results <- as.data.frame(forward_primer_design(reversed_gene, primer_length, low_melt_temp, high_melt_temp))

  reverse_temp <- as.character(proto_reverse_results[2,])
  proto_reverse_primer <- as.character(proto_reverse_results[1,])

  proto_reverse_primer_df <- as.data.frame(strsplit(proto_reverse_primer, split = ""))
  reverse_primer <- ""

  for (x in 1:nchar(proto_reverse_primer)) {
    target_nuc <- proto_reverse_primer_df[x,]
    comp_nuc <- complement(target_nuc)

    reverse_primer <- paste0(reverse_primer, comp_nuc)
  }

  reverse_temp <- as.numeric(reverse_temp)
  rev_results <- data.frame(rbind(reverse_primer = as.character(reverse_primer), melt_temp = reverse_temp, primer_length = nchar(reverse_primer)))
  return(rev_results)
}

test_seq <- reverse_primer_design("atggctaggaagttcatggattggtggaatacagtaccagctgatatacgcaacaaagcaaagggcaacgatgattggacaaagtacaagccattactaaaccaagtaaactatgcaatggtagcactacacttgcaaggtaatcatagtgcaaagccttctgcagaggaactcttaagctggataaagaacggagagatagatgtagttagagttagtaagtaa", 18, 55, 60)
test_seq
