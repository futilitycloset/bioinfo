PatternCount <- function(text, pattern){
  count <- 0
  for (i in length(text) - length(pattern)){
    print(text[i : i + length(pattern))
    #if (text[i : i+length(pattern)] == pattern){
    #  count <- count + 1
    }
    i <- i + 1
  }
  print(count)
}

PatternCount("GCGCG", "GCG")

PatternCount2 <- function(text, pattern){
  #print(pattern)
  pattern_count <- 0
  pattern_length <- nchar(pattern)
  look_length <- nchar(text) - nchar(pattern)
  for (i in 1:look_length) {
    #print(c("i = ", i))
    end_pos <- i + pattern_length
    #print(end_pos)
    test_text <- substr(text, i, end_pos-1)
    #print(test_text)
    if (test_text == pattern){
      pattern_count <- pattern_count + 1
    }
  }
  print(pattern_count)
}

PatternCount2("GCGCGGGATATATTA", "GCG")

text <- "GCGCGGGATATATTA"
substr(text, 2, 5)



Pattern2 <- function(text, pattern){
  pattern_length <- nchar(pattern)
  text_length <- nchar(text)
  look_length <- nchar(text) - nchar(pattern)
  for (i in 1:look_length){
    end_pos <- i + pattern_length
    test_text <- substr(text, i, end_pos-1)
    print(test_text)
  }
}

Pattern2("GCGCGGGATATATTA", "GCG")
