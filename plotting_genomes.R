library(tidyverse)
library(janitor)

#organize csv file from GH compare_clusters
pan_N_df <- read.csv("~/Desktop/pangenome_matrix_t0.tr.csv", header = TRUE)
pan_N_df <- select(pan_N_df, -(2:14))
colnames(pan_N_df) <- c("Clusters", "GBSB", "HL72", "D5", "ISA2", "HL4", "DSB", "GBSF")
pan_N_df <- pan_N_df[c(1, 3, 4, 7, 6, 5, 2, 8)]

#Total summaries for rows and columns, used library(janitor)
pan_N_df <- adorn_totals(pan_N_df, where = "row")

?plot
genome_names <- colnames(pan_N_df[2:8])
genome_names
total_data <- as.list(pan_N_df[nrow(pan_N_df), 2:8])
plot(x = total_data, y = genome_names, type = "p")

new_df <- data_frame(genome_names, total_data)
plot(x = new_df$genome_names, y = new_df$total_data, type = "b")

pan_N_df[nrow(pan_N_df), 2:8]


x <- as.character(colnames(pan_N_df[,2:8]))
y <- as.numeric(pan_N_df[2185,2:8])

plot(y, type = "b", xaxt = "n")
axis(1, at=1:7, labels=genome_names)
