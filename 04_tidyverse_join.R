library(tidyverse)

# загружаем
proteome <- read_csv("luad_fc_proteome_500g,for_students.csv")
transcriptome <- read_csv("luad_fc_transcriptome_500g,for_students.csv")
hgnc <- read_tsv("hgnc_complete_set_17_for_students.txt")

# сопоставляю названия
transcriptome2 <- transcriptome %>%
  left_join(hgnc %>% select(ensembl_gene_id, symbol),
            by = c("gene" = "ensembl_gene_id"))

proteome_long <- proteome %>%
  filter(gene %in% c("SLC25A13", "ACSM3", "BRCA1")) %>%
  pivot_longer(-gene, names_to = "sample", values_to = "protein_fc")

transcriptome_long <- transcriptome2 %>%
  filter(symbol %in% c("SLC25A13", "ACSM3", "BRCA1")) %>%
  pivot_longer(-c(gene, symbol), names_to = "sample", values_to = "transcript_fc")

data_joined <- inner_join(proteome_long,
                          transcriptome_long,
                          by = c("gene" = "symbol", "sample"))

# График
ggplot(data_joined,
       aes(x = transcript_fc,
           y = protein_fc,
           color = gene,
           shape = gene)) +
  geom_point()

ggplot(data_joined,
       aes(x = transcript_fc,
           y = protein_fc,
           color = gene,
           shape = gene)) +
  geom_point() +
  xlim(0, 15) +
  ylim(0, 15) +
  scale_x_log10() +
  scale_y_log10()