# Visualisasi Persentase Ijazah Tertinggi 
# Developed by Wihelmus Wedo 
# install.packages("tidyverse", "treemapify")

library(tidyverse)
ijazah <- read_csv("Ijazah.csv")

urutan_ijazah <- c("Tidak Mempunyai Ijazah SD", "SD Sederajat", "SMP Sederajat", 
                   "SMA Sederajat", "D1 D2 D3", "S1 S2 S3")

urutan_Kota <- LETTERS[1:7]

ijazah <- ijazah %>% 
  mutate(Tingkat_Ijazah = ordered(Tingkat_Ijazah, urutan_ijazah)) %>% 
  mutate(Kota = ordered(Kota, urutan_Kota))

## Visual 
clr_labels <- c("Tidak Mempunyai Ijazah SD", "SD Sederajat", "SMP Sederajat", 
             "SMA Sederajat", "Diploma (D1, D2, D3)", "Sarjana & Pasca Sarjana")

clr_values <- c("#FC0080", "#4AC2CB", "#40456E", "#FC3900", "#00C1A9", "#E6DC00")
ijazah_clr <- c("#716674", "#E3806D", "#5D3C57", "#82AFB5", "#DDAD49", "#F5C0AE")

# direktori gambar 
ggsave_path <- "Gambar/"

# Treemap 
library(treemapify)
source("ggplot_themes.R")
theme_set(theme_fte())

TreeKota <- ggplot(ijazah, aes(area = Persentase, fill = Tingkat_Ijazah)) + 
  geom_treemap() + 
  geom_treemap_text(
    aes(label = round(Persentase, 0) %>% paste0(., "%")), 
    color = "white", 
    place = "center"
    ) +
  facet_wrap(~Kota, ncol = 2) +
  scale_fill_manual(label = clr_labels, values = ijazah_clr) + 
  theme(legend.title = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 12)) + 
  labs(fill = "Ijazah Tertinggi") + 
  theme(legend.position = "top")

ggsave(plot = TreeKota, 
       filename = "Ijazah_Treemap.png", 
       path = ggsave_path,
       device = "png", dpi = 300, 
       units = "cm", height = 25, width = 25)

# Column chart 
ColIjazah <- ijazah %>% 
  mutate(Kota = Kota %>% fct_rev()) %>% 
  ggplot(aes(
    x = Kota, 
    y = Persentase, 
    fill = Tingkat_Ijazah
  )) + 
  geom_col(position = position_stack(reverse = TRUE)) + 
  coord_flip() + 
  scale_fill_manual(label = clr_labels, values = ijazah_clr) + 
  scale_y_continuous(labels = function(x) { paste0(x, "%") }) + 
  labs(
    title = "Persentase Pendidikan Menurut Ijazah Tertinggi",
    fill = "Ijazah Tertinggi"
  ) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(plot = ColIjazah, filename = "Ijazah_Column.png", 
       path = ggsave_path,
       device = "png", dpi = 300, 
       units = "cm", height = 10, width = 25)
