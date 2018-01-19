# visualisasi kontribusi indikator untuk tiap provinsi : 10 s.d 12 Indikator 
# Developed by Wihelmus Wedo 
# install.packages("tidyverse")

library(tidyverse)

# buat data frame baru... 
kontribusi_indikator <- tibble(
  indikator = 10:12,
  Jenis = map_chr(indikator, ~ paste0("kontribusi_", .x, ".csv")),
  Data  = map(Jenis, ~ read_csv(.x)), 
  indikator_title = case_when(
    indikator == 10 ~ "3 Dimensi : 10 Indikator",
    indikator == 11 ~ "4 Dimensi : 11 Indikator", 
    indikator == 12 ~ "4 Dimensi : 12 Indikator"
  )
)

# Visual Touch 
source("ggplot_themes.R")
theme_set(theme_fte())
clr_val <- c("#60ADFF","#FF009F", "#A129FF", "#F8A300", "#00B859")

# urutan dalam gambar 
urutan_dimensi <- c("Kesehatan", "Pendidikan", "Standar Hidup", "Ketenagakerjaan")

urutan_indikator <- c("Gizi/Nutrisi", "Kematian Balita", 
                      "Partisipasi Sekolah", "Lama Sekolah", 
                      "Lantai", "Air minum", 
                      "Sanitasi", "Sumber Penerangan", 
                      "BB Memasak", "Kepemilikan Aset", 
                      "Stabilitas dan jaminan kerja", "Produktivitas")

urutan_Kota <- LETTERS[1:8]

# lokasi gambar 
ggsave_path <- "Gambar/"

# Use purrr power -----------------------
kontribusi_indikator2 <- kontribusi_indikator %>% 
  mutate(
    Plot = map2(.x = Data,
                .y = indikator_title,
                .f = ~ .x %>% 
                  mutate(
                    Kota = Kota %>% ordered(levels = urutan_Kota), 
                    Indikator     =     Indikator %>% ordered(levels = urutan_indikator) %>% fct_rev(), 
                    Dimensi       =       Dimensi %>% ordered(levels = urutan_dimensi)
                  ) %>% 
                  ggplot(aes(x = Indikator, y = Kontribusi, fill = Dimensi)) + 
                  facet_wrap(~ Kota, ncol = 4) + 
                  geom_col() + 
                  coord_flip() + 
                  theme(#text = element_text("Liberation Sans"), 
                    axis.title.x = element_blank(), 
                    axis.title.y = element_blank(), 
                    axis.text.x = element_blank(), 
                    legend.title = element_text(size = 15), 
                    legend.text = element_text(size = 13)
                  ) + 
                  scale_fill_manual(values = clr_val) +
                  geom_text(
                    aes(label = paste0(round(Kontribusi, 1), "%")), 
                    color = "#555753", 
                    show.legend = FALSE, 
                    hjust = 1, 
                    fontface = "bold",
                    position = position_nudge(y = 15)
                  ) + 
                  labs(title = paste0("Kontribusi per Indikator Untuk ", .y))
    )
  )

# Save it -------------------------------
kontribusi_indikator2 %>% 
  mutate(
    save = walk2(.x = Plot, 
                 .y = map(indikator_title, ~ paste0("Kontribusi per Indikator Untuk ", .x, ".png")), 
                 .f = ~ ggsave(plot = .x, 
                               filename = .y,
                               path = ggsave_path,
                               device = "png", units = "cm", 
                               width = 40, height = 18, dpi = 300)
    )
  )

# Kontribusi Dimensi ---------------------------------------
# kontribusi dimensi = jumlah kontribusi indikator 
kontribusi_dimensi <- kontribusi_indikator %>% 
  mutate(
    Plot = map2(.x = Data,
                .y = indikator_title,
                .f = ~ .x %>% 
                  group_by(Dimensi, Kota) %>% 
                  summarise(Kontribusi = sum(Kontribusi)) %>% 
                  ungroup() %>% 
                  mutate(
                    Kota = Kota %>% ordered(levels = urutan_Kota) %>% fct_rev(), 
                    Dimensi       =       Dimensi %>% ordered(levels = urutan_dimensi)
                  ) %>% 
                  ggplot(aes(x = Kota, y = Kontribusi, fill = Dimensi)) +
                  geom_col(position = position_stack(reverse = TRUE)) + 
                  coord_flip() + 
                  theme(axis.title.x = element_blank(), 
                        axis.title.y = element_blank(), 
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        axis.text.x = element_blank()) + 
                  geom_text(aes(label = round(Kontribusi, 0) %>% paste0(., "%")), 
                            color = 'white', size = 5, 
                            position = position_stack(vjust = .5, reverse = TRUE)) + 
                  scale_fill_manual(values = clr_val) + 
                  labs(title = paste0("Kontribusi per Dimensi Untuk ", .y))
                
    )
  )

# Simpan 
kontribusi_dimensi %>% 
  mutate(
    save = walk2(Plot, map(indikator_title, ~ paste0("Kontribusi per Dimensi Untuk ", .x, ".png")), 
          .f = ~ ggsave(plot = .x, 
                        filename = .y,
                        path = ggsave_path,
                        device = "png", units = "cm", 
                        width = 25, height = 9, dpi = 300
                        )
        )
  )
