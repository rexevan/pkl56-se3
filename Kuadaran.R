# visualisasi kuadran rumah tangga & Provinsi
# Developed by Wihelmus Wedo 
# install.packages("tidyverse", "ggrepel")

library(tidyverse)

# Kuadran Dalam Kota -------------------
kuadran <- read_csv("Kuadran.csv")

kuadran <- kuadran %>% 
  mutate(
    perkap = log(perkap), 
    GK     = log(GK)
  ) %>% 
  gather(indikator_num, ikm, score12, score11, score3) %>% 
  mutate(
    indikator_title = case_when(
      indikator_num == "score12" ~ "4 Dimensi : 12 Indikator",
      indikator_num == "score11" ~ "4 Dimensi : 11 Indikator", 
      indikator_num == "score3"  ~ "3 Dimensi : 10 Indikator" )
    ) %>% select(-indikator_num) %>% 
  group_by(indikator_title) %>% nest()

# Viz 
# Urutan Kota 
urutan_Kota <- LETTERS[1:7]

# Cut off, garis kemiskinan dan posisi anotasi 
cut_off <- 1/3
gk_intercept <- 13.5 

# Set Themes & colors 
source("ggplot_themes.R")
theme_set(theme_fte())

# Direktori gambar
ggsave_path <- "Gambar/"


# warna intersep 
vcol <- "#EF2DA5"
hcol <- "#4BA1F9"
intersep_size <- 1

# Visualization 
kuadran <- kuadran %>% 
  mutate(
    kuadran_plot = map2(.x = data, 
                       .y = indikator_title, 
                       .f = ~ .x %>% 
                         mutate(Kota = Kota %>% ordered(levels = urutan_Kota)) %>% 
                         ggplot(aes(x = ikm, y = perkap)) +
                         # intersep 
                         geom_vline(xintercept = cut_off, color = vcol, size = intersep_size) + 
                         geom_hline(yintercept = gk_intercept, color = hcol, size = intersep_size) +
                         # poin 
                         geom_point() +
                         facet_wrap(~Kota, ncol = 2) + 
                         # skala x dan y
                         scale_x_continuous(limits = c(0, 1)) +
                         scale_y_continuous(limits = c(10, 17)) + 
                         labs(
                           title = paste0("Kuadran untuk ", .y),
                           x = "Indeks Kemiskinan Multidimensi",
                           y = "Rata-rata Pengeluaran (Skala Logaritma)"
                         )
    )
  )


# Save it 
kuadran %>% 
  mutate(
    save = walk2(.x = kuadran_plot, 
          .y = indikator_title, 
          .f = ~ ggsave(
            plot = .x,
            filename = paste0("Kuadran untuk ", .y, ".png"),
            path = ggsave_path,
            device = "png", units = "cm", 
            dpi = 300, width = 20, height = 25)
          )
  )

# Kuadran Antar Kota ---------------------------------
kuadran_Kota <- read_csv("Kuadran_AntarKota.csv")

kuadran_Kota <- kuadran_Kota %>% 
  gather(indikator_num, ikm, -Kota, -GK.miskin.persen) %>% 
  mutate(
    indikator_title = case_when(
      indikator_num == "ikm.12" ~ "4 Dimensi : 12 Indikator",
      indikator_num == "ikm.11" ~ "4 Dimensi : 11 Indikator", 
      indikator_num == "ikm.10"  ~ "3 Dimensi : 10 Indikator" )
    ) %>% select(-indikator_num)

kuadran_Kota <- kuadran_Kota %>% 
  group_by(indikator_title) %>% 
  nest() %>% 
  mutate(
    gk_intersep = .0607, 
    ikm_intersep = c(.0603, .1429, .1607)
  )

library(ggrepel)
kuadran_Kota <- kuadran_Kota %>% 
  mutate(
    kuadran_plot = pmap(
      .l = list(data,
                indikator_title, 
                ikm_intersep, 
                gk_intersep),
      .f = ~ ..1 %>% 
        ggplot(aes(x = ikm, y = GK.miskin.persen)) + 
        scale_x_continuous(limits = c(0, 0.3)) + 
        scale_y_continuous(limits = c(0.02, 0.10)) + 
        labs(
          title = paste0("Kuadran AntarKota Untuk ", ..2),
          x = "Indeks Kemiskinan Multidimensi", 
          y = "Persentase Penduduk Miskin"
        ) + 
        geom_vline(xintercept = ..3, color = vcol, size = intersep_size) + 
        geom_hline(yintercept = ..4, color = hcol, size = intersep_size) +
        geom_point(size = 5) + 
        geom_label_repel(aes(fill = Kota, label = Kota), 
                         show.legend = FALSE, 
                         color = 'white',
                         size = 5,
                         box.padding = unit(0.35, "lines"),
                         point.padding = unit(0.5, "lines"), 
                         segment.color = 'black'))
  )

# Save it 
kuadran_Kota %>% 
  mutate(
    save = walk2(
      .x = kuadran_plot, 
      .y = indikator_title, 
      .f = ~ ggsave(
        plot = .x, 
        filename = paste0("Kuadran AntarKota Untuk ", .y, ".png"), 
        path = ggsave_path,
        device = "png", units = "cm", 
        height = 10, width = 20, dpi = 300
      )
    )
  )
