# ============================================================================
# Figure 3 — Distribution of Con arguments by type of criticism, by case
# Media coverage of US military interventions: Libya 1986, Iran 2020,
# Yugoslavia 1999, Libya 2011
# ============================================================================
#
# Panel layout (2x2), same order as Figures 1-2, no overall title:
#   Row 1: Iran 2020        | Libya 1986
#   Row 2: Libya 2011       | Yugoslavia 1999
#
# Only Con arguments are included. Each Con code is grouped into one of four
# criticism types (Tactical/strategic, Procedural, Legal, Moral) per the
# "Categorized_argument_codes.docx" document. Percentages are of that case's
# total Con-argument count (consistent with Figure 2's logic, where % was of
# the relevant total).
#
# Argument codes are pulled from BOTH the opinion-piece "Argument 1..6"
# columns AND the news-article "Source 1..8 argument" columns. Semicolon-
# separated cells are split into separate code occurrences.

# ---- Packages ----------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(forcats)
library(ggplot2)
library(patchwork)

# ---- File path -----------------------------------------------------------
file_path <- "legacy_media_content_data.xlsx"   # update path if needed

sheet_names <- c("Libya 1986", "Iran 2020", "Yugoslavia 1999", "Libya 2011")

# ---- Con-argument codebooks with criticism type (from Categorized_argument
# ---- _codes.docx, verified directly against the raw table) ---------------

codebook_iran2020 <- tribble(
  ~code, ~type,
  "104","Tactical/strategic", "105","Tactical/strategic", "106","Tactical/strategic",
  "107","Procedural",
  "108","Tactical/strategic", "109","Tactical/strategic",
  "110","Legal",
  "111","Moral",
  "112","Legal",
  "113","Moral",
  "115","Legal",
  "116","Tactical/strategic", "117","Tactical/strategic"
)

codebook_libya1986 <- tribble(
  ~code, ~type,
  "205","Tactical/strategic", "206","Tactical/strategic", "207","Tactical/strategic",
  "208","Tactical/strategic", "209","Tactical/strategic",
  "210","Procedural",
  "211","Tactical/strategic", "212","Tactical/strategic",
  "213","Moral",
  "214","Tactical/strategic",
  "215","Moral", "216","Moral",
  "217","Legal"
)

codebook_libya2011 <- tribble(
  ~code, ~type,
  "310","Procedural",
  "311","Tactical/strategic", "312","Tactical/strategic", "313","Tactical/strategic",
  "314","Tactical/strategic", "315","Tactical/strategic",
  "316","Legal",
  "317","Moral", "318","Moral",
  "319","Legal",
  "320","Moral", "321","Moral", "322","Moral",
  "323","Tactical/strategic"
)

codebook_yugoslavia1999 <- tribble(
  ~code, ~type,
  "407","Procedural",
  "408","Tactical/strategic", "409","Tactical/strategic", "410","Tactical/strategic",
  "411","Tactical/strategic", "412","Tactical/strategic", "413","Tactical/strategic",
  "414","Tactical/strategic",
  "415","Legal",
  "416","Moral", "417","Moral", "418","Moral", "419","Moral", "420","Moral"
)

codebooks <- list(
  "Libya 1986"      = codebook_libya1986,
  "Iran 2020"       = codebook_iran2020,
  "Yugoslavia 1999" = codebook_yugoslavia1999,
  "Libya 2011"      = codebook_libya2011
)

type_levels <- c("Tactical/strategic", "Procedural", "Legal", "Moral")

# ---- Extraction: every argument code from Argument 1..6 and Source 1..8
# ---- argument columns -------------------------------------------------

extract_argument_codes <- function(df) {
  arg_cols <- names(df)[
    str_detect(names(df), regex("^Argument\\s*\\d+$", ignore_case = TRUE)) |
    str_detect(names(df), regex("^Source\\s*\\d+.*argument$", ignore_case = TRUE))
  ]

  df %>%
    select(all_of(arg_cols)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(.row_id = row_number()) %>%
    pivot_longer(-.row_id, names_to = "column", values_to = "raw_value") %>%
    filter(!is.na(raw_value), str_trim(raw_value) != "") %>%
    separate_rows(raw_value, sep = ";") %>%
    mutate(code = str_trim(raw_value)) %>%
    filter(code != "") %>%
    select(.row_id, column, code)
}

# ---- Read + extract + validate + aggregate per sheet -----------------------
# Note: validity is checked against the Con-argument codebook only, since
# this figure is Con arguments exclusively; Pro and OTHER codes are expected
# to appear in the raw extraction and are simply not part of this codebook,
# not flagged as errors.

case_freq <- map(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  codes <- extract_argument_codes(df)
  cb <- codebooks[[sheet]]

  agg <- codes %>%
    inner_join(cb, by = "code") %>%   # keep only Con codes (drops Pro/Other)
    count(type, name = "n") %>%
    mutate(type = factor(type, levels = type_levels)) %>%
    complete(type, fill = list(n = 0))

  total_n <- sum(agg$n)
  agg %>% mutate(pct = if (total_n > 0) n / total_n * 100 else 0, total_n = total_n)
})
names(case_freq) <- sheet_names

# ---- Plotting function (greyscale, no plot titles other than case name) ---

type_colors <- c(
  "Tactical/strategic" = "#2b2b2b",
  "Procedural"          = "#6e6e6e",
  "Legal"               = "#a6a6a6",
  "Moral"               = "#d4d4d4"
)

make_panel <- function(freq, case_name, y_max) {
  ggplot(freq, aes(x = type, y = pct, fill = type)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)),
              vjust = -0.4, size = 2.8, color = "#333333") +
    scale_fill_manual(values = type_colors, guide = "none") +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.05))) +
    labs(title = case_name, x = NULL, y = "Percent") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(size = 8.5),
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Shared y-axis across this figure's own four panels only (not tied to
# Figures 1 or 2).
local_max_pct <- max(map_dbl(case_freq, ~ max(.x$pct)))
y_axis_max <- local_max_pct * 1.15

panels <- imap(case_freq, function(d, sheet) make_panel(d, sheet, y_axis_max))

# ---- Combine into 2x2 layout: Iran 2020 | Libya 1986 // Libya 2011 | Yugoslavia 1999
figure3 <- (panels[["Iran 2020"]] | panels[["Libya 1986"]]) /
           (panels[["Libya 2011"]] | panels[["Yugoslavia 1999"]])

ggsave("figure3_con_criticism_type.png", figure3,
       width = 11, height = 9, dpi = 300)

print(figure3)

# ---- Print frequency tables to console --------------------------------------
walk(names(case_freq), function(sheet) {
  cat("\n===", sheet, "( Con n =", case_freq[[sheet]]$total_n[1], ") ===\n")
  print(case_freq[[sheet]] %>% select(type, n, pct))
})
