# ============================================================================
# Figure 2 — Distribution of Pro vs. Con arguments by case
# Media coverage of US military interventions: Libya 1986, Iran 2020,
# Yugoslavia 1999, Libya 2011
# ============================================================================
#
# Panel layout (2x2), same order as Figure 1, no overall title:
#   Row 1: Iran 2020        | Libya 1986
#   Row 2: Libya 2011       | Yugoslavia 1999
#
# Every argument code in each case's codebook is Pro or Con, except a single
# catch-all "OTHER" code per case (114/218/324/421) which is neither and is
# excluded here, since this figure is specifically Pro vs. Con.
#
# Argument codes are pulled from BOTH the opinion-piece "Argument 1..6"
# columns AND the news-article "Source 1..8 argument" columns (per the
# codebook: opinion pieces are coded in the Argument columns, news pieces
# are coded via sources). Semicolon-separated cells are split into separate
# code occurrences.

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

# ---- Argument codebooks (Pro/Con only; verified directly against the raw
# ---- docx tables, including checking every blank Pro/Con cell) -----------

codebook_iran2020 <- tribble(
  ~code, ~pro_con,
  "101","Pro", "102","Pro", "103","Pro",
  "104","Con", "105","Con", "106","Con", "107","Con", "108","Con",
  "109","Con", "110","Con", "111","Con", "112","Con", "113","Con",
  "114","Other",
  "115","Con", "116","Con", "117","Con"
)

codebook_libya1986 <- tribble(
  ~code, ~pro_con,
  "201","Pro", "202","Pro", "203","Pro", "204","Pro",
  "205","Con", "206","Con", "207","Con", "208","Con", "209","Con",
  "210","Con", "211","Con", "212","Con", "213","Con", "214","Con",
  "215","Con", "216","Con", "217","Con",
  "218","Other"
)

codebook_libya2011 <- tribble(
  ~code, ~pro_con,
  "301","Pro", "302","Pro", "303","Pro", "304","Pro", "306","Pro",
  "307","Pro", "308","Pro", "309","Pro",
  "310","Con", "311","Con", "312","Con", "313","Con", "314","Con",
  "315","Con", "316","Con", "317","Con", "318","Con", "319","Con",
  "320","Con", "321","Con", "322","Con", "323","Con",
  "324","Other"
)

codebook_yugoslavia1999 <- tribble(
  ~code, ~pro_con,
  "401","Pro", "402","Pro", "404","Pro", "405","Pro", "406","Pro",
  "407","Con", "408","Con", "409","Con", "410","Con", "411","Con",
  "412","Con", "413","Con", "414","Con", "415","Con", "416","Con",
  "417","Con", "418","Con", "419","Con", "420","Con",
  "421","Other"
)

codebooks <- list(
  "Libya 1986"      = codebook_libya1986,
  "Iran 2020"       = codebook_iran2020,
  "Yugoslavia 1999" = codebook_yugoslavia1999,
  "Libya 2011"      = codebook_libya2011
)

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

case_freq <- map(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  codes <- extract_argument_codes(df)
  cb <- codebooks[[sheet]]

  invalid <- codes %>% filter(!code %in% cb$code)
  if (nrow(invalid) > 0) {
    message(sprintf(
      "[%s] %d non-codebook value(s) found and excluded: %s",
      sheet, nrow(invalid), paste(unique(invalid$code), collapse = " | ")
    ))
  } else {
    message(sprintf("[%s] no data-quality issues found.", sheet))
  }

  agg <- codes %>%
    filter(code %in% cb$code) %>%
    left_join(cb, by = "code") %>%
    filter(pro_con %in% c("Pro", "Con")) %>%   # exclude the single OTHER code
    count(pro_con, name = "n") %>%
    mutate(pro_con = factor(pro_con, levels = c("Pro", "Con"))) %>%
    complete(pro_con, fill = list(n = 0))

  total_n <- sum(agg$n)
  agg %>% mutate(pct = if (total_n > 0) n / total_n * 100 else 0, total_n = total_n)
})
names(case_freq) <- sheet_names

# ---- Plotting function ------------------------------------------------------
# No plot titles other than the case name.

pro_con_colors <- c("Pro" = "#a6a6a6", "Con" = "#404040")

make_panel <- function(freq, case_name, y_max) {
  ggplot(freq, aes(x = pro_con, y = pct, fill = pro_con)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)),
              vjust = -0.4, size = 3, color = "#52514e") +
    scale_fill_manual(values = pro_con_colors, guide = "none") +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.02))) +
    labs(title = case_name, x = NULL, y = "Percent") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Shared y-axis across this figure's own four panels only (not tied to
# Figures 1 or 3).
local_max_pct <- max(map_dbl(case_freq, ~ max(.x$pct)))
y_axis_max <- local_max_pct * 1.15

panels <- imap(case_freq, function(d, sheet) make_panel(d, sheet, y_axis_max))



# ---- Combine into 2x2 layout: Iran 2020 | Libya 1986 // Libya 2011 | Yugoslavia 1999
figure2 <- (panels[["Iran 2020"]] | panels[["Libya 1986"]]) /
           (panels[["Libya 2011"]] | panels[["Yugoslavia 1999"]])

ggsave("figure2_pro_con_distribution.png", figure2,
       width = 10, height = 9, dpi = 300)

print(figure2)

# ---- Print frequency tables to console --------------------------------------
walk(names(case_freq), function(sheet) {
  cat("\n===", sheet, "( n =", case_freq[[sheet]]$total_n[1], ") ===\n")
  print(case_freq[[sheet]] %>% select(pro_con, n, pct))
})
