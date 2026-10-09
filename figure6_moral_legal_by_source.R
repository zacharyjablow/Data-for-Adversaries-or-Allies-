# ============================================================================
# Figure 6 — Which sources/authors are associated with Moral and Legal
# criticism, by case
# Media coverage of US military interventions: Libya 1986, Iran 2020,
# Yugoslavia 1999, Libya 2011
# ============================================================================
#
# Panel layout (2x2), same order as Figures 1-5, no overall title:
#   Row 1: Iran 2020        | Libya 1986
#   Row 2: Libya 2011       | Yugoslavia 1999
#
# Only Con arguments classified as "Legal" or "Moral" criticism (per the
# Categorized_argument_codes.docx "Type of criticism" column) are included,
# combined into a single count per source/author type (not split by Legal
# vs. Moral). All sources are included (no foreign/civilian exclusion here,
# unlike Figure 5). Each argument occurrence is attributed to its
# originating source (for news articles, that specific quoted source; for
# opinion pieces, the author; editorials get "Editorial board" per Figure
# 1's convention, since Author_code is blank for editorials). Bars show
# raw count and percent of that case's total Legal+Moral argument count;
# source/author types with zero Legal/Moral occurrences are omitted.
#
# Source/author codes 2, 11, 16 (not in the codebook document but present
# in the raw data) are remapped to their parent codes 1, 10, 15 per the
# earlier locked decision for Figure 1.

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

# ---- Source/Author code labels (verbatim from codebook; see Figure 1) -----

source_author_labels <- tribble(
  ~origin_code, ~origin_label,
  "1",  "US administration or military/intelligence official",
  "3",  "Foreign official, politician, or foreign media organization",
  "4",  "Republican Party politician or official",
  "5",  "Democratic Party politician or official",
  "6",  "UN official",
  "7",  "Ordinary US citizen",
  "8",  "Ordinary civilian from targeted country",
  "9",  "NATO official",
  "10", "Retired or former US official",
  "12", "US celebrity or prominent citizen",
  "13", "Religious leader",
  "14", "US veteran",
  "15", "Think tank, NGO, or interest group",
  "17", "Human rights organization",
  "18", "Independent expert or academic",
  "19", "Unnamed or unattributed \"expert\"",
  "20", "Unnamed or unattributed \"critic\"",
  "21", "Anti-war group",
  "22", "Regular opinion columnist",
  "23", "Other"
)

# ---- Argument codebooks: code -> criticism type (Legal/Moral only kept) ---

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

type_levels <- c("Legal", "Moral")

# ---- Extraction: every argument code occurrence, tagged with its full
# ---- resolved origin (source/author label, or "Editorial board") ---------

extract_with_full_origin <- function(df) {
  df <- df %>%
    mutate(
      .row_id = row_number(),
      is_editorial = str_to_lower(str_trim(`Article type`)) == "editorial"
    )

  arg_cols <- names(df)[str_detect(names(df), regex("^Argument\\s*\\d+$", ignore_case = TRUE))]
  author_part <- df %>%
    select(.row_id, is_editorial, Author_code, all_of(arg_cols)) %>%
    mutate(across(all_of(arg_cols), as.character)) %>%
    pivot_longer(cols = all_of(arg_cols), names_to = "column", values_to = "raw_value") %>%
    mutate(origin_raw = as.character(Author_code)) %>%
    select(.row_id, is_editorial, raw_value, origin_raw)

  source_parts <- map_dfr(1:8, function(num) {
    code_col <- names(df)[str_detect(names(df), regex(paste0("^Source\\s*", num, "[_ ]*code$"), ignore_case = TRUE))]
    arg_col  <- names(df)[str_detect(names(df), regex(paste0("^Source\\s*", num, "[_ ]*argument$"), ignore_case = TRUE))]
    if (length(code_col) == 0 || length(arg_col) == 0) return(tibble())
    df %>%
      select(.row_id, .code_val = all_of(code_col[1]), raw_value = all_of(arg_col[1])) %>%
      mutate(raw_value = as.character(raw_value),
             origin_raw = as.character(.code_val),
             is_editorial = FALSE) %>%
      select(.row_id, is_editorial, raw_value, origin_raw)
  })

  bind_rows(author_part, source_parts) %>%
    filter(!is.na(raw_value), str_trim(raw_value) != "") %>%
    separate_rows(raw_value, sep = ";") %>%
    mutate(code = str_trim(raw_value)) %>%
    filter(code != "") %>%
    mutate(origin_code_raw = str_extract(str_trim(origin_raw), "^\\d+")) %>%
    # Remap legacy codes 2/11/16 to their parent categories (per Figure 1).
    mutate(origin_code = case_when(
      origin_code_raw == "2"  ~ "1",
      origin_code_raw == "11" ~ "10",
      origin_code_raw == "16" ~ "15",
      TRUE ~ origin_code_raw
    )) %>%
    select(.row_id, code, origin_code, is_editorial)
}

# ---- Read + extract + aggregate per sheet -----------------------------------

case_freq <- map(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  codes <- extract_with_full_origin(df)
  cb <- codebooks[[sheet]]

  resolved <- codes %>%
    inner_join(cb, by = "code") %>%
    filter(type %in% c("Legal", "Moral")) %>%    # keep only Legal/Moral Con codes
    left_join(source_author_labels, by = "origin_code") %>%
    mutate(origin_label = if_else(is_editorial, "Editorial board", origin_label))

  unresolved <- resolved %>% filter(is.na(origin_label))
  if (nrow(unresolved) > 0) {
    message(sprintf(
      "[%s] %d Legal/Moral argument occurrence(s) have no resolvable source/author origin and are excluded: %s",
      sheet, nrow(unresolved), paste(unique(unresolved$origin_code), collapse = " | ")
    ))
  }

  agg <- resolved %>%
    filter(!is.na(origin_label)) %>%
    count(origin_label, name = "n")   # Legal + Moral combined per source/author type

  total_n <- sum(agg$n)
  agg <- agg %>% mutate(pct = if (total_n > 0) n / total_n * 100 else 0, total_n = total_n)

  # Order source/author categories by count (descending); zero-count types
  # never appear here since count() only includes observed combinations.
  agg %>%
    arrange(desc(n)) %>%
    mutate(origin_label = factor(origin_label, levels = rev(origin_label)))
})
names(case_freq) <- sheet_names

# ---- Plotting function (greyscale, no plot titles other than case name) ---

make_panel <- function(freq, case_name, y_max) {
  ggplot(freq, aes(x = origin_label, y = pct)) +
    geom_col(fill = "#595959", width = 0.7) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)),
              hjust = -0.05, size = 2.6, color = "#333333") +
    coord_flip(clip = "off") +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.02))) +
    labs(title = case_name, x = NULL, y = "Percent") +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.margin = margin(5.5, 25, 5.5, 5.5)
    )
}

# Shared y-axis across this figure's own four panels.
local_max_pct <- max(map_dbl(case_freq, ~ max(.x$pct)))
y_axis_max <- local_max_pct * 1.15

panels <- imap(case_freq, function(d, sheet) make_panel(d, sheet, y_axis_max))

# ---- Combine into 2x2 layout: Iran 2020 | Libya 1986 // Libya 2011 | Yugoslavia 1999
figure6 <- (panels[["Iran 2020"]] | panels[["Libya 1986"]]) /
           (panels[["Libya 2011"]] | panels[["Yugoslavia 1999"]])

ggsave("figure6_moral_legal_by_source.png", figure6,
       width = 15, height = 15, dpi = 300)

print(figure6)

# ---- Print frequency tables to console --------------------------------------
walk(names(case_freq), function(sheet) {
  cat("\n===", sheet, "( Legal+Moral n =", case_freq[[sheet]]$total_n[1], ") ===\n")
  print(case_freq[[sheet]] %>% select(origin_label, n, pct) %>% arrange(desc(n)))
})
