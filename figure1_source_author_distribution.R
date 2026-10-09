# ============================================================================
# Figure 1 — Distribution of authors/sources by case
# Media coverage of US military interventions: Libya 1986, Iran 2020,
# Yugoslavia 1999, Libya 2011
# ============================================================================
#
# Panel layout (2x2), no overall title (added manually later):
#   Row 1: Iran 2020        | Libya 1986
#   Row 2: Libya 2011       | Yugoslavia 1999
#
# Source/Author Codes (from Codebook and Procedure - Study 1, verbatim):
#   Codes 2, 11, and 16 do NOT exist in the codebook (the numbering skips
#   1->3, 10->12, 15->17) -- confirmed directly from the raw document text,
#   not assumed. Valid codes are: 1,3,4,5,6,7,8,9,10,12,13,14,15,17,18,19,
#   20,21,22,23 (20 codes total).
#
# Every quoted/cited source in a news article (Source 1..8) and every
# opinion-piece author (Author_code) is tagged with one of these codes.
# Some cells specify further detail per the codebook's "(specify)" /
# "(specify what country)" instructions (e.g., "3 (Iraq)", "23 (Foreign
# Demonstrators)") -- these are extracted by leading digit, not treated as
# errors. Semicolon-separated multi-codes are split into separate counts.

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

# ---- Source/Author code labels (verbatim from codebook) -------------------

source_author_labels <- tribble(
  ~code, ~label,
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
valid_codes <- source_author_labels$code

# ---- Extraction: every Author_code + Source 1..8 _code value --------------
# Leading-digit extraction handles "(specify)" suffixes like "3 (Iraq)".

extract_source_author_codes <- function(df) {
  df <- df %>% mutate(.row_id = row_number())

  author_part <- df %>%
    select(.row_id, raw_value = Author_code) %>%
    mutate(raw_value = as.character(raw_value), origin = "author")

  source_part <- map_dfr(1:8, function(num) {
    code_col <- names(df)[str_detect(names(df), regex(paste0("^Source\\s*", num, "[_ ]*code$"), ignore_case = TRUE))]
    if (length(code_col) == 0) return(tibble())
    df %>%
      select(.row_id, raw_value = all_of(code_col[1])) %>%
      mutate(raw_value = as.character(raw_value), origin = paste0("source_", num))
  })

  bind_rows(author_part, source_part) %>%
    filter(!is.na(raw_value), str_trim(as.character(raw_value)) != "") %>%
    mutate(raw_value = as.character(raw_value)) %>%
    separate_rows(raw_value, sep = ";") %>%
    mutate(piece = str_trim(raw_value),
           code = str_extract(piece, "^\\d+")) %>%
    filter(!is.na(code), piece != "") %>%
    # Codes 2, 11, 16 are not in this codebook document (numbering skips
    # 1->3, 10->12, 15->17) but appear in the raw data -- these are legacy
    # sub-categories later merged into their parent codes; remap accordingly.
    mutate(code = case_when(
      code == "2"  ~ "1",
      code == "11" ~ "10",
      code == "16" ~ "15",
      TRUE ~ code
    )) %>%
    select(.row_id, origin, piece, code)
}

# ---- Read + extract + validate per sheet -----------------------------------
# "Editorial board" is a synthetic category, not part of the codebook's 1-23
# scale: editorials have no coded author/source (Author_code is blank), so
# each article with Article type == "editorial" contributes one count here.

case_freq <- map(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  codes <- extract_source_author_codes(df)

  invalid <- codes %>% filter(!code %in% valid_codes)
  if (nrow(invalid) > 0) {
    message(sprintf(
      "[%s] %d non-codebook value(s) found and excluded: %s",
      sheet, nrow(invalid), paste(unique(invalid$piece), collapse = " | ")
    ))
  } else {
    message(sprintf("[%s] no data-quality issues found.", sheet))
  }

  n_editorials <- df %>%
    filter(str_to_lower(str_trim(`Article type`)) == "editorial") %>%
    nrow()

  # Denominator: total UNIQUE sources/authors, not total category-code
  # instances. A single source with multiple codes (e.g. "3;7") is one
  # source, so it's counted once here even though it contributes to two
  # bars below. This means bar percentages can sum to more than 100% when
  # sources carry multiple codes -- expected, since the categories are not
  # mutually exclusive for such sources.
  n_unique_sources <- codes %>%
    filter(code %in% valid_codes) %>%
    distinct(.row_id, origin) %>%
    nrow()
  total_n <- n_unique_sources + n_editorials

  freq <- codes %>%
    filter(code %in% valid_codes) %>%
    count(code, name = "n") %>%
    right_join(source_author_labels, by = "code") %>%
    mutate(n = replace_na(n, 0)) %>%
    mutate(code_num = as.numeric(code)) %>%
    bind_rows(tibble(code = "22.5", label = "Editorial board", n = n_editorials, code_num = 22.5)) %>%
    arrange(code_num) %>%
    mutate(y_label = factor(label, levels = label))

  freq %>% mutate(pct = if (total_n > 0) n / total_n * 100 else 0, total_n = total_n)
})
names(case_freq) <- sheet_names

# ---- Plotting function ------------------------------------------------------
# No plot titles other than the case name (per instructions: no figure title,
# just label each panel with the case name).

make_panel <- function(freq, case_name, y_max, show_labels) {
  p <- ggplot(freq, aes(x = fct_rev(y_label), y = pct)) +
    geom_col(fill = "#595959", width = 0.7) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)),
              hjust = -0.05, size = 3, color = "#52514e") +
    coord_flip(clip = "off") +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.02))) +
    labs(title = case_name, x = NULL, y = "Percent") +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.margin = margin(5.5, 25, 5.5, 5.5)
    )

  if (show_labels) {
    p + theme(axis.text.y = element_text(size = 11))
  } else {
    p + theme(axis.text.y = element_blank())
  }
}

# Shared y-axis across this figure's own four panels only (not tied to
# Figures 2 or 3).
local_max_pct <- max(map_dbl(case_freq, ~ max(.x$pct)))
y_axis_max <- local_max_pct * 1.15

# Category labels are identical and identically ordered across all four
# cases, so they're only printed once per row (left-hand panels) to save
# space and let the label font be much larger while still fitting one page.
panels <- list(
  "Iran 2020"       = make_panel(case_freq[["Iran 2020"]],       "Iran 2020",       y_axis_max, show_labels = TRUE),
  "Libya 1986"      = make_panel(case_freq[["Libya 1986"]],      "Libya 1986",      y_axis_max, show_labels = FALSE),
  "Libya 2011"      = make_panel(case_freq[["Libya 2011"]],      "Libya 2011",      y_axis_max, show_labels = TRUE),
  "Yugoslavia 1999" = make_panel(case_freq[["Yugoslavia 1999"]], "Yugoslavia 1999", y_axis_max, show_labels = FALSE)
)

# ---- Combine into 2x2 layout: Iran 2020 | Libya 1986 // Libya 2011 | Yugoslavia 1999
figure1 <- (panels[["Iran 2020"]] | panels[["Libya 1986"]]) /
           (panels[["Libya 2011"]] | panels[["Yugoslavia 1999"]])

ggsave("figure1_source_author_distribution.png", figure1,
       width = 10, height = 12, dpi = 300)

print(figure1)

# ---- Print frequency tables to console --------------------------------------
walk(names(case_freq), function(sheet) {
  cat("\n===", sheet, "( n =", case_freq[[sheet]]$total_n[1], ") ===\n")
  print(case_freq[[sheet]] %>% select(code, label, n, pct) %>% arrange(desc(n)))
})
