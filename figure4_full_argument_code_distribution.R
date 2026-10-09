# ============================================================================
# Figure 4 — Distribution of arguments across the full range of argument
# codes, by case
# Media coverage of US military interventions: Libya 1986, Iran 2020,
# Yugoslavia 1999, Libya 2011
# ============================================================================
#
# Panel layout (2x2), same order as Figures 1-3, no overall title:
#   Row 1: Iran 2020        | Libya 1986
#   Row 2: Libya 2011       | Yugoslavia 1999
#
# Every individual argument code (not grouped) is shown, ordered by code
# number, colored by Pro/Con/Other (greyscale). Bars represent percent of
# that case's total coded arguments; counts are kept in the bar labels.
# Y-axis (the frequency axis, horizontal here due to coord_flip) is shared
# across this figure's own four panels only.
#
# Argument codes are pulled from BOTH the opinion-piece "Argument 1..6"
# columns AND the news-article "Source 1..8 argument" columns. Semicolon-
# separated cells are split into separate code occurrences. Codebook labels
# and Pro/Con/Other classification were generated programmatically from a
# direct dump of the raw docx table cells (not retyped by hand), and cross-
# verified for code 316 (Libya 2011) given an earlier merged-cell reading
# discrepancy -- confirmed "Con" via direct cell inspection.

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

# ---- Argument codebooks (code, pro_con, label) -- generated programmatically
# ---- from a direct dump of the raw docx table cells --------------------

codebook_iran2020 <- tribble(
  ~code, ~pro_con, ~label,
  "101", "Pro", "US acted in self-defense, appropriate retaliation, or operation otherwise justified because Soleimani was responsible for American and/or civilian deaths",
  "102", "Pro", "Justified because Iranian government is unjust or brutal",
  "103", "Pro", "Consistent with international and/or domestic law",
  "104", "Con", "Foreign policy strategy is incoherent, not well thought out, or messaging and public rationale is unclear",
  "105", "Con", "Raises risk of full-scale war with Iran",
  "106", "Con", "Puts Americans at risk of Iranian retaliation",
  "107", "Con", "Congress should have been consulted",
  "108", "Con", "Not in US interest: risks blowback to US national  interests",
  "109", "Con", "Strike may increase the popularity of the Iranian regime",
  "110", "Con", "Assassination is illegal under international law",
  "111", "Con", "Act of aggression, terrorism, and/or imperialism",
  "112", "Con", "Violation of Iraqi sovereignty",
  "113", "Con", "Hypocrisy or double standard",
  "114", "Other", "OTHER",
  "115", "Con", "Assassination is illegal under domestic (US) law",
  "116", "Con", "False or questionable intelligence",
  "117", "Con", "Bombing makes the US less safe or causes terrorism",
)

codebook_libya1986 <- tribble(
  ~code, ~pro_con, ~label,
  "201", "Pro", "Self-defense, deterrence, or appropriate retaliation",
  "202", "Pro", "Justified because Libyan government is unjust or brutal",
  "203", "Pro", "US has taken appropriate steps to limit \u201Ccollateral damage\u201D",
  "204", "Pro", "Campaign was consistent with international or domestic law",
  "205", "Con", "Not effective counterterrorism strategy, or poor foreign policy strategy in general",
  "206", "Con", "Bombing will only make Gaddafi more popular or unite Libya/Arabs against America",
  "207", "Con", "Bombing gives the terrorists what they want",
  "208", "Con", "Relations with other countries will deteriorate",
  "209", "Con", "Not in US national interest",
  "210", "Con", "Congress should have been consulted",
  "211", "Con", "Should have been law enforcement rather than military operation",
  "212", "Con", "Bombing only increases the risk of terrorism or makes the US less safe",
  "213", "Con", "Bombing is wrong, hypocritical, etc. because of the civilian casualties or general human suffering caused",
  "214", "Con", "False or questionable intelligence",
  "215", "Con", "Act of aggression, terrorism, or imperialism",
  "216", "Con", "Hypocrisy or double standard",
  "217", "Con", "Illegal under international law",
  "218", "Other", "OTHER",
)

codebook_libya2011 <- tribble(
  ~code, ~pro_con, ~label,
  "301", "Pro", "\u201CHumanitarian intervention\u201D: operation protects civilians",
  "302", "Pro", "Multilateralism: many countries participating makes the operation legitimate",
  "303", "Pro", "Democracy promotion",
  "304", "Pro", "Internationally sanctioned by UN",
  "306", "Pro", "Strikes will end civil war more quickly",
  "307", "Pro", "Appropriate measures taken to minimize risk to civilians",
  "308", "Pro", "Campaign is consistent with US and/or international law",
  "309", "Pro", "Campaign is in US national interest",
  "310", "Con", "Lacks congressional authorization",
  "311", "Con", "Intervention not forceful enough or \u201Chalf-hearted\u201D",
  "312", "Con", "General mismanagement, poor tactics, or failure to meet objectives",
  "313", "Con", "US is not doing enough compared to its allies; US should shoulder more of the burden of the intervention",
  "314", "Con", "Intervention is not in US national interest",
  "315", "Con", "Goals are unclear, inconsistent, or \u201Cmission creep\u201D",
  "316", "Con", "Exceeds mandate of UN Security Council",
  "317", "Con", "Deception: US/NATO after power, influence, resources, etc., not humanitarianism",
  "318", "Con", "Civilian casualties and human suffering",
  "319", "Con", "Illegal under international law",
  "320", "Con", "Libyan rebels are not worthy of support",
  "321", "Con", "Hypocrisy or double standard",
  "322", "Con", "Act of aggression, terrorism, or imperialism",
  "323", "Con", "Bombing will only increase Gaddafi\u2019s popularity",
  "324", "Other", "OTHER",
)

codebook_yugoslavia1999 <- tribble(
  ~code, ~pro_con, ~label,
  "401", "Pro", "\u201CHumanitarian intervention\u201D: intervention protects civilians, stops aggression in Kosovo",
  "402", "Pro", "Intervention will bring stability to the region, end the conflict more quickly, force Milosevic to negotiate, etc.",
  "404", "Pro", "Campaign is consistent with international or domestic law",
  "405", "Pro", "Appropriate measures taken to minimize risk to civilians",
  "406", "Pro", "Campaign is in US national interest",
  "407", "Con", "Congress should be (or should have been) consulted",
  "408", "Con", "Intervention not in US national interest",
  "409", "Con", "General mismanagement, poor tactics, or failure to meet objectives",
  "410", "Con", "Intervention not forceful enough",
  "411", "Con", "US is bearing too much of the burden, while other NATO states are doing comparatively little",
  "412", "Con", "Bombing will only increase Milosevic\u2019s popularity",
  "413", "Con", "Mission and rationale is unclear",
  "414", "Con", "NATO involvement could lead to wider war",
  "415", "Con", "Illegal under international law",
  "416", "Con", "Deception: US/NATO after power, influence, resources, etc., not humanitarianism",
  "417", "Con", "NATO intervention itself has accelerated human suffering",
  "418", "Con", "Hypocrisy or double standard",
  "419", "Con", "Act of aggression, terrorism, or imperialism",
  "420", "Con", "Kosovo Liberation Army is unworthy of support",
  "421", "Other", "OTHER",
)

codebooks <- list(
  "Libya 1986"      = codebook_libya1986,
  "Iran 2020"       = codebook_iran2020,
  "Yugoslavia 1999" = codebook_yugoslavia1999,
  "Libya 2011"      = codebook_libya2011
)

# ---- Short labels (verbatim, no truncation) from the "Label" column of the
# ---- updated Categorized_argument_codes.docx ------------------------------

labels_iran2020 <- tribble(
  ~code, ~short_label,
  "101", "Self-defense, appropriate retaliation",
  "102", "Iranian government unjust",
  "103", "Lawful",
  "104", "Poor strategy, unclear rationale",
  "105", "Raises risk of war",
  "106", "Americans at risk of retaliation",
  "107", "Consult Congress",
  "108", "Not in US interest",
  "109", "Could increase regime popularity",
  "110", "Illegal under international law",
  "111", "Aggression, terrorism, or imperialism",
  "112", "Violation of Iraqi sovereignty",
  "113", "Hypocrisy or double standard",
  "114", "Other",
  "115", "Illegal under domestic law",
  "116", "False or questionable intelligence",
  "117", "Makes the US less safe",
)

labels_libya1986 <- tribble(
  ~code, ~short_label,
  "201", "Self-defense, appropriate retaliation",
  "202", "Libyan government unjust",
  "203", "US minimized risk to civilians",
  "204", "Lawful",
  "205", "Poor strategy",
  "206", "Could increase Qaddafi\u2019s popularity",
  "207", "Gives terrorists what they want",
  "208", "Worsens relations with other countries",
  "209", "Not in US interest",
  "210", "Consult Congress",
  "211", "Should be law enforcement instead",
  "212", "Makes the US less safe",
  "213", "Civilian casualties, human suffering",
  "214", "False or questionable intelligence",
  "215", "Aggression, terrorism, or imperialism",
  "216", "Hypocrisy or double standard",
  "217", "Illegal under international law",
  "218", "Other",
)

labels_libya2011 <- tribble(
  ~code, ~short_label,
  "301", "Humanitarian intervention",
  "302", "Multilateralism",
  "303", "Democracy promotion",
  "304", "Sanctioned by UN",
  "306", "Will end civil war",
  "307", "US minimized risk to civilians",
  "308", "Lawful",
  "309", "In US interest",
  "310", "Consult Congress",
  "311", "Not forceful enough",
  "312", "Mismanagement, poor tactics",
  "313", "US should do more than allies",
  "314", "Not in US interest",
  "315", "Goals are unclear, inconsistent",
  "316", "Exceeds UNSC mandate",
  "317", "Deception: US/NATO are not humanitarians",
  "318", "Civilian casualties and human suffering",
  "319", "Illegal under international law",
  "320", "Libyan rebels not worthy of support",
  "321", "Hypocrisy or double standard",
  "322", "Aggression, terrorism, or imperialism",
  "323", "Could increase Qaddafi\u2019s popularity",
  "324", "Other",
)

labels_yugoslavia1999 <- tribble(
  ~code, ~short_label,
  "401", "Humanitarian intervention",
  "402", "Will bring stability, end conflict",
  "404", "Lawful",
  "405", "US minimized risk to civilians",
  "406", "In US interest",
  "407", "Consult Congress",
  "408", "Not in US interest",
  "409", "Mismanagement, poor tactics",
  "410", "Not forceful enough",
  "411", "Allies should do more",
  "412", "Could increase Milosevic\u2019s popularity",
  "413", "Mission, rationale is unclear",
  "414", "Raises risk of wider war",
  "415", "Illegal under international law",
  "416", "Deception: US/NATO are not humanitarians",
  "417", "Civilian casualties and human suffering",
  "418", "Hypocrisy or double standard",
  "419", "Aggression, terrorism, or imperialism",
  "420", "KLA is unworthy of support",
  "421", "Other",
)

short_labels <- list(
  "Libya 1986"      = labels_libya1986,
  "Iran 2020"       = labels_iran2020,
  "Yugoslavia 1999" = labels_yugoslavia1999,
  "Libya 2011"      = labels_libya2011
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

  freq <- codes %>%
    filter(code %in% cb$code) %>%
    count(code, name = "n") %>%
    right_join(cb, by = "code") %>%
    left_join(short_labels[[sheet]], by = "code") %>%
    mutate(n = replace_na(n, 0)) %>%
    mutate(
      code_num = as.numeric(code),
      sort_key = if_else(pro_con == "Other", Inf, code_num)
    ) %>%
    arrange(sort_key) %>%
    mutate(y_label = factor(short_label, levels = short_label))

  total_n <- sum(freq$n)
  freq <- freq %>% mutate(pct = if (total_n > 0) n / total_n * 100 else 0, total_n = total_n)

  # Drop zero-observation arguments from the display (denominator above is
  # computed before this filter, so percentages are unaffected).
  freq %>%
    filter(n > 0) %>%
    mutate(y_label = fct_drop(y_label))
})
names(case_freq) <- sheet_names

# ---- Plotting function (greyscale, no plot titles other than case name) ---

pro_con_colors <- c("Pro" = "#a6a6a6", "Con" = "#404040", "Other" = "#d4d4d4")

make_panel <- function(freq, case_name, y_max) {
  ggplot(freq, aes(x = fct_rev(y_label), y = pct, fill = pro_con)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)),
              hjust = -0.05, size = 2.3, color = "#333333") +
    coord_flip(clip = "off") +
    scale_fill_manual(values = pro_con_colors, name = NULL) +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.02))) +
    labs(title = case_name, x = NULL, y = "Percent") +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.margin = margin(5.5, 25, 5.5, 5.5),
      legend.position = "bottom"
    )
}

# Shared y-axis across this figure's own four panels only.
local_max_pct <- max(map_dbl(case_freq, ~ max(.x$pct)))
y_axis_max <- local_max_pct * 1.15

panels <- imap(case_freq, function(d, sheet) make_panel(d, sheet, y_axis_max))

# ---- Combine into 2x2 layout: Iran 2020 | Libya 1986 // Libya 2011 | Yugoslavia 1999
figure4 <- (panels[["Iran 2020"]] | panels[["Libya 1986"]]) /
           (panels[["Libya 2011"]] | panels[["Yugoslavia 1999"]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.justification = "center")

ggsave("figure4_full_argument_code_distribution.png", figure4,
       width = 16, height = 18, dpi = 300)

print(figure4)

# ---- Print frequency tables to console --------------------------------------
walk(names(case_freq), function(sheet) {
  cat("\n===", sheet, "( n =", case_freq[[sheet]]$total_n[1], ") ===\n")
  print(case_freq[[sheet]] %>% select(code, pro_con, n, pct) %>% arrange(desc(n)))
})
