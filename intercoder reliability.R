library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidycomm)

setwd("~/Library/CloudStorage/OneDrive-Personal/UIUC/Dissertation/study 1 content analysis/intercoder reliability")



################################
# ICR ON NUMBER OF UNITS CODED #
################################
dat <- read_xlsx('intercoding.xlsx', sheet="Yugoslavia 1999")

# Opinion: count non-empty Argument cells
dat <- dat %>%
  mutate(arg_count = rowSums(!is.na(select(., `Argument 1`:`Argument 6`)))) %>%
  relocate(arg_count, .after = `Argument 6`)

# News: count total argument code occurrences across all source-argument cells
# Cells may contain multiple codes separated by semicolons (e.g. "104; 105")
# Same code appearing in multiple sources counts multiple times 
count_news_args <- function(row) {
  src_arg_cols <- grep("(?i)source.*argument", names(row), value = TRUE)
  vals <- unlist(row[src_arg_cols])
  vals <- vals[!is.na(vals) & trimws(vals) != ""]
  if (length(vals) == 0) return(0L)
  codes <- unlist(strsplit(as.character(vals), ";"))
  codes <- trimws(codes)
  codes <- codes[codes != ""]
  return(length(codes))   # total occurrences, NOT unique
}

dat <- dat %>%
  mutate(source_count = sapply(
    split(., seq_len(nrow(.))),
    count_news_args
  )) %>%
  relocate(source_count, .after = last_col())

# Combined: news uses occurrence count, opinion uses arg_count
dat <- dat %>%
  mutate(count_combined = case_when(
    tolower(`Article type`) == "news" | tolower(`Article type`) =="news analysis" ~ source_count,
    TRUE ~ arg_count
  ))

# Krippendorff's alpha
dat_small <- dat %>%
  select(Coder, Link, `Article type`, Date, Publication, count_combined) %>%
  arrange(Link) %>% 
  drop_na()

test_icr(dat_small, unit_var = Link, coder_var = Coder, count_combined,
         levels = c(count_combined = "interval"))

#dat_extrasmall <- dat_small %>% filter(Coder!="Scarlett") %>% arrange(Link)

#test_icr(dat_extrasmall, unit_var = Link, coder_var = Coder, count_combined,
#        levels = c(count_combined = "interval"))

#dat_sorted <- dat %>% select(Coder, Publication, Date, "Article type", Link, count_combined) %>% arrange(Link)

#####################################
#DECRIPTIVES ON INTEGER DISAGREEMENT#
#####################################
iran <- read_excel("intercoding.xlsx", sheet = "Iran 2020")
libya <- read_excel("intercoding.xlsx", sheet = "Libya 1986")

calc_disagreement <- function(df) {
  # Pivot so each passage is one row with one column per coder
  dat_small %>%
    pivot_wider(names_from = Coder, values_from = count_combined) %>%
    # Compute pairwise absolute differences
    mutate(
      diff_Zach_Scarlett = abs(Zach - Scarlett),
      diff_Zach_Tim      = abs(Zach - Tim),
      diff_Scarlett_Tim  = abs(Scarlett - Tim)
    ) %>%
    summarise(across(starts_with("diff_"), 
                     list(mean = mean, median = median), 
                     na.rm = TRUE))
}

calc_disagreement(iran)
calc_disagreement(libya)

################################
# ICR on argument/source codes #
################################
test <- read_xlsx("intercoding 2.xlsx", sheet = "Libya 2011")

test <- test %>%
  filter(!is.na(Coder)) %>%                        # remove the 1 blank Coder row
  group_by(Link, Passage) %>%
  mutate(Unit = cur_group_id()) %>%                # unique ID per passage (Link is sometimes duplicated)
  ungroup()


test_icr(test, unit_var = Passage, coder_var = Coder, "Argument code")
test_icr(test, unit_var = Passage, coder_var = Coder, "Author/source code", na.omit=T)


