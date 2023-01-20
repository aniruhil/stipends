
library(tidyverse)

# CIP 2010 Codes
# https://nces.ed.gov/ipeds/cipcode/resources.aspx?y=55

read.csv("cip-codes/CIPCode2010.csv") %>%
  janitor::clean_names() %>%
  mutate(
    cip_family = stringr::str_remove_all(cip_family, "="),
    cip_code = stringr::str_remove_all(cip_code, "="),
    cip_title = stringr::str_remove_all(cip_title, "\\."),
    cip2010 = stringr::str_remove_all(cip_code, "\\."),
    cip2010 = stringr::str_pad(cip2010, width = 6, side = "right", pad = "0")
    ) -> cip2010


# OU Fall 2021 Data

read.csv("oudata/oufall2021.csv") %>%
  janitor::clean_names() %>%
  arrange(
    school_name, program_code, program_name
    ) -> oufall2021

oufall2021 %>%
  distinct() %>%
  select(
    -c(fellowship_hours, other_award_hours)
    ) -> oufall2021_nodupes

summary(oufall2021_nodupes)

# https://www.ohio.edu/iea/university-data/program-inventory

readxl::read_excel("cip-codes/OHIO CIP Codes 2018-2022.xlsx") %>%
  janitor::clean_names() %>%
  mutate(
    cip2010 = stringr::str_remove_all(cip1, "\\."),
    cip2010 = stringr::str_pad(cip2010, width = 6, side = 'left', pad = '0')
    ) -> oucip

oufall2021_nodupes %>%
  left_join(
    oucip[, c(2, 3, 4, 5, 6, 7)],
    by = c("program_code" = "program")
  ) -> oufall2021_nodupes

oufall2021_nodupes %>%
  mutate(
    cip2010 = as.character(cip1)
    , cip2010 = stringr::str_remove_all(cip2010, "\\.")
    , cip2010 = stringr::str_pad(cip2010, width = 6, side = "left", pad = "0")
  ) -> oufall2021_nodupes

oufall2021_nodupes %>%
  relocate(
    11, 1, 3, 2, 10, 14, 4:8
  ) -> oufall2021_nodupes

save(oufall2021_nodupes, file = "oufall2021_nodupes.RData")

writexl::write_xlsx(
  oufall2021_nodupes,
  "oufall2021_nodupes.xlsx"
  )


# Stipend Survey Data

read_fwf(
  'GASDatafeed202122.txt',
  fwf_positions(
    c(1,  7, 13, 14, 22, 30, 38, 44, 52, 59,  66, 137),
    c(6, 12, 13, 21, 29, 37, 43, 51, 58, 65, 136, 145),
    c(
      "institution", "cip2010", "faculty_rank", "low_stipend",
      "high_stipend", "average_stipend", "number_of_GAs",
      "GA_mix_percent", "number_of_institutions", "salary_factor",
      "cip_code_description", "academic_year"
      )
    )
  ) %>%
  mutate(
    GA_mix_percent = stringr::str_remove_all(GA_mix_percent, "%"),
    GA_mix_percent = as.numeric(GA_mix_percent),
    faculty_rank = case_when(
      faculty_rank == 6 ~ "Teaching Assistant",
      faculty_rank == 7 ~ "Research Assistant",
      faculty_rank == 8 ~ "Graduate Associate",
      faculty_rank == 9 ~ "All ranks combined"
      )
    ) -> stipends

save(stipends, file = "stipends.RData")

## OU Fall 2021 data

readxl::read_excel("oudata/Stipends Fall 2021.xlsx") -> ou21


stipends %>%
  group_by(cip_code_description) %>%
  count()

stipends %>%
  ggplot(
    aes(
      x = low_stipend,
      y = high_stipend,
      col = average_stipend
      )
    ) +
  geom_point() +
  facet_wrap(~ faculty_rank) +
  scale_color_viridis_c(direction = -1)

## Preliminary Analysis





