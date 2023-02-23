# takes about 30 seconds to process about two dozen files

# this file should be opened and run in a directory that
# has whatever sub-directories (arbitrarily named and structured) you like containing documents to be processed
# currently it processed pdf, docx and pptx files
# no real error checking is provided yet, but if (for example) a pdf that contains only page images
# is processed, it just will be excluded from teh results.

# pdftools requires poppler, but i believe it installs it automatically when you install the pdftools
# library (at least on a Mac - IIRC it was a manual install in linux)

# the output is a csv file called tidydocs.csv that must be put in teh directory with the shiny app

# the shiny app uses "short file names" in the drop-downs, etc as the riginal filenames are unweildy
# currently this mapping if a real file name to a reasonable short file name is done manually
# indicated by "# short name creation"

# Note this is also done similarly (but not identically) in the shiny app, so it will have to be
# manually changed in that code too.

# this whole process is so that a bunch of processing can be done outside of teh shiny app, so that it is quite responsive to user unputs.

library(tidyverse)
library(tidytext)
library(officer)
library(pdftools)
library(stringi)

fns <- list.files(recursive = TRUE) %>%
  enframe() %>%
  rename(file_name = value)
fns_pdf <- fns %>% filter(str_detect(str_to_lower(file_name), "\\.pdf$"))
fns_docx <- fns %>% filter(str_detect(str_to_lower(file_name), "\\.docx$"))
fns_doc <- fns %>% filter(str_detect(str_to_lower(file_name), "\\.doc$"))
fns_ppt <- fns %>% filter(str_detect(str_to_lower(file_name), "\\.ppt$"))
fns_pptx <- fns %>% filter(str_detect(str_to_lower(file_name), "\\.pptx$"))

pdf_docs_df <- tibble()
for (i in 1:nrow(fns_pdf)) {
  file_name_1 <- fns_pdf$file_name[i]
  content_1 <- pdf_text(file_name_1)
  content_1_df <- content_1 %>%
    map(stri_split_lines) %>%
    flatten() %>%
    flatten_chr() %>%
    as_tibble() %>%
    rowid_to_column("doc_index") %>%
    rename(text = value) %>%
    filter(text != "") %>%
    mutate(file_name = file_name_1)
  pdf_docs_df <- pdf_docs_df %>% bind_rows(content_1_df)
}

docx_docs_df <- tibble()
for (i in 1:nrow(fns_docx)) {
  file_name_1 <- fns_docx$file_name[i]
  file_1 <- read_docx(file_name_1)
  content_1 <- docx_summary(file_1)
  paragraphs_1 <- content_1 %>%
    filter(content_type == "paragraph") %>%
    filter(text != "") %>%
    select(doc_index, text) %>%
    mutate(file_name = file_name_1)
  docx_docs_df <- docx_docs_df %>% bind_rows(paragraphs_1)
}

pptx_docs_df <- tibble()
for (i in 1:nrow(fns_pptx)) {
  file_name_1 <- fns_pptx$file_name[i]
  file_1 <- read_pptx(file_name_1)
  content_1 <- pptx_summary(file_1)
  content_1 <- content_1 %>%
    filter(content_type == "paragraph") %>%
    filter(text != "") %>%
    select(slide_id, text) %>%
    mutate(file_name = file_name_1)
  pptx_docs_df <- pptx_docs_df %>% bind_rows(content_1)
}

pptx_docs_df <- pptx_docs_df %>%
  rowid_to_column("doc_index") %>%
  select(-slide_id)

docs_df <- bind_rows(
  pdf_docs_df, pptx_docs_df, docx_docs_df
)

# short name creation
docs_df <- docs_df %>%
  mutate(
    short_name = case_when(
      str_detect(file_name, fixed("BC_final_ic_report_2020.pdf")) ~ "BC",
      str_detect(file_name, fixed("BizPal_Final_Report-eng.pdf")) ~ "BizPal",
      str_detect(file_name, fixed("2021-05-28 - DND - Evaluation of the Sustainment Initiative - Report (EN).pdf")) ~ "DND_Sustainment",
      str_detect(file_name, fixed("2021-06-23 - DND - Evaluation of Cyber Forces - Report (EN).pdf")) ~ "DND_Cyber",
      str_detect(file_name, fixed("2019-06-30 - ESDC - Apprenticeship Grants Evaluation - Report (EN).pptx")) ~ "ESDC_Apprenticeship",
      str_detect(file_name, fixed("2019-03-04 - ESDC - Evaluation of the Guaranteed income Supplement - Phase I - Report (EN).docx")) ~ "ESDC_Income",
      str_detect(file_name, fixed("2019-03-06 - ISED - Post-Secondary Institutions Strategic Investment Fund - Report (EN).pdf")) ~ "ISED_PostSecondary",
      str_detect(file_name, fixed("2020-09-10 - ISED - Evaluation of ISED’s involvement in the International Telecommunications Union - Report (EN)  (June 10 2020).pdf")) ~ "ISED_ITU",
      str_detect(file_name, fixed("2021-03-25 - JUS - Evaluation of the Justice Canada Federal Victims Strategy - Report (EN).docx")) ~ "JUS",
      str_detect(file_name, fixed("2021-01-04 - NRC - Grants to International Affiliations Program Evaluation - Report (EN).pdf")) ~ "NRC_Affiliations",
      str_detect(file_name, fixed("2021-01-04 - NRC - Design and Fabrication Services Branch - Report (EN).pdf")) ~ "NRC_Services",
      str_detect(file_name, fixed("2020-10-28 - NRC - Codes Canada Evaluation - Report (EN).pptx")) ~ "NRC_Codes",
      str_detect(file_name, fixed("2019-12-30 - NRCan - Evaluation of the Forest Sector Innovation (FSI) Programs - Report (EN).docx")) ~ "NRCan_Forest",
      str_detect(file_name, fixed("2021-02-10 - NRCan - West Coast Energy Initiative - Report (EN).docx")) ~ "NRCan_West",
      str_detect(file_name, fixed("2021-04-22 - NRCan - Energy and Climate Change Program - Report (EN).docx")) ~ "NRCan_Climate",
      str_detect(file_name, fixed("ON_Innovation-Impacts-Full-Report-EN.pdf")) ~ "ON_Innovation",
      str_detect(file_name, fixed("ON_mltsd-owrac-future-of-work-in-ontario-november-2021-en-2021-12-09.pdf")) ~ "ON_Future",
      str_detect(file_name, fixed("ON_mof-capital-markets-modernization-taskforce-final-report-en-2021-01-22-v2.pdf")) ~ "ON_Capital",
      str_detect(file_name, fixed("2021-04-01 - PHAC - Evaluation of the Community Action Program for Children (CAPC) and Canada Prenatal Nutrition Program (CPNP) - Report (EN).pdf")) ~ "PHAC_CAPC",
      str_detect(file_name, fixed("2019-03-19 - PHAC - Evaluation of The Food Safety Program (2012-13 to 2017-18) - Summary (EN).DOCX")) ~ "PHAC_Food",
      str_detect(file_name, fixed("2019-10-31 - PHAC - Evaluation of the Supporting the Health of Survivors of Family Violence Program (2015-16 to 2018-19) - Report (EN).DOCX")) ~ "PHAC_Violence",
      str_detect(file_name, fixed("2019-04-05 - PS - Evaluation of the Aboriginal Community Safety Planning Initiative - Report (EN).DOCX")) ~ "PS",
      str_detect(file_name, fixed("SK_saskatchewan_eval_strategic_research_ag dev_2013.pdf")) ~ "SK"
    )
  )

docs_df <- docs_df %>%
  mutate(doc_name = str_c(short_name, "#", doc_index)) %>%
  mutate(text = str_squish(text)) %>%
  filter(text != "", text != " ")

tidy_1 <- docs_df %>%
  select(short_name, text) %>%
  mutate(text = str_to_lower(text))
tidy_1 <- tidy_1 %>%
  unnest_tokens(gram, text, token = "ngrams", n = 1) %>%
  anti_join(stop_words, by = c("gram" = "word"))
tidy_1 <- tidy_1 %>% mutate(gram = str_replace_all(gram, "\\d", ""))
tidy_1 <- tidy_1 %>% mutate(gram = str_replace_all(gram, "[:punct:]", ""))
tidy_1 <- tidy_1 %>% mutate(gram = str_squish(gram))
tidy_1 <- tidy_1 %>% filter(gram != "")
tidy_1 <- tidy_1 %>% filter(str_length(gram) > 2)
tidy_1 <- tidy_1 %>% anti_join(stop_words, by = c("gram" = "word"))
tidy_1 <- tidy_1 %>% filter(!is.na(gram))

tidy_2 <- docs_df %>%
  select(short_name, text) %>%
  mutate(text = str_to_lower(text))
tidy_2 <- tidy_2 %>%
  unnest_tokens(gram, text, token = "ngrams", n = 2) %>%
  anti_join(stop_words, by = c("gram" = "word"))
tidy_2 <- tidy_2 %>% filter(!is.na(gram))
tidy_2 <- tidy_2 %>% mutate(gram = str_replace_all(gram, "\\d", ""))
tidy_2 <- tidy_2 %>% mutate(gram = str_replace_all(gram, "[:punct:]", ""))
tidy_2 <- tidy_2 %>% mutate(gram = str_squish(gram))
tidy_2 <- tidy_2 %>% filter(gram != "")

tidy_2 <- tidy_2 %>%
  separate(gram, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  mutate(
    word1 = str_squish(word1),
    word2 = str_squish(word2)
  ) %>%
  filter(str_length(word1) > 2, str_length(word2) > 2) %>%
  select(-word1, -word2)

tidy_3 <- docs_df %>%
  select(short_name, text) %>%
  mutate(text = str_to_lower(text))
tidy_3 <- tidy_3 %>%
  unnest_tokens(gram, text, token = "ngrams", n = 3) %>%
  anti_join(stop_words, by = c("gram" = "word"))
tidy_3 <- tidy_3 %>% filter(!is.na(gram))
tidy_3 <- tidy_3 %>% mutate(gram = str_replace_all(gram, "\\d", ""))
tidy_3 <- tidy_3 %>% mutate(gram = str_replace_all(gram, "[:punct:]", ""))
tidy_3 <- tidy_3 %>% mutate(gram = str_squish(gram))
tidy_3 <- tidy_3 %>% filter(gram != "")

tidy_3 <- tidy_3 %>%
  separate(gram, c("word1", "word2", "word3"), sep = " ", remove = FALSE) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  mutate(
    word1 = str_squish(word1),
    word2 = str_squish(word2),
    word3 = str_squish(word3)
  ) %>%
  filter(str_length(word1) > 2, str_length(word2) > 2, str_length(word3) > 2) %>%
  select(-word1, -word2, -word3)

df_grams <- bind_rows(
  tidy_1 %>% mutate(ng = 1) %>% rename(most_common_token = gram),
  tidy_2 %>% mutate(ng = 2) %>% rename(most_common_token = gram),
  tidy_3 %>% mutate(ng = 3) %>% rename(most_common_token = gram)
)

df_grams %>% write_csv("tidydocs.csv")
