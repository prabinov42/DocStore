library(tidyverse)
library(lubridate)
library(knitr)
library(ggthemes)
library(patchwork)
library(scales)
library(ggplot2)
library(shiny)
library(tidytext)

df_grams <- read_csv("tidydocs.csv")

df <- df_grams %>%
  filter(ng == 1) %>%
  select(-ng)
df2 <- df_grams %>%
  filter(ng == 2) %>%
  select(-ng)
df3 <- df_grams %>%
  filter(ng == 3) %>%
  select(-ng)

tidy_tf_idf <- df %>%
  count(short_name, most_common_token, sort = TRUE) %>%
  bind_tf_idf(most_common_token, short_name, n) %>%
  arrange(desc(tf_idf))

tidy_tf_idf2 <- df2 %>%
  count(short_name, most_common_token, sort = TRUE) %>%
  bind_tf_idf(most_common_token, short_name, n) %>%
  arrange(desc(tf_idf))

tidy_tf_idf3 <- df3 %>%
  count(short_name, most_common_token, sort = TRUE) %>%
  bind_tf_idf(most_common_token, short_name, n) %>%
  arrange(desc(tf_idf))

short_names <- df %>%
  pull(short_name) %>%
  unique() %>%
  sort()

# short name creation
docs_df <- tribble(
  ~file_name, ~document,
  "BC_final_ic_report_2020.pdf", "BC",
  "BizPal_Final_Report-eng.pdf", "BizPal",
  "2021-05-28 - DND - Evaluation of the Sustainment Initiative - Report (EN).pdf", "DND_Sustainment",
  "2021-06-23 - DND - Evaluation of Cyber Forces - Report (EN).pdf", "DND_Cyber",
  "2019-06-30 - ESDC - Apprenticeship Grants Evaluation - Report (EN).pptx", "ESDC_Apprenticeship",
  "2019-03-04 - ESDC - Evaluation of the Guaranteed income Supplement - Phase I - Report (EN).docx", "ESDC_Income",
  "2019-03-06 - ISED - Post-Secondary Institutions Strategic Investment Fund - Report (EN).pdf", "ISED_PostSecondary",
  "2020-09-10 - ISED - Evaluation of ISED’s involvement in the International Telecommunications Union - Report (EN)  (June 10 2020).pdf", "ISED_ITU",
  "2021-03-25 - JUS - Evaluation of the Justice Canada Federal Victims Strategy - Report (EN).docx", "JUS",
  "2021-01-04 - NRC - Grants to International Affiliations Program Evaluation - Report (EN).pdf", "NRC_Affiliations",
  "2021-01-04 - NRC - Design and Fabrication Services Branch - Report (EN).pdf", "NRC_Services",
  "2020-10-28 - NRC - Codes Canada Evaluation - Report (EN).pptx", "NRC_Codes",
  "2019-12-30 - NRCan - Evaluation of the Forest Sector Innovation (FSI) Programs - Report (EN).docx", "NRCan_Forest",
  "2021-02-10 - NRCan - West Coast Energy Initiative - Report (EN).docx", "NRCan_West",
  "2021-04-22 - NRCan - Energy and Climate Change Program - Report (EN).docx", "NRCan_Climate",
  "ON_Innovation-Impacts-Full-Report-EN.pdf", "ON_Innovation",
  "ON_mltsd-owrac-future-of-work-in-ontario-november-2021-en-2021-12-09.pdf", "ON_Future",
  "ON_mof-capital-markets-modernization-taskforce-final-report-en-2021-01-22-v2.pdf", "ON_Capital",
  "2021-04-01 - PHAC - Evaluation of the Community Action Program for Children (CAPC) and Canada Prenatal Nutrition Program (CPNP) - Report (EN).pdf", "PHAC_CAPC",
  "2019-03-19 - PHAC - Evaluation of The Food Safety Program (2012-13 to 2017-18) - Summary (EN).DOCX", "PHAC_Food",
  "2019-10-31 - PHAC - Evaluation of the Supporting the Health of Survivors of Family Violence Program (2015-16 to 2018-19) - Report (EN).DOCX", "PHAC_Violence",
  "2019-04-05 - PS - Evaluation of the Aboriginal Community Safety Planning Initiative - Report (EN).DOCX", "PS",
  "SK_saskatchewan_eval_strategic_research_ag dev_2013.pdf", "SK"
)

plotAggregateWordFrequency <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  p1 <- df %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    count(most_common_token, sort = TRUE) %>%
    ggplot(aes(x = n)) +
    geom_histogram(bins = 100) +
    theme_minimal() +
    labs(
      y = "Count",
      x = "Word Frequency",
      title = paste0("Document: ", dn)
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma)
  p2 <- p1 +
    scale_y_continuous(trans = "log10", labels = scales::comma) +
    scale_x_continuous(trans = "log10", labels = scales::comma)
  pl <- p1 / p2
  return(pl)
}

plotAggregateSentiment <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  sentiment_df <- df %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    rename(word = most_common_token) %>%
    inner_join(get_sentiments("bing")) %>%
    count(doc_name = short_name, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment = positive - negative)
  word_counts_df <- df %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    rename(word = most_common_token) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, short_name, sentiment, sort = TRUE) %>%
    ungroup()
  sentiment_colour <- c("red", "darkgreen")
  names(sentiment_colour) <- c("negative", "positive")
  wrds <- 10
  pl <- word_counts_df %>%
    group_by(word, sentiment) %>%
    summarize(n = sum(n)) %>%
    group_by(sentiment) %>%
    slice_max(n, n = wrds) %>%
    ungroup() %>%
    ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = sentiment_colour) +
    theme_minimal() +
    coord_flip() +
    labs(y = "Contribution to sentiment", x = NULL, title = paste0(dn, " Sentiment"), fill = "Sentiment") +
    scale_y_continuous(label = comma)
  return(pl)
}

tblAggregateCommonWords <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  ans <- df %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    count(most_common_token, sort = TRUE) %>%
    head(100) %>%
    rename(word = most_common_token, count = n)
  return(ans)
}

tblAggregateCommonWords2 <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  ans <- df2 %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    count(most_common_token, sort = TRUE) %>%
    head(100) %>%
    rename(word = most_common_token, count = n)
  return(ans)
}

tblAggregateCommonWords3 <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  ans <- df3 %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    count(most_common_token, sort = TRUE) %>%
    head(100) %>%
    rename(word = most_common_token, count = n)
  return(ans)
}

plotTfidf <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  wrds <- 25
  pl <- tidy_tf_idf %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    arrange(desc(tf_idf)) %>%
    head(wrds) %>%
    ggplot(aes(x = reorder(most_common_token, tf_idf), y = tf_idf)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip() +
    labs(y = "tf-idf", x = NULL, title = paste0("Document: ", dn)) +
    scale_y_continuous(label = comma)
  return(pl)
}

plotTfidf2 <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  wrds <- 25
  pl <- tidy_tf_idf2 %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    arrange(desc(tf_idf)) %>%
    head(wrds) %>%
    ggplot(aes(x = reorder(most_common_token, tf_idf), y = tf_idf)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip() +
    labs(y = "tf-idf", x = NULL, title = paste0("Document: ", dn)) +
    scale_y_continuous(label = comma)
  return(pl)
}

plotTfidf3 <- function(fvals, side) {
  dn <- if_else(side == "L", fvals$docL, fvals$docR)
  wrds <- 25
  pl <- tidy_tf_idf3 %>%
    {
      if (dn != "ALL") filter(., short_name == dn) else .
    } %>%
    arrange(desc(tf_idf)) %>%
    head(wrds) %>%
    ggplot(aes(x = reorder(most_common_token, tf_idf), y = tf_idf)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip() +
    labs(y = "tf-idf", x = NULL, title = paste0("Document: ", dn)) +
    scale_y_continuous(label = comma)
  return(pl)
}

tblDocNames <- function(fvals) {
  ans <- docs_df %>%
    arrange(document) %>%
    relocate(document, file_name)
  return(ans)
}

uii <- fluidPage(
  titlePanel("DocStore"),
  fluidRow(column(6, "", selectInput("docL", "Document:", c("ALL", short_names))), column(6, "", selectInput("docR", "Document:", c("ALL", short_names)))),
  tabsetPanel(
    type = "tabs",
    tabPanel("1 grams", tabsetPanel(
      type = "tabs",
      tabPanel("Common Words", fluidRow(column(6, "", dataTableOutput("aggregateCommonWordsL")), column(6, "", dataTableOutput("aggregateCommonWordsR")))),
      tabPanel("Sentiment", fluidRow(column(6, "", plotOutput("aggregateSentimentL")), column(6, "", plotOutput("aggregateSentimentR")))),
      tabPanel("TF IDF", fluidRow(column(6, "", plotOutput("tfidfL")), column(6, "", plotOutput("tfidfR"))))
    )),
    tabPanel("2 grams", tabsetPanel(
      type = "tabs",
      tabPanel("Common Words", fluidRow(column(6, "", dataTableOutput("aggregateCommonWordsL2")), column(6, "", dataTableOutput("aggregateCommonWordsR2")))),
      tabPanel("TF IDF", fluidRow(column(6, "", plotOutput("tfidfL2")), column(6, "", plotOutput("tfidfR2"))))
    )),
    tabPanel("3 grams", tabsetPanel(
      type = "tabs",
      tabPanel("Common Words", fluidRow(column(6, "", dataTableOutput("aggregateCommonWordsL3")), column(6, "", dataTableOutput("aggregateCommonWordsR3")))),
      tabPanel("TF IDF", fluidRow(column(6, "", plotOutput("tfidfL3")), column(6, "", plotOutput("tfidfR3"))))
    )),
    tabPanel(
      "Info",
      tabPanel("Document - File Names", fluidRow(column(12, "", dataTableOutput("docNames"))))
    )
  )
)

serveri <- function(input, output) {
  fvals <- reactive({
    docL <- input$docL
    docR <- input$docR
    return(list(docL = docL, docR = docR))
  })
  output$aggregateWordFrequencyL <- renderPlot({
    plotAggregateWordFrequency(fvals(), "L")
  })
  output$aggregateWordFrequencyR <- renderPlot({
    plotAggregateWordFrequency(fvals(), "R")
  })
  output$aggregateSentimentL <- renderPlot({
    plotAggregateSentiment(fvals(), "L")
  })
  output$aggregateSentimentR <- renderPlot({
    plotAggregateSentiment(fvals(), "R")
  })
  output$tfidfL <- renderPlot({
    plotTfidf(fvals(), "L")
  })
  output$tfidfR <- renderPlot({
    plotTfidf(fvals(), "R")
  })
  output$aggregateCommonWordsL <- renderDataTable(
    {
      tblAggregateCommonWords(fvals(), "L")
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$aggregateCommonWordsR <- renderDataTable(
    {
      tblAggregateCommonWords(fvals(), "R")
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$docNames <- renderDataTable(
    {
      tblDocNames(fvals())
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$aggregateCommonWordsL2 <- renderDataTable(
    {
      tblAggregateCommonWords2(fvals(), "L")
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$aggregateCommonWordsR2 <- renderDataTable(
    {
      tblAggregateCommonWords2(fvals(), "R")
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$tfidfL2 <- renderPlot({
    plotTfidf2(fvals(), "L")
  })
  output$tfidfR2 <- renderPlot({
    plotTfidf2(fvals(), "R")
  })
  output$aggregateCommonWordsL3 <- renderDataTable(
    {
      tblAggregateCommonWords3(fvals(), "L")
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$aggregateCommonWordsR3 <- renderDataTable(
    {
      tblAggregateCommonWords3(fvals(), "R")
    },
    options = list(pageLength = 10, searching = FALSE)
  )
  output$tfidfL3 <- renderPlot({
    plotTfidf3(fvals(), "L")
  })
  output$tfidfR3 <- renderPlot({
    plotTfidf3(fvals(), "R")
  })
}

shinyApp(ui = uii, server = serveri)
