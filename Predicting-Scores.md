Document Text Processing: Predicting Scores
================
Peter Rabinovitch
2023-04-07 09:55:29

``` r
library(tidyverse)
library(tidytext)
library(SnowballC)
library(glmnet)
library(knitr)

set.seed(2023)

g_fignum <- 0
figcap <- function() {
  g_fignum <<- g_fignum + 1
  return(str_c("Figure ", g_fignum))
}

g_tabnum <- 0
tabcap <- function() {
  g_tabnum <<- g_tabnum + 1
  return(str_c("Table ", g_tabnum))
}
```

# Intro

In this document we explore the potential of predicting documents cores
from text summarizations of the documents.

In this example, only two documents were summarized and had their goals
scored, as the scores and summarizations were human generated.
Furthermore, only one of the documents had its goals was scored. Hence,
the approach described below is an illustration of what may be a
feasible approach once more documents are summarized and scored.

One could also easily imagine using a system like ChatGPT to summarize
the documents, saving human labour as well as ensuring a consistent
approach to summarization.

# Data

Data here was manually copied from two spreadsheets, one per project.
The spreadsheets were fairly consistent in their formatting, but to ease
future work the following format is recommended:

- type, selected from: ‘immediate’, ‘intermediate’, ‘long term’,
  ‘ultimate’
- outcome: a short text describing the desired outcome
- achievement, selected from: ‘achieved’, ‘partly achieved’, ‘too early
  to evaluate’
- evidence: text string describing evidence
- score: 0-5, 5 is better

<details>
<summary>
Click to See the Data
</summary>

``` r
# "Report 4 NRCan West Coast Initiative"
df_1 <- tribble(
  ~type, ~outcome, ~achievement, ~evidence, ~score,
  "immediate", "Positive relationships are developed with Indigenous communities", "Partly achieved", "IPO-West has contributed to building trust and good relationships with Indigenous stakeholders, even with communities that may be strongly opposed to energy development projects. Evidence indicates that positive relationships have been developed through SPI-WCE and SPF-WCE.", 4,
  "immediate", "Indigenous communication of, and federal response to, non-regulatory issues of concern to communities that could be impacted by energy development", "Partly achieved", "The engagement activities undertaken by IPO-West allowed the office to identify a wide range of needs, issues and priorities expressed by Indigenous communities. According to SPI-WCE evaluation findings, progress toward this outcome shows room for improvement", 4,
  "immediate", "Improved understanding across government of Indigenous issues, needs and concerns related to energy development", "Achieved", "IPO-West activities have increased government stakeholders’ understanding of Indigenous concerns and issues", 5,
  "immediate", "Flexible and innovative interdepartmental engagement, collaboration and partnerships with Indigenous communities", "Achieved", "Efforts were successfully deployed by the IPO-West Office to engage with other involved departments and directly with Indigenous communities, in respectful and innovative ways (e.g., co-development of terms of reference for the IAMCs).", 5,
  "immediate", "Federal investments and actions are coordinated with Indigenous, provincial and other partners", "Achieved", "IPO-West contributed to a coordinated approach to project funding with the BC provincial government. SPI-WCE contributed to improved partnerships with other stakeholders.", 5,
  "immediate", "Support and facilitation of Indigenous participation and leadership on addressing environmental concerns", "Partly achieved", "CEMI pilot projects did not go ahead. SPI-WCE and DFO projects supporting research and monitoring projects were successful.", 4,
  "immediate", "Regional CEMI pilot projects that bring together multiple First Nations and scientific expertise", "Partly achieved", "While the engagement phase for CEMI was successful, the pilot projects did not proceed.", 3,
  "intermediate", "Contribute to a renewed federal relationship with Indigenous communities", "Partly achieved", "IPO-West has conducted engagement activities with most Indigenous communities through direct contact with stakeholders", 4,
  "intermediate", "Identified issues are effectively addressed in collaboration with Indigenous communities and other partners, supporting (for example):- Environment and habitat, - Jobs, training, business and economic development, - Community capacity", "Partly achieved", "There is evidence that IPO-West activities have increased government stakeholders’ understanding of Indigenous concerns and issues. Some communities expressed concerns in the area of environment, including risks and how they are managed/researched, and these concerns were only partly addressed.", 4,
  "intermediate", "Whole-of-government response to issues identified by Indigenous communities", "Partly achieved", "Cooperation and collaboration among federal departments around West Coast energy issues has increased. Potential improvements remain to ensure a whole-of-government and single-window approach.", 4,
  "intermediate", "Increased involvement of Indigenous communities in assessing and addressing potential environmental impacts of energy infrastructure development", "Partly achieved", "CEMI pilot projects did not go ahead. SPI-WCE and DFO projects supporting research and monitoring projects were successful.", 4,
  "intermediate", "Greater certainty for Indigenous communities on how cumulative effects are being tracked", "Partly achieved", "CEMI pilot projects did not go ahead. SPI-WCE and DFO projects supporting research and monitoring projects were successful.", 4,
  "ultimate", "Indigenous communities have improved capacity to make informed decisions on West Coast energy infrastructure", "Partly achieved", "There is improved capacity of the communities to make informed decisions as a result of various engagement and funded activities, including workshops and projects that increased the community’s knowledge and awareness of various issues and opportunities.", 4,
  "ultimate", "Increased Indigenous participation in energy infrastructure projects in BC", "Too early to evaluate", "Delays in the energy projects have generally delayed the developmental activities to help communities directly participate in the energy projects. Projects supported by SPF-WCE, while ongoing, have shown early positive results in terms of skills development and employment.", NA
) %>% mutate(doc_name = "Report 4 NRCan West Coast Initiative") %>% rowid_to_column("goal_id")


# "report 1 ised at itu"
df_2 <- tribble(
  ~type, ~outcome, ~evidence, ~score,
  "immediate", "Consistency of ITU’s strategic and financial plans with Canadian priorities and positions", "In the 2017 and 2018 Council meetings, performance tracking data shows that 90-100% of Canada’s high priority positions were adopted,", 4.5,
  "immediate", "Consistency of ITU Development action plans with Canadian positions", "Of the proposals submitted, 28 out of 29 (96%) were either adopted in full or reflected in modifications to existing resolutions – meeting the target of 80-100%.", 4.5,
  "immediate", "Consistency of ITU telecommunications/ICT resolutions and voluntary standards with Canadian positions", "ISED was able to advance 100% of Canada’s high priority positions at the 2016 Assembly – meeting the target of 80-100%.", 5,
  "immediate", "Consistency of ITU’s focus and mandate with Canadian positions", "Between September 2016 and March 2019, ISED attended five meetings pertaining to the Plenipotentiary (1), Council (2), World Telecommunication Development Conference (1) and World Telecommunication Standardization Assembly (1). All five meetings advanced 90-100% of Canada’s positions – meeting the target of 80-100%.", 4.5,
  "immediate", "Consistency of ITU’s allocation of radio spectrum and adoption of binding radio regulations with Canadian positions and proposals", "At the early 2019 Conference Preparatory Meeting, ISED advanced 90% of Canada’s high priority positions – meeting the target of 80-100%.", 4.5,
  "immediate", "Consistency of ITU radiocommunication resolutions, recommendations and reports with Canadian positions", "No performance metrics available. There were 1,228 registered Canadian frequency assignments recorded in the Master Register by the ITU versus 3,970 returned Canadian frequency assignments to ISED for non-compliance with the Radio Regulations or for incomplete coordination ", NA,
  "intermediate", "An open private sector-led Internet governed by the multi-stakeholder model is reinforced", "Narrative, no real data", NA,
  "intermediate", "Canada is protecting and maintaining its secure network infrastructure and ICT services and applications", "Narrative, no real data", NA,
  "intermediate", "Canadian companies exploit/develop new services and technologies", "Narrative, no real data", NA,
  "intermediate", "Global connectivity and interoperability of telecommunications networks and services", "Narrative, no real data", NA,
  "intermediate", "Interference between radio services is managed", "Narrative, no real data", NA,
  "long term", "Canadian consumers and businesses benefit from innovative products, services, and information technology and telecommunications infrastructure", "Narrative, no real data", NA,
  "long term", "Increased international market access for Canadian telecommunications businesses    ISED’s involvement in the ITU is only one contributing factor to increased international market access.", "Narrative, no real data", NA,
  "long term", "Canadian government and industry offer radio and satellite services to users", "Narrative, no real data", NA,
) %>% mutate(doc_name = "report 1 ised at itu", score = NA, achievement=NA)%>% rowid_to_column("goal_id")



# "Report 11 Family Violence Prevention Partnership"
df_3 <- tribble(
  ~type, ~outcome, ~evidence, ~score,
'immediate','Men, women and children in need or at-risk have access to and use FVPP prevention and protection supports and services',   "Interviews and site visits confirm that FVPP-funded shelters assist women and children fleeing family violence and when space is available, serve as emergency housing for these same women and others from on-reserve communities. However, and depending on the region/province, shelter or in some cases emergency housing can be provided for Indigenous women outside of the reserve from various locations and distances. For example, some women travel several hours to access shelters as there is no shelter in their community",3,
'immediate','Existing prevention projects’ effectiveness is maximized',"Shelter staff are limited in their capacity to deliver prevention programming in the community as they are fully engaged in protection activities, including shelter client support.despite a demonstrated need for prevention activities, the successful design and delivery of prevention projects are hindered",2,       
'immediate','Existing levels of shelter capacity is sufficient to meet FVPP objectives',"Data on shelter use is inconsistent and manually collected across the INAC – funded shelter network. This creates major limitations for programmers in terms of making strategic decisions regarding shelter use and capacity and in responding to calls for increased transitional housing",3,        
'immediate','The effectiveness of partnerships and coordination is maximized',"An innovation[1] occurring with support from INAC (Ontario) is enabling knowledge transfer among southern Ontario shelters and more remote shelters in northern Ontario (Section 5.3.2). Akwesasne First Nation is paired with Mishkeegogamang Safe House[2] to share knowledge and training modules for delivery in the more remote northern community",3       
) %>% mutate(doc_name = "Report 11 Family Violence Prevention Partnership", achievement=NA)%>% rowid_to_column("goal_id")


# "Report 10 public safety canada aboriginal community safety planning initiativep"
df_4 <- tribble(
  ~type, ~outcome, ~evidence, ~score,
  'immediate','increased awareness of grants',  "Recipients indicated that almost all did not know about the Apprenticeship Grants until they became apprentices. Therefore, the grants have a limited impact on their decision to enroll in an apprenticeship",    3,
  'intermediate','increased participation in apprenticeship programs’ effectiveness is maximized',"The take-up rate for the Apprenticeship Incentive Grant decreased by 9.1 percentage points when comparing 2010 to 2018; The take-up rate for the Apprenticeship Completion Grant increased by 2.5 percentage points when comparing 2011 to 2018",    3,
  'intermediate','increased progression through each level, including to completion',"72.5% of grant recipients agree that the Apprenticeship Grants encouraged them to complete the apprenticeship",   4
) %>% mutate(doc_name = "Report 10 public safety canada aboriginal community safety planning initiative", achievement=NA)%>% rowid_to_column("goal_id")


    

# "Report 9 Sask strategic research program and ad dev fund"
df_5 <- tribble(
  ~type, ~outcome, ~evidence, ~score,
'immediate','new crops and/or cultivars meeting market demands and consumer preferences',"respondents mentioned the crops area most frequently, including plant breeding for new",4,
  'immediate','Increased livestock competitiveness',"varieties that produce improved yields and and can serve new markets (especially pulses). They also",  3,
'immediate','Optimized livestock feeding systems',"mentioned agronomic production improvements, especially with ADF’s early involvement in direct", 3,
  'immediate','New and innovative food, bioproducts, and processing technologies',"seeding and no/low-till methods that are seen to have transformed agicultural practices. New livestock", 4,
'immediate','Improved food quality',"feeding systems and technologies that have helped increased production and reduce feed costs were",    3,
  'immediate','Innovative and sustainable farming systems and practices',"also mentioned, but less frequently. It was also noted that R&D into biofuels started in the province",   4,
'immediate','Enhanced adaptive capacity of the Provincial soil and ecosystem resources',"and are spreading more widely into the economy. On the engineering side, good work was mentioned", 3,
  'immediate','Utilization of biotechnology to enhance agriculture and value-added production',"in areas such as improved agricultural machines (including those involved in low/no till), improved",   3,
'immediate','Decreased agricultural production risks',"safety, increased efficiency monitoring, and better production and processing control proceses and technologies",    3,
'intermediate','Strategic Research Program funds chairs in group genetics, livestock development, food and bio products, soils and environments', NA,NA,        
'intermediate','Agricultural Development Fund ensures relevant research delivered to help farmers and ranchers be successful. (ADF)', NA,NA,        
'ultimte', 'Positive economic impact',"Stakeholders believed programs are of great use.Benefit Cost at 16.6",   4
) %>% mutate(doc_name = "Report 9 Sask strategic research program and ad dev fund", achievement=NA)%>% rowid_to_column("goal_id")


# "Report 7 DND Evaluation of the Sustainment Initiative"
df_6 <- tribble(
  ~type, ~outcome, ~evidence, ~score,
'immediate','Development of informed, tailored sustainment solutions that optimizeperformance, value for money, flexibility and economic benefits',"Most survey respondents indicated that the SI/SBCA process contributes somewhat or quite a bit to informed sustainment solutions",4,
'intermediate','Increased competencies to develop tailored sustainment solutions, Improved collaboration and coordination among stakeholders;Increased recognition of the value of the SBCA among stakeholders', "Most supervisors indicated that DND SBCA practitioners are adequately and consistentlyapplying the SBCA process in developing sustainment solutions",4,
) %>% mutate(doc_name = "Report 7 DND Evaluation of the Sustainment Initiative", achievement=NA)%>% rowid_to_column("goal_id")





df <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6)
rm(df_1, df_2, df_3, df_4, df_5, df_6)
```

</details>

Note there are two sets of features we can use in this data: - the
outcome: a short text string describing the results of the goal - the
evidence: a longer text string describing the evidence of the outcome

Suffixes *e* and *o* are used throughout to differentiate the data
frames and results.

Below we try both approaches, as at this point with so little data it is
not possible to determine a preferred approach.\]

For each approach, we convert all words to lower case, and remove all
punctuation and numbers. We remove stop-words, and then we stem each
word. As the stem can be difficult to interpret, we determine the most
commonly used word in the corpus with the same stem, and use that *most
common representative*, or *mcr*, throughout.

``` r
get_df_w_stems<-function(df, colName){
df_w_stems <- df %>%
  mutate(iword = {{colName}}) %>%
  mutate(
    iword = str_to_lower(iword),
    iword = str_replace_all(iword, "\\d", " "),
    iword = str_replace_all(iword, "[:punct:]", " ")
  ) %>%
  unnest_tokens(word, iword) %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  mutate( stem = wordStem( word, language="en")) 

df_stems <- df_w_stems %>%
  count(stem, word) %>%
  arrange(stem, desc(n)) %>%
  group_by(stem) %>%
  summarize( word=first(word))

df_w_stems <- df_w_stems %>% left_join(df_stems, join_by(stem==stem)) %>%
  rename(original_word=word.x, mcr=word.y)

return(df_w_stems)
}


dfs_o <- get_df_w_stems(df, outcome)
dfs_e <- get_df_w_stems(df, evidence)
```

# Models

We use the [lasso](https://en.wikipedia.org/wiki/Lasso_(statistics)) in
order to predict the goal score from the words used in the text.

``` r
get_lasso_coefs <- function(df) {
  x <- model.matrix(score ~ mcr, df)
  y <- df$score
  train <- sample(1:nrow(x), nrow(x) * 0.8)
  test <- (-train)
  y.test <- y[test]
  grid <- 10^seq(10, -2, length = 100)
  lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
  cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
  bestlam <- cv.out$lambda.1se
  lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
  out <- glmnet(x, y, alpha = 1, lambda = grid)
  lasso.coef <- predict(out, type = "coefficients", s = bestlam)
  coefsdf <- as.data.frame(summary(lasso.coef))
  coefsdf$var_name <- NA
  cn <- dimnames(lasso.coef)[[1]]
  for (i in 1:nrow(coefsdf)) {
    r <- coefsdf[i, ]
    coefsdf$var_name[i] <- cn[r[1, 1]]
  }
  coefsdf <- coefsdf %>%
    mutate(var_name = if_else(var_name == "(Intercept)", "Intercept", str_sub(var_name, 4))) %>%
    select(var_name, estimate = x)
  return(coefsdf)
}
```

## Outcomes

``` r
dfs_o_m <- dfs_o %>% filter(!is.na(score))
get_lasso_coefs(dfs_o_m) %>% kable()
```

| var_name  |   estimate |
|:----------|-----------:|
| Intercept |  3.7702717 |
| maximized | -0.0271354 |

The results show the coefficients of the best lasso model, similar to a
regression. We see that the intercept is about 3.77 (typical score) and
the coefficient for the words *maximized* contribute to the goal’s score
by the estimate shown. In other words, a goal that mentioned the word
*maximized* in the outcome can be expected to have a score of about
3.7702717-0.0271354=3.743136 if none of the other words shown in the
table are mentioned in the outcome. And no other words affect the score.

Note however that only one word affects the score - this is almost
certainly a function of the small amount of data used to build the
model.

## Evidence

``` r
dfs_e_m <- dfs_e %>% filter(!is.na(score))
get_lasso_coefs(dfs_e_m) %>% kable()
```

| var_name   |   estimate |
|:-----------|-----------:|
| Intercept  |  3.6105177 |
| prevention | -0.0474512 |
| shelter    | -0.0087223 |

We see similar results as with outcome, but here only two words affect
the score. If the evidence contains *prevention* the score can be
expected to decrease by 0.0474512, and if the evidence contains
*shelter* the score can be expected to decrease by 0.0087223.

Again, the small number of words listed is likely an artificial of the
small sample size.

# Conclusion

The lasso approach is well known and frequent works very well. Once more
documents are summarized and scored, this code should be adapted and run
to see how well the models predict the scores in order to assess it’s
usefulness.

Also to be noted is that the supplied summaries are somewhat
inconsistent in their format and style - again suggesting that an
automated system for creating the summaries, like ChatGPT may prove very
useful.

# Appendices

<details>
<summary>
References
</summary>

[Tidy Text Mining in R](https://www.tidytextmining.com)

</details>
<details>
<summary>
SessionInfo
</summary>

``` r
sessionInfo()
```

    ## R version 4.2.2 (2022-10-31)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur ... 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.41      glmnet_4.1-6    Matrix_1.5-3    SnowballC_0.7.0
    ##  [5] tidytext_0.4.1  lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0  
    ##  [9] dplyr_1.1.0     purrr_1.0.1     readr_2.1.4     tidyr_1.3.0    
    ## [13] tibble_3.2.0    ggplot2_3.4.1   tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] shape_1.4.6       tidyselect_1.2.0  xfun_0.36         splines_4.2.2    
    ##  [5] lattice_0.20-45   colorspace_2.1-0  vctrs_0.5.2       generics_0.1.3   
    ##  [9] htmltools_0.5.4   yaml_2.3.6        utf8_1.2.3        survival_3.5-3   
    ## [13] rlang_1.1.0       pillar_1.8.1      glue_1.6.2        withr_2.5.0      
    ## [17] foreach_1.5.2     lifecycle_1.0.3   munsell_0.5.0     gtable_0.3.2     
    ## [21] codetools_0.2-18  evaluate_0.19     tzdb_0.3.0        fastmap_1.1.0    
    ## [25] fansi_1.0.4       highr_0.10        tokenizers_0.3.0  Rcpp_1.0.10      
    ## [29] scales_1.2.1      hms_1.1.2         digest_0.6.31     stringi_1.7.12   
    ## [33] grid_4.2.2        cli_3.6.0         tools_4.2.2       magrittr_2.0.3   
    ## [37] janeaustenr_1.0.0 pkgconfig_2.0.3   ellipsis_0.3.2    timechange_0.1.1 
    ## [41] rmarkdown_2.19    rstudioapi_0.14   iterators_1.0.14  R6_2.5.1         
    ## [45] compiler_4.2.2

</details>
