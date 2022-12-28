##----Reading-Packages----
options(future.rng.onMisuse = "ignore")

library(future)
plan(strategy = c(multiprocess, multisession)[2])

library(doParallel)
library(foreach)

Clusters <- makeCluster(detectCores() - 1)
registerDoParallel(Clusters)
options(Ncpus = detectCores() - 1, mc.cores = detectCores() - 1, scipen = 999)

Packages <- 
    c(
        "cricketdata"
      , "fastverse"
      , "furrr"
      , "knitr"
      , "kableExtra"
      , "pdftools"
      , "scales"
      , "stringi"
      , "tidytable"
      , "tidyverse"
    )

Packages[!(Packages %in% installed.packages()[ ,"Package"])]

if (!require("pacman")) install.packages("pacman")
pacman::p_load(Packages, character.only = TRUE)


##----Reading-Functions----

##----Reading-Themes----

##----Reading-Data----
BATBA <-
  fetch_player_data(
      playerid  = find_player_id(searchstring = "Babar Azam")$ID
    , matchtype = c("test", "odi", "t20")[1]
    , activity  = c("batting", "bowling", "fielding")[1]
    ) %>%
  as.data.table()
# save(BATBA, file = "./data/BATBA.RData", compress = "xz")
# load("./data/BATBA.RData")

BATBA %>% 
  head()

##----Analysis----
BATBAOut1 <- 
  BATBA %>% 
  fsummarise(
    Mat      = ceiling(fnobs(Innings)/2)
  , Inns     = fnobs(Innings)
  , NO       = fsum(stri_detect(str = Dismissal, regex = "not out|notout"))
  , Runs     = fsum(Runs)
  , HS       = fmax(Runs)
  , Ave      = fsum(Runs)/(fnobs(Runs) - fsum(stri_detect(str = Dismissal, regex = "not out|notout")))
  , BF       = fsum(parse_number(BF))
  , SR       = 100*fsum(Runs)/fsum(parse_number(BF))
  , Hundreds = fsum(Runs >= 100)
  , Fifties  = fsum(Runs < 100 & Runs >= 50) 
  , Fours    = fsum(parse_number(X4s))
  , Sixes    = fsum(parse_number(X6s))
    )

BATBAOut1

BATBAOut2 <- 
  BATBA %>%
  fmutate(Year = as_factor(year(Date))) %>% 
   fgroup_by(Year) %>% 
   fsummarise(
      Mat      = ceiling(fnobs(Innings)/2)
    , Inns     = fnobs(Innings)
    , NO       = fsum(stri_detect(str = Dismissal, regex = "not out|notout"))
    , Runs     = fsum(Runs)
    , HS       = fmax(Runs)
    , Ave      = fsum(Runs)/(fnobs(Runs) - fsum(stri_detect(str = Dismissal, regex = "not out|notout")))
    , BF       = fsum(parse_number(BF))
    , SR       = 100*fsum(Runs)/fsum(parse_number(BF))
    , Hundreds = fsum(Runs >= 100)
    , Fifties  = fsum(Runs < 100 & Runs >= 50) 
    , Fours    = fsum(parse_number(X4s))
    , Sixes    = fsum(parse_number(X6s))
      )

BATBAOut2

BATBAOut2Plot1 <- 
  ggplot(data = BATBAOut2, mapping = aes(x = Year, y = Ave, fill = Year)) +
  geom_col(width = 0.5) +
  #geom_text(mapping = aes(label = paste0(round(Ave), " (", comma(Runs), " in ", Mat, " matches ", ")")), fontface = "bold", size = 3, vjust = -1.2) +
  geom_text(mapping = aes(label = paste0("Average: ", round(Ave))), fontface = "bold", size = 3, vjust = -3.5) +
  geom_text(mapping = aes(label = paste0("Runs   : ", comma(Runs))), fontface = "bold", size = 3, vjust = -2.0) +
  geom_text(mapping = aes(label = paste0("Matches: ", Mat)), fontface = "bold", size = 3, vjust = -0.5) +
  scale_x_discrete(breaks = breaks_pretty(8)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma, breaks = breaks_pretty(8), limits = c(0, 90)) +
  labs(
    x       = "Year"
  , y       = "Average"
  , title   = "Test Batting Performance of Babar Azam over the years"
  , caption = "https://myaseen208.com/"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  , plot.title      = element_text(hjust = 0.5)
    )
BATBAOut2Plot1


BATBAOut3 <- 
  BATBA %>%
  fmutate(Year = as_factor(year(Date))) %>% 
   fgroup_by(Opposition) %>% 
   fsummarise(
      Mat      = ceiling(fnobs(Innings)/2)
    , Inns     = fnobs(Innings)
    , NO       = fsum(stri_detect(str = Dismissal, regex = "not out|notout"))
    , Runs     = fsum(Runs)
    , HS       = fmax(Runs)
    , Ave      = fsum(Runs)/(fnobs(Runs) - fsum(stri_detect(str = Dismissal, regex = "not out|notout")))
    , BF       = fsum(parse_number(BF))
    , SR       = 100*fsum(Runs)/fsum(parse_number(BF))
    , Hundreds = fsum(Runs >= 100)
    , Fifties  = fsum(Runs < 100 & Runs >= 50) 
    , Fours    = fsum(parse_number(X4s))
    , Sixes    = fsum(parse_number(X6s))
      )

BATBAOut3

BATBAOut3Plot1 <- 
  ggplot(data = BATBAOut3, mapping = aes(x = reorder(Opposition, -Ave), y = Ave, fill = Opposition)) +
  geom_col(width = 0.5) +
  #geom_text(mapping = aes(label = paste0(round(Ave), " (", comma(Runs), " in ", Mat, " matches ", ")")), fontface = "bold", size = 3, vjust = -1.2) +
  geom_text(mapping = aes(label = paste0("Average: ", round(Ave))), fontface = "bold", size = 3, vjust = -3.5) +
  geom_text(mapping = aes(label = paste0("Runs   : ", comma(Runs))), fontface = "bold", size = 3, vjust = -2.0) +
  geom_text(mapping = aes(label = paste0("Matches: ", Mat)), fontface = "bold", size = 3, vjust = -0.5) +
  #scale_x_discrete(breaks = breaks_pretty(8)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma, breaks = breaks_pretty(8), limits = c(0, 100)) +
  #facet_wrap(facets = vars(Opposition), labeller = "label_both") + 
  labs(
    x       = "Year"
  , y       = "Average"
  , title   = "Test Batting Performance of Babar Azam versus different oppositions"
  , caption = "https://myaseen208.com/"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  , plot.title      = element_text(hjust = 0.5)
    )
BATBAOut3Plot1

BATBAOut4 <- 
  BATBA %>%
  fmutate(Year = as_factor(year(Date))) %>% 
   fgroup_by(Year, Innings) %>% 
   fsummarise(
      Mat      = ceiling(fnobs(Innings)/2)
    , Inns     = fnobs(Innings)
    , NO       = fsum(stri_detect(str = Dismissal, regex = "not out|notout"))
    , Runs     = fsum(Runs)
    , HS       = fmax(Runs)
    , Ave      = fsum(Runs)/(fnobs(Runs) - fsum(stri_detect(str = Dismissal, regex = "not out|notout")))
    , BF       = fsum(parse_number(BF))
    , SR       = 100*fsum(Runs)/fsum(parse_number(BF))
    , Hundreds = fsum(Runs >= 100)
    , Fifties  = fsum(Runs < 100 & Runs >= 50) 
    , Fours    = fsum(parse_number(X4s))
    , Sixes    = fsum(parse_number(X6s))
      )

BATBAOut4

BATBAOut4Plot1 <- 
  ggplot(data = BATBAOut4, mapping = aes(x = Year, y = Ave, fill = Year)) +
  geom_col(width = 0.5) +
  #geom_text(mapping = aes(label = paste0(round(Ave), " (", comma(Runs), " in ", Mat, " matches ", ")")), fontface = "bold", size = 3, vjust = -1.2) +
  geom_text(mapping = aes(label = paste0("Average: ", round(Ave))), fontface = "bold", size = 2.5, vjust = -3.5) +
  geom_text(mapping = aes(label = paste0("Runs   : ", comma(Runs))), fontface = "bold", size = 2.5, vjust = -2.0) +
  geom_text(mapping = aes(label = paste0("Matches: ", Mat)), fontface = "bold", size = 2.5, vjust = -0.5) +
  scale_x_discrete(breaks = breaks_pretty(8)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma, breaks = breaks_pretty(8), limits = c(0, 150)) +
  facet_wrap(facets = vars(Innings), labeller = "label_both") + 
  labs(
    x       = "Year"
  , y       = "Average"
  , title   = "Test Batting Performance of Babar Azam over the years by each inning"
  , caption = "https://myaseen208.com/"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  , plot.title      = element_text(hjust = 0.5)
    )
BATBAOut4Plot1

##----BATBAOut1----
kbl(
        x           = BATBAOut1
      , format      = c("latex", "html", "markdown", "pandoc", "rst")[2]
      , digits      = c(0, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0)
      , row.names   = FALSE  
      , caption     = "Test Batting Performance of Babar Azam"
      , format.args = list(decimal.mark = ".", big.mark = ",")
      , escape      = FALSE
      , longtable   = TRUE
      , booktabs    = TRUE
      , linesep     = ""
      )  %>%
  row_spec(row = c(0), bold = TRUE, italic = TRUE, align = "l") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")
  

##----BATBAOut2----
kbl(
        x           = BATBAOut2
      , format      = c("latex", "html", "markdown", "pandoc", "rst")[2]
      , digits      = c(0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0)
      , row.names   = FALSE  
      , caption     = "Test Batting Performance of Babar Azam over the years"
      , format.args = list(decimal.mark = ".", big.mark = ",")
      , escape      = FALSE
      , longtable   = TRUE
      , booktabs    = TRUE
      , linesep     = ""
      )  %>%
  row_spec(row = c(0), bold = TRUE, italic = TRUE, align = "l") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

##----BATBAOut3----
kbl(
        x           = BATBAOut3
      , format      = c("latex", "html", "markdown", "pandoc", "rst")[2]
      , digits      = c(0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0, 0)
      , row.names   = FALSE  
      , caption     = "Test Batting Performance of Babar Azam versus different oppositions"
      , format.args = list(decimal.mark = ".", big.mark = ",")
      , escape      = FALSE
      , longtable   = TRUE
      , booktabs    = TRUE
      , linesep     = ""
      )  %>%
  row_spec(row = c(0), bold = TRUE, italic = TRUE, align = "l") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

  

##----BATBAOut4----
kbl(
        x           = BATBAOut4
      , format      = c("latex", "html", "markdown", "pandoc", "rst")[2]
      , digits      = c(0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0, 0)
      , row.names   = FALSE  
      , caption     = "Test Batting Performance of Babar Azam over the years by each inning"
      , format.args = list(decimal.mark = ".", big.mark = ",")
      , escape      = FALSE
      , longtable   = TRUE
      , booktabs    = TRUE
      , linesep     = ""
      )  %>%
  row_spec(row = c(0), bold = TRUE, italic = TRUE, align = "l") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")



