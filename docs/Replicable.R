# ijc437 coursework — billboard hot 100 (2000–2023)
# outputs created:
# -the cleaned dataset (one row per song, best peak position)
# -the table 1 correlations
# -the figure 1 scatterplots (valence, tempo, acousticness)
# -the trend table and figure 2 trend plots (danceability, energy, loudness)
# -the logistic regression accuracy + confusion matrix
# -the decision tree accuracy + tree plot
# -the github pages files in /docs

library(tidyverse)
library(rpart)

set.seed(123)

dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("docs", showWarnings = FALSE)
dir.create(file.path("docs", "figures"), showWarnings = FALSE)
dir.create(file.path("docs", "tables"), showWarnings = FALSE)

# 1) loading the data!

filepath <- "billboard_24years_lyrics_spotify.csv"
billboard <- read_csv(filepath, show_col_types = FALSE)

# clean column names (lowercase + remove symbols)
names(billboard) <- tolower(gsub("[^a-z0-9]+", "", names(billboard)))

# setting the columns 
peakcolumn   <- "ranking"
idcolumn     <- "id"
yearcolumn   <- "year"

# spotify audio features used in the report
audiofeatures <- c(
  "danceability","energy","valence","tempo","loudness",
  "acousticness","instrumentalness","speechiness","liveness","mode"
)
audiofeatures <- audiofeatures[audiofeatures %in% names(billboard)]

# 2) cleaning and preprocessing!

dfsong <- billboard %>%
  mutate(
    peakposition = suppressWarnings(as.numeric(.data[[peakcolumn]])),
    year = suppressWarnings(as.integer(.data[[yearcolumn]]))
  ) %>%
  filter(!is.na(peakposition), peakposition >= 1) %>%
  filter(if_all(all_of(audiofeatures), ~ !is.na(.))) %>%
  mutate(trackkey = as.character(.data[[idcolumn]])) %>%
  group_by(trackkey) %>%
  arrange(peakposition, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(top10 = if_else(peakposition <= 10, 1L, 0L))

# saving the report summary
dir.create("tables", showWarnings = FALSE)
dir.create("docs", showWarnings = FALSE)
dir.create(file.path("docs", "tables"), showWarnings = FALSE)
dir.create(file.path("docs", "figures"), showWarnings = FALSE)

reportsummary <- tibble(uniquesongs = nrow(dfsong))
write_csv(reportsummary, file.path("tables", "reportsummarymetrics.csv"))
write_csv(reportsummary, file.path("docs", "tables", "reportsummarymetrics.csv"))

# 3) rq1 — correlations table (table 1 in the report)

cortable <- map_dfr(audiofeatures, function(f) {
  tibble(
    feature = f,
    correlationr = cor(dfsong[[f]], dfsong$peakposition, use = "complete.obs")
  )
}) %>%
  arrange(desc(abs(correlationr)))

write_csv(cortable, file.path("tables", "table1featurecorrelations.csv"))
write_csv(cortable, file.path("docs", "tables", "table1featurecorrelations.csv"))

# 4) figure 1 — the scatterplots (valence, tempo, acousticness)

scatterfeatures <- c("valence", "tempo", "acousticness")
scatterfeatures <- scatterfeatures[scatterfeatures %in% names(dfsong)]

for (f in scatterfeatures) {
  
  p <- ggplot(dfsong, aes(x = .data[[f]], y = peakposition)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      x = f,
      y = "peak billboard chart position (lower = better)",
      title = paste("peak chart position vs", f)
    )
  
  ggsave(
    filename = file.path("figures", paste0("figure1_scatter_", f, ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggsave(
    filename = file.path("docs", "figures", paste0("figure1_scatter_", f, ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

# 5) rq2 —trend table and figure 2 trends
# (using danceability, energy, loudness)

trendtable <- dfsong %>%
  group_by(year, top10) %>%
  summarise(across(all_of(audiofeatures), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = if_else(top10 == 1, "top 10", "not top 10"))

write_csv(trendtable, file.path("tables", "trendtableyearlymeans.csv"))
write_csv(trendtable, file.path("docs", "tables", "trendtableyearlymeans.csv"))

trendfeatures <- c("danceability", "energy", "loudness", "valence", "acousticness")
trendfeatures <- trendfeatures[trendfeatures %in% names(trendtable)]

for (f in trendfeatures) {
  
  rq2 <- ggplot(trendtable, aes(x = year, y = .data[[f]], linetype = group)) +
    geom_line(linewidth = 1) +
    labs(
      x = "year",
      y = paste("yearly average", f),
      linetype = "chart group",
      title = paste("yearly average", f, "(top 10 vs not top 10)")
    )
  
  ggsave(
    filename = file.path("figures", paste0("figure2_trend_", f, ".png")),
    plot = rq2,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  ggsave(
    filename = file.path("docs", "figures", paste0("figure2_trend_", f, ".png")),
    plot = rq2,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# 6) rq3 —the train/test split

trainindex <- sample(seq_len(nrow(dfsong)), size = floor(0.7 * nrow(dfsong)))
train <- dfsong[trainindex, ]
test  <- dfsong[-trainindex, ]

# 7) rq3 —the logistic regression and confusion matrix

glmformula <- as.formula(paste("top10 ~", paste(audiofeatures, collapse = " + ")))
glmfit <- glm(glmformula, data = train, family = binomial())

prob <- predict(glmfit, newdata = test, type = "response")
predclass <- if_else(prob >= 0.5, 1L, 0L)

glmaccuracy <- mean(predclass == test$top10)
glmbaseline <- max(mean(test$top10 == 1), mean(test$top10 == 0))

glmperformance <- tibble(
  metric = c("test accuracy", "baseline accuracy"),
  value  = c(glmaccuracy, glmbaseline)
)

write_csv(glmperformance, file.path("tables", "tableglmperformance.csv"))
write_csv(glmperformance, file.path("docs", "tables", "tableglmperformance.csv"))

confmat <- table(
  actual = factor(test$top10, levels = c(0,1), labels = c("not top 10","top 10")),
  predicted = factor(predclass, levels = c(0,1), labels = c("not top 10","top 10"))
)

confusionmatrix <- as.data.frame(confmat)
write_csv(confusionmatrix, file.path("tables", "tableglmconfusionmatrix.csv"))
write_csv(confusionmatrix, file.path("docs", "tables", "tableglmconfusionmatrix.csv"))

# 8) rq3 —all of the decision tree, accuracy, and plot

treefit <- rpart(glmformula, data = train, method = "class")

png(file.path("figures", "figure3decisiontreetop10.png"), width = 1200, height = 800, res = 150)
plot(treefit, uniform = TRUE, margin = 0.1)
text(treefit, use.n = TRUE, cex = 0.75)
dev.off()

png(file.path("docs", "figures", "figure3decisiontreetop10.png"), width = 1200, height = 800, res = 150)
plot(treefit, uniform = TRUE, margin = 0.1)
text(treefit, use.n = TRUE, cex = 0.75)
dev.off()

treepred <- predict(treefit, newdata = test, type = "class")
treeaccuracy <- mean(treepred == factor(test$top10))

treeperformance <- tibble(
  metric = "decision tree accuracy",
  value = treeaccuracy
)

write_csv(treeperformance, file.path("tables", "tabletreeperformance.csv"))
write_csv(treeperformance, file.path("docs", "tables", "tabletreeperformance.csv"))

# 9) final summary metrics

finalsummary <- tibble(
  uniquesongs  = nrow(dfsong),
  glmaccuracy  = glmaccuracy,
  glmbaseline  = glmbaseline,
  treeaccuracy = treeaccuracy
)

write_csv(finalsummary, file.path("tables", "reportsummarymetrics.csv"))
write_csv(finalsummary, file.path("docs", "tables", "reportsummarymetrics.csv"))

# viewing everything

View(reportsummary)
View(cortable)
View(trendtable)
View(glmperformance)
View(confusionmatrix)
View(treeperformance)

getwd()
browseURL(file.path(getwd(), "figures"))

