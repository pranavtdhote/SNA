# =============================================================================
#  Introduction to Text Analysis
#  Mentions (@handle) in senator tweets: link with favorites and difference by gender
#
#  HOW TO RUN  (from the Practicals folder - the one that contains the .RData files)
#      Rscript 4/introduction_to_text_analysis.R
#  or open the file in RStudio, set the working directory to Practicals, and Source it.
#
#  Output : 4/output.txt   (everything printed below)
#           4/*.png        (every graph, also shown in the Plots pane in RStudio)
#
#  A "mention" is an @ followed by non-whitespace characters, found with the
#  regular expression  @\S+  (written "@\\S+" inside R strings).
# =============================================================================

needed <- c("dplyr", "ggplot2", "stringr")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(stringr) })

# ---- run-folder checks ------------------------------------------------------
out_dir <- "4"
if (!file.exists("congress.RData")) {
  stop("congress.RData not found in the current folder (", getwd(), ").\n",
       "Set the working directory to the Practicals folder and run again.")
}
dir.create(out_dir, showWarnings = FALSE)
options(width = 100, dplyr.summarise.inform = FALSE)

# ---- small printing helpers -------------------------------------------------
hr      <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
part    <- function(txt) { cat("\n"); hr(); cat(" ", txt, "\n", sep = ""); hr() }
kv      <- function(label, value) cat(sprintf("  %-52s %s\n", label, format(value, big.mark = ",")))
show_df <- function(d) print(as.data.frame(d), row.names = FALSE)
fmt_p   <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
save_plot <- function(p, file, w = 8, h = 5.5) {
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 150, bg = "white")
  if (interactive()) print(p)
  cat("  [graph saved]", file.path(out_dir, file), "\n")
}
gender_cols <- c(Female = "#d6604d", Male = "#4393c3")
strength_word <- function(r) {
  a <- abs(r)
  ifelse(a < 0.1, "negligible", ifelse(a < 0.3, "weak", ifelse(a < 0.5, "moderate", "strong")))
}

sink(file.path(out_dir, "output.txt"), split = TRUE)
tryCatch({

  cat("Introduction to Text Analysis\n")
  cat("Run on:", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")

  suppressWarnings(load("congress.RData"))
  suppressWarnings(load("senator_tweets.RData"))

  # ---------------------------------------------------------------------------
  #  Count mentions in every tweet
  # ---------------------------------------------------------------------------
  tweet_data <- senator_tweet_sample %>%
    mutate(mentions = str_count(text, "@\\S+"))

  part("DATA AND MENTION COUNTS")
  kv("Tweets analysed (senator_tweet_sample)", nrow(tweet_data))
  kv("Twitter accounts", n_distinct(tweet_data$user_id))
  kv("Average tweet length (characters)", round(mean(nchar(tweet_data$text)), 1))
  kv("Tweets with at least one mention", sum(tweet_data$mentions > 0))
  kv("Share of tweets with a mention", paste0(round(100 * mean(tweet_data$mentions > 0), 1), "%"))
  kv("Average mentions per tweet", round(mean(tweet_data$mentions), 4))

  cat("\nHow many mentions do tweets contain?\n")
  show_df(tweet_data %>%
            mutate(Mentions = ifelse(mentions >= 4, "4 or more", as.character(mentions))) %>%
            count(Mentions, name = "Tweets") %>%
            mutate(Share = paste0(round(100 * Tweets / sum(Tweets), 1), "%")))

  cat("\nThe 10 most mentioned accounts:\n")
  show_df(tibble(handle = unlist(str_extract_all(tweet_data$text, "@\\w+"))) %>%
            mutate(handle = tolower(handle)) %>% count(handle, sort = TRUE, name = "Times_mentioned") %>%
            head(10))

  cat("\nExample tweets with the mentions found:\n")
  show_df(tweet_data %>% filter(mentions >= 2) %>% head(5) %>%
            transmute(Mentions = mentions, Favorites = favorite_count,
                      Text = str_trunc(str_squish(text), 70)))

  # ===========================================================================
  #  A. Correlation between mentions and favorites
  # ===========================================================================
  part("A.  CORRELATION: NUMBER OF MENTIONS VS. NUMBER OF FAVORITES")
  pearson  <- cor.test(tweet_data$mentions, tweet_data$favorite_count)
  spearman <- suppressWarnings(cor.test(tweet_data$mentions, tweet_data$favorite_count,
                                        method = "spearman", exact = FALSE))
  kv("Pearson correlation  (ANSWER)", round(unname(pearson$estimate), 4))
  kv("  95% confidence interval",
     paste0(round(pearson$conf.int[1], 3), " to ", round(pearson$conf.int[2], 3)))
  kv("  p-value", fmt_p(pearson$p.value))
  kv("Spearman rank correlation", round(unname(spearman$estimate), 4))
  kv("  p-value", fmt_p(spearman$p.value))

  cat("\n  Reading the result: the relationship is", strength_word(pearson$estimate),
      ifelse(pearson$estimate < 0, "(negative)", "(positive)"), "and",
      ifelse(pearson$p.value < 0.05, "statistically significant (p < 0.05).",
             "NOT statistically significant (p >= 0.05)."), "\n")
  cat("  So the number of mentions in a tweet",
      ifelse(pearson$p.value < 0.05 && abs(pearson$estimate) >= 0.1,
             "is related to the favorites it receives.",
             "tells us almost nothing about how many favorites it gets."), "\n")
  if (spearman$p.value < 0.05 && pearson$p.value >= 0.05) {
    cat("  Note: the rank-based (Spearman) test IS significant (rho =", round(unname(spearman$estimate), 3),
        ") - tweets with more mentions\n  tend to get fewer favorites, but a few very popular tweets hide this in the Pearson value.\n")
  }
  cat("  (Correlation does not imply causation.)\n")

  cat("\nAverage favorites by number of mentions:\n")
  fav_by_mentions <- tweet_data %>%
    mutate(Mentions = ifelse(mentions >= 3, "3 or more", as.character(mentions))) %>%
    group_by(Mentions) %>%
    summarise(Tweets = n(), Average_favorites = round(mean(favorite_count), 1),
              Median_favorites = median(favorite_count))
  show_df(fav_by_mentions)

  p_a <- ggplot(tweet_data, aes(x = mentions, y = favorite_count)) +
    geom_jitter(width = 0.12, height = 0, alpha = 0.35, colour = "#2166ac", size = 1.6) +
    geom_smooth(method = "lm", se = TRUE, colour = "#b2182b", linewidth = 0.9, formula = y ~ x) +
    scale_y_continuous(trans = "pseudo_log", breaks = c(0, 10, 100, 1000, 10000)) +
    scale_x_continuous(breaks = 0:max(tweet_data$mentions)) +
    labs(title = "Mentions in a tweet vs. favorites received",
         subtitle = paste0("Pearson r = ", round(unname(pearson$estimate), 3),
                           ", Spearman rho = ", round(unname(spearman$estimate), 3),
                           " (y-axis on a log-like scale)"),
         x = "Number of mentions (@handle) in the tweet", y = "Favorites") +
    theme_minimal(base_size = 12)
  save_plot(p_a, "mentions_vs_favorites.png")

  # ===========================================================================
  #  B. Average number of mentions by gender
  # ===========================================================================
  part("B.  AVERAGE NUMBER OF MENTIONS PER TWEET BY GENDER")

  # Link each Twitter account to a senator (and so to a gender):
  #   1. exact match with the handle listed in congress_contact (case-insensitive)
  #   2. otherwise the one senator whose last name appears in the screen name
  senators <- congress %>%
    filter(type == "sen") %>%
    inner_join(congress_contact, by = "bioguide_id") %>%
    mutate(handle_lower = tolower(twitter),
           last_name = tolower(word(str_replace_all(full_name, "[^A-Za-z ]", ""), -1)),
           gender_label = ifelse(gender == "F", "Female", "Male"))

  accounts <- tweet_data %>% distinct(user_id, screen_name) %>%
    mutate(screen_lower = tolower(screen_name),
           bioguide_id = senators$bioguide_id[match(screen_lower, senators$handle_lower)])
  for (i in which(is.na(accounts$bioguide_id))) {
    hits <- senators$bioguide_id[str_detect(accounts$screen_lower[i], fixed(senators$last_name))]
    if (length(hits) == 1) accounts$bioguide_id[i] <- hits
  }

  tweet_gender <- tweet_data %>%
    left_join(accounts %>% select(user_id, bioguide_id), by = "user_id") %>%
    left_join(senators %>% select(bioguide_id, gender_label), by = "bioguide_id")

  kv("Tweets linked to a senator in the dataset", sum(!is.na(tweet_gender$gender_label)))
  kv("Tweets not linked (former senators etc.)", sum(is.na(tweet_gender$gender_label)))

  mentions_by_gender <- tweet_gender %>%
    filter(!is.na(gender_label)) %>%
    group_by(Gender = gender_label) %>%
    summarise(Senators = n_distinct(bioguide_id), Tweets = n(),
              Total_mentions = sum(mentions),
              Average_mentions = round(mean(mentions), 4),
              Median = median(mentions), SD = round(sd(mentions), 3),
              Tweets_with_mention = paste0(round(100 * mean(mentions > 0), 1), "%"))
  cat("\nAverage mentions per tweet by gender  (ANSWER):\n")
  show_df(mentions_by_gender)

  wt <- t.test(mentions ~ gender_label, data = tweet_gender %>% filter(!is.na(gender_label)))
  kv("Welch t-test p-value", fmt_p(wt$p.value))
  cat("  ->", ifelse(wt$p.value < 0.05, "the difference between genders is statistically significant (p < 0.05).",
                     "no statistically significant difference between genders (p >= 0.05)."), "\n")

  p_b <- ggplot(mentions_by_gender, aes(x = Gender, y = Average_mentions, fill = Gender)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = paste0(Average_mentions, "\n(", Tweets, " tweets)")), vjust = -0.3, size = 4) +
    scale_fill_manual(values = gender_cols) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Average number of mentions per tweet, by gender of the senator",
         x = NULL, y = "Average mentions per tweet") +
    theme_minimal(base_size = 12)
  save_plot(p_b, "average_mentions_by_gender.png")

  # ---------------------------------------------------------------------------
  part("RESULT SUMMARY")
  kv("A. correlation mentions vs. favorites (Pearson)", round(unname(pearson$estimate), 4))
  kv("B. average mentions  Female / Male", paste(mentions_by_gender$Average_mentions, collapse = " / "))
  cat("\nIntroduction to Text Analysis - completed successfully.\n")

}, error = function(e) {
  cat("\nERROR:", conditionMessage(e), "\n")
  stop(e)
}, finally = {
  while (sink.number() > 0) sink()
})
