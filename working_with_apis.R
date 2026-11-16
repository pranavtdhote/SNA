# =============================================================================
#  Working with APIs
#  Senator Twitter data: accounts found, tweet activity, age, and retweets by gender
#
#  HOW TO RUN  (from the Practicals folder - the one that contains the .RData files)
#      Rscript 2/working_with_apis.R
#  or open the file in RStudio, set the working directory to Practicals, and Source it.
#
#  Output : 2/output.txt   (everything printed below)
#           2/*.png        (every graph, also shown in the Plots pane in RStudio)
#
#  NOTE ON THE DATA
#  The Twitter API calls (rtweet) were already made when the course data was created,
#  so this script works on the saved results: senator_tweet_ids (500 tweet ids) and
#  senator_tweet_sample (990 tweets from the senators' accounts).
#  Account-level details (total tweets published, account creation date) are NOT
#  stored in the files; where a question needs them, the script says so and uses a
#  clearly-labelled proxy.
# =============================================================================

needed <- c("dplyr", "ggplot2", "stringr")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(stringr) })

# ---- run-folder checks ------------------------------------------------------
out_dir <- "2"
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
save_plot <- function(p, file, w = 8, h = 5.5) {
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 150, bg = "white")
  if (interactive()) print(p)
  cat("  [graph saved]", file.path(out_dir, file), "\n")
}
gender_cols <- c(Female = "#d6604d", Male = "#4393c3")

reference_date <- as.Date("2019-01-01")   # the tweets are from 2018, so ages are taken at this date

sink(file.path(out_dir, "output.txt"), split = TRUE)
tryCatch({

  cat("Working with APIs\n")
  cat("Run on:", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")

  suppressWarnings(load("congress.RData"))
  suppressWarnings(load("senator_tweets.RData"))

  # ---------------------------------------------------------------------------
  #  Link every Twitter account to a senator
  #    1. exact match with the handle listed in congress_contact (case-insensitive)
  #    2. otherwise: the one senator whose last name appears inside the screen name
  # ---------------------------------------------------------------------------
  senators <- congress %>%
    filter(type == "sen") %>%
    inner_join(congress_contact, by = "bioguide_id") %>%
    mutate(
      handle_lower = tolower(twitter),
      last_name    = tolower(word(str_replace_all(full_name, "[^A-Za-z ]", ""), -1)),
      age          = as.numeric(reference_date - as.Date(birthdate)) / 365.25,
      gender_label = ifelse(gender == "F", "Female", "Male")
    )

  accounts <- senator_tweet_sample %>%
    distinct(user_id, screen_name) %>%
    mutate(screen_lower = tolower(screen_name),
           bioguide_id  = senators$bioguide_id[match(screen_lower, senators$handle_lower)],
           matched_by   = ifelse(is.na(bioguide_id), NA_character_, "listed handle"))
  for (i in which(is.na(accounts$bioguide_id))) {
    hits <- senators$bioguide_id[str_detect(accounts$screen_lower[i], fixed(senators$last_name))]
    if (length(hits) == 1) {
      accounts$bioguide_id[i] <- hits
      accounts$matched_by[i]  <- "last name in screen name"
    }
  }

  tweets <- senator_tweet_sample %>%
    left_join(accounts %>% select(user_id, bioguide_id), by = "user_id") %>%
    left_join(senators %>% select(bioguide_id, full_name, party, age, gender_label), by = "bioguide_id")

  part("DATA LOADED")
  kv("Senators in the dataset", nrow(senators))
  kv("Tweet ids supplied (senator_tweet_ids)", length(senator_tweet_ids))
  kv("Tweets in the sample (senator_tweet_sample)", nrow(senator_tweet_sample))
  kv("Date range of the tweets",
     paste(format(min(senator_tweet_sample$created_at), "%Y-%m-%d"), "to",
           format(max(senator_tweet_sample$created_at), "%Y-%m-%d")))

  # ===========================================================================
  #  A. How many senator Twitter accounts were found on Twitter?
  # ===========================================================================
  part("A.  SENATOR TWITTER ACCOUNTS FOUND")
  listed <- sum(!is.na(senators$twitter) & senators$twitter != "")
  matched_senators <- n_distinct(accounts$bioguide_id[!is.na(accounts$bioguide_id)])

  kv("Senators with a Twitter handle listed in the dataset", listed)
  kv("Distinct Twitter accounts found in the tweet data", n_distinct(accounts$user_id))
  kv("  accounts that belong to a senator in the dataset", sum(!is.na(accounts$bioguide_id)))
  kv("  accounts of people who are not in the dataset", sum(is.na(accounts$bioguide_id)))
  kv("Senators with at least one tweet found  (ANSWER)", matched_senators)
  kv("Senators with no tweet in the data", nrow(senators) - matched_senators)
  kv("Senators who have two accounts in the data",
     sum(!is.na(accounts$bioguide_id)) - matched_senators)

  cat("\nHow the accounts were linked to senators:\n")
  show_df(accounts %>% count(Linked_by = coalesce(matched_by, "not linked (not a senator in the data)"),
                             name = "Accounts"))

  cat("\nAccounts that could not be linked (the dataset lists the current senators,\n")
  cat("so former senators, the Vice President etc. do not match):\n")
  cat(" ", paste(sort(accounts$screen_name[is.na(accounts$bioguide_id)]), collapse = ", "), "\n")

  # ===========================================================================
  #  B. Average number of published Tweets by gender
  # ===========================================================================
  part("B.  AVERAGE NUMBER OF TWEETS PER SENATOR BY GENDER")
  cat("  The files hold a sample of tweets, not each account's lifetime tweet count,\n")
  cat("  so this is the average number of SAMPLED tweets per senator.\n\n")

  tweets_per_senator <- tweets %>%
    filter(!is.na(bioguide_id)) %>%
    group_by(bioguide_id, gender_label) %>%
    summarise(tweets = n(), .groups = "drop")

  tweets_by_gender <- tweets_per_senator %>%
    group_by(Gender = gender_label) %>%
    summarise(Senators = n(), Total_tweets = sum(tweets),
              Average_tweets = round(mean(tweets), 2),
              Median_tweets = median(tweets),
              Min = min(tweets), Max = max(tweets))
  show_df(tweets_by_gender)

  wt <- t.test(tweets ~ gender_label, data = tweets_per_senator)
  kv("Welch t-test p-value (difference between genders)", signif(wt$p.value, 3))
  cat("  ->", ifelse(wt$p.value < 0.05, "the difference is statistically significant (p < 0.05).",
                     "no statistically significant difference between genders (p >= 0.05)."), "\n")

  p_b <- ggplot(tweets_by_gender, aes(x = Gender, y = Average_tweets, fill = Gender)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = paste0(Average_tweets, "\n(n = ", Senators, ")")), vjust = -0.3, size = 4) +
    scale_fill_manual(values = gender_cols) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Average number of sampled tweets per senator",
         subtitle = "Senators whose Twitter account appears in the tweet data",
         x = NULL, y = "Average tweets per senator") +
    theme_minimal(base_size = 12)
  save_plot(p_b, "average_tweets_by_gender.png")

  # ===========================================================================
  #  C. Correlation between senator age and when the account was created
  # ===========================================================================
  part("C.  SENATOR AGE VS. TWITTER ACCOUNT CREATION")
  cat("  The exact creation date (created_at from lookup_users) is not in the saved data.\n")
  cat("  PROXY USED: Twitter user ids grow with time, so a smaller user_id means an older\n")
  cat("  account. The senator's oldest account is used, and ranks are compared (Spearman).\n\n")

  age_account <- tweets %>%
    filter(!is.na(bioguide_id)) %>%
    group_by(bioguide_id, full_name, party, age) %>%
    summarise(user_id_num = min(as.numeric(user_id)),
              first_tweet = min(created_at), .groups = "drop") %>%
    mutate(account_order = rank(user_id_num))      # 1 = oldest account

  ct <- cor.test(age_account$age, age_account$account_order, method = "spearman", exact = FALSE)
  kv("Senators used", nrow(age_account))
  kv("Spearman correlation (age vs. account order)", round(unname(ct$estimate), 3))
  kv("p-value", signif(ct$p.value, 3))
  cat("\n  How to read it: a NEGATIVE value would mean older senators opened their accounts\n")
  cat("  EARLIER (small order number); a value near 0 means no linear/rank relationship.\n")
  cat("  Result:", ifelse(ct$p.value < 0.05,
        paste0("a ", ifelse(ct$estimate < 0, "negative", "positive"),
               ", statistically significant relationship (p < 0.05)."),
        "no statistically significant relationship (p >= 0.05)."), "\n")

  p_c <- ggplot(age_account, aes(x = age, y = account_order)) +
    geom_point(aes(colour = party), size = 2.6, alpha = 0.85) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey30", linewidth = 0.8, formula = y ~ x) +
    scale_colour_manual(values = c(Democrat = "#2166ac", Republican = "#b2182b", Independent = "#4daf4a")) +
    labs(title = "Senator age vs. order in which the Twitter account was opened",
         subtitle = paste0("Spearman rho = ", round(unname(ct$estimate), 3),
                           " (user-id rank used as proxy for creation date)"),
         x = "Age (years, at 1 Jan 2019)", y = "Account order (1 = oldest account)", colour = "Party") +
    theme_minimal(base_size = 12)
  save_plot(p_c, "age_vs_account_order.png")

  # ===========================================================================
  #  D. Last two Tweets of each senator and average retweets by gender
  # ===========================================================================
  part("D.  LAST TWO TWEETS PER SENATOR - AVERAGE RETWEETS BY GENDER")
  last_two <- tweets %>%
    filter(!is.na(bioguide_id)) %>%
    group_by(bioguide_id) %>%
    arrange(desc(created_at), .by_group = TRUE) %>%
    slice_head(n = 2) %>%
    ungroup()

  kv("Senators included", n_distinct(last_two$bioguide_id))
  kv("Tweets selected (last two per senator)", nrow(last_two))

  cat("\nFirst 10 selected tweets:\n")
  show_df(last_two %>%
            transmute(Senator = full_name, Date = format(created_at, "%Y-%m-%d"),
                      Retweets = retweet_count, Is_RT = is_retweet,
                      Text = str_trunc(str_squish(text), 50)) %>% head(10))

  rt_by_gender <- function(d) d %>%
    group_by(Gender = gender_label) %>%
    summarise(Senators = n_distinct(bioguide_id), Tweets = n(),
              Average_retweets = round(mean(retweet_count), 1),
              Median_retweets = median(retweet_count), Max_retweets = max(retweet_count))

  cat("\nAverage retweets by gender of the publishing senator  (ANSWER):\n")
  rt_all <- rt_by_gender(last_two)
  show_df(rt_all)

  cat("\nSame table without retweets (a retweet carries the ORIGINAL tweet's count):\n")
  rt_orig <- rt_by_gender(last_two %>% filter(!is_retweet))
  show_df(rt_orig)

  wt2 <- t.test(retweet_count ~ gender_label, data = last_two)
  kv("Welch t-test p-value", signif(wt2$p.value, 3))
  cat("  ->", ifelse(wt2$p.value < 0.05, "significant difference (p < 0.05).",
                     "no statistically significant difference (p >= 0.05); retweet counts are very skewed."), "\n")

  p_d <- ggplot(rt_all, aes(x = Gender, y = Average_retweets, fill = Gender)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = paste0(Average_retweets, "\n(", Tweets, " tweets)")), vjust = -0.3, size = 4) +
    scale_fill_manual(values = gender_cols) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Average retweets of each senator's last two tweets",
         x = NULL, y = "Average retweets") +
    theme_minimal(base_size = 12)
  save_plot(p_d, "average_retweets_by_gender.png")

  # ---------------------------------------------------------------------------
  part("RESULT SUMMARY")
  kv("A. senators with a Twitter account found", paste(matched_senators, "of", nrow(senators)))
  kv("B. avg sampled tweets  F / M", paste(tweets_by_gender$Average_tweets, collapse = " / "))
  kv("C. Spearman rho (age vs. account order)", round(unname(ct$estimate), 3))
  kv("D. avg retweets  F / M", paste(rt_all$Average_retweets, collapse = " / "))
  cat("\nWorking with APIs - completed successfully.\n")

  # ---------------------------------------------------------------------------
  #  Optional: with Twitter API credentials the real account details could be fetched
  #  (kept as a comment because it needs a developer account and a network connection)
  #
  #    library(rtweet)
  #    users <- lookup_users(senators$twitter)      # created_at, statuses_count, ...
  #    # B: mean(users$statuses_count) by gender
  #    # C: cor(age, as.numeric(users$created_at))
  #    # D: get_timeline(handle, n = 2)$retweet_count
  # ---------------------------------------------------------------------------

}, error = function(e) {
  cat("\nERROR:", conditionMessage(e), "\n")
  stop(e)
}, finally = {
  while (sink.number() > 0) sink()
})
