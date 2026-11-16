# =============================================================================
#  Modeling a Network
#  Age of Congress members, their social media presence, and a linear regression
#
#  HOW TO RUN  (from the Practicals folder - the one that contains the .RData files)
#      Rscript 3/modeling_a_network.R
#  or open the file in RStudio, set the working directory to Practicals, and Source it.
#
#  Output : 3/output.txt   (everything printed below)
#           3/*.png        (every graph, also shown in the Plots pane in RStudio)
#
#  "Number of social media accounts" = how many of Twitter, Facebook and YouTube a
#  member has listed in congress_contact (0 to 3).
# =============================================================================

needed <- c("dplyr", "ggplot2")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

# ---- run-folder checks ------------------------------------------------------
out_dir <- "3"
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
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", ""))))
gender_cols <- c(Female = "#d6604d", Male = "#4393c3")

reference_date <- as.Date("2019-01-01")   # approximate age is taken at this date

sink(file.path(out_dir, "output.txt"), split = TRUE)
tryCatch({

  cat("Modeling a Network\n")
  cat("Run on:", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")

  suppressWarnings(load("congress.RData"))

  # ---------------------------------------------------------------------------
  #  Prepare the data: approximate age and number of social media accounts
  # ---------------------------------------------------------------------------
  has_account <- function(x) !is.na(x) & trimws(x) != ""

  members <- congress %>%
    inner_join(congress_contact, by = "bioguide_id") %>%
    mutate(
      age = as.numeric(reference_date - as.Date(birthdate)) / 365.25,
      social_media_accounts = has_account(twitter) + has_account(facebook) + has_account(youtube),
      gender_label = ifelse(gender == "F", "Female", "Male"),
      chamber = ifelse(type == "sen", "Senator", "Representative")
    )

  part("DATA PREPARED")
  kv("Members of Congress", nrow(members))
  kv("Age reference date", format(reference_date))
  kv("Age range (years)", paste(round(min(members$age), 1), "to", round(max(members$age), 1)))
  cat("\nMembers by number of social media accounts (Twitter, Facebook, YouTube):\n")
  show_df(members %>% count(Accounts = social_media_accounts, name = "Members") %>%
            mutate(Share = paste0(round(100 * Members / sum(Members), 1), "%")))
  cat("\nMembers by chamber, party and gender:\n")
  print(table(Chamber = members$chamber, Party = members$party))
  print(table(Chamber = members$chamber, Gender = members$gender_label))

  # ===========================================================================
  #  A. Box plot: approximate age by gender
  # ===========================================================================
  part("A.  APPROXIMATE AGE BY GENDER (BOX PLOT)")
  age_by_gender <- members %>%
    group_by(Gender = gender_label) %>%
    summarise(Members = n(), Mean_age = round(mean(age), 1), Median_age = round(median(age), 1),
              SD = round(sd(age), 1), Youngest = round(min(age), 1), Oldest = round(max(age), 1))
  show_df(age_by_gender)

  tt <- t.test(age ~ gender_label, data = members)
  kv("Difference in mean age (Female - Male)", round(unname(diff(rev(tt$estimate))), 2))
  kv("Welch t-test p-value", fmt_p(tt$p.value))
  cat("  ->", ifelse(tt$p.value < 0.05, "the age difference is statistically significant (p < 0.05).",
                     "no statistically significant age difference between the genders (p >= 0.05)."), "\n")

  p_a <- ggplot(members, aes(x = gender_label, y = age, fill = gender_label)) +
    geom_boxplot(width = 0.5, alpha = 0.75, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(width = 0.12, alpha = 0.25, size = 1, show.legend = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
    scale_fill_manual(values = gender_cols) +
    labs(title = "Age of Congress members by gender",
         subtitle = "Box = middle 50%, line = median, white diamond = mean",
         x = NULL, y = "Approximate age (years)") +
    theme_minimal(base_size = 12)
  save_plot(p_a, "age_by_gender_boxplot.png")

  # ===========================================================================
  #  B. Scatter plot: age vs number of social media accounts + linear trendline
  # ===========================================================================
  part("B.  AGE VS. NUMBER OF SOCIAL MEDIA ACCOUNTS (SCATTER PLOT)")
  pearson  <- cor.test(members$age, members$social_media_accounts)
  spearman <- suppressWarnings(cor.test(members$age, members$social_media_accounts,
                                        method = "spearman", exact = FALSE))
  kv("Pearson correlation", round(unname(pearson$estimate), 3))
  kv("  p-value", fmt_p(pearson$p.value))
  kv("Spearman rank correlation", round(unname(spearman$estimate), 3))
  kv("  p-value", fmt_p(spearman$p.value))

  trend <- lm(social_media_accounts ~ age, data = members)
  kv("Trendline slope (accounts per extra year of age)", round(coef(trend)[["age"]], 4))
  kv("Trendline intercept", round(coef(trend)[["(Intercept)"]], 3))

  cat("\nAverage number of accounts by age group:\n")
  age_groups <- members %>%
    mutate(Age_group = cut(age, breaks = c(0, 44, 54, 64, 74, 120),
                           labels = c("under 45", "45-54", "55-64", "65-74", "75+"))) %>%
    group_by(Age_group) %>%
    summarise(Members = n(), Average_accounts = round(mean(social_media_accounts), 2))
  show_df(age_groups)

  p_b <- ggplot(members, aes(x = age, y = social_media_accounts)) +
    geom_jitter(aes(colour = chamber), width = 0.25, height = 0.12, alpha = 0.5, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.9, formula = y ~ x) +
    scale_colour_manual(values = c(Senator = "#b2182b", Representative = "#2166ac")) +
    scale_y_continuous(breaks = 0:3) +
    labs(title = "Age vs. number of social media accounts",
         subtitle = paste0("Linear trendline: slope = ", round(coef(trend)[["age"]], 4),
                           " accounts per year (r = ", round(unname(pearson$estimate), 3), ")"),
         x = "Approximate age (years)", y = "Number of social media accounts", colour = "Chamber") +
    theme_minimal(base_size = 12)
  save_plot(p_b, "age_vs_social_media_accounts.png")

  # ===========================================================================
  #  C. Linear regression
  # ===========================================================================
  part("C.  LINEAR REGRESSION: ACCOUNTS ~ TYPE + AGE + PARTY + GENDER")
  regression_data <- members %>%
    mutate(type = factor(type), party = factor(party), gender = factor(gender)) %>%
    filter(!is.na(age), !is.na(type), !is.na(party), !is.na(gender))

  model <- lm(social_media_accounts ~ type + age + party + gender, data = regression_data)
  cat("\nFull R summary:\n")
  print(summary(model))

  # tidy coefficient table
  sm <- summary(model)$coefficients
  ci <- confint(model)
  coef_table <- data.frame(
    Term      = rownames(sm),
    Estimate  = round(sm[, "Estimate"], 4),
    Std_Error = round(sm[, "Std. Error"], 4),
    CI_low    = round(ci[, 1], 4),
    CI_high   = round(ci[, 2], 4),
    t_value   = round(sm[, "t value"], 2),
    p_raw     = sm[, "Pr(>|t|)"],
    p_value   = fmt_p(sm[, "Pr(>|t|)"]),
    Sig       = stars(sm[, "Pr(>|t|)"]),
    row.names = NULL
  )
  cat("\nCoefficient table with 95% confidence intervals:\n")
  show_df(coef_table %>% select(-p_raw))
  cat("  Signif.: *** p<0.001   ** p<0.01   * p<0.05   . p<0.1\n")

  fs <- summary(model)$fstatistic
  cat("\nModel fit:\n")
  kv("Observations used", nobs(model))
  kv("R-squared", round(summary(model)$r.squared, 4))
  kv("Adjusted R-squared", round(summary(model)$adj.r.squared, 4))
  kv("Residual standard error", round(summary(model)$sigma, 4))
  kv("F-statistic", round(unname(fs["value"]), 2))
  kv("Model p-value", fmt_p(pf(fs["value"], fs["numdf"], fs["dendf"], lower.tail = FALSE)))

  # ---- the age coefficient -------------------------------------------------
  age_row  <- coef_table[coef_table$Term == "age", ]
  age_coef <- age_row$Estimate
  cat("\n------------------------------------------------------------------\n")
  cat("INTERPRETATION OF THE AGE COEFFICIENT\n")
  cat("------------------------------------------------------------------\n")
  cat(sprintf(
    "Controlling for chamber (type), party and gender, each additional year of age is\nassociated with %s %.4f social media accounts (95%% CI %.4f to %.4f).\n",
    ifelse(age_coef >= 0, "an increase of", "a decrease of"), abs(age_coef), age_row$CI_low, age_row$CI_high))
  cat(sprintf("Over 10 years of age that is %.3f accounts (out of a maximum of 3).\n", age_coef * 10))
  if (age_row$p_raw < 0.05) {
    cat(sprintf("The effect is statistically significant (p %s): older members tend to have %s accounts.\n",
                ifelse(age_row$p_raw < 0.001, "< 0.001", paste("=", age_row$p_value)),
                ifelse(age_coef < 0, "fewer", "more")))
  } else {
    cat(sprintf("The effect is NOT statistically significant (p = %s): once chamber, party and gender\nare held constant, age does not reliably predict how many accounts a member has.\n",
                age_row$p_value))
  }
  cat("Remember: this is an association in one snapshot of Congress, not proof of cause.\n")

  # ---- coefficient plot ----------------------------------------------------
  plot_terms <- coef_table %>% filter(Term != "(Intercept)") %>%
    mutate(Term = factor(Term, levels = rev(Term)))
  p_c <- ggplot(plot_terms, aes(x = Estimate, y = Term)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0.2, orientation = "y") +
    geom_point(aes(colour = p_raw < 0.05), size = 3.2) +
    scale_colour_manual(values = c(`TRUE` = "#b2182b", `FALSE` = "grey40"),
                        labels = c(`TRUE` = "p < 0.05", `FALSE` = "p >= 0.05"), name = NULL) +
    labs(title = "Linear regression: effect on number of social media accounts",
         subtitle = "Points = estimates, bars = 95% confidence intervals (reference: Representative, Democrat, Female)",
         x = "Change in number of accounts", y = NULL) +
    theme_minimal(base_size = 12)
  save_plot(p_c, "regression_coefficients.png", 9, 5)

  # ---------------------------------------------------------------------------
  part("RESULT SUMMARY")
  kv("A. mean age  Female / Male", paste(age_by_gender$Mean_age, collapse = " / "))
  kv("B. correlation age vs. accounts (Pearson)", round(unname(pearson$estimate), 3))
  kv("C. age coefficient (per year)", age_coef)
  kv("C. R-squared of the model", round(summary(model)$r.squared, 4))
  cat("\nModeling a Network - completed successfully.\n")

}, error = function(e) {
  cat("\nERROR:", conditionMessage(e), "\n")
  stop(e)
}, finally = {
  while (sink.number() > 0) sink()
})
