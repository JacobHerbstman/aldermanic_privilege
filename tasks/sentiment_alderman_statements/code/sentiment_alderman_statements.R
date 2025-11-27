rm(list = ls())
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

strictness_scores <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv")

statements <- read_csv("../input/alderman_prerogative_statements.csv")
# Data
df <- statements %>%
  mutate(
    doc_id = row_number(),
    text_raw = prerogative_answer,
    text = text_raw %>% str_to_lower() %>% str_replace_all("[^a-z\\s']", " ") %>% str_squish()
  )

# --- Hyperparameters ----------------------------------------------------------
W_SR <- .5 # weight on negation-aware sentiment (z-scored)
W_DOMAIN <- 0.2 # weight on domain phrase score (z-scored)
W_EARLY <- 4.0 # weight on early-cue score (raw in [-1,1])
GAMMA_LEN <- 0.1 # length penalty strength (subtracts gamma * z(log(1+wc)))
K_DECAY <- 0.8 # exponential decay for early-cue by position
EARLY_WIN <- 5 # only look at first EARLY_WIN tokens for early cue

# --- Custom lexicon for sentimentr (unigrams only here) -----------------------
custom_unigrams_pos <- c(
  "yes", "support", "steward", "advocate",
  "accountable", "influence", "deference", "firmly"
)
custom_unigrams_neg <- c(
  "abuse", "abused", "corrupt", "segregation", "flawed", "block", "blocking", "needs", "unsavory",
  "strip", "stripped", "dilution", "barrier", "trump", "discriminatory", "impacted",
  "citywide", "affordable", "end", "transparent", "limits", "tradition"
)

# 2) Build the keyed data.table with the right column names
polarity_dt <- data.table(
  x = c(custom_unigrams_pos, custom_unigrams_neg),
  y = c(
    rep(1, length(custom_unigrams_pos)),
    rep(-1, length(custom_unigrams_neg))
  )
)
setkey(polarity_dt, x)

# --- Domain phrases (multi-word, regex; counted separately) -------------------
hot_phrases <- c(
  "\\bfinal\\s+say\\b",
  "\\b(do|does|did)\\s+(not|n't)\\s+(have|give)\\s+(the\\s+)?final\\s+say\\b",
  "\\bbest\\s+(representative|steward)\\b",
  "\\blocal\\s+control\\b",
  "\\bcommunity(\\'s)?\\s+wellbeing\\b",
  "\\bresident\\s+prerogative\\b",
  "\\bfirmly\\s+support\\b",
  "\\bworth\\s+keeping\\b",
  "\\b(doing|does)\\s+his\\s+job\\b",
  "\\bdeference\\b",
  "\\bsignificant\\s+influence\\b",
  "\\bseat\\s+at\\s+the\\s+table\\b"
)
cold_phrases <- c(
  "\\bend(ing)?\\s+(the\\s+)?tradition\\b",
  "\\b(do|does|did)\\s+not\\s+support\\b",
  "\\bdon\\'?t\\s+support\\b",
  "\\boppose(s|d)?\\b",
  "\\babelish|eliminat(e|ed|ing)\\b",
  "\\bunsavory\\s+tool\\b",
  "\\blegacy\\s+of\\s+segregation\\b",
  "\\bdiscriminatory\\b"
)

count_phrases <- function(txt, pats) {
  sum(vapply(pats, function(p) str_count(txt, regex(p, ignore_case = TRUE)), numeric(1)))
}

# --- Early-cue score: position-weighted Yes/No in first tokens ----------------
hot_tokens <- c("yes", "yeah", "yep", "affirmative", "support", "pro", "final", "nuanced")
hot_bigrams <- c("i support", "firmly support", "final say", "believe in", "not have", "more knowledge")
cold_tokens <- c("no", "nope", "nah", "oppose", "against", "although", "history", "limits", "corrupt")
cold_bigrams <- c("i oppose", "do not", "don't", "should end", "must end", "end tradition", "hot topic")

early_cue <- function(txt) {
  toks <- unlist(str_split(txt, "\\s+"))
  m <- min(length(toks), EARLY_WIN)
  if (m == 0) {
    return(0)
  }

  for (i in seq_len(m)) {
    one <- toks[i]
    two <- if (i < m) paste(one, toks[i + 1]) else ""
    if (one %in% hot_tokens || two %in% hot_bigrams) {
      return(+exp(-K_DECAY * (i - 1)))
    }
    if (one %in% cold_tokens || two %in% cold_bigrams) {
      return(-exp(-K_DECAY * (i - 1)))
    }
  }
  0
}

# --- Channels -----------------------------------------------------------------
# (1) Negation-aware sentiment (sentimentr)
sent_sr <- sentiment_by(
  get_sentences(df$text),
  by = df$doc_id,
  polarity_dt = polarity_dt
) %>%
  transmute(doc_id, s_sr = ave_sentiment)

# (2) Domain phrase counts (length-normalized)
dom <- df %>%
  mutate(
    wc = str_count(text, boundary("word")),
    hot_ct = vapply(text, count_phrases, numeric(1), pats = hot_phrases),
    cold_ct = vapply(text, count_phrases, numeric(1), pats = cold_phrases),
    s_domain_raw = (hot_ct - cold_ct) / pmax(1, sqrt(wc))
  ) %>%
  transmute(doc_id, wc, s_domain_raw)

# (3) Early cue
early <- df %>% transmute(doc_id, s_early = vapply(text, early_cue, numeric(1)))

# --- Combine & scale ----------------------------------------------------------
scores <- df %>%
  select(doc_id, alderman, ward, text_raw) %>%
  left_join(sent_sr, by = "doc_id") %>%
  left_join(dom, by = "doc_id") %>%
  left_join(early, by = "doc_id") %>%
  mutate(
    # z-score the comparable channels
    z_sr = as.numeric(scale(s_sr)),
    z_domain = as.numeric(scale(s_domain_raw)),
    z_len = as.numeric(scale(log1p(wc))), # length proxy
    # final composite: hot increases, cold decreases
    hot_cold = W_SR * z_sr + W_DOMAIN * z_domain + W_EARLY * s_early - GAMMA_LEN * z_len,
    hot_cold_01 = scales::rescale(hot_cold, to = c(0, 1))
  ) %>%
  arrange(desc(hot_cold)) %>%
  relocate(alderman, hot_cold_01)


correlation_df <- scores %>%
  inner_join(strictness_scores, by = "alderman") %>%
  relocate(alderman, strictness_index, hot_cold_01)

corr <- cor.test(correlation_df$strictness_index, correlation_df$hot_cold_01)
b <- corr$estimate

reg <- feols(hot_cold_01 ~ strictness_index | 0, data = correlation_df)
etable(reg, vcov = "HC1")

# b  <- unname(coef_tab[2, "Estimate"])
# se <- unname(coef_tab[2, "Std. Error"])
# p  <- unname(coef_tab[2, "Pr(>|t|)"])


# # Stars if applicable
# stars_from_p <- function(p){
#   if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**"
#   else if (p < 0.05) "*" else if (p < 0.10) "†" else ""
# }

# Label string
lbl <- sprintf(
  "Correlation = %.2f",
  b
)

xr <- range(correlation_df$strictness_index, na.rm = TRUE)
yr <- range(correlation_df$hot_cold_01, na.rm = TRUE)
x_pos <- xr[1] + 0.02 * diff(xr)
y_pos <- yr[2] - 0.02 * diff(yr)

p <- ggplot(correlation_df, aes(x = strictness_index, y = hot_cold_01)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # place text just outside the top-right corner of the panel
  annotate("text",
    x = Inf, y = Inf, label = lbl,
    hjust = 1, vjust = 2, # nudge right/up into the margin
    size = 3
  ) + # smaller font
  labs(
    title = "Alderman Strictness Validation",
    x = "Alderman Strictness Score (standardized)",
    y = "Prerogative Sentiment Score (0–1)"
  ) +
  coord_cartesian(clip = "off") + # allow drawing outside the panel
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # remove all grid lines
    axis.line = element_line(color = "black", linewidth = 0.4), # keep axes
    panel.border = element_blank()
  )

p
#
ggsave("../output/alderman_strictness_validation.png", p,
  width = 6, height = 4, units = "in", dpi = 300
)
