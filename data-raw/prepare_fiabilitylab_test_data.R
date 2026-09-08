# -----------------------------------------------------------------------------
# FiabilityLab - Test dataset generator
# ES: Generador de la base de datos de prueba de FiabilityLab.
#
# Builds fiabilitylab_test_data.csv (n = 450), a single wide file with a
# purpose-built block of columns for every reliability design FiabilityLab
# targets: dichotomous parallel test forms, a hierarchical (2nd-order)
# Likert scale, multi-rater nominal/ordinal/continuous agreement data, and
# a test-retest block with realistic attrition. Every block is simulated
# from an explicit latent-variable model (not pure noise), so every module
# has a real signal to recover, and several blocks carry deliberate,
# documented data-quality flaws (a weak item, a forgotten reverse-code, a
# biased rater, skewed category prevalence, MCAR missingness, retest
# dropout) that exercise the diagnostic/interpretation engine, not just
# the point estimate.
#
# ES: Construye fiabilitylab_test_data.csv (n = 450), un archivo ancho con
# un bloque de columnas dedicado a cada diseño de confiabilidad que
# FiabilityLab cubre: formas paralelas dicotómicas, una escala Likert
# jerárquica (2do orden), datos de acuerdo entre jueces nominal/ordinal/
# continuo, y un bloque test-retest con deserción realista. Cada bloque se
# simula desde un modelo de variable latente explícito, y varios bloques
# llevan fallas de calidad de datos deliberadas y documentadas (un ítem
# débil, un ítem sin recodificar en reversa, un juez sesgado, prevalencia
# de categoría sesgada, datos faltantes MCAR, deserción en el retest) para
# ejercitar el motor diagnóstico/interpretativo, no solo el punto estimado.
#
# Regenerate with:
#   Rscript data-raw/prepare_fiabilitylab_test_data.R
# -----------------------------------------------------------------------------

set.seed(20260908)
n <- 450

inject_mcar <- function(x, prop = 0.02) {
    n_na <- round(length(x) * prop)
    if (n_na > 0) x[sample(seq_along(x), n_na)] <- NA
    x
}

gen_likert_item <- function(factor_scores, loading, thresholds = qnorm(c(.10, .30, .60, .85))) {
    resid_sd <- sqrt(max(1 - loading^2, 0.05))
    latent <- loading * factor_scores + rnorm(length(factor_scores), 0, resid_sd)
    cats <- cut(latent, breaks = c(-Inf, thresholds, Inf), labels = 1:5)
    as.integer(as.character(cats))
}

# -----------------------------------------------------------------------------
# 1. Demographics
# ES: 1. Datos demográficos.
# -----------------------------------------------------------------------------

id <- seq_len(n)

sex <- sample(c("Female", "Male"), n, replace = TRUE, prob = c(.52, .48))

age_bin_labels <- c("18-25", "26-35", "36-45", "46-55", "56-65", "66-70")
age_bin_ranges <- list(c(18, 25), c(26, 35), c(36, 45), c(46, 55), c(56, 65), c(66, 70))
age_range <- sample(age_bin_labels, n, replace = TRUE, prob = c(.14, .27, .26, .18, .11, .04))
age_range[1:6] <- age_bin_labels # force first-appearance order for jamovi

age <- vapply(age_range, function(lbl) {
    rng <- age_bin_ranges[[match(lbl, age_bin_labels)]]
    sample(rng[1]:rng[2], 1)
}, numeric(1))

education_level <- sample(
    c("Secondary", "Technical", "Undergraduate", "Graduate"),
    n, replace = TRUE, prob = c(.15, .25, .40, .20)
)
education_level[1:4] <- c("Secondary", "Technical", "Undergraduate", "Graduate") # force first-appearance order

marital_status <- sample(
    c("Single", "Married/Cohabiting", "Divorced/Separated", "Widowed"),
    n, replace = TRUE, prob = c(.40, .42, .13, .05)
)

site <- sample(c("Site A", "Site B", "Site C"), n, replace = TRUE, prob = c(.40, .35, .25))

educ_num <- c(Secondary = 0, Technical = 1, Undergraduate = 2, Graduate = 3)[education_level]

# -----------------------------------------------------------------------------
# 2. Dichotomous parallel test forms (knowledge/aptitude test)
# ES: 2. Formas paralelas dicotómicas (prueba de conocimientos/aptitud).
#
# One latent ability theta_k drives both forms, with a modest education
# effect and independent form-specific noise, so Form A and Form B
# correlate strongly but not perfectly (realistic parallel-forms
# reliability, not r = 1). Two items per form are deliberately weak
# (low discrimination) to exercise item-total-correlation diagnostics.
# -----------------------------------------------------------------------------

theta_k <- rnorm(n, mean = 0.15 * scale(educ_num)[, 1], sd = 1)
theta_ka <- theta_k + rnorm(n, 0, 0.30)
theta_kb <- theta_k + rnorm(n, 0, 0.30)

gen_dichot_form <- function(theta, discrim, difficulty) {
    resp <- matrix(NA_integer_, nrow = length(theta), ncol = length(discrim))
    for (i in seq_along(discrim)) {
        p <- plogis(discrim[i] * (theta - difficulty[i]))
        resp[, i] <- rbinom(length(theta), 1, p)
    }
    resp
}

discrim_a   <- c(1.4, 1.1, 1.3, 0.9, 1.2, 1.0, 1.3, 0.25, 1.1, 0.30)
difficulty_a <- seq(-1.4, 1.4, length.out = 10)
form_a <- gen_dichot_form(theta_ka, discrim_a, difficulty_a)
colnames(form_a) <- sprintf("knowledge_a%02d", 1:10)

discrim_b    <- c(1.3, 1.2, 1.0, 1.1, 0.9, 1.4, 1.0, 1.2, 0.28, 1.1)
difficulty_b <- seq(-1.3, 1.5, length.out = 10)
form_b <- gen_dichot_form(theta_kb, discrim_b, difficulty_b)
colnames(form_b) <- sprintf("knowledge_b%02d", 1:10)

for (col in colnames(form_a)) form_a[, col] <- inject_mcar(form_a[, col], .02)
for (col in colnames(form_b)) form_b[, col] <- inject_mcar(form_b[, col], .02)

# -----------------------------------------------------------------------------
# 3. Hierarchical (2nd-order) Likert scale: Academic Self-Regulation
# ES: 3. Escala Likert jerárquica (2do orden): Autorregulación Académica.
#
# Three first-order factors (Planning, Monitoring, Reflection) load onto
# one second-order global factor. planning_05 is deliberately a weak item
# (low loading) and reflection_04 is a reverse-worded item generated with
# a negative loading and left un-reverse-coded, on purpose, to exercise
# the item-analysis and alpha-if-dropped diagnostics on a realistic
# "forgotten to reverse-score" case.
# -----------------------------------------------------------------------------

g <- rnorm(n, mean = 0.15 * scale(age)[, 1], sd = 1) # older -> slightly higher self-regulation

lambda_g <- c(planning = 0.75, monitoring = 0.70, reflection = 0.65)
f_planning   <- lambda_g["planning"]   * g + rnorm(n, 0, sqrt(1 - lambda_g["planning"]^2))
f_monitoring <- lambda_g["monitoring"] * g + rnorm(n, 0, sqrt(1 - lambda_g["monitoring"]^2))
f_reflection <- lambda_g["reflection"] * g + rnorm(n, 0, sqrt(1 - lambda_g["reflection"]^2))

planning_loadings   <- c(0.80, 0.75, 0.78, 0.72, 0.30)          # item 5: weak/noisy item
monitoring_loadings <- c(0.82, 0.77, 0.74, 0.79, 0.76)
reflection_loadings <- c(0.70, 0.73, 0.68, -0.66)               # item 4: reverse-worded, NOT reverse-scored

planning <- sapply(planning_loadings, function(l) gen_likert_item(f_planning, l))
colnames(planning) <- sprintf("planning_%02d", 1:5)

monitoring <- sapply(monitoring_loadings, function(l) gen_likert_item(f_monitoring, l))
colnames(monitoring) <- sprintf("monitoring_%02d", 1:5)

reflection <- sapply(reflection_loadings, function(l) gen_likert_item(f_reflection, l))
colnames(reflection) <- sprintf("reflection_%02d", 1:4)

for (col in colnames(planning))   planning[, col]   <- inject_mcar(planning[, col], .02)
for (col in colnames(monitoring)) monitoring[, col] <- inject_mcar(monitoring[, col], .02)
for (col in colnames(reflection)) reflection[, col] <- inject_mcar(reflection[, col], .02)

# -----------------------------------------------------------------------------
# 4. Multi-rater agreement block (nominal / ordinal / continuous)
# ES: 4. Bloque de acuerdo entre jueces (nominal / ordinal / continuo).
#
# Same 450 cases rated by several raters under three measurement levels.
# Category prevalence is deliberately skewed (mostly "Low") to reproduce
# the Kappa/Gwet-AC1 "prevalence paradox" already flagged in the Library
# content. rater3_score carries a deliberate +8-point mean bias, so
# consistency-type ICC stays high while absolute-agreement-type ICC drops
# -- a real contrast for the two ICC forms to show.
# -----------------------------------------------------------------------------

true_category <- sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(.65, .25, .10))

rate_category <- function(true_cat, acc, levels = c("Low", "Medium", "High")) {
    idx <- match(true_cat, levels)
    out <- true_cat
    wrong <- runif(n) > acc
    neighbor <- pmin(pmax(idx[wrong] + sample(c(-1, 1), sum(wrong), replace = TRUE), 1), length(levels))
    out[wrong] <- levels[neighbor]
    out
}

rater1_category <- rate_category(true_category, .88)
rater2_category <- rate_category(true_category, .85)
rater3_category <- rate_category(true_category, .80)
rater4_category <- rate_category(true_category, .70) # least accurate rater

true_severity <- sample(1:5, n, replace = TRUE, prob = c(.10, .25, .30, .25, .10))
round_clip <- function(x, lo = 1, hi = 5) pmin(pmax(round(x), lo), hi)
rater1_severity <- round_clip(true_severity + rnorm(n, 0, 0.5))
rater2_severity <- round_clip(true_severity + rnorm(n, 0, 0.6))
rater3_severity <- round_clip(true_severity + 0.6 + rnorm(n, 0, 0.6)) # systematic +shift before rounding

true_score <- pmin(pmax(rnorm(n, 65, 12), 0), 100)
rater1_score <- pmin(pmax(true_score + rnorm(n, 0, 4), 0), 100)
rater2_score <- pmin(pmax(true_score + rnorm(n, 0, 6), 0), 100)
rater3_score <- pmin(pmax(true_score + 8 + rnorm(n, 0, 5), 0), 100) # biased rater (+8 mean shift)

rater1_score <- inject_mcar(rater1_score, .02)
rater2_score <- inject_mcar(rater2_score, .02)
rater3_score <- inject_mcar(rater3_score, .02)

# -----------------------------------------------------------------------------
# 5. Test-retest block: Mood State scale (Time 1 / Time 2)
# ES: 5. Bloque test-retest: escala de Estado de Ánimo (Tiempo 1 / Tiempo 2).
#
# One unidimensional 6-item scale measured twice, ~2-week interval.
# Latent stability r = .72 at the factor level. ~30% of respondents have
# no Time 2 data (dropout), a realistic retest attrition rate rather than
# a complete panel.
# -----------------------------------------------------------------------------

theta_t1 <- rnorm(n)
theta_t2 <- 0.72 * theta_t1 + rnorm(n, 0, sqrt(1 - 0.72^2)) + 0.05 # small practice effect

mood_loadings <- c(0.75, 0.78, 0.70, 0.73, 0.68, 0.71)

mood_t1 <- sapply(mood_loadings, function(l) gen_likert_item(theta_t1, l))
colnames(mood_t1) <- sprintf("mood_t1_%02d", 1:6)

mood_t2 <- sapply(mood_loadings, function(l) gen_likert_item(theta_t2, l))
colnames(mood_t2) <- sprintf("mood_t2_%02d", 1:6)

dropout <- sample(id, size = round(.30 * n))
mood_t2[dropout, ] <- NA

# -----------------------------------------------------------------------------
# 6. Assemble & write
# ES: 6. Ensamblar y escribir.
# -----------------------------------------------------------------------------

dat <- data.frame(
    id, sex, age, age_range, education_level, marital_status, site,
    form_a, form_b,
    planning, monitoring, reflection,
    rater1_category, rater2_category, rater3_category, rater4_category,
    rater1_severity, rater2_severity, rater3_severity,
    rater1_score, rater2_score, rater3_score,
    mood_t1, mood_t2,
    stringsAsFactors = FALSE
)

out_path <- file.path("data", "fiabilitylab_test_data.csv")
write.csv(dat, out_path, row.names = FALSE, na = "")

cat("Wrote", nrow(dat), "rows x", ncol(dat), "columns to", out_path, "\n")
