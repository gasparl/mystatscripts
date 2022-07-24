# define mean estimation function
mean_ci = function(sd, n) {
    z = qnorm(1 - (1 - .95) / 2)
    return((((z ^ 2 * sd ^ 2) / n) ** 0.5) * 2)
}

for (nn in c(20,50,70)) {
    vv = 10
    rr = mean_ci(sd = vv, n = nn)
    cat("---- mine: ", rr, fill = T)
    print(samplingbook::sample.size.mean(e = rr/2, S = vv))
}


# get meta data
EMGmeta = data.table::fread(neatStats::path_neat("PrEMG_metaanalysis_data.txt"))
EMG_stats = list()
for (colnam in names(EMGmeta)) {
    if (is.numeric(EMGmeta[[colnam]])) {
        EMG_stats[[colnam]] = c(
            x_mean = mean(EMGmeta[[colnam]], na.rm = TRUE),
            x_sd = sd(EMGmeta[[colnam]], na.rm = TRUE)
        )
    }
}

# set sample size
group_size = 100 # 100 or 200
total_size = group_size*2

## get power

# All power calculations relate to individual-level aggregate values. For instance, "RT variability" refers to the one individual SDs per participant, calculated from each given participant's responses. Then these variables can be correlated between two conditions, and their group-level means (and SDs) can be compared with a single t-test (or variance test) each.


# Correlation between any two of the variables
pwr::pwr.r.test(n = total_size, power = 0.9)
pwr::pwr.r.test(n = group_size, power = 0.9)

# Standardized mean difference between any of the variables; see Table 1 for raw effect sizes
pwr::pwr.t.test(
    power = 0.9,
    n = group_size,
    alternative = "two.sided"
)
pwr::pwr.t.test(
    power = 0.9,
    n = total_size,
    alternative = "two.sided"
)

# TABLE

names(EMG_stats)
# Response omission rate

# Incorrect response rate

# RT mean/median
sample.size.mean(5, N = group_size, level = 0.95)


# RT variability

# Response rate on stop trials

# RT on (unsuccessful) stop trials

# Average stop-signal delay

# SSRT

# Split-half reliability


## Group-level variables:

# goRT vs. SSRT correlation (r = −0.61)

# goRT vs. prTF correlation (r = −0.33)


