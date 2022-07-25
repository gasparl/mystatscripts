# define mean estimation function
mean_ci = function(sd, n) {
    z = qnorm(1 - (1 - .95) / 2)
    return((((z ^ 2 * sd ^ 2) / n) ** 0.5) * 2)
}

z_pow = function (r1, r2, n) {
    z1 = 0.5 * log((1 + r1) / (1 - r1))
    z2 = 0.5 * log((1 + r2) / (1 - r2))
    sigma = (1 / (n - 3) + 1 / (n - 3)) ** 0.5
    z = qnorm(1 - (1 - .95) / 2)
    beta = (z1 - z2) / sigma - z
    return(pnorm(beta))
}
z_pow(.84,.63855, 100)

z_r2(.84, 100)

z_r2 = function (r1, n) {
    z1 = 0.5 * log((1 + r1) / (1 - r1))
    sigma = (1 / (n - 3) + 1 / (n - 3)) ** 0.5
    z = qnorm(1 - (1 - .95) / 2)
    z_beta = pnorm(qnorm(1 - .2/2)) # power of .9
    z2 = z1 - sigma * (z_beta + z)
    r2 = (exp(z2 * 2) - 1) / (1 + exp(z2 * 2))
    return(r2)
}

# get meta data
EMGmeta = data.table::fread(neatStats::path_neat("PrEMG_metaanalysis_data.txt"))
EMG_stats = list()
for (colnam in names(EMGmeta)) {
    if (is.numeric(EMGmeta[[colnam]])) {
        EMG_stats[[colnam]] = list(
            x_mean = mean(EMGmeta[[colnam]], na.rm = TRUE),
            x_sd = sd(EMGmeta[[colnam]], na.rm = TRUE)
        )
    }
}

# set sample size
group_size = 100 # 100 or 200
total_size = group_size * 2

## get power

# power for partial response detection based on Koller et al.
pwr::pwr.t.test(
    d =  2.93 / 3.73,
    power = 0.90,
    sig.level = 0.05,
    type = "one.sample",
    alternative = "greater"
)

# Correlation between any two of the variables
d1 = pwr::pwr.t.test(n = group_size, sig.level = .05, power = .9, type = "two")
d2 = pwr::pwr.t.test(n = total_size, sig.level = .05, power = .9, type = "paired")

# Standardized mean difference between any of the variables. See Table 1 for standardized effect sizes in comparison.
pwr::pwr.t.test(power = 0.9,
                n = group_size,
                alternative = "two.sided")
pwr::pwr.t.test(power = 0.9,
                n = total_size,
                alternative = "two.sided")

# TABLE

# note: raw numbers are from the findings of Raud et al.'s main study

power_table = list(

    ## Individual-level variables

    # Response omission rate

    # Incorrect response rate

    # RT mean/median
    RTmean1 = mean_ci(sd = EMG_stats$goRTmean$x_sd, n = total_size),
    RTmean2 = mean_ci(sd = EMG_stats$goRTmean$x_sd, n = group_size),
    RTmean_d = EMG_stats$goRTmean$x_mean / EMG_stats$goRTmean$x_sd,

    # RT variability
    RTsd1 = mean_ci(sd = EMG_stats$goRTsd$x_sd, n = total_size),
    RTsd2 = mean_ci(sd = EMG_stats$goRTsd$x_sd, n = group_size),
    RTsd_d = EMG_stats$goRTsd$x_mean / EMG_stats$goRTsd$x_sd,

    # Response rate on stop trials

    # RT on (unsuccessful) stop trials

    # Stop-signal delay mean
    SSDmean1 = mean_ci(sd = EMG_stats$SSDmean$x_sd, n = total_size),
    SSDmean2 = mean_ci(sd = EMG_stats$SSDmean$x_sd, n = group_size),
    SSDmean_d = EMG_stats$SSDmean$x_mean / EMG_stats$SSDmean$x_sd,

    # Stop-signal delay sd
    SSDsd1 = mean_ci(sd = EMG_stats$SSDsd$x_sd, n = total_size),
    SSDsd2 = mean_ci(sd = EMG_stats$SSDsd$x_sd, n = group_size),
    SSDsd_d = EMG_stats$SSDsd$x_mean / EMG_stats$SSDsd$x_sd,

    # SSRT mean
    SSRTmean1 = mean_ci(sd = EMG_stats$SSRTmean$x_sd, n = total_size),
    SSRTmean2 = mean_ci(sd = EMG_stats$SSRTmean$x_sd, n = group_size),
    SSRTmean_d = EMG_stats$SSRTmean$x_mean / EMG_stats$SSRTmean$x_sd,

    # SSRT sd
    SSRTsd1 = mean_ci(sd = EMG_stats$SSRTsd$x_sd, n = total_size),
    SSRTsd2 = mean_ci(sd = EMG_stats$SSRTsd$x_sd, n = group_size),
    SSRTsd_d = EMG_stats$SSRTsd$x_mean / EMG_stats$SSRTsd$x_sd,

    # TFlatency mean
    TFlatency_mean1 = mean_ci(sd = EMG_stats$EMGlatmean$x_sd, n = total_size),
    TFlatency_mean2 = mean_ci(sd = EMG_stats$EMGlatmean$x_sd, n = group_size),
    TFlatency_mean_d = EMG_stats$EMGlatmean$x_mean / EMG_stats$EMGlatmean$x_sd,

    # TFlatency sd
    TFlatency_sd1 = mean_ci(sd = EMG_stats$EMGlatsd$x_sd, n = total_size),
    TFlatency_sd2 = mean_ci(sd = EMG_stats$EMGlatsd$x_sd, n = group_size),
    TFlatency_sd_d = EMG_stats$EMGlatsd$x_mean / EMG_stats$EMGlatsd$x_sd,

    # TFfreq mean
    TFfreq_mean1 = mean_ci(sd = EMG_stats$EMGfreqmean$x_sd, n = total_size),
    TFfreq_mean2 = mean_ci(sd = EMG_stats$EMGfreqmean$x_sd, n = group_size),
    TFfreq_mean_d = EMG_stats$EMGfreqmean$x_mean / EMG_stats$EMGfreqmean$x_sd,

    # TFfreq sd
    TFfreq_sd1 = mean_ci(sd = EMG_stats$EMGfreqsd$x_sd, n = total_size),
    TFfreq_sd2 = mean_ci(sd = EMG_stats$EMGfreqsd$x_sd, n = group_size),
    TFfreq_sd_d = EMG_stats$EMGfreqsd$x_mean / EMG_stats$EMGfreqsd$x_sd,

    ## Group-level variables:

    # goRT vs. SSRT correlation (r = −0.61)

    # goRT vs. prTF correlation (r = −0.33)

    # SSRT vs. prTF correlation
    RT_x_SSRT1 = presize::prec_cor(
        r = EMG_stats$SSRT_prEMG_r$x_mean,
        n =  group_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_SSRT2 = presize::prec_cor(
        r = EMG_stats$SSRT_prEMG_r$x_mean,
        n =  total_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_SSRT_d = EMG_stats$SSRT_prEMG_r$x_mean
)

# high correlation CI
presize::prec_cor(
    r = 0.8,
    n =  total_size,
    conf.level = .95,
    method = 'pearson'
)$conf.width
