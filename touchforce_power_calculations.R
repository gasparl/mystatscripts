# define mean estimation function
mean_ci = function(sd, n) {
    z = qnorm(1 - (1 - .95) / 2)
    return((((z ^ 2 * sd ^ 2) / n) ** 0.5) * 2)
}

# get meta data
EMGmeta = data.table::fread(neatStats::path_neat("PrEMG_metaanalysis_data.txt"))
EMG_stats = list()
for (colnam in names(EMGmeta)) {
    if (is.numeric(EMGmeta[[colnam]])) {
        EMG_stats[[colnam]] = mean(EMGmeta[[colnam]], na.rm = TRUE)
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

# comparing two correlations
z_r2(.6, group_size)
z_r2(.6, total_size)

# TABLE

# note: raw numbers are from the findings of Raud et al.'s main study

power_table = list(

    ## Individual-level variables

    # Response omission rate
    omit_rate1 = mean_ci(sd = 1.74 , n = total_size),
    omit_rate2 = mean_ci(sd = 1.74 , n = group_size),
    omit_rate_d = 1.50 / 1.74,

    # Incorrect response rate
    error_rate1 = mean_ci(sd = 1.07, n = total_size),
    error_rate2 = mean_ci(sd = 1.07, n = group_size),
    error_rate_d = 0.66 / 1.07,

    # RT mean/median
    RTmean1 = mean_ci(sd = EMG_stats$goRTsd, n = total_size),
    RTmean2 = mean_ci(sd = EMG_stats$goRTsd, n = group_size),
    RTmean_d = EMG_stats$goRTmean / EMG_stats$goRTsd,

    # Response rate on stop trials
    stop_error1 = mean_ci(sd = 2.09, n = total_size),
    stop_error2 = mean_ci(sd = 2.09, n = group_size),
    stop_error_d = 50 / 2.09,

    # RT on (unsuccessful) stop trials
    stop_RT1 = mean_ci(sd = 72.64, n = total_size),
    stop_RT2 = mean_ci(sd = 72.64, n = group_size),
    stop_RT_d = 441.05 / 72.64,

    # Stop-signal delay mean
    SSDmean1 = mean_ci(sd = EMG_stats$SSDsd, n = total_size),
    SSDmean2 = mean_ci(sd = EMG_stats$SSDsd, n = group_size),
    SSDmean_d = EMG_stats$SSDmean / EMG_stats$SSDsd,

    # SSRT mean
    SSRTmean1 = mean_ci(sd = EMG_stats$SSRTsd, n = total_size),
    SSRTmean2 = mean_ci(sd = EMG_stats$SSRTsd, n = group_size),
    SSRTmean_d = EMG_stats$SSRTmean / EMG_stats$SSRTsd,

    # TFlatency mean
    TFlatency_mean1 = mean_ci(sd = EMG_stats$EMGlatsd, n = total_size),
    TFlatency_mean2 = mean_ci(sd = EMG_stats$EMGlatsd, n = group_size),
    TFlatency_mean_d = EMG_stats$EMGlatmean / EMG_stats$EMGlatsd,

    # TFfreq mean
    TFfreq_mean1 = mean_ci(sd = EMG_stats$EMGfreqsd, n = total_size),
    TFfreq_mean2 = mean_ci(sd = EMG_stats$EMGfreqsd, n = group_size),
    TFfreq_mean_d = EMG_stats$EMGfreqmean / EMG_stats$EMGfreqsd,

    ## Group-level variables:

    # goRT vs. SSRT correlation (r = −0.61)
    RT_x_SSRT1 = presize::prec_cor(
        r = 0.61,
        n =  group_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_SSRT2 = presize::prec_cor(
        r = 0.61,
        n =  total_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_SSRT_d = 0.61,

    # goRT vs. prTF correlation (r = −0.33)
    RT_x_EMG1 = presize::prec_cor(
        r = 0.33,
        n =  group_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_EMG2 = presize::prec_cor(
        r = 0.33,
        n =  total_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_EMG_d = 0.33,


    # goRT vs. prTF frequency correlation (r = −0.33)
    RT_x_EMGfreq1 = presize::prec_cor(
        r = 0.35,
        n =  group_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_EMGfreq2 = presize::prec_cor(
        r = 0.35,
        n =  total_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    RT_x_EMGfreq_d = 0.35,

    # SSRT vs. prTF correlation
    EMG_x_SSRT1 = presize::prec_cor(
        r = EMG_stats$SSRT_prEMG_r,
        n =  group_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    EMG_x_SSRT2 = presize::prec_cor(
        r = EMG_stats$SSRT_prEMG_r,
        n =  total_size,
        conf.level = .95,
        method = 'pearson'
    )$conf.width,
    EMG_x_SSRT_d = EMG_stats$SSRT_prEMG_r
)

# high correlation CI
presize::prec_cor(
    r = 0.8,
    n =  total_size,
    conf.level = .95,
    method = 'pearson'
)$conf.width


# power for comparing two correlations
pwr2ppl::depcorr0(
    r12 = .7,
    rxy = .824,
    r1x = .5,
    r1y = .5,
    r2x = .5,
    r2y = .5,
    nlow = 200,
    nhigh = 200
)
pwr2ppl::indcorr(
    r1 = 0.7,
    r2 = 0.91,
    nlow = 100,
    nhigh = 100
)
