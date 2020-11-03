library("neatStats")
library("MASS")
library("ggpubr")
library("reshape2")
library("correlate")
# note: "correlate" needs to be installed from archived source
# e.g. install.packages("correlate_1.0.tar.gz", repos = NULL, type="source")

theme_set(theme_pubr())

N = 10000

for (cycl in c(1, 2)) {
    # default values
    means_prob_g = 506
    means_irr_g = 476
    sds_prob_g = 46.2
    sds_irr_g = 41.6
    corrs_g = 0.81 # correlation from the 2020 data
    # corrs_g = 0.88 # correlation from the 2017 meta paper
    means_prob_i = 477
    means_irr_i = 475
    sds_prob_i = 40.1
    sds_irr_i = 37.4
    corrs_i = 0.88
    d_const = 0.69
    
        
    ### EFFECT SIZE does not equal MEAN DIFFERENCE
    # 1a same d but different SD and M (for guilty, with constant innocent) -> different AUC
    
    # from 2020 data, 95% confidence limits
    
    if (cycl == 1) {
        sds_prob_g = 43.0
    } else {
        sds_prob_g = 49.5
    }
    
    ### 1b same SD, but different CORRELATION
    
    # from 2017 paper:
    # if (cycl == 1) {
    #     corrs_g = 0.698
    # } else {
    #     corrs_g = 0.880
    # }

    means_pi_diff = d_const * sqrt((sds_prob_g ** 2 + sds_irr_g ** 2) - 2 * corrs_g *
                                       sds_prob_g * sds_irr_g)
    means_prob_g = means_irr_g + means_pi_diff
    
    # calculations
    probes1 = bayestestR::distribution_normal(n = N, mean = means_prob_g, sd = sds_prob_g)
    irrs1 = bayestestR::distribution_normal(n = N, mean = means_irr_g, sd = sds_irr_g)
    pi_data = correlate(cbind(probes1, irrs1), corrs_g)
    probes1 = pi_data[, 1]
    irrs1 = pi_data[, 2]
    p_vs_i_1 = probes1 - irrs1
    
    probes2 = bayestestR::distribution_normal(n = N, mean = means_prob_i, sd = sds_prob_i)
    irrs2 = bayestestR::distribution_normal(n = N, mean = means_irr_i, sd = sds_irr_i)
    pi_data = cbind(probes2, irrs2)
    pi_data = correlate(pi_data, corrs_i)
    probes2 = pi_data[, 1]
    irrs2 = pi_data[, 2]
    p_vs_i_2 = probes2 - irrs2
    
    t_neat(probes1, irrs1, pair = TRUE)
    t_neat(probes2, irrs2, pair = TRUE)
    
    # plots
    
    data_dens = data.frame(probes = probes1, irrs = irrs1)
    data_dens = melt(data_dens)
    plot_1 = ggplot(data = data_dens, aes(
        x = value,
        group = variable,
        fill = variable
    )) + geom_density(alpha = 0.4) +
        theme_bw() +
        theme(
            text = element_text(family = "serif"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        geom_vline(
            xintercept = c(mean(probes1), mean(irrs1), mean(p_vs_i_1)) ,
            color = "grey",
            linetype = "dotted",
            size = 1
        ) +
        scale_fill_grey(start = 0.8, end = 0.0)
    
    data_dens = data.frame(probes = probes2, irrs = irrs2)
    data_dens = melt(data_dens)
    plot_2 = ggplot(data = data_dens, aes(
        x = value,
        group = variable,
        fill = variable
    )) + geom_density(alpha = 0.4) +
        theme_bw() +
        theme(
            text = element_text(family = "serif"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        geom_vline(
            xintercept = c(mean(probes2), mean(irrs2), mean(p_vs_i_2)) ,
            color = "grey",
            linetype = "dotted",
            size = 1
        ) +
        scale_fill_grey(start = 0.8, end = 0.0)
    
    data_dens = data.frame(p_vs_i_1 = p_vs_i_1, p_vs_i_2 = p_vs_i_2)
    data_dens = melt(data_dens)
    plot_3 = ggplot(data = data_dens, aes(
        x = value,
        group = variable,
        fill = variable
    )) + geom_density(alpha = 0.4) +
        theme_bw() +
        theme(
            text = element_text(family = "serif"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        geom_vline(
            xintercept = c(mean(p_vs_i_1), mean(p_vs_i_2)) ,
            color = "grey",
            linetype = "dotted",
            size = 1
        ) +
        scale_fill_grey()
    
    t_info = t_neat(p_vs_i_1, p_vs_i_2, auc_added = T)
    
    # ggarrange( plot_1, plot_2, plot_3, labels = c("A", "B", "C"), nrow = 3)
    
    ##
    if (cycl == 1) {
        plots_list_1 = list(plot_1, plot_2, plot_3, t_info) # save with first settings
    } else {
        plots_list_2 = list(plot_1, plot_2, plot_3, t_info) # save with second settings
        theplot = ggarrange(
            plots_list_1[[1]],
            plots_list_2[[1]],
            plots_list_1[[2]],
            plots_list_2[[2]],
            plots_list_1[[3]],
            plots_list_2[[3]],
            labels = c("A1", "B1", "A2", "B2", "A3", "B3"),
            ncol = 2,
            nrow = 3,
            common.legend = T
        )
        t1 = plots_list_1[[4]]
        t2 = plots_list_2[[4]]
        print("---")
        print(c(t1$stats["d"], t1$stats["auc"], t1$stats["accuracy"]))
        print(c(t2$stats["d"], t2$stats["auc"], t2$stats["accuracy"]))
    }
}

