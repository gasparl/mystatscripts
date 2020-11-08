library("neatStats")
library("MASS")
library("ggpubr")
library("correlate")
# note: "correlate" needs to be installed from archived source
# e.g. install.packages("correlate_1.0.tar.gz", repos = NULL, type="source")

theme_set(theme_pubr())

N = 10000
set.seed(1)

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
    
    ### 1a same SD, but different CORRELATION (for guilty, with constant innocent)
    
    # from 2020 data:
    if (cycl == 1) {
        corrs_g = 0.74
        corrs_i = 0.88
    } else {
        corrs_g = 0.88
        corrs_i = 0.88
    }
    
    # from 2017 paper:
    # if (cycl == 1) {
    #     corrs_g = 0.698
    #     corrs_i = (0.698+0.880)/2
    # } else {
    #     corrs_g = 0.880
    #     corrs_i = (0.698+0.880)/2
    # }
    
    # 1b same d but different SD and M (for guilty, with constant innocent) -> different AUC
    
    # from 2020 data, 95% confidence limits
    
    # if (cycl == 1) {
    #     sds_prob_g = 43.0
    # } else {
    #     sds_prob_g = 49.5
    # }


    means_pi_diff = d_const * sqrt((sds_prob_g ** 2 + sds_irr_g ** 2) - 2 * corrs_g *
                                       sds_prob_g * sds_irr_g)
    means_prob_g = means_irr_g + means_pi_diff
    
    # calculations
    stddev <- c(sds_prob_g, sds_irr_g)
    corMat <- matrix(c(1, corrs_g,
                       corrs_g, 1),
                     ncol = 2)
    covMat <- stddev %*% t(stddev) * corMat
    
    pi_data <- mvrnorm(n = N, mu = c(means_prob_g, means_irr_g), Sigma = covMat, empirical = TRUE)
    probes1 = pi_data[, 1]
    irrs1 = pi_data[, 2]
    p_vs_i_1 = probes1 - irrs1
    #
    stddev <- c(sds_prob_i, sds_irr_i)
    corMat <- matrix(c(1, corrs_i,
                       corrs_i, 1),
                     ncol = 2)
    covMat <- stddev %*% t(stddev) * corMat
    
    pi_data <- mvrnorm(n = N, mu = c(means_prob_i, means_irr_i), Sigma = covMat, empirical = TRUE)
    probes2 = pi_data[, 1]
    irrs2 = pi_data[, 2]
    p_vs_i_2 = probes2 - irrs2
    
    print(" -- GUILTY --")
    t_neat(probes1, irrs1, pair = TRUE)
    print("INNOCENT")
    t_neat(probes2, irrs2, pair = TRUE)
    
    # plots
    
    print(" -- SMD between --")
    t_info = t_neat(p_vs_i_1, p_vs_i_2, auc_added = T, plots = T,
                    x_label = NULL,
                    var_names = c('liar', 'truthteller'),
                    reverse = F, aspect_ratio = 0.8)
    
    # ggarrange( plot_1, plot_2, plot_3, labels = c("A", "B", "C"), nrow = 3)
    
    ##
    limits_x =  c(-100, 150)
    limits_y = c(0, 0.0047)
    if (cycl == 1) {
        fig1 = t_info$t_plot +
            scale_x_continuous(limits = limits_x,
                               breaks = seq(-50, 50, by = 50)) + 
            scale_y_continuous(limits = limits_y) + scale_fill_manual(name = NULL, values = c('#006600', '#b3b3ff'))
        t_info1 = t_info
    } else {
        fig2 = t_info$t_plot +
            scale_x_continuous(limits = limits_x,
                               breaks = seq(-50, 50, by = 50)) + 
            scale_y_continuous(limits = limits_y) + scale_fill_manual(name = NULL, values = c('#006600', '#b3b3ff'))
        t_info2 = t_info
        print("---")
        print(round(c(
            t_info1$stats["d"], t_info1$stats["auc"], t_info1$stats["accuracy"]
        ), 3))
        print(round(c(
            t_info2$stats["d"], t_info2$stats["auc"], t_info2$stats["accuracy"]
        ), 3))
        show(ggpubr::annotate_figure(
            ggpubr::ggarrange(
                fig1,
                fig2,
                ncol = 1,
                nrow = 2,
                common.legend = T,
                labels = c('A', 'B'),
                font.label = list(size = 20),
                hjust = -0.9,
                vjust = 1
            ),
            bottom = ggpubr::text_grob(
                "probe-control difference values",
                hjust = 0.4,
                vjust = 0.3,
                size = 16,
                family = 'serif'
            )
        ))
    }
}

