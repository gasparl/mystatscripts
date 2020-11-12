library('neatStats')
library('bayestestR')
library('ggplot2')
# library('sn') # for skewness only

calc_diffs_aucs = function(g_sd_lower,
                           i_sd_lower,
                           g_sd_upper,
                           i_sd_upper,
                           m_g_diff = 0,
                           m_g_avg = 36.78897,
                           limits_x =  c(-100, 150),
                           limits_y = c(0, 0.005),
                           smpl = 10000,
                           skewed = FALSE,
                           guilty_gamma = 0,
                           innocent_gamma = 0) {
    m_g_lower =  m_g_avg - m_g_diff / 2
    m_g_upper = m_g_avg + m_g_diff / 2
    
    # smaller g-i difference but smaller SDs:
    g_1 = distribution_normal(n = smpl, sd = g_sd_lower, mean = m_g_lower)
    i_1 = distribution_normal(n = smpl, sd = i_sd_lower, mean = 0)
    
    # larger g-i difference but larger SDs:
    g_2 = distribution_normal(n = smpl, sd = g_sd_upper, mean = m_g_upper)
    i_2 = distribution_normal(n = smpl, sd = i_sd_upper, mean = 0)
    
    if (skewed != FALSE) {
        
        cp <- c(mean = m_g_lower, s.d. = g_sd_lower, gamma1 = guilty_gamma)
        g_1 = rsn(
            n = smpl,
            dp = cp2dp(cp, family="SN")
        )
        cp <- c(mean = 0, s.d. = i_sd_lower, gamma1 = innocent_gamma)
        i_1 = rsn(
            n = smpl,
            dp = cp2dp(cp, family="SN")
        )
        if (skewed == 2) {
            cp <- c(mean = m_g_upper, s.d. = g_sd_upper, gamma1 = guilty_gamma)
            g_2 = rsn(
                n = smpl,
                dp = cp2dp(cp, family="SN")
            )
            cp <- c(mean = 0, s.d. = i_sd_upper, gamma1 = innocent_gamma)
            i_2 = rsn(
                n = smpl,
                dp = cp2dp(cp, family="SN")
            )
        }
            
    }
    cat('\n1. Smaller guilty-innocent difference but smaller SDs:',
        fill = T)
    cat(
        '- Guilty mean:',
        m_g_lower,
        ' Guilty SD:',
        g_sd_lower,
        ' Innocent SD:',
        i_sd_lower,
        fill = T
    )
    t1 = t_neat(
        g_1,
        i_1,
        auc_added = T,
        bf_added = F,
        plots = T,
        x_label = NULL,
        var_names = c('liar', 'truthteller'),
        reverse = F, aspect_ratio = 0.8
    )
    roc1 <<- t1$roc_obj
    fig1 = t1$t_plot +
        scale_x_continuous(limits = limits_x,
                           breaks = seq(-50, 50, by = 50)) + 
        scale_y_continuous(limits = limits_y) + scale_fill_manual(name = NULL, values = c('#006600', '#b3b3ff'))
    
    cat('\n2. Larger guilty-innocent difference but larger SDs:',
        fill = T)
    cat(
        '- Guilty mean:',
        m_g_upper,
        ' Guilty SD:',
        g_sd_upper,
        ' Innocent SD:',
        i_sd_upper,
        fill = T
    )
    t2 = t_neat(
        g_2,
        i_2,
        auc_added = T,
        bf_added = F,
        plots = T,
        x_label = NULL,
        var_names = c('liar', 'truthteller'),
        reverse = F, aspect_ratio = 0.8
    )
    roc2 <<- t2$roc_obj
    fig2 = t2$t_plot  +
        scale_x_continuous(limits = limits_x,
                           breaks = seq(-50, 50, by = 50)) +
        scale_y_continuous(limits = limits_y) +
        scale_fill_manual(name = NULL, values = c('#006600', '#b3b3ff'))

    cat('\nDifferences between two guilty groups:', fill = T)
    t_neat(g_1, g_2, bf_added = F)
    
    endfig <<- ggpubr::annotate_figure(
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
            "predictor values",
            hjust = 0.4,
            vjust = 0.3,
            size = 16,
            family = 'serif'
        )
    )
    show(endfig)
    
    #fx1 <<- fig1
    #fx2 <<- fig2
    
    cat("---", fill = TRUE)
    cat(
        'CDR = ',
        ro(t1$stats["accuracy"], 3, leading_zero = FALSE),
        ', AUC = ',
        ro(t1$stats["auc"], 3, leading_zero = FALSE),
        ' (B-SMD = ',
        ro(t1$stats["d"], 3),
        ')',
        fill = TRUE,
        sep = ''
    )
    cat(
        'CDR = ',
        ro(t2$stats["accuracy"], 3, leading_zero = FALSE),
        ', AUC = ',
        ro(t2$stats["auc"], 3, leading_zero = FALSE),
        ' (B-SMD = ',
        ro(t2$stats["d"], 3),
        ')',
        fill = TRUE,
        sep = ''
    )
}

# LARGER MEAN DIFFERENCE does not mean larger AUC or accuracy
# 2 same SD and M, but different innocent SD -> different AUC


# liar min, max:
calc_diffs_aucs(
    g_sd_lower = 33.61352, # (29.59950+37.62754)/2
    i_sd_lower = 15.23694,
    g_sd_upper = 33.61352, # (29.59950+37.62754)/2
    i_sd_upper = 29.47704,
    limits_y = c(0, 0.0063)
)

# both min, max:
calc_diffs_aucs(
    g_sd_lower = 19.87472,
    i_sd_lower = 15.23694,
    g_sd_upper = 43.64081,
    i_sd_upper = 29.47704,
    limits_y = c(0, 0.0063)
)

# 95% CI:
calc_diffs_aucs(
    g_sd_lower = 33.61352, # (29.59950+37.62754)/2
    i_sd_lower = 20.88514,
    g_sd_upper = 33.61352, # (29.59950+37.62754)/2
    i_sd_upper = 26.12452,
    limits_y = c(0, 0.0047)
)


setwd(path_neat('Figs'))

# ggplot2::ggsave(
#     filename = 'Figure3.pdf',
#     endfig,
#     units = "mm",
#     width = 118,
#     height = 150,
#     dpi = 600,
#     device = cairo_pdf
# )
