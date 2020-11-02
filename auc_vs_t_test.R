library("neatStats")
library("MASS")
library("ggpubr")
library("reshape2")
library("correlate")
# note: "correlate" needs to be installed from archived source
# e.g. install.packages("correlate_1.0.tar.gz", repos = NULL, type="source")

theme_set(theme_pubr())

N = 1000

means_prob_g = 506
means_irr_g = 476
sds_prob_g = 46.2
sds_irr_g = 41.6
corrs_g = 0.81

means_prob_i = 477
means_irr_i = 475
sds_prob_i = 40.1
sds_irr_i = 37.4
corrs_i = 0.88



#
sd_pr = 46.2
sd_irr = 41.6
m_irr = 576
pi_corr = 0.81


# 1 same SD and M, but different innocent SD -> different AUC
sd_irr1 = 15
sd_irr2 = 30 # 15 or 30

pi_corr = 0.81

# graphs
set.seed(1)
dat_g <- mvrnorm(
    N,
    mu = c(means_prob_g, means_irr_g),
    Sigma = matrix(
        c(sds_prob_g ** 2, corrs_g,
          corrs_g, sds_irr_g ** 2),
        ncol = 2,
        byrow = TRUE
    ),
    empirical = TRUE
)


r = .6
n = 1000
d_one = bayestestR::distribution_normal(n = n, mean = 70, sd = 25) 
d_two = bayestestR::distribution_normal(n = n, mean = 90, sd = 35) 

data <- cbind(d_one, d_two)
cor(data)
[1] 0.03

library("correlate")
newdata <- correlate(data, 0.8)
cor(newdata)

mean(newdata[,1])
mean(newdata[,2])
sd(newdata[,1])
sd(newdata[,2])
cor(newdata[,1], newdata[,2])
corr_neat(x, y)


dat_g = mvrnorm(N, c(means_prob_g, means_irr_g), matrix(
    c(
        sds_prob_g ** 2,
        corrs_g * sds_prob_g * sds_irr_g,
        corrs_g * sds_prob_g * sds_irr_g,
        sds_irr_g ** 2
    ),
    nrow = 2
),
empirical = TRUE)

mean(dat_g[,1])

probes1 = dat_g[,1]
irrs1 = dat_g[,2]
p_vs_i_1 = probes1 - irrs1

t_neat(probes1, irrs1, pair = TRUE)

dat_corred2 = mvrnorm(N, c(0, 0), matrix(c(sd_irr2**2, pi_corr*sd_irr2*sd_irr2,
                                     pi_corr*sd_irr2*sd_irr2, sd_irr2**2), nrow=2),
                      empirical = TRUE)
probes2 = dat_corred2[,1]
irrs2 = dat_corred2[,2]
p_vs_i_2 = probes2 - irrs2

#t_neat(probes2, irrs2)
toplim = 0.06

data_dens = data.frame( probes = probes1, irrs = irrs1, p_vs_i = p_vs_i_1 )
data_dens = melt(data_dens)
plot_1 = ggplot( data= data_dens, aes(x= value, group = variable, fill = variable ) ) + geom_density( alpha=0.4 )+
    theme_bw()+
    theme( text = element_text( family = "serif" ),
           panel.grid.minor = element_blank(), panel.grid.major = element_blank() ) +
    geom_vline( xintercept= c(mean(probes1), mean(irrs1), mean(p_vs_i_1)) ,
               color="grey", linetype="dotted", size=1) +
    scale_y_continuous(breaks = round(seq(0, 0.05, by = 0.02), 2), limits = c(0, toplim) )+
    scale_x_continuous(breaks = round(seq(-100, 100, by = 50), 2), limits = c(-100, 100) )+
    scale_fill_grey(start = 0.8, end = 0.0)

data_dens = data.frame( probes = probes2, irrs = irrs2, p_vs_i = p_vs_i_2 )
data_dens = melt(data_dens)
plot_2 = ggplot( data= data_dens, aes(x= value, group = variable, fill = variable ) ) + geom_density(  alpha=0.4 )+
    theme_bw()+
    theme( text = element_text( family = "serif" ),
           panel.grid.minor = element_blank(), panel.grid.major = element_blank() ) +
    geom_vline( xintercept= c(mean(probes2), mean(irrs2), mean(p_vs_i_2)) ,
                color="grey", linetype="dotted", size=1)+
scale_y_continuous(breaks = round(seq(0, 0.05, by = 0.02), 2), limits = c(0, toplim) )+
    scale_x_continuous(breaks = round(seq(-100, 100, by = 50), 2), limits = c(-100, 100) )+
    scale_fill_grey(start = 0.8, end = 0.0)

data_dens = data.frame( p_vs_i_1 = p_vs_i_1, p_vs_i_2 = p_vs_i_2 )
data_dens = melt(data_dens)
plot_3 = ggplot( data= data_dens, aes(x= value, group = variable, fill = variable ) ) + geom_density( alpha=0.4 )+
    theme_bw()+
    theme( text = element_text( family = "serif" ),
           panel.grid.minor = element_blank(), panel.grid.major = element_blank() ) +
    geom_vline( xintercept= c( mean(p_vs_i_1), mean(p_vs_i_2)) ,
                color="grey", linetype="dotted", size=1)+
    scale_y_continuous(breaks = round(seq(0, 0.05, by = 0.02), 2), limits = c(0, toplim) )+
    scale_x_continuous(breaks = round(seq(-100, 100, by = 50), 2), limits = c(-100, 100) )+
    scale_fill_grey()

t_info = t_neat( p_vs_i_1, p_vs_i_2, auc_added = T )


ggarrange( plot_1, plot_2, plot_3, labels = c("A", "B", "C"), nrow = 3)

##

plots_list_1 = list(plot_1, plot_2, plot_3, t_info) # save with first settings
plots_list_2 = list(plot_1, plot_2, plot_3, t_info) # save with second settings

ggarrange( plots_list_1[[1]], plots_list_2[[1]], plots_list_1[[2]], plots_list_2[[2]], plots_list_1[[3]], plots_list_2[[3]], labels = c("A1", "B1", "A2", "B2", "A3", "B3"), ncol=2, nrow = 3, common.legend = T)

t1 = plots_list_1[[4]]
t2 = plots_list_2[[4]]
c( t1$stats["d"], t1$stats["auc"], t1$stats["accuracy"] )
c( t2$stats["d"], t2$stats["auc"], t2$stats["accuracy"] )

# 2 same d but different SD and M (for guilty, with constant innocent) -> different AUC


d_const = 1.0

pi_corr = 0.90  # 0.88

means_prob_g = d_const * sqrt( (sd_pr**2 + sd_irr1**2)- 2*pi_corr*sd_pr*sd_irr1 )


# 3 same effect but different SD ratio -> same AUC -> but different accuracy

N = 1000
sd_g = 20 # 20
sd_i = 20 # 15
d_const = 1.0

plot_list = list()
for ( sd_pair in list( c(20, 20, 1), c(30, 10, 2)  ) ) {
    sd_g = sd_pair[1]
    sd_i = sd_pair[2]
    indx = sd_pair[3]

    m_g = d_const * ( ( (sd_g**2 + sd_i**2)/2  ) **0.5 )

    #g_preds = mvrnorm(N, mu = m_g, Sigma = sd_g**2, empirical = TRUE )
    #i_preds = mvrnorm(N, mu = 0, Sigma = sd_i**2, empirical = TRUE )

    g_preds = bayestestR::distribution_normal( n = N, mean = m_g, sd = sd_g )
    i_preds = bayestestR::distribution_normal( n = N, mean = 0, sd = sd_i )

    data_dens = data.frame( guilty = g_preds, innocent = i_preds )
    data_dens = melt(data_dens)
    info = t_neat( g_preds, i_preds, auc_added = T )
    thres = info$best_thresholds[ "threshold", ]

    the_plot = ggplot( data= data_dens, aes(x= value, group = variable, fill = variable ) ) + geom_density( alpha=0.4 )+
        theme_bw()+
        theme( text = element_text( family = "serif" ),
               panel.grid.minor = element_blank(), panel.grid.major = element_blank() ) +
        geom_vline( xintercept= c( mean(g_preds), mean(i_preds) ) ,
                    color="grey", linetype="dotted", size=1)+
        geom_vline( xintercept= c(  thres ) ,
                    color="black", linetype="solid", size= 0.2 ) +
        scale_y_continuous(breaks = round(seq(0, 0.05, by = 0.01), 2), limits = c(0, 0.04) )+
        scale_x_continuous(breaks = round(seq(-100, 100, by = 25), 2), limits = c(-100, 100) )+
        scale_fill_grey()

    print(info$stats["accuracy"])

    plot_list[[indx]] = the_plot
}

ggarrange(plot_list[[1]], plot_list[[2]], labels = c("A", "B"), nrow = 2)



####### --- TODO - change correlation?
