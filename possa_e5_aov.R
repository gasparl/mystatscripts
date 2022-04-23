# library('POSSA')

sampleAOV = function(samp_size) {
  dat_h0_human = faux::rnorm_multi(
    n = samp_size,
    vars = 2,
    mu = 1,
    sd = 1.03,
    r = 0.8
  )
  dat_h0_robot = faux::rnorm_multi(
    n = samp_size,
    vars = 2,
    mu = 1,
    sd = 1.03,
    r = 0.8
  )
  dat_h1_human = faux::rnorm_multi(
    n = samp_size,
    vars = 2,
    mu = c(1.03, 1.41),
    sd = 1.03,
    r = 0.8
  )
  dat_h1_robot = faux::rnorm_multi(
    n = samp_size,
    vars = 2,
    mu = c(0.98, 1.01),
    sd = 1.03,
    r = 0.8
  )
  list(
    # human_cheerful
    grp_1_human_cheerful_h0 = dat_h0_human$X1,
    grp_1_human_cheerful_h1 = dat_h1_human$X1,
    # human_sad
    grp_1_human_sad_h0  = dat_h0_human$X2,
    grp_1_human_sad_h1 = dat_h1_human$X2,
    # robot_cheerful
    grp_2_robot_cheerful_h0 = dat_h0_robot$X1,
    grp_2_robot_cheerful_h1 = dat_h1_robot$X1,
    # robot_sad
    grp_2_robot_sad_h0 = dat_h0_robot$X2,
    grp_2_robot_sad_h1 = dat_h1_robot$X2
  )
}

testAOV = function(grp_1_human_cheerful_h0,
                   grp_1_human_cheerful_h1,
                   grp_1_human_sad_h0,
                   grp_1_human_sad_h1,
                   grp_2_robot_cheerful_h0,
                   grp_2_robot_cheerful_h1,
                   grp_2_robot_sad_h0,
                   grp_2_robot_sad_h1) {
  len_grp1 = length(grp_1_human_cheerful_h0)
  len_grp2 = length(grp_2_robot_cheerful_h0)
  raw_data = data.frame(
    obs = c(
      grp_1_human_cheerful_h0,
      grp_1_human_sad_h0,
      grp_2_robot_cheerful_h0,
      grp_2_robot_sad_h0
    ),
    id = c(paste0('g1_', c(
      1:len_grp1, 1:len_grp1
    )),
    paste0('g2_', c(
      1:len_grp2, 1:len_grp2
    ))),
    voice = c(rep('human', len_grp1 * 2), rep('robot', len_grp2 * 2)),
    emotion = c(
      rep('cheerful', len_grp1),
      rep('sad', len_grp1),
      rep('cheerful', len_grp2),
      rep('sad', len_grp2)
    )
  )
  raw_data <<-raw_data
  aov_h0 = summary(aov(obs ~ voice * emotion + Error(id / emotion), data =
                         raw_data))
  raw_data$obs = c(
    grp_1_human_cheerful_h1,
    grp_1_human_sad_h1,
    grp_2_robot_cheerful_h1,
    grp_2_robot_sad_h1
  )
  aov_h1 = summary(aov(obs ~ voice * emotion + Error(id / emotion), data =
                         raw_data))
  return(
    c(
      p_voice_h0 = aov_h0[[1]][[1]][['Pr(>F)']][1],
      p_voice_h1 = aov_h1[[1]][[1]][['Pr(>F)']][1],
      p_emo_h0 = aov_h0[[2]][[1]][['Pr(>F)']][1],
      p_emo_h1 = aov_h1[[2]][[1]][['Pr(>F)']][1],
      p_interact_h0 = aov_h0[[2]][[1]][['Pr(>F)']][2],
      p_interact_h1 = aov_h1[[2]][[1]][['Pr(>F)']][2],
      p_sad_rob_vs_hum_h0 = t.test(grp_1_human_sad_h0, grp_2_robot_sad_h0, var.equal = TRUE)$p.value,
      p_sad_rob_vs_hum_h1 = t.test(grp_1_human_sad_h1, grp_2_robot_sad_h1, var.equal = TRUE)$p.value,
      p_cheer_rob_vs_hum_h0 = t.test(
        grp_1_human_cheerful_h0,
        grp_2_robot_cheerful_h0,
        var.equal = TRUE
      )$p.value,
      p_cheer_rob_vs_hum_h1 = t.test(
        grp_1_human_cheerful_h1,
        grp_2_robot_cheerful_h1,
        var.equal = TRUE
      )$p.value
    )
  )
}

# do.call(testAOV, sampleAOV(100))


df_ps_aov = sim(
  fun_obs = sampleAOV,
  n_obs = 40,
  fun_test = testAOV,
  n_iter = 1000
)


# saveRDS(df_ps_aov, file = neatStats::path_neat('df_example_aov'))
# df_ps_aov = readRDS(neatStats::path_neat('df_example_aov'))
pow_res_aov = pow(df_ps_aov)

pow(df_ps_aov, alpha_locals = 0.5, adjust = FALSE)

# same in Superpower
design_result <- Superpower::ANOVA_design(
  design = "2b*2w",
  n = 40,
  mu = c(1.03, 1.41, 0.98, 1.01),
  sd = 1.03,
  r = 0.8,
  labelnames = c("voice", "human", "robot", "emotion", "cheerful", "sad"),
  plot = TRUE
)

super_power <- Superpower::ANOVA_power(
  design_result,
  alpha = 0.05,
  nsims = 1000,
  seed = 1234,
  emm = FALSE
)
# Power and Effect sizes for ANOVA tests
# power effect_size
# anova_voice         18.04     0.02565
# anova_emotion       79.89     0.10068
# anova_voice:emotion 66.51     0.07856
super_power
sim_out = super_power$sim_data
super_power$main_results
super_power$manova_results
