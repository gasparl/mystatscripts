### basics ----
library("neatStats")
library("data.table")
library("ggplot2")
library("ggpubr")
setwd(script_path('cd_data'))
source("microsacc.R")
source("smoothdata.R")
source("vecvel.R")
# v_d = 57 # viewing distance; 180 for freezer
# v_m_cm = 50 # monitor width in cm; 200 for freezer
# v_m_px = 1920 # monitor width in pixels
mon = mon_neat(
    distance = 180,
    mon_width_cm = 200,
    mon_width_pixel = 1920
)
# mon_conv(mon, 1, from = 'deg', to = 'pix')
plot_saccade = function() {
    # Plot trajectory
    par(mfrow = c(1, 2))
    plot(
        smoothed[, 1],
        smoothed[, 2],
        type = 'l',
        asp = 1,
        xlab = expression(x[l]),
        ylab = expression(y[l]),
        main = "Position"
    )
    j <- saccades$table[1]:saccades$table[2]
    lines(
        smoothed[j, 1],
        smoothed[j, 2],
        type = 'l',
        col = 'red',
        lwd = 1
    )
    points(smoothed[saccades$table[2], 1], smoothed[saccades$table[2], 2], col =
               'red')
    
    # Plot trajectory in 2D velocity space
    vls <- vecvel(eye_single, SAMPLING)
    plot(
        vls[, 1],
        vls[, 2],
        type = 'l',
        asp = 1,
        xlab = expression(v[x]),
        ylab = expression(v[y]),
        main = "Velocity"
    )
    j <- saccades$table[1]:saccades$table[2]
    lines(vls[j, 1],
          vls[j, 2],
          type = 'l',
          col = 'red',
          lwd = 1)
}
SAMPLING = 1000
MINDUR = 15
VFAC = 10
file_names = list.files(pattern = "^cd_assessment_.*txt$")
file_names = list('cd_assessment_01_20190627085927.txt')
file_names = list('cd_assessment_02_20190627110403.txt')
file_names = list('cd_assessment_01_20190627085927.txt','cd_assessment_02_20190627110403.txt')

file_names = list('cd_assessment_71_20190713171445.txt')
file_names = list('cd_assessment_21_20190714144532.txt')
file_names = list('cd_assessment_22_20190716115613.txt')
file_names = list('cd_assessment_23_20190722074613.txt')
file_names = list('cd_assessment_24_20190722092344.txt')
file_names = list('cd_assessment_25_20190723140206.txt')
file_names = list('cd_assessment_26_20190724135134.txt')
file_names = list('cd_assessment_27_20190725093014.txt') # too many forward
file_names = list('cd_assessment_28_20190726133736.txt')


file_names = list('cd_assessment_21_20190714144532.txt', 'cd_assessment_22_20190716115613.txt', 'cd_assessment_23_20190722074613.txt', 'cd_assessment_24_20190722092344.txt','cd_assessment_25_20190723140206.txt', 'cd_assessment_26_20190724135134.txt')

#
remove_practice = T
add_indeterminates = T
options(warn = 1)
### data ----
if (exists("data_merged")) {
    rm(data_merged)
}
for (name_txt in file_names) {
    #name_txt = file_names[[1]]
    
    name_msg = sub('\\.txt', '.msg', name_txt)
    name_dat = sub('\\.txt', '.dat', name_txt)
    
    con = file(name_msg, open = "r")
    
    trial_list = list()
    if (remove_practice == TRUE) {
        to_remove = 0
    } else {
        to_remove = -9
    }
    while (length(event_line <- readLines(con, n = 1)) > 0) {
        event_line = strsplit(event_line, " ")[[1]]
        if (event_line[2] == 'TRIALID' &
            event_line[3] != to_remove) {
            # exclude practice block (event_line[3]: blocknum)
            trial_name = paste0('block', event_line[3], 'trial', event_line[4])
            if (is.null(trial_list[[event_line[4]]])) {
                # (event_line[4]: trialnum)
                trial_list[[event_line[4]]] = c()
            }
            trial_list[[trial_name]][event_line[5]] = as.numeric(substr(event_line[1], 5, 19)) # (event_line[5]: event title)
        }
    }
    close(con)
    tracking_msgs = trial_list
    
    tracking_data = read.table(name_dat, stringsAsFactors = F)
    resp_data = read.table(
        name_txt,
        stringsAsFactors = F,
        fill = T,
        header = T
    )
    
    
    
    subj_num = strsplit(as.character(name_txt), "_")[[1]][3]
    dems_string = resp_data[startsWith(as.character(resp_data$subject_id), 'dems'),][, 2]
    dems = c(subj_num, strsplit(as.character(dems_string), "/")[[1]])
    dems = do.call(rbind.data.frame, list(dems))
    colnames(dems) = c("subject_id", "gender", "age", "duration")
    
    resp_data = na.omit(resp_data, cols = c('trial'))
    if (remove_practice == TRUE) {
        resp_data = resp_data[resp_data$block != '0',] # remove practice phase
    }
    
    resp_data$response_key[resp_data$response_key == 'left'] = -1
    resp_data$response_key[resp_data$response_key == 'down'] = 0
    resp_data$response_key[resp_data$response_key == 'right'] = 1
    resp_data$response_key[resp_data$response_key == 'too_slow'] = 91
    resp_data$response_key[resp_data$response_key == 'no_arrival'] = 92
    resp_data$response_key = as.numeric(resp_data$response_key)
    
    #####
    
    trials_full = list()
    for (trial_name in names(tracking_msgs)) {
        #trial_name = 'block1trial44'
        cat(trial_name, fill = T)
        current_trial = tracking_msgs[[trial_name]]
        
        if ('no_arrival' %in% names(current_trial) ||
            'too_slow' %in% names(current_trial)) {
            next
        }
        
        
        trial_num = strsplit(as.character(trial_name), "trial")[[1]][2]
        resp_line = resp_data[resp_data$trial == trial_num, ]
        resp_line = unlist(resp_line[c(
            'block',
            'target_jump_pix',
            'landmark_place',
            'dp_target',
            'dp_landmark',
            'response_key'
        )])
        if (!resp_line['response_key'] %in% c(-1, 0, 1)) {
            stop('Check - resp_line response_key: ', resp_line['response_key'])
        }
        
        t_start = current_trial['target_shown'] - 10
        
        s_start_py = current_trial['sacc_start_py'] - t_start
        trial_data = tracking_data[tracking_data$V1 > t_start &
                                       tracking_data$V1 < (current_trial['sacc_start_py'] + 150) ,]
        if (trial_data$V2[trial_data$V1 == current_trial['sacc_start_py']] == -1) {
            warning('missing tracking data (py sacc start) - skipping trial num ',
                    trial_num)
            next
        }
        # Select epoch from trial, transform to matrix
        eye_single <- as.matrix(trial_data[, 2:3])
        # Apply running average
        smoothed <- smoothdata(eye_single)
        # Detection of microsaccades
        saccades <- microsacc(smoothed, VFAC, MINDUR, SAMPLING)
        if (is.null(saccades))  {
            warning('no saccade detected - skipping trial num ', trial_num)
            next
        }
        
        
        
        sacc_table = saccades$table
        jump_length = saccades$table[, 4] - (trial_data[saccades$table[, 1], 2] - mon$mon_width_pixel / 2)
        filtered = saccades$table[abs(jump_length - resp_line['target_jump_pix']) < abs(resp_line['target_jump_pix'] / 2) &
                                      abs(saccades$table[, 5]) < mon_conv(mon, 4, from = 'deg', to = 'pix') &
                                      (s_start_py - saccades$table[, 1]) >= 0 &
                                      (s_start_py - saccades$table[, 1]) < 25,]
        
        if (class(filtered) != "numeric") {
            if ((!is.na(trial_data[saccades$table[2], 2])) &
                trial_data[saccades$table[2], 2] == -1) {
                warning('--- missing tracking data (sacc end)',
                        trial_num)
            }
            if (all(abs(jump_length - resp_line['target_jump_pix']) < abs(resp_line['target_jump_pix'] / 2)) == FALSE)  {
                warning(
                    'saccade length not matching ',
                    jump_length,
                    ' vs ',
                    resp_line['target_jump_pix'],
                    ' - skipping trial num ',
                    trial_num
                )
                next
            } else if (all((s_start_py - saccades$table[, 1]) >= 0) == FALSE)  {
                warning(
                    'saccade before py detected ',
                    list(abs(
                        saccades$table[, 1] -  s_start_py
                    )),
                    ' - skipping trial num ',
                    trial_num
                )
                next
            } else if (all((s_start_py - saccades$table[, 1]) < 25) == FALSE)  {
                warning('saccade nowhere near ',
                        list(abs(
                            saccades$table[, 1] -  s_start_py
                        )),
                        ' - skipping trial num ',
                        trial_num)
                next
            } else if (all(abs(saccades$table[, 5]) < mon_conv(mon, 4, from = 'deg', to = 'pix')) == FALSE) {
                warning(
                    'vertical move ',
                    mon_conv(mon, saccades$table[, 5], from = 'pix', to = 'deg'),
                    ' - skipping trial num ',
                    trial_num
                )
                next
            } else {
                stop('Check - nrow(filtered): ', nrow(filtered))
            }
        } else {
            saccades$table = filtered
        }
        
        sacc_start = saccades$table[1] + t_start
        sacc_end = saccades$table[2] + t_start
        
        current_trial = current_trial[c('target_shown', 'blank')]
        
        current_trial['sacc_start'] = sacc_start
        current_trial['sacc_start_py'] = s_start_py + t_start
        current_trial['sacc_end'] = sacc_end
        current_trial['sacc_end_x'] = trial_data[saccades$table[2], 2]
        current_trial['sacc_end_y'] = trial_data[saccades$table[2], 3]
        
        current_trial['sacc_amp'] = mon_conv(mon,
                                             (saccades$table[4] - saccades$table[5]),
                                             from = 'pix',
                                             to = 'deg')
        
        trials_full[[trial_name]] = c(
            subject_id = as.numeric(subj_num),
            trial_num = as.numeric(trial_num),
            current_trial,
            resp_line
        )
    }
    
    subj_data = as.data.frame(do.call(rbind, trials_full))
    if (exists("data_merged")) {
        # add subject aggregations
        data_merged = rbind(data_merged, subj_data)
    } else {
        data_merged = subj_data
    }
}
# plot_saccade()

data_merged$sacc_duration = data_merged$sacc_end - data_merged$sacc_start
data_merged$sacc_start_diff = data_merged$sacc_start_py - data_merged$sacc_start
data_merged$sacc_start_py_to_blank = data_merged$blank - data_merged$sacc_start_py
data_merged$sacc_start_to_blank = data_merged$blank - data_merged$sacc_start
data_merged$blank_to_sacc_end = data_merged$sacc_end - data_merged$blank

data_merged$landing = mon_conv(
    mon,
    (data_merged$sacc_end_x - (mon$mon_width_pixel / 2)) - data_merged$target_jump_pix,
    from = 'pix',
    to = 'deg'
)

# median(data_merged$sacc_start_diff) # 101 : 14
# mean(data_merged$sacc_start_diff) # 101 : 13.8676
# min(data_merged$sacc_start_diff) # 101 : 6
# max(data_merged$sacc_start_diff) # 101 : 21
# sd(data_merged$sacc_start_diff) # 101 : 2.751331

## landing site: forw_landing e.g. -0.8 (minus 0.8) means it landed 0.8 degree short ("before") target

unique(data_merged$block)

subtest = 22
subtest = NA
blocks_to_test = c(2,4) # c(1,2,3,4)
divide_lands = T
blocks_to_test = 3
divide_lands = F

plot_list = list()
check_list = list()
divisions = 2 # 6 is nice for merged
for (plt_index in seq(1, 2)) {
    for (indx in seq(1, divisions)) {
        # indx =2
        color_gen = colorRampPalette(c('#FFA0A0', '#000000'))
        the_colors = color_gen(divisions)
        color_i = the_colors[indx]
        indep_var = c(-10, 0, 10)[plt_index] # landmark displacement
        indep_var = c(0, 1)[plt_index]
        all_data = data_merged
        all_data = all_data[all_data$block %in% blocks_to_test,]
        if (!is.na(subtest)) {
            all_data = all_data[all_data$subject_id == subtest,]
        }
        
        # convert into 1 direction
        all_data$forw_landmark_place = ifelse(
            sign(all_data$target_jump_pix) == 1,
            all_data$landmark_place,-all_data$landmark_place
        )
        all_data$forw_dp_target = ifelse(
            sign(all_data$target_jump_pix) == 1,
            all_data$dp_target,-all_data$dp_target
        )
        all_data$response_key = ifelse(
            sign(all_data$target_jump_pix) == 1,
            all_data$response_key,-all_data$response_key
        )
        all_data$resp[all_data$response_key == -1] = 'back'
        all_data$resp[all_data$response_key == 0] = 'none'
        all_data$resp[all_data$response_key == 1] = 'forward'
        all_data$forw_dp_landmark = all_data$dp_landmark
        # separate by dp_landmark where 1 is landmark displayed and 0 not
        if (divide_lands == TRUE) {
            all_data = all_data[all_data$forw_dp_landmark == indep_var, ]
        }
        
        thres_perc = 1 / divisions * indx
        all_data$forw_landing = ifelse(sign(all_data$target_jump_pix) == 1,
                                       all_data$landing,-all_data$landing)
        thres_val_low = quantile(all_data$forw_landing, thres_perc - (1 / divisions))
        thres_val_upp = quantile(all_data$forw_landing, thres_perc)
        all_data = all_data[all_data$forw_landing > thres_val_low &
                                all_data$forw_landing <= thres_val_upp,]
        
        if (indx == 1) {
            check_list[[plt_index]] = list()
        }
        check_list[[plt_index]][[indx]] = c(
            min = min(all_data$forw_landing),
            max = max(all_data$forw_landing),
            mean = mean(all_data$forw_landing),
            n = length(all_data$forw_landing)
        )
        
        ### calculate below
        
        check_rows = nrow(all_data[!all_data$resp %in% c('back', 'none', 'forward'),])
        if (check_rows != 0) {
            stop('Check - check_rows not 0: ', check_rows)
        }
        
        accuracy = as.data.frame.matrix(xtabs( ~ forw_dp_target + resp, all_data))
        accuracy = data.frame(forw_dp_target = rownames(accuracy), accuracy)
        accuracy$forw_dp_target = as.numeric(as.character(accuracy$forw_dp_target))
        if (is.null(accuracy$forward)) {
            accuracy$forward = 0
        }
        # add_indeterminates = T
        if ((!is.null(accuracy$none)) & (add_indeterminates == T)) {
            accuracy$back = accuracy$back + (accuracy$none %/% 2)
            accuracy$forward = accuracy$forward + (accuracy$none %/% 2)
        }
        # add proportions
        accuracy$props = accuracy$forward / (accuracy$forward + accuracy$back)
        
        ## curve fitting ----
        model = glm(
            formula = cbind(forward, back) ~ forw_dp_target,
            family = binomial(link = "probit"),
            data = accuracy
        )
        # confint(model)
        if (indx == 1) {
            psych_curve = ggplot(data = accuracy, aes(x = forw_dp_target, y = props)) + ylim(0, 1)
        }
        psych_curve = psych_curve + geom_point(data = accuracy,
                                               aes(x = forw_dp_target, y = props),
                                               color = color_i)
        
        xseq = seq(min(accuracy$forw_dp_target),
                   max(accuracy$forw_dp_target),
                   len = 1000)  #I used, for example, a 1000 points
        yseq = predict(model, data.frame(forw_dp_target  = xseq), type = "response")
        the_curve = data.frame(xseq, yseq)
        
        psych_curve = psych_curve + geom_line(data = the_curve,
                                              aes(x = xseq, y = yseq),
                                              color = color_i)
    }
    plot_list[[plt_index]] = psych_curve + theme(plot.margin = unit(c(1.5, 0, 0, 0), "lines"))
    # unit(c(top, right, bottom, left), units
}

ggarrange(
    plot_list[[1]],
    plot_list[[2]],
    labels = c("0", "1"),
    ncol = 1,
    nrow = 2,
    common.legend = T,
    vjust = 1.3
) # hjust = 0.3
