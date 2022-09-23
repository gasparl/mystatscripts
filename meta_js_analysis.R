# neatStats usage example: pipeline from raw data to reportable statistics

library('neatStats') # load package

setwd(path_neat('results_2a')) # set the result files' folder path as current working directory

filenames = list.files(pattern = "^numcog_.*\\.txt$") # get all result file names

for (file_name in enum(filenames)) {
    # file_name = c(0, "numcog_FYWC_220923100821.txt")

    # print current file name - just to monitor the process
    cat(file_name, fill = TRUE)

    # read the data with given file name
    subject_data = data.table::fread(
        file_name[2],
        fill = TRUE
    )

    # check if trial number is correct
    if (nrow(subject_data) != 513) {
        stop("unexpected trial number: ", nrow(subject_data))
    } else if (!all(subject_data$trusted == TRUE)) {
        #stop("untrusted keypress")
    }

    dems_row = subject_data[startsWith(subject_data$subject_id, 'dems')]
    dems = as.list(setNames(strsplit(dems_row[[3]], "/")[[1]][-1], strsplit(dems_row[[2]], "/")[[1]][-1]))

    subject_data = subject_data[!(subject_data$phase == 'practice' | subject_data$subject_id == 'dems'),]
    subject_data$num_type = ifelse((subject_data$digit > 5 &
                                        subject_data$response_key == 'k') |
                                       (subject_data$digit < 5 &
                                            subject_data$response_key == 'd'),
                                   'congr',
                                   'incong'
    )
    subject_data$rt_inraf = subject_data$keydown - subject_data$display_digit_now
    subject_data$rt_noraf = subject_data$keydown - subject_data$display_digit_noraf
    subject_data$rt_raf = subject_data$keydown - subject_data$display_digit
    subject_data$valid = (subject_data$rt_raf &
                              subject_data$rt_raf < 1000 &
                              subject_data$rt_raf > 150)

    # now aggregate rt data per type
    rt_inraf = aggr_neat(
        subject_data,
        rt_inraf,
        group_by = c('num_type'),
        method = mean,
        prefix = 'rt_inraf',
        filt = (valid == TRUE)
    )
    rt_noraf = aggr_neat(
        subject_data,
        rt_noraf,
        group_by = c('num_type'),
        method = mean,
        prefix = 'rt_noraf',
        filt = (valid == TRUE)
    )
    rt_raf = aggr_neat(
        subject_data,
        rt_raf,
        group_by = c('num_type'),
        method = mean,
        prefix = 'rt_raf',
        filt = (valid == TRUE)
    )

    # get overall error rate
    er_overall = 1 - (sum(subject_data$valid) / nrow(subject_data))

    # merge subject data
    rbind_loop(
        subjects_merged,
        subject_id = dems,
        er_overall = er_overall,
        rt_raf,
        rt_noraf,
        rt_inraf
    )
}

# list column names to take a look and easily copy
str(subjects_merged)

# check error rates
# peek_neat(subjects_merged,
#           values = 'er_overall',
#           f_plot = plot_neat)

# exclude subjects with overall error rate congrr than 20%
data_final = excl_neat(subjects_merged, er_overall < 0.20)
data_final$rt_raf = data_final$rt_raf_incong - data_final$rt_raf_congr
data_final$rt_noraf = data_final$rt_noraf_incong - data_final$rt_noraf_congr
data_final$rt_inraf = data_final$rt_inraf_incong - data_final$rt_inraf_congr

# look at rt data range and distribution, potential outliers
peek_neat(
    data_final$rt_raf_incong,
    values = c(
        'rt_green_negative',
        'rt_red_negative',
        'rt_green_positive',
        'rt_red_positive'
    ),
    f_plot = plot_neat
)
