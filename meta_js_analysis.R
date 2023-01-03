# neatStats usage example: pipeline from raw data to reportable statistics

library('neatStats') # load package

setwd(path_neat('results_2a')) # set the result files' folder path as current working directory

filenames = list.files(pattern = "^numcog_.*\\.txt$") # get all result file names

for (file_name in enum(filenames)) {
    # file_name = c(0, filenames[1])

    # print current file name - just to monitor the process
    cat(file_name, fill = TRUE)

    # read the data with given file name
    subject_data = data.table::fread(
        file_name[2],
        fill = TRUE
    )

    # check if trial number is correct
    if (!(nrow(subject_data) %in% c(433, 999))) {
        stop("unexpected trial number: ", nrow(subject_data))
    } else if (!all(head(subject_data$trusted, -1) == TRUE)) {
        #stop("untrusted keypress")
    }

    dems_row = subject_data[startsWith(subject_data$subject_id, '{')]$subject_id
    dems = jsonlite::fromJSON(dems_row)
    dems$raf_samples = NULL

    subject_data = subject_data[!(subject_data$phase == 'practice' |
                                      startsWith(subject_data$subject_id, '{')),]
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
        group_by = c('snarc'),
        method = mean,
        prefix = 'rt_inraf',
        filt = (valid == TRUE)
    )
    rt_noraf = aggr_neat(
        subject_data,
        rt_noraf,
        group_by = c('snarc'),
        method = mean,
        prefix = 'rt_noraf',
        filt = (valid == TRUE)
    )
    rt_raf = aggr_neat(
        subject_data,
        rt_raf,
        group_by = c('snarc'),
        method = mean,
        prefix = 'rt_raf',
        filt = (valid == TRUE)
    )

    # get overall error rate
    er_overall = 1 - (sum(subject_data$valid) / nrow(subject_data))

    # merge subject data
    rbind_loop(
        subjects_merged,
        dems,
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
data_final$rt_raf = data_final$rt_raf_incong - data_final$rt_raf_cong
data_final$rt_noraf = data_final$rt_noraf_incong - data_final$rt_noraf_cong
data_final$rt_inraf = data_final$rt_inraf_incong - data_final$rt_inraf_cong

# look at rt data range and distribution, potential outliers
peek_neat(data_final,
          values = c('rt_raf',
                     'rt_noraf',
                     'rt_inraf'))
