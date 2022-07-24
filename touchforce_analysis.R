# neatStats usage example: pipeline from raw data to reportable statistics

library('neatStats')
library('ggplot2')

setwd(path_neat('touchforce_results')) # set the result files' folder path as current working directory

filenames = list.files(pattern = "^touchforce_pilot.*\\.txt$") # get all result file names

for (file_name in enum(filenames)) {
    #  file_name = c(0, filenames[2])

    # print current file name - just to monitor the process
    cat(file_name, fill = TRUE)

    subject_data = data.table::fread(file_name[2],
                                     fill = TRUE)

    force_data = jsonlite::fromJSON(subject_data[startsWith(subject_data$datetime, '{')]$datetime)

    dems_row = subject_data[startsWith(subject_data$datetime, 'client')]
    dems = as.list(setNames(strsplit(dems_row[[3]], "/")[[1]][-1], strsplit(dems_row[[2]], "/")[[1]][-1]))

    subject_data = subject_data[!(
        startsWith(subject_data$datetime, 'client') |
            startsWith(subject_data$datetime, '{')
    ),]

    by(subject_data, seq_len(nrow(subject_data)), function(rowx)
    {
        trial_force = as.data.frame(force_data[[rowx$trial_number]])
        if (nrow(trial_force) > 3 &&
            (max(trial_force$V2) - rowx$disp_start) > 200) {
            colnames(trial_force) = c("now", "timestamp", "force", "x", "y")

            force_plot = ggplot(data = trial_force, aes(
                x = timestamp,
                y = force,
                group = 1
            )) +
                geom_line(linetype = "dashed") +
                geom_point(color = "blue") +
                geom_vline(
                    xintercept = rowx$disp_start,
                    linetype = "dotted",
                    color = "#00802b"
                ) +
                geom_text(
                    aes(
                        x = rowx$disp_start,
                        label = "Start",
                        y = .87
                    ),
                    nudge_x = 25,
                    colour = "#00802b",
                    angle = 90
                ) + theme_bw() + scale_x_continuous(
                    labels = function(x)
                        x - rowx$disp_start,
                    breaks = seq(rowx$disp_start,
                                 max(trial_force$timestamp),
                                 by = 200),
                    limits = c(rowx$disp_start - 110, rowx$disp_start + 710)
                ) + scale_y_continuous(
                    labels = function(x)
                        x * 100,
                    limits = c(0, 1)
                ) + labs(
                    title = paste0(
                        'subject #',
                        file_name[1],
                        ', trial #',
                        rowx$trial_number,
                        ''
                    ),
                    x = "time (ms)",
                    y = "touch force (%)"
                )
            if (!is.na(rowx$disp_stop)) {
                force_plot = force_plot +
                    geom_vline(
                        xintercept = rowx$disp_stop,
                        linetype = "dotted",
                        color = "#cc0000"
                    ) +
                    geom_text(
                        aes(
                            x = rowx$disp_stop,
                            label = "Stop",
                            y = .87
                        ),
                        nudge_x = 25,
                        colour = "#cc0000",
                        angle = 90
                    )
            }
            #plot(force_plot)
            message("trial: ", rowx$trial_number)

            ggplot2::ggsave(
                filename = paste0(
                    './figs/fig_',
                    file_name[1],
                    '_',
                    rowx$trial_number,
                    '.tiff'
                ),
                force_plot,
                units = "mm",
                width = 100,
                height = 100,
                dpi = 600,
                device = 'tiff'
            )
        } else {
            message("MISSED trial: ", rowx$trial_number)
        }
    })
}


for (file_name in enum(filenames)) {
    #  file_name = c(0, filenames[2])
    cat(file_name, fill = TRUE)

    subject_data = data.table::fread(file_name[2],
                                     fill = TRUE)

    force_data = jsonlite::fromJSON(subject_data[startsWith(subject_data$datetime, '{')]$datetime)

    dems_row = subject_data[startsWith(subject_data$datetime, 'client')]
    dems = as.list(setNames(strsplit(dems_row[[3]], "/")[[1]][-1], strsplit(dems_row[[2]], "/")[[1]][-1]))
    total_duration = max(force_data$`29`[, 1]) - as.numeric(dems$start)
    duration = max(force_data$`29`[, 1]) - min(force_data$`1`[, 1])
    cat(
        "   --- total:",
        ro(total_duration / 1000, 2),
        "sec, task:",
        ro(duration / 1000, 2),
        "sec",
        fill = TRUE
    )
}
