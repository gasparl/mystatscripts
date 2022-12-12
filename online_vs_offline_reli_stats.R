# library('ggplot2')
library('data.table')
library('irrCAC')
title_match = function(short, full) {
    full = substr(full, 0, nchar(short))
    if (nchar(full) > 10) {
        short = substr(full, 0, nchar(full))
    }
    tolower(oor_data$title)
    return((adist(tolower(full), tolower(short)) < 4)[1])
}

oor_data = readRDS(neatStats::path_neat('online_vs_offline_reli_data.rds'))

for (replacer in list(
    c('&amp;', 'and'),
    c('Journal of Experimental Psychology:', 'JEP'),
    c(' PRPF', ''),
    c(' Psychologische Forschung', '')
)) {
    oor_data$journal2 = sub(replacer[1], replacer[2], oor_data$journal2)
}
if (any(oor_data$journal != oor_data$journal2)) {
    stop('journal mismatch ',
         paste(oor_data$doi[oor_data$journal != oor_data$journal2], collapse = ', '))
}
if (!all(mapply(title_match, oor_data$title, oor_data$title_full))) {
    stop('title mismatch ',
         paste(oor_data$title[!mapply(title_match,
                                       oor_data$title, oor_data$title_full)], collapse = ', '))
}

oor_data$journal2 = NULL
oor_data$title = oor_data$title_full
oor_data$title_full = NULL
oor_data$total = oor_data$offline + oor_data$online
oor_data$added[oor_data$added == 'MRM'] = 'MR'

##

oor_data = transform(oor_data, freq = ave(seq(nrow(oor_data)), title, FUN = length))
if (any(oor_data$freq != 3)) {
    stop('Missing ratings ',
         paste(oor_data$doi[oor_data$freq != 3], collapse = ', '))
}
# oor_data = oor_data[oor_data$freq == 3]

# str(oor_data)
oor_data_wide = dcast(
    oor_data,
    title + journal ~ added,
    value.var = c("online", "offline", "total")
)

oor_online = oor_data_wide[, grep('online_',
                                  names(oor_data_wide),
                                  value = TRUE,
                                  fixed = TRUE), with = FALSE]
oor_offline = oor_data_wide[, grep('offline_',
                                   names(oor_data_wide),
                                   value = TRUE,
                                   fixed = TRUE), with = FALSE]
oor_total = oor_data_wide[, grep('total_',
                                   names(oor_data_wide),
                                   value = TRUE,
                                   fixed = TRUE), with = FALSE]

# AC1 coefficient with precision measures
gwet.ac1.raw(oor_online)$est
gwet.ac1.raw(oor_offline)$est
gwet.ac1.raw(oor_total)$est
