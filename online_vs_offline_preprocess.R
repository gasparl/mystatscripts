library('data.table')
str_sum = function (x) {
    if (is.na(x)) {
        return(0)
    } else {
        return(sum(as.numeric(
            strsplit(as.character(x), '[;]|[+]')[[1]]
        )))
    }
}
title_match = function(short, full) {
    full = substr(full, 0, nchar(short))
    if (nchar(full) > 10) {
        short = substr(full, 0, nchar(full))
    }
    tolower(full_data$title)
    return((adist(tolower(full), tolower(short)) < 4)[1])
}


setwd(neatStats::path_neat('phase3'))
f_name = list.files(pattern = "^online_offline_studies_phase3_all.xlsx")[1] # get all result file names

gs_dat = data.table()

for (xls_tab in readxl::excel_sheets(f_name)) {
    sheet_dat = as.data.table(readxl::read_xlsx(f_name, sheet = xls_tab))
    sheet_dat$journal = xls_tab
    sheet_dat = sheet_dat[!is.na(sheet_dat$year)]
    gs_dat = rbindlist(list(gs_dat, sheet_dat), fill = TRUE)
}

gs_dat = Filter(function(x)!all(is.na(x)), gs_dat)

gs_dat = gs_dat[!grepl('SKIP', gs_dat$notes, fixed = TRUE), ]

gs_dat$offline = unname(sapply(unlist(gs_dat$offline), str_sum))
gs_dat$online = unname(sapply(unlist(gs_dat$online), str_sum))

gs_dat$doi = sub('https://doi.org/', '', gs_dat$doi)

# str(gs_dat)

if (anyDuplicated(gs_dat$doi)) {
    stop('doi duplicated ',
         paste(full_data$doi[duplicated(full_data$doi)], collapse = ', '))
}

# cref_dat = as.data.table(rcrossref::cr_works(dois = unique(gs_dat$doi))$data)
cref_dat = as.data.table(rcrossref::cr_works(dois = gs_dat$doi)$data)
cref_dat_list = list()
for (cref_doi in cref_dat$doi) {
    cref_datx = cref_dat[doi == cref_doi]
    cref_dat_list[[length(cref_dat_list) + 1]] =
        c(
            doi = cref_datx$doi,
            journal2 = cref_datx$container.title,
            date = cref_datx$published.print,
            issued = cref_datx$issued,
            citations = as.numeric(cref_datx$is.referenced.by.count),
            title_full = cref_datx$title,
            authors = nrow(cref_datx$author[[1]])
        )
}

full_data = merge(gs_dat, as.data.table(do.call(rbind, cref_dat_list)), by = 'doi')

for (replacer in list(
    c('&amp;', 'and'),
    c('Journal of Experimental Psychology:', 'JEP'),
    c(' PRPF', ''),
    c(' Psychologische Forschung', '')
)) {
    full_data$journal2 = sub(replacer[1], replacer[2], full_data$journal2)
}
if (any(full_data$journal != full_data$journal2)) {
    stop('journal mismatch ',
         paste(full_data$doi[full_data$journal != full_data$journal2], collapse = ', '))
}
if (!all(mapply(title_match, full_data$title, full_data$title_full))) {
    stop('title mismatch ',
         paste(full_data$title[!mapply(title_match,
                                       full_data$title, full_data$title_full)], collapse = ', '))
}

full_data$journal2 = NULL
full_data$title = full_data$title_full
full_data$title_full = NULL

saveRDS(full_data,
        neatStats::path_neat('online_vs_offline_data.rds'))
