library('data.table')
str_sum = function (x) {
    if (is.na(x)) {
        return(0)
    } else {
        return(sum(as.numeric(strsplit(
            as.character(x), ';'
        )[[1]])))
    }
}
googlesheets4::gs4_deauth()
gs_id = '1HPb-9SZ0LXAWOzyh6XsXaKhg_OCwmBS7nJc3ZUB38zg'

gs_dat = data.table()
for (gs_tab in googlesheets4::sheet_names(gs_id)) {
    sheet_dat = as.data.table(googlesheets4::read_sheet(gs_id, sheet = gs_tab))
    sheet_dat$journal = gs_tab
    gs_dat = rbindlist(list(gs_dat, sheet_dat))
}
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

for (replacer in list(c('&amp;', 'and'),
                      c('Journal of Experimental Psychology:', 'JEP'))) {
    full_data$journal2 = sub(replacer[1], replacer[2], full_data$journal2)
}
if (any(full_data$journal != full_data$journal2)) {
    stop('journal mismatch ',
         paste(full_data$doi[full_data$journal != full_data$journal2], collapse = ', '))
}
if (!all(startsWith(tolower(full_data$title_full), tolower(full_data$title)))) {
    stop('title mismatch ',
         paste(full_data$title[!startsWith(tolower(full_data$title_full),
                                          tolower(full_data$title))], collapse = ', '))
}
full_data$journal2 = NULL
full_data$title = full_data$title_full
full_data$title_full = NULL

saveRDS(full_data, neatStats::path_neat('online_vs_offline_data.rds'))
