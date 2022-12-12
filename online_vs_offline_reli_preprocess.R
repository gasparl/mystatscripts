library('data.table')
str_sum = function (x) {
    if (is.na(x)) {
        return(0)
    } else {
        return(sum(as.numeric(strsplit(
            as.character(x), '[;]|[+]'
        )[[1]])))
    }
}

setwd(neatStats::path_neat('phase2')) # set the result files' folder path as current working directory

filenames = list.files(pattern = "^online_.*\\.xlsx$") # get all result file names

xls_dat = data.table()
for (f_name in filenames) {
    # f_name = filenames[1]
    for (xls_tab in readxl::excel_sheets(f_name)) {
        sheet_dat = as.data.table(readxl::read_xlsx(f_name, sheet = xls_tab))
        sheet_dat$journal = xls_tab
        sheet_dat = sheet_dat[!is.na(sheet_dat$year)]
        xls_dat = rbindlist(list(xls_dat, sheet_dat), fill = TRUE)
    }
}
xls_dat = Filter(function(x)!all(is.na(x)), xls_dat)

xls_dat = xls_dat[!grepl('SKIP', xls_dat$notes, fixed = TRUE), ]

xls_dat$offline = unname(sapply(unlist(xls_dat$offline), str_sum))
xls_dat$online = unname(sapply(unlist(xls_dat$online), str_sum))

xls_dat$doi = sub('https://doi.org/', '', xls_dat$doi)
xls_dat$doi = sub('https://psycnet.apa.org/doi/', '', xls_dat$doi)

# str(xls_dat)

cref_dat = as.data.table(rcrossref::cr_works(dois = unique(xls_dat$doi))$data)
cref_dat_list = list()
for (cref_doi in cref_dat$doi) {
    cref_datx = cref_dat[doi == cref_doi]
    cref_dat_list[[length(cref_dat_list) + 1]] =
        c(
            doi = cref_datx$doi,
            journal2 = cref_datx$container.title,
            print_date = cref_datx$published.print,
            issued = cref_datx$issued,
            citations = cref_datx$is.referenced.by.count,
            title_full = cref_datx$title,
            authors = nrow(cref_datx$author[[1]])
        )
}

full_data = merge(xls_dat, as.data.table(do.call(rbind, cref_dat_list)), by = 'doi')

saveRDS(full_data,
        neatStats::path_neat('online_vs_offline_reli_data.rds'))
