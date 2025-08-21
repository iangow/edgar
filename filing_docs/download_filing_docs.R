#!/usr/bin/env Rscript
library(DBI)
library(dplyr, warn.conflicts = FALSE)
library(tools)

target_schema <- "edgar"
filing_docs_table <- "filing_docs"
processed_table <- "filing_docs_processed"

# Functions ----
get_file_list <- function(num_files = Inf, form_types = NULL) {

    pg <- dbConnect(RPostgres::Postgres())

    rs <- dbExecute(pg, paste0("SET search_path TO ", target_schema, ", edgar, public"))

    filings <- tbl(pg, "filings")
    filing_docs  <- tbl(pg, filing_docs_table)

    if (is.null(form_types)) {
        filing_docs_to_get <- filing_docs
    } else {
        filing_docs_to_get <-
            filings %>%
            filter(form_type %in% form_types) %>%
            inner_join(filing_docs)
    }

    get_file_path <- function(file_name, document) {
        url <- gsub("(\\d{10})-(\\d{2})-(\\d{6})\\.txt", "\\1\\2\\3", file_name)
        file.path(url, document)
    }

    new_table <- !dbExistsTable(pg, processed_table)

    if (!new_table) {
        filing_docs_processed <- tbl(pg, processed_table)
        files <-
            filing_docs_to_get  %>%
            anti_join(filing_docs_processed)
    } else {
        files <- filing_docs_to_get
    }

    files <-
        files %>%
        filter(document %~*% "htm$") %>%
        collect(n = num_files)
    dbDisconnect(pg)

    if (nrow(files) > 0) {
        files %>%
            mutate(html_link = get_file_path(file_name, document))
    } else {
        files
    }
}

get_filing_file_list <- function(num_files = Inf) {

    pg <- dbConnect(RPostgres::Postgres())

    rs <- dbExecute(pg, paste0("SET search_path TO ", target_schema, ", edgar, public"))

    filing_docs <- tbl(pg, filing_docs_table)
    filings <- tbl(pg, "filings")

    get_file_path <- function(file_name, document) {
        url <- gsub("(\\d{10})-(\\d{2})-(\\d{6})\\.txt", "\\1\\2\\3", file_name)
        file.path(url, document)
    }

    new_table <- !dbExistsTable(pg, processed_table)
    if (!new_table) {

        filing_docs_processed <- tbl(pg, processed_table)

        files <-
            filing_docs  %>%
            anti_join(filing_docs_processed)

    } else {
        files <- filing_docs
    }

    files <-
        files %>%
        filter(document %~*% "htm$") %>%
        semi_join(filings %>% filter(year(date_filed) == 2020)) %>%
        collect(n = num_files) %>%
        mutate(html_link = get_file_path(file_name, document))

    dbDisconnect(pg)

    files
}

get_filing_docs <- function(path) {

    local_filename <- file.path(raw_directory, path)

    #     print(path[!file.exists(local_filename) & !is.na(path)])
    link <- file.path("https://www.sec.gov/Archives", path)
    dir.create(dirname(local_filename), showWarnings=FALSE, recursive=TRUE)

    # Only download the file if we don't already have a local copy
    if (!file.exists(local_filename)) {
        try(download.file(url=link, destfile=local_filename, quiet=TRUE))
    }

    # Return the local filename if the file exists
    return(file.exists(local_filename))
}


download_filing_files <- function(max_files = Inf) {

    pg <- dbConnect(RPostgres::Postgres())
    dbExecute(pg, paste0("SET search_path TO ", target_schema))
    new_table <- !dbExistsTable(pg, processed_table)
    dbDisconnect(pg)
    while (nrow(files <- get_filing_file_list(num_files = max_files))>0) {
        print("Getting files...")
        st <- system.time(files$downloaded <-
                              unlist(lapply(files$html_link, get_filing_docs)))

        print(sprintf("Downloaded %d files in %3.2f seconds",
                      nrow(files), st[["elapsed"]]))

        downloaded_files <-
            files %>%
            select(file_name, document, downloaded)

        pg <- dbConnect(PostgreSQL())
        rs <- dbExecute(pg, paste0("SET search_path TO ", target_schema, ", edgar, public"))
        dbWriteTable(pg, processed_table, downloaded_files,
                     append = !new_table,
                     row.names = FALSE)
        if (new_table) {
            dbGetQuery(pg, paste("CREATE INDEX ON", processed_table,"(file_name)"))
            dbGetQuery(pg, paste("ALTER TABLE ", processed_table," OWNER TO edgar"))
            dbGetQuery(pg, paste("GRANT SELECT ON TABLE ", processed_table," TO edgar_access"))
            new_table <- FALSE
        }
        dbDisconnect(pg)
    }
}

# Run code ----
library(parallel)

raw_directory <- Sys.getenv("EDGAR_DIR")

pg <- dbConnect(RPostgres::Postgres())
dbExecute(pg, paste0("SET search_path TO ", target_schema))
new_table <- !dbExistsTable(pg, processed_table)

rs <- dbDisconnect(pg)
while (nrow(files <- get_file_list(num_files = 1000)) > 0) {
    print("Getting files...")
    st <- system.time(files$downloaded <-
                          unlist(lapply(files$html_link, get_filing_docs)))

    print(sprintf("Downloaded %d files in %3.2f seconds",
                  nrow(files), st[["elapsed"]]))

    downloaded_files <-
        files %>%
        select(file_name, document, downloaded)

    pg <- dbConnect(RPostgres::Postgres())
    rs <- dbExecute(pg, paste0("SET search_path TO ", target_schema, ", edgar, public"))
    dbWriteTable(pg, processed_table, downloaded_files,
                 append = !new_table,
                 row.names = FALSE)
    if (new_table) {
        dbGetQuery(pg, paste("CREATE INDEX ON ", processed_table," (file_name)"))
        dbGetQuery(pg, paste("ALTER TABLE ", processed_table," OWNER TO edgar"))
        dbGetQuery(pg, paste("GRANT SELECT ON TABLE ", processed_table," TO edgar_access"))
        new_table <- FALSE
    }
    dbDisconnect(pg)
}
