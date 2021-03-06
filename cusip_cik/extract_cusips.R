#!/usr/bin/env Rscript
library(dplyr, warn.conflicts = FALSE)
library(DBI)

# Do basic table set-up ----
pg <- dbConnect(RPostgreSQL::PostgreSQL())

if (!dbExistsTable(pg, c("edgar", "cusip_cik"))) {
    dbGetQuery(pg, "
        CREATE TABLE edgar.cusip_cik
            (
              file_name text,
              cusip text,
              cik integer,
              company_name text,
              format text
            )

        GRANT SELECT ON TABLE edgar.cusip_cik TO edgar_access;
        ALTER TABLE edgar.cusip_cik OWNER TO edgar;

        CREATE INDEX ON edgar.cusip_cik (file_name);
        CREATE INDEX ON edgar.cusip_cik (cusip);
        CREATE INDEX ON edgar.cusip_cik (cik);")
}

rs <- dbDisconnect(pg)

# Get a list of files that need to be processed ----
# Note that this assumes that streetevents.calls is up to date.
get_filing_list <- function(num_files = Inf) {
    pg <- dbConnect(RPostgreSQL::PostgreSQL())

    dbExecute(pg, "SET work_mem='2GB'")
    dbExecute(pg, "SET search_path TO edgar, public")
    filings <- tbl(pg, "filings")
    filing_docs <- tbl(pg, "filing_docs")
    cusip_cik <- tbl(pg, "cusip_cik")

    html_filings <-
        filing_docs %>%
        filter(document %~% '.htm$')

    file_list <-
        filings %>%
        filter(form_type %in% c('SC 13G', 'SC 13G/A', 'SC 13D', 'SC 13D/A')) %>%
        anti_join(cusip_cik, by="file_name") %>%
        left_join(html_filings, by = "file_name")
    file_list
    collect(n = num_files)

    rs <- dbDisconnect(pg)
    return(file_list)
}

# Create function to parse a SC 13D or SC 13F filing ----
parseFile <- function(file_name) {

    # Parse the indicated file using a Perl script
    system(paste("perl extract_cusips.pl", file_name),
           intern = TRUE)
}

# Apply parsing function to files ----
library(parallel)
while(nrow(file_list <- get_filing_list(1000)) > 0) {
    system.time({
        res <- unlist(mclapply(file_list$file_name, parseFile, mc.cores=12))
    })
}
