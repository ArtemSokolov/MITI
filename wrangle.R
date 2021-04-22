library( tidyverse )

fn <- "raw/01-clinical-data-elements.tsv"

parse_valid_vals <- function(v) {
    str_split(v, ',') %>%
        pluck(1) %>%
        str_trim()
}

X <- read_tsv(fn, col_types=cols()) %>%
    rename_all( str_to_lower ) %>%
    nest( data=c(-attribute) ) %>%
    deframe() %>%
    map( select_if, ~!is.na(.x) ) %>%
    map( as.list ) %>%
    map( map_at, "valid-values", parse_valid_vals )

yaml::write_yaml(X, "01-clinical-data-elements.yaml")
