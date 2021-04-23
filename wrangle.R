library( tidyverse )

parse_key_val <- function(kv) {
    key_val <- str_split(kv, ":") %>% pluck(1)
    set_names( as.numeric(key_val[2]), key_val[1] )
}

parse_valid_vals <- function(v) {
    ## Parse keywords
    val <- str_split(v, ',') %>%
        pluck(1) %>%
        str_trim()

    ## Determine if the keywords specify min: max: intervals
    map_if( val, ~(grepl("min",.x) || grepl("max",.x)),
           parse_key_val ) %>% unlist %>% as.list
}

main <- function(fn) {
    fnIn <- str_c("raw/", fn)
    cat( "Parsing", fnIn, "\n" )

    X <- read_tsv(fnIn, col_types=cols()) %>%
        rename_all( str_to_lower )

    cat( "Found data types:   ", unique(X$type), "\n" )
    cat( "Significance levels:", unique(X$significance), "\n" )
    
    Y <- X %>% nest( data=c(-attribute) ) %>%
        deframe() %>%
        map( select_if, ~!is.na(.x) ) %>%
        map( as.list ) %>%
        map( map_at, "valid-values", parse_valid_vals ) %>%
        map( map_at, "significance", str_to_lower )

    fnOut <- str_replace(fn, ".tsv", ".yaml")
    cat( "Writing", fnOut, "\n" )
    yaml::write_yaml(Y, fnOut)
}

list.files("raw") %>% walk(main)

