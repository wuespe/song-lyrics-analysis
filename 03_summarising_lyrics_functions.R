### setup ----
library(data.table)
library(magrittr)

coalesce = function(x1, x2) {
  ifelse(is.na(x1), x2, x1)
}


### read song and process it to data.table ----
read_song = function(path) {
  song = readLines(path) %>% 
    strsplit("\\W+")
  
  dt_song = data.table(word = Reduce(c, song) %>% tolower)
  dt_song[nchar(word) > 0]
}

### join to dict function -> joining bases to each word ----
# dict_lite must be in the calling environment
join_to_dict = function(dt) {
  # aggregating words and keeping frequency info in 'N' column
  dt = dt[, .(.N), keyby=word]
  dt[, word_id := .I]
  # right join on key columns (word==word)
  dt_tmp = dict_lite[dt, .(word_id, word, stem, freq = N)]
  dt_tmp[, in_dict := ifelse(!is.na(stem), 1L, 0L)]
  dt_tmp[, stem := fcoalesce(stem, word)]
  
  # base forms in frequency (distinct words) order
  stems_all_sorted = dt_tmp[, .N, stem][order(-N), stem]
  # words got duplicated because of multiple base forms 
  # -> aggregate to the word once again and put each word possible bases in a bases_list column
  dt_ded = dt_tmp[, .(uqN_forms = uniqueN(stem),
                      freq = freq[1],
                      in_dict = in_dict[1],
                      stems_list = factor(stem, levels = stems_all_sorted) %>% sort(., na.last = F) %>% as.character %>% unique %>% list
  ),
  word
  ]
  
  # reduplicate rows to original frequency
  ind_redup = dt_ded[, rep(.I, freq)]
  dt_ded[ind_redup, .(word, uqN_forms, in_dict, stems_list)]
}


# picking most frequent base form - giving lower bound to number of unique words ----

fill_most_freq = function(dt) {
  dt[, most_freq := sapply(stems_list, . %>% head(., 1L))]
}


# picking least frequent base form - giving upper bound to number of unique words ----

fill_least_freq = function(dt, verbose = TRUE) {
  # fill least_freq iteratively
  # 1) for words that have only 1 stem, pick it and go to 2), if there are none END
  # 2) remove already used stems from the other words' stems list, then go to 1)
  fill_lf_iter = function(dt1) {
    if(verbose) cat("fill_lf_iter... ", iter)
    while(dt1[, any(uqN_forms==1)]) {
      counted_stems = dt[uqN_forms == 1, do.call(c, stems_list)]
      dt1[uqN_forms == 1, least_freq := sapply(stems_list, . %>% tail(., 1L))]
      
      # new stems list and count
      dt1[, stems_list := sapply(stems_list, . %>% setdiff(., counted_stems))]
      dt1[, uqN_forms := sapply(stems_list, uniqueN)]
      if(verbose) cat("# ")
    }
    if(verbose) cat("\n")
  }
  
  # if there are no words with 1 stem left, check if any have >1 
  #   for the first such word with least number of stems, pick one stem (last one) and do away with the rest
  #   if there are none such words it returns FALSE
  pick_random_form = function(dt1) {
    setkey(dt1, uqN_forms) # sort in ascending order
    idx = dt1[, .(uqN_forms, .I)][uqN_forms > 1, I[1]]
    if(is.na(idx)) return(invisible(FALSE))
    
    last_val = dt1[idx, unlist(stems_list)] %>% tail(1L)
    if(verbose) cat("pick_random_form: ", dt1[idx, word], " --> ", last_val, "\n")
    dt1[idx, `:=`(stems_list = list(last_val), uqN_forms = 1L)]
    return(invisible(TRUE))
  }
  
  iter = 1
  fill_lf_iter(dt)
  while(pick_random_form(dt)) {
    iter = iter + 1
    fill_lf_iter(dt)
  }
  dt[, stems_list := NULL]
  sprintf("fill least freq iterations: %d", iter) %>% cat("\n")
}

# all together - join and calculate bases ----
join_and_ded = function(dt_songs, verbose = TRUE) {
  system.time({
    dt_songs_j = join_to_dict(dt_songs)
  })
  
  fill_most_freq(dt_songs_j)
  system.time({
    fill_least_freq(dt_songs_j, verbose)
  })
  dt_songs_j
}

# helper to sample from data.table ----
sample_dt = function(dt, n = 5000L) {
  stopifnot(dt[, .N] > n)
  ind = sample.int(dt[, .N], n)
  dt[ind]
}
