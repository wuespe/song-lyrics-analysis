### setup ----
library(data.table)
library(magrittr)
library(jsonlite)
library(httr)


`%||%` = function(a, b) paste0(a, b)

### request authorization ----

get_access_token = function(cred) {
  encoded_cred = cred %>% 
    charToRaw %>% 
    base64enc::base64encode(.) %>% 
    paste0("Basic ", .)
  
  token = POST(url = "https://accounts.spotify.com/api/token",
               add_headers(Authorization = encoded_cred),
               body = list(grant_type="client_credentials"),
               encode = "form",
               verbose(info = TRUE, ssl = TRUE)
  )
  stop_for_status(token)
  
  content(token)$access_token
}

# access_token = get_access_token("")

### search for artist id ----

search_artist = function(q, type = "artist", market = "PL") {
  search_query_url = "https://api.spotify.com/v1/search"
  
  result =  GET(url = search_query_url,
                add_headers(Authorization = paste0("Bearer ", access_token)),
                query = list(q = q, type = type, market = market),
                content_type_json(),
                accept_json(),
                verbose(info = TRUE, ssl = TRUE)
  )
  
  fields_from_response = c("name", "type", "genres", "followers.total", "popularity", "uri")
  
  stop_for_status(result)
  
  wynik = content(result)$artists$items %>% lapply(. %>% unlist %>% .[fields_from_response]) %>% do.call(rbind, .) %>% data.table %>% setnames(fields_from_response)
  wynik[, search_q := q][]
}

# dt_result = search_artist("łona")
# raperzy = c("łona i webber", "paktofonika", "ten typ mes", "ostr")
# dt_all_results = lapply(raperzy, search_artist) %>% rbindlist


### get artist albums and tracks ----
# search by name and get the one with highest popularity or use artist_id 

# access_token - must be available in environment

download_artist_song_lyrics = function(q, artist_id = NULL) {
  
  if(is.null(artist_id)) {
    search_query_url = "https://api.spotify.com/v1/search"
    result =  GET(url = search_query_url,
                  add_headers(Authorization = paste0("Bearer ", access_token)),
                  query = list(q = q, type = "artist", market = "PL"),
                  content_type_json(),
                  accept_json(),
                  verbose(info = TRUE, ssl = TRUE)
    )
    
    fields_from_response = c("name", "genres", "followers.total", "uri", "images.url")
    stop_for_status(result)
    
    wynik = content(result)$artists$items %>% lapply(. %>% unlist %>% .[fields_from_response]) %>% do.call(rbind, .) %>% data.table %>% setnames(fields_from_response)
    wynik[, `:=`(followers.total = as.integer(followers.total),
                 artist_uri = uri,
                 artist_id = sub("spotify:artist:", "", uri),
                 artist = name
    )]
    artist_DT = wynik[order(-followers.total), .(artist, artist_id, genres, followers.total, images.url)][1]
    
    
  } else {
    search_query_url = "https://api.spotify.com/v1/artists/" %||% artist_id
    result =  GET(url = search_query_url,
                  add_headers(Authorization = paste0("Bearer ", access_token)),
                  content_type_json(),
                  accept_json(),
                  verbose(info = TRUE, ssl = TRUE)
    )
    
    fields_from_response = c("name", "genres", "followers.total", "uri", "images.url")
    stop_for_status(result)
    
    wynik = content(result) %>% unlist %>% .[fields_from_response] %>% as.list %>% do.call(data.table, .)
    wynik[, `:=`(followers.total = as.integer(followers.total),
                 artist_uri = uri,
                 artist_id = sub("spotify:artist:", "", uri),
                 artist = name
    )]
    artist_DT = wynik[, .(artist, artist_id, genres, followers.total, images.url)]
  }
  
  
  
  
  
  # albumy
  search_query_url = "https://api.spotify.com/v1/artists/" %||% artist_DT$artist_id %||% "/albums"
  result =  GET(url = search_query_url,
                add_headers(Authorization = paste0("Bearer ", access_token)),
                query = list(country = "PL", limit = 50),
                content_type_json(),
                accept_json(),
                verbose(info = TRUE, ssl = TRUE)
  )
  stop_for_status(result)
  result_albums = content(result)$items
  result_albums = lapply(result_albums, function(x) c(x, album_total_artists_count = length(x[["artists"]])))
  
  fields_from_response = c("album_total_artists_count", "album_group", "album_type", "name", "release_date", "release_date_precision", "total_tracks", "type", "uri")
  
  albums = result_albums %>% lapply(. %>% unlist %>% .[fields_from_response]) %>% do.call(rbind, .) %>% data.table %>% setnames(fields_from_response)
  albums[, `:=`(artist = artist_DT$artist,
                artist_id = artist_DT$artist_id,
                album_id = sub("spotify:album:", "", uri),
                album = name,
                total_tracks = as.integer(total_tracks), 
                album_total_artists_count = as.integer(album_total_artists_count)
  )]
  
  albums_DT = albums[, .(album, album_id, album_group, album_type, release_date, total_tracks, album_total_artists_count, artist, artist_id)]
  exclude_terms_album = c("remix", "live", "instrumental", "wersja czysta", "clean version")
  grep_pattern = paste0("\\b", exclude_terms_album, "\\b", collapse = "|")
  albums_DT[, query_for_tracks := as.integer(album_group != "appears_on" & album_type != "compilation" & !grepl(grep_pattern, album, ignore.case = TRUE))]
  albums_Q = albums_DT[query_for_tracks == TRUE]
  
  # utwory w pętli
  albums_list = vector(mode = "list", length = albums_Q[, .N])
  for(album_iter in seq_len(albums_Q[, .N])) {
    search_query_url = "https://api.spotify.com/v1/albums/" %||% albums_Q[album_iter, album_id] %||% "/tracks"
    result =  GET(url = search_query_url,
                  add_headers(Authorization = paste0("Bearer ", access_token)),
                  query = list(market = "PL", limit = 50, offset = 0),
                  content_type_json(),
                  accept_json(),
                  verbose(info = TRUE, ssl = TRUE)
    )
    
    result_tracks = content(result)$items
    result_tracks = lapply(result_tracks, function(x) c(x, track_total_artists_count = length(x[["artists"]])))
    fields_from_response = c("track_total_artists_count", "duration_ms", "explicit", "name", "id", "uri")
    
    tracks = result_tracks %>% lapply(. %>% unlist %>% .[fields_from_response]) %>% do.call(rbind, .) %>% data.table %>% setnames(fields_from_response)
    tracks_DT = tracks[, .(track = name,
                           track_id = id,
                           duration_ms = as.integer(duration_ms),
                           track_total_artists_count = as.integer(track_total_artists_count),
                           explicit = as.integer(as.logical(explicit)),
                           artist = artist_DT$artist,
                           artist_id = artist_DT$artist_id,
                           artist_genres = artist_DT$genres,
                           artist_followers.total = artist_DT$followers.total,
                           artist_images.url = artist_DT$images.url,
                           album = albums_Q[album_iter, album],
                           album_id = albums_Q[album_iter, album_id],
                           album_release_date = albums_Q[album_iter, release_date],
                           album_group = albums_Q[album_iter, album_group]
    )]
    
    # tracks_DT = tracks[, .(track, track_id, duration_ms, track_total_artists_count, explicit, artist, artist_id, album, album_id)]
    albums_list[[album_iter]] = tracks_DT
  }
  
  tracks_DT = rbindlist(albums_list)
  
  exclude_terms_tracks = c("remix", "live", "instrumental", "wersja czysta", "clean version")
  grep_pattern_tracks = paste0("\\b", exclude_terms_tracks, "\\b", collapse = "|")
  tracks_DT[, query_for_lyrics := as.integer(!grepl(grep_pattern_tracks, album, ignore.case = TRUE))]
  
  list(artist = artist_DT, albums = albums_DT, tracks = tracks_DT)
}


# tmp = download_artist_song_lyrics(q = "Łona i Webber")
# tmp2 = download_artist_song_lyrics(artist_id = "6DMuRVdgTNmZCYRlekKWvO")
