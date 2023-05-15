require(spotifyr)
require(dplyr)

#SPOTIFY API AUTHENTICATION
Sys.setenv(SPOTIFY_CLIENT_ID = '4628f2a64bf840349770226f1e48ed2c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8dcc24c661b64196aa3c75cef38fc4e2')
spotify_access_token <- get_spotify_access_token()

#GETTING RAW DATA
onedirection_raw <- get_artist_audio_features(artist="One Direction",
                                          include_groups="album",
                                          authorization = get_spotify_access_token())
zayn <- get_artist_audio_features(artist="ZAYN",
                                          include_groups="album",
                                          authorization = get_spotify_access_token())
harry <- get_artist_audio_features(artist="Harry Styles",
                                  include_groups="album",
                                  authorization = get_spotify_access_token())
louis <- get_artist_audio_features(artist="Louis Tomlinson",
                                  include_groups="album",
                                  authorization = get_spotify_access_token())
niall <- get_artist_audio_features(artist="Niall Horan",
                                  include_groups="album",
                                  authorization = get_spotify_access_token())
liam <- get_artist_audio_features(artist="Liam Payne",
                                  include_groups="album",
                                  authorization = get_spotify_access_token())
bind_rows(onedirection_raw,zayn,harry,louis,niall,liam) -> onedirection_raw

#ADDTIONAL RAW DATA: Removes Duplicates/Focusing on Albums (not EPs) and Adds Popularity Score
get_playlist_audio_features(username="sarahrizky", 
                            playlist_uris ="6jptnQo72cLfmhcnQ6avk9", 
                            authorization = get_spotify_access_token()) -> onedirection_raw2
onedirection_raw2 %>%
  rename("track_id"="track.id") ->onedirection_raw2

#INITIAL DATA MANAGEMENT
onedirection <- merge(onedirection_raw, onedirection_raw2, by = c("track_id"))  
onedirection %>%
  select("artist_name","album_release_date","album_release_year","danceability.x","energy.x","key.x","loudness.x",
         "speechiness.x","acousticness.x","instrumentalness.x","liveness.x","valence.x","tempo.x","time_signature.x","duration_ms",
         "explicit","track_name","track_number","album_name","key_mode.x", "track.popularity") -> onedirection
names(onedirection) <- c("artist_name","album_release_date","album_release_year","danceability","energy","key","loudness",
        "speechiness","acousticness","instrumentalness","liveness","valence","tempo","time_signature","duration_ms",
        "explicit","track_name","track_number","album_name","key_mode", "popularity")

write.csv(onedirection, file="onedirection.csv", row.names = FALSE)











