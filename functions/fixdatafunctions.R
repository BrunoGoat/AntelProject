clean_data_frame <- function(nombre_archivo_csv) {
  # Leer el CSV
  df_raw <- read.csv(nombre_archivo_csv, stringsAsFactors = FALSE)
  df_original <- df_raw  # Backup por si acaso
  
  # Extraer ga_session_id
  df_raw <- df_raw %>%
    mutate(ga_session_id = map_dbl(event_params, function(x) {
      parsed <- tryCatch(fromJSON(x), error = function(e) return(NULL))
      if (is.null(parsed)) return(NA_real_)
      params <- parsed$event_params
      if (is.null(params) || !is.data.frame(params)) return(NA_real_)
      fila <- params[params$key == "ga_session_id", ]
      if (nrow(fila) == 0) return(NA_real_)
      valor <- fila$value$int_value
      valor <- suppressWarnings(as.numeric(valor))
      if (is.na(valor)) return(NA_real_)
      return(valor)
    }))
  
  # Eliminar sesiones con múltiples user_pseudo_id
  sesiones_invalidas <- df_raw %>%
    group_by(ga_session_id) %>%
    filter(n_distinct(user_pseudo_id) > 1) %>%
    pull(ga_session_id) %>%
    unique()
  
  df_clean <- df_raw %>%
    filter(!is.na(ga_session_id)) %>%
    filter(!ga_session_id %in% sesiones_invalidas)
  
  # Eliminar columnas innecesarias
  columnas_a_eliminar <- c(
    "collected_traffic_source", "is_active_user", "batch_page_id",
    "batch_ordering_id", "session_traffic_source_last_click",
    "publisher", "event_dimensions", "user_ltv", "user_id", "event_value_in_usd"
  )
  df_clean <- df_clean %>%
    select(-any_of(columnas_a_eliminar))
  
  # Extraer geo_city y geo_country
  extraer_ciudad <- function(geo_json) {
    if (is.na(geo_json) || geo_json == "") return(NA_character_)
    json <- tryCatch(fromJSON(geo_json), error = function(e) return(NA_character_))
    if (is.null(json$geo$city) || json$geo$city == "") return(NA_character_)
    return(json$geo$city)
  }
  
  extraer_pais <- function(geo_json) {
    if (is.na(geo_json) || geo_json == "") return(NA_character_)
    json <- tryCatch(fromJSON(geo_json), error = function(e) return(NA_character_))
    if (is.null(json$geo$country) || json$geo$country == "") return(NA_character_)
    return(json$geo$country)
  }
  
  df_clean <- df_clean %>%
    mutate(
      geo_city = as.factor(unname(vapply(geo, extraer_ciudad, character(1)))),
      geo_country = as.factor(unname(vapply(geo, extraer_pais, character(1))))
    ) %>%
    select(-geo) %>%
    group_by(geo_country) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    group_by(geo_city) %>%
    filter(n() >= 10) %>%
    ungroup()
  
  # Extraer device_category
  extraer_categoria_device <- function(device_json) {
    if (is.na(device_json) || device_json == "") return(NA_character_)
    json <- tryCatch(fromJSON(device_json), error = function(e) return(NA_character_))
    if (is.null(json$device$category) || json$device$category == "") return(NA_character_)
    return(json$device$category)
  }
  
  df_clean <- df_clean %>%
    mutate(
      device_category = as.factor(unname(vapply(device, extraer_categoria_device, character(1))))
    ) %>%
    select(-device)
  
  # Extraer item_category
  df_clean <- df_clean %>%
    mutate(item_category = sapply(items, function(x) {
      if (is.na(x) || x == "" || x == "{}") return(NA)
      parsed <- tryCatch(fromJSON(x), error = function(e) return(NULL))
      if (is.null(parsed) || is.null(parsed$items) || length(parsed$items) == 0) {
        return(NA)
      }
      return(parsed$items$item_category[1])
    })) %>%
    mutate(
      item_category = ifelse(is.na(item_category), "NADA", item_category),
      item_category = as.factor(item_category)
    ) %>%
    select(-items)
  
  # Extraer source y medium
  extraer_source <- function(traffic_json) {
    if (is.na(traffic_json) || traffic_json == "") return(NA_character_)
    parsed <- tryCatch(fromJSON(traffic_json), error = function(e) return(NA_character_))
    if (is.null(parsed$traffic_source$source)) return(NA_character_)
    return(parsed$traffic_source$source)
  }
  
  extraer_medium <- function(traffic_json) {
    if (is.na(traffic_json) || traffic_json == "") return(NA_character_)
    parsed <- tryCatch(fromJSON(traffic_json), error = function(e) return(NA_character_))
    if (is.null(parsed$traffic_source$medium)) return(NA_character_)
    return(parsed$traffic_source$medium)
  }
  
  df_clean <- df_clean %>%
    mutate(
      source = as.factor(unname(vapply(traffic_source, extraer_source, character(1)))),
      medium = as.factor(unname(vapply(traffic_source, extraer_medium, character(1))))
    ) %>%
    select(-traffic_source)
  
  # Extraer ads_storage
  df_clean <- df_clean %>%
    mutate(ads_storage = sapply(privacy_info, function(x) {
      if (is.na(x) || x == "" || x == "{}") return(NA)
      parsed <- tryCatch(fromJSON(x), error = function(e) return(NULL))
      if (is.null(parsed) || is.null(parsed$privacy_info)) return(NA)
      return(parsed$privacy_info$ads_storage)
    })) %>%
    mutate(ads_storage = ads_storage == "Yes")
  
  # Extraer first_open_time
  extraer_first_open_time <- function(json_str) {
    if (is.na(json_str) || json_str == "") return(FALSE)
    parsed <- tryCatch(fromJSON(json_str), error = function(e) return(FALSE))
    return("first_open_time" %in% names(parsed$user_properties))
  }
  
  df_clean <- df_clean %>%
    mutate(
      first_open_time = vapply(user_properties, extraer_first_open_time, logical(1))
    )
  
  # Agregar info resumen por sesión
  info_tiempo <- df_clean %>%
    group_by(ga_session_id) %>%
    summarise(
      mean_event_timestamp = mean(event_timestamp, na.rm = TRUE),
      mean_event_previous_timestamp = mean(event_previous_timestamp, na.rm = TRUE),
      nro_events = n(),
      ads_storage = any(ads_storage == TRUE, na.rm = TRUE),
      first_open_time = any(first_open_time == TRUE, na.rm = TRUE),
      user_first_touch_timestamp = first(user_first_touch_timestamp)
    ) %>%
    ungroup()
  
  df_clean <- df_clean %>%
    left_join(info_tiempo, by = "ga_session_id")
  
  # Guardar resultados
  save(df_clean, info_tiempo, file = "data_cleaned.RData")
  
  return(df_clean)
}

generate_sessions_df <- function(df) {
  
  # 1) Extraer campos de interés desde event_params ------------------------
  extraer_event_params <- function(x) {
    tryCatch({
      parsed <- fromJSON(x)
      params <- parsed$event_params
      resultado <- list(
        engagement_time_msec     = 0,
        firebase_screen_class    = NA_character_,
        firebase_previous_class  = NA_character_,
        firebase_event_origin    = NA_character_
      )
      for (i in seq_len(nrow(params))) {
        key <- params$key[i]; val <- params$value[i, ]
        if (key == "engagement_time_msec" && !is.null(val$int_value)) {
          resultado$engagement_time_msec <- as.numeric(val$int_value)
        } else if (key == "firebase_screen_class" && !is.null(val$string_value)) {
          resultado$firebase_screen_class <- val$string_value
        } else if (key == "firebase_previous_class" && !is.null(val$string_value)) {
          resultado$firebase_previous_class <- val$string_value
        } else if (key == "firebase_event_origin" && !is.null(val$string_value)) {
          resultado$firebase_event_origin <- val$string_value
        }
      }
      resultado
    }, error = function(e) {
      list(
        engagement_time_msec     = 0,
        firebase_screen_class    = NA_character_,
        firebase_previous_class  = NA_character_,
        firebase_event_origin    = NA_character_
      )
    })
  }
  
  # 2) Enriquecer df con event_params --------------------------------------
  event_info <- map_dfr(df$event_params, extraer_event_params)
  df2 <- bind_cols(df, event_info)
  
  # 3) Unificar columnas con sufijos .x / .y ------------------------------
  df2 <- df2 %>% mutate(
    user_first_touch_timestamp = coalesce(user_first_touch_timestamp.x, user_first_touch_timestamp.y),
    first_open_time            = coalesce(first_open_time.x,            first_open_time.y),
    ads_storage                = coalesce(ads_storage.x,                ads_storage.y)
  )
  
  # 4) Info adicional por sesión ------------------------------------------ ------------------------------------------ por sesión ------------------------------------------
  info_extra_sesion <- df2 %>%
    group_by(ga_session_id) %>%
    summarise(
      event_date      = first(event_date),
      platform        = first(platform),
      geo_city        = first(geo_city),
      geo_country     = first(geo_country),
      device_category = first(device_category),
      source          = first(source),
      medium          = first(medium),
      ads_storage     = first(ads_storage),
      .groups = "drop"
    )
  

# 5) Info temporal y conteos ---------------------------------------------
info_tiempo <- df2 %>%
  group_by(ga_session_id) %>%
  summarise(
    mean_event_timestamp           = mean(event_timestamp,            na.rm = TRUE),
    mean_event_previous_timestamp  = mean(event_previous_timestamp,   na.rm = TRUE),
    first_open_time                = first(first_open_time),
    user_first_touch_timestamp     = first(user_first_touch_timestamp),
    nro_events                     = n(),
    .groups = "drop"
  )

# 6) Resumen principal por sesión ---------------------------------------
session_df <- df2 %>%
  group_by(ga_session_id) %>%
  summarise(
    user_pseudo_id          = first(user_pseudo_id),
    user_engagement         = any(event_name == "user_engagement"),
    play                    = any(event_name == "play"),
    session_start           = any(event_name == "session_start"),
    first_use               = any(event_name == "first_open"),
    screen_view_count       = sum(event_name == "screen_view"),
    firebase_screen_class   = last(na.omit(firebase_screen_class)),
    firebase_previous_class = last(na.omit(firebase_previous_class)),
    firebase_event_origin   = last(na.omit(firebase_event_origin)),
    engagement_time_msec    = sum(engagement_time_msec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(info_extra_sesion, by = "ga_session_id") %>%
  left_join(info_tiempo,       by = "ga_session_id")

# 7) Renombrar y ordenar columnas ---------------------------------------
session_df %>%
  rename(
    date = event_date,
    id   = ga_session_id
  ) %>%
  select(
    date, id, session_start, first_use, user_engagement,
    engagement_time_msec, mean_event_timestamp, mean_event_previous_timestamp,
    first_open_time, user_first_touch_timestamp, screen_view_count,
    firebase_screen_class, firebase_previous_class, firebase_event_origin,
    nro_events, device_category, platform, medium, source, geo_city,
    geo_country, ads_storage, play
  )
}








