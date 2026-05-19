guess_encoding_and_read_text <- function(file_path,
                                         default_guess = "UTF-8",
                                         only_allow = c("UTF*"),
                                         raw_data = NULL) {
  # If raw_data is not already provided, read it from the file
  if (is.null(raw_data)) {
    raw_data <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  }
  
  # Detect encodings using stringi
  enc_info_list <- stri_enc_detect(raw_data)
  
  # If there are no guesses at all, fall back immediately
  if (length(enc_info_list) == 0 || nrow(enc_info_list[[1]]) == 0) {
    warning(sprintf("No encoding guess could be made. Defaulting to '%s'.", default_guess))
    best_enc <- default_guess
  } else {
    enc_info <- enc_info_list[[1]]  # data frame with "Encoding" and "Confidence"
    
    # Filter encodings only if `only_allow` is not NULL
    if (!is.null(only_allow)) {
      # Convert each element of only_allow to a regex via glob2rx()
      keep_rows <- lapply(only_allow, function(p) {
        grep(glob2rx(p), enc_info$Encoding, ignore.case = TRUE)
      })
      keep_rows <- unique(unlist(keep_rows))
      
      enc_info_filtered <- enc_info[keep_rows, ]
      
      if (nrow(enc_info_filtered) == 0) {
        warning(sprintf(
          "No encodings matched patterns: %s. Defaulting to '%s'.", 
          paste(only_allow, collapse=", "), default_guess
        ))
        best_enc <- default_guess
      } else {
        best_row <- which.max(enc_info_filtered$Confidence)
        best_enc <- enc_info_filtered$Encoding[best_row]
      }
    } else {
      # only_allow is NULL -> allow any encodings
      best_row <- which.max(enc_info$Confidence)
      best_enc <- enc_info$Encoding[best_row]
    }
  }
  
  # Convert the raw bytes to text (best_enc -> UTF-8)
  text_content <- stri_encode(raw_data, from = best_enc, to = "UTF-8")
  
  # Return a list with raw data, text, and final guessed encoding
  list(
    raw              = raw_data,
    text             = text_content,
    guessed_encoding = best_enc
  )
}


guess_encoding_and_read_html <- function(file_path, 
                                         default_guess = "UTF-8", 
                                         only_allow = c("UTF*")) {
  restore.point("guess_encoding_and_read_html")
  library(xml2)
  # (A) Read raw bytes once
  raw_data <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  
  # (B) Decode as UTF-8 (the "initial assumption") and parse HTML
  text_default <- stringi::stri_encode(raw_data, from = "UTF-8", to = "UTF-8")
  
  # # Handle any decoding errors gracefully by ignoring invalid bytes:
  # text_default <- stringi::stri_encode(raw_data, from = "UTF-8", to = "UTF-8", 
  #                                       to_raw = FALSE, 
  #                                       ignore_errors = TRUE)
  
  doc <- tryCatch(
    read_html(text_default),
    error = function(e) {
      # If decoding as UTF-8 fails badly, fall back to guess_encoding_and_read_text
      message("Failed to parse as UTF-8: ", e$message)
      return(NULL)
    }
  )
  
  # If we couldn't parse it at all, skip meta detection
  if (is.null(doc)) {
    # fallback: guess
    guessed <- guess_encoding_and_read_text(file_path, default_guess, only_allow, raw_data)
    doc <- read_html(guessed$text)
    return(list(
      doc              = doc,
      text             = guessed$text,
      guessed_encoding = guessed$guessed_encoding
    ))
  }
  
  # (C) Check for meta charset
  # First, try:  <meta charset="...">
  meta_node <- xml_find_first(doc, "//meta[@charset]")
  meta_charset <- NA_character_
  if (!inherits(meta_node, "xml_missing") && !is.na(meta_node)) {
    meta_charset <- xml_attr(meta_node, "charset")
  }
  
  # If not found, try: <meta http-equiv="Content-Type" content="text/html; charset=...">
  if (is.na(meta_charset) || meta_charset == "") {
    meta_node_2 <- xml_find_first(doc, "//meta[@http-equiv='Content-Type'][contains(@content, 'charset=')]")
    if (!inherits(meta_node_2, "xml_missing") && !is.na(meta_node_2)) {
      content_val <- xml_attr(meta_node_2, "content")
      # parse out "charset=..."
      # e.g. content="text/html; charset=ISO-8859-1"
      meta_charset <- sub(".*charset=([^;]+).*", "\\1", content_val, ignore.case = TRUE)
    }
  }
  
  # (D) If we found a declared encoding in the meta tag, re-decode using that
  if (!is.na(meta_charset) && meta_charset != "") {
    # Re-decode raw data with meta_charset -> UTF-8
    new_text <- tryCatch(
      stringi::stri_encode(raw_data, from = meta_charset, to = "UTF-8"),
      error = function(e) {
        message(sprintf("Failed to decode using meta charset '%s': %s", meta_charset, e$message))
        return(NULL)
      }
    )
    
    if (is.null(new_text)) {
      # If decoding with meta_charset fails, fallback to guess_encoding
      guessed <- guess_encoding_and_read_text(file_path, default_guess, only_allow, raw_data)
      final_doc <- read_html(guessed$text)
      return(list(
        doc              = final_doc,
        text             = guessed$text,
        guessed_encoding = guessed$guessed_encoding
      ))
    } else {
      # Successfully re-decoded the data -> parse it
      final_doc <- read_html(new_text)
      return(list(
        doc              = final_doc,
        text             = new_text,
        guessed_encoding = meta_charset
      ))
    }
  } else {
    # (E) No meta charset found -> fallback to heuristics
    guessed <- guess_encoding_and_read_text(file_path, default_guess, only_allow, raw_data)
    final_doc <- read_html(guessed$text)
    return(list(
      doc              = final_doc,
      text             = guessed$text,
      guessed_encoding = guessed$guessed_encoding
    ))
  }
}

