#' Load the cleaned and jittered data. Optionally do various cleaning/data prep
#' tasks.
#' #' @export
#'
#' @param google_id (str) Google file ID. File must be set to shareable via link.
#' @param local_path (str) (Optional) Local path to the data.
#' @param dedup (bool) (Optional) Whether or not to remove duplicate rows (occurs when someone moves)
#' @param drop_missing_coords (bool) (Optional) Whether or not to drop rows with missing X/Y coordinates
#' @param to_numeric (bool) (Optional) Whether or not to convert character columns (gender) to numeric
#' @param N (int) (Optional) A positive integer indicating the years to keep for each person
#' @return (data.frame) A data frame of people
#' @examples
#' load_data('abc123', dedup=TRUE, drop_missing_coords=FALSE, ...)
load_data = function(google_id, local_path=NULL, dedup=FALSE, drop_missing_coords=FALSE, to_numeric=FALSE, N=0) { # nolint
  if (!is.null(local_path)) {
    df = read.csv(local_path)
  } else {
    df = read.csv(paste0('https://drive.google.com/uc?id=', google_id))
  }

  if (dedup) {
    # Remove duplicate rows (when someone moves)
    df = df %>% dplyr::group_by(PID, YEAR) %>% dplyr::summarise_all(dplyr::first)
  }

  if (drop_missing_coords) {
    # Drop rows with missing coordinates
    df = df[!is.na(df$X_COORD), ]
    df = df[!is.na(df$Y_COORD), ]
  }

  if (to_numeric) {
    # Convert gender to numeric
    df$GENDER = as.numeric(as.factor(df$GENDER))
  }

  if (N > 0) {
    df = df %>% dplyr::group_by(PID) %>% dplyr::filter(max(YEAR) - YEAR <= N)
  }

  return(df)
}


#' Fill missing smoking values for each CIG_CAT
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @return (data.frame) Same data as df, but with missing CIGDUR values filled
#' @examples
#' fill_missing_cigdur(df)
fill_missing_cigdur = function(df) {
  # Fill -1 (occasional) with the 25th percentile of CIGDUR for current smokers (2).
  if(nrow(df[df$CIG_CAT == -1 & is.na(df$CIGDUR),]) > 0) {
    occ_cigdur = quantile(df[df$CIG_CAT == 2, ]$CIG_CAT, 0.25)
    occ_cigdur = unname(occ_cigdur)
    df[df$CIG_CAT == -1 & is.na(df$CIGDUR),]$CIGDUR = occ_cigdur
  }

  # Fill 0 (non-smoker) with zero.
  if(nrow(df[df$CIG_CAT == 0 & is.na(df$CIGDUR),]) > 0) {
    df[df$CIG_CAT == 0 & is.na(df$CIGDUR),]$CIGDUR = 0
  }

  # Fill 1 (former smoker) with median of former smokers
  if(nrow(df[df$CIG_CAT == 1 & is.na(df$CIGDUR),]) > 0) {
    form_cigdur = median(df[df$CIG_CAT == 1, ]$CIGDUR, na.rm=TRUE)
    df[df$CIG_CAT == 1 & is.na(df$CIGDUR), ]$CIGDUR = form_cigdur
  }

  # Fill 2 (current smoker) with median of current smokers
  if(nrow(df[df$CIG_CAT == 2 & is.na(df$CIGDUR),]) > 0) {
    curr_cigdur = median(df[df$CIG_CAT == 2, ]$CIGDUR, na.rm=TRUE)
    df[df$CIG_CAT == 2 & is.na(df$CIGDUR), ]$CIGDUR = curr_cigdur
  }

  # Fill 9 (unknown) with the overall data median
  if(nrow(df[df$CIG_CAT == 9 & is.na(df$CIGDUR),]) > 0) {
    unk_cigdur = median(df$CIGDUR, na.rm=TRUE)
    df[df$CIG_CAT == 9 & is.na(df$CIGDUR), ]$CIGDUR = unk_cigdur
  }

  return(df)
}


#' Get a data frame with just the PID and CASE_CNTL values
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @return (data.frame) Data frame with just the PID and CASE_CNTL values
#' @examples
#' get_case_cntl(df)
get_case_cntl = function(df) {
  person_cc_df = df %>% dplyr::group_by(PID) %>% dplyr::summarise(cc = max(CASE_CNTL))
  cc_df = person_cc_df[, c('PID', 'cc')]
  return(cc_df)
}


#' Train-test split by person, so that no person shows up in both the training and test set.
#' Note that test_size refers to the number/percentage of _people_, not of _rows_.
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param test_size (number) Float between 0 and 1 indicating a percentage of the data to use
#' as the test set, or integer indicating the number of rows to use as the test set.
#' @param combine (bool) (default=FALSE) Whether or not to combine the training and test sets with test
#' set marked as `validation == 1`.
#' @return (list<data.frame>) Training set, test set, optionally combined set.
#' @examples
#' tts_df_list = get_case_cntl(df)
#' tts_df_list$train
#' tts_df_list$test
train_test_split = function(df, test_size, combine=FALSE) {
  if (test_size > 0 && test_size < 1) {
    test_size = round(length(unique(df$PID)) * test_size)
  } else if (test_size >= 1) {
    test_size = round(test_size)
  } else {
    stop("test_size must be a number between 0 and 1 or an integer less than the number of rows of df")
  }
  test_pid = sample(unique(df$PID), size=test_size, replace=FALSE)
  train_pid = setdiff(unique(df$PID), test_pid)

  train_df = df[df$PID %in% train_pid, ]
  test_df = df[df$PID %in% test_pid, ]

  if (combine) {
    train_df$validation = 0
    test_df$validation = 1
    comb_df = rbind(train_df, test_df)
    return(list(train=train_df, test=test_df, combined=comb_df))
  } else {
    return(list(train=train_df, test=test_df))
  }
}

#' Take data with one row per residential location, and expand it to have one
#' row per year. Since the last year of one location coincides with the first
#' year of the next location, we average their pollutant exposures to arrive
#' at a single value.
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @return (data.frame) Same data, but with one row per year
#' @examples
#' df_expanded = explode_year_rows(df)
explode_year_rows = function(df) {
  # Add a column YEAR which is a CSV of the sequence of years
  for(i in 1:nrow(df)) {
    df[i, 'YEARS']=paste0(as.character(seq(df[i, 'RES_YRIN'], df[i, 'RES_YROUT'])), collapse=",")
  }

  # Split the rows by year
  df = tidyr::separate_rows(df, YEARS, sep=',', convert=TRUE)

  # Convert to data frame
  df = as.data.frame(df)

  # This introduces duplicate rows where one combo ends and another begins. So average the values.
  df = df %>% group_by(PID, YEARS) %>% summarise_all(mean)

  return(as.data.frame(df))
}
