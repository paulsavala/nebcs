#' Load the cleaned and jittered data. Optionally do various cleaning/data prep
#' tasks.
#'
#' @param path (str) path to data file
#' @param dedup (bool) (Optional) Whether or not to remove duplicate rows (occurs when someone moves)
#' @param drop_missing_coords (bool) (Optional) Whether or not to drop rows with missing X/Y coordinates
#' @param to_numeric (bool) (Optional) Whether or not to convert character columns (gender) to numeric
#' @param N (int) (Optional) A positive integer indicating the years to keep for each person
#' @return df (data.frame) A data frame of people
#' @examples
#' load_data('my_data.csv', dedup=TRUE, drop_missing_coords=FALSE, ...)
load_data = function(path, dedup=FALSE, drop_missing_coords=FALSE, to_numeric=FALSE, N=0) {
  df = read.csv(path)

  if (dedup) {
    # Remove duplicate rows (when someone moves)
    df = df %>% dplyr::group_by(PID, YEAR) %>% dplyr::summarise_all(first)
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


#' Fill missing smoking values using the medians for each CIG_CAT
#'
#' @param df (data.frame) NEBCS data
#' @return df (data.frame) Same data as df, but with missing CIGDUR values filled
#' @examples
#' fill_missing_cigdur(df)
fill_missing_cigdur = function(df) {
  ###### Smoking missing values ######
  # Fill -1 (occasional) with the 25th percentile of CIGDUR for current smokers (2).
  if(nrow(df[df$CIG_CAT == -1 & is.na(df$CIGDUR),]) > 0) {
    occ_cigdur = quantile(df[df$CIG_CAT == 2, ]$CIG_CAT, 0.25)
    occ_cigdur = unname(occ_cigdur)
    df[df$CIG_CAT == -1 & is.na(df$CIGDUR),]$CIGDUR = occ_cigdur
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
