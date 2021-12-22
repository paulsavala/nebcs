
#' Returns health column names
#' @export
#'
#' @return (array) Array of health column names
#' @examples
#' get_health_cols()
get_health_cols = function() {
    health_cols = c('REGION', 'GENDER', 'REF_AGE_GRP2',
       'RACE_GRP2', 'HISPANIC_YN', 'FRENCH', 'CIG_CAT', 'BLACKEVER',
       'EDUC_GRP', 'CIGDUR')
    return(health_cols)
}