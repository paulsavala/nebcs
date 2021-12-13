

#' Returns pollutant columns
#' @export
#'
#' @return (array) Array of pollutant column names
#' @examples
#' get_pollutant_cols()
get_pollutant_cols = function() {
    pol_cols = c(
        'RES_ASCNC_PERYR', # Residential arsenic per year
        'RES_ASWAT_PERYR', # Residential arsenic from water per year
        'RO_ASCNC_PERYR', # Residential and work arsenic per year
        'RO_ASWAT_PERYR', # Residential and work arsenic from water per year
        'RES_TTHMCNC_PERYR', # Residential THM per year
        'RES_TTHMWAT_PERYR', # Residential THM from water per year
        'RO_TTHMCNC_PERYR', # Residential and work THM per year
        'RO_TTHMWAT_PERYR', # Residential and work THM from water per year
        'NITRATE_PWSPM_CNC_PERYR', # Nitrate per year
        'NITRATE_PWSPM_WAT_PERYR', # Nitrate from water per year
        'RO_NITPWSPM_CNC_PERYR', # Residential and work nitrate per year
        'RO_NITPWSPM_WAT_PERYR', # Residential and work nitrate from water per year
        'RO_UNCONS3_PERYR', # Residential and work unconsolidated wells per year
        'POTATOES',
        'BLUEBERRIES', 
        'APPLES',
        'APPLEBLUE',
        'ALLCROPS')
    return(pol_cols)
}


#' Returns named list of pollutant column names, grouped by pollutant type
#' @export
#'
#' @return (list<array>) Named list of pollutant column names, grouped by pollutant type
#' @examples
#' group_pollutant_cols()
group_pollutant_cols = function() {
    arsenic_cols = c('RES_ASCNC_PERYR', 'RES_ASWAT_PERYR', 'RO_ASCNC_PERYR', 'RO_ASWAT_PERYR')
    thm_cols = c('RES_TTHMCNC_PERYR', 'RES_TTHMWAT_PERYR', 'RO_TTHMCNC_PERYR', 'RO_TTHMWAT_PERYR')
    nitrate_cols = c('NITRATE_PWSPM_CNC_PERYR', 'NITRATE_PWSPM_WAT_PERYR', 'RO_NITPWSPM_CNC_PERYR', 'RO_NITPWSPM_WAT_PERYR')
    water_cols = c('RO_UNCONS3_PERYR')
    crop_cols = c('POTATOES', 'BLUEBERRIES', 'APPLES', 'APPLEBLUE', 'ALLCROPS')
    return(list(arsenic_cols=arsenic_cols, thm_cols=thm_cols, nitrate_cols=nitrate_cols, water_cols=water_cols, crop_cols=crop_cols))
}
