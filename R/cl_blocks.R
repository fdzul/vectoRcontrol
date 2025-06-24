#' Load the CL dataset and links it with the INE blocks
#'
#' @param path It is the path where the dataset are hosted
#' @param cve_edo is the state id (INEGI)
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @returns a sf object
#' @export
#' @references xxxxx
#'
#' @seealso \link[vectoRcontrol]{cl_tb}
#'
#' @details xxx
#' @examples cl_blocks(path = "/Users/felipedzul/Dropbox/CENAPRECE/2025/14_jalisco", cve_edo = "14")
cl_blocks <- function(path, cve_edo){

    # Step 1. read the control larvario dataset ####
    l_files <- purrr::map(list.dirs(path = path,
                                    full.names = TRUE),
                          list.files,
                          pattern = ".txt",
                          full.names = TRUE)
    l <- unlist(purrr::map(l_files, stringr::str_subset, c("Control")))
    col_class <- c(rep("character", 12),
                   "integer", "character",
                   rep("numeric", 3), "character", rep("numeric", 2),
                   rep("character", 3),

                   rep("numeric", 17), rep("character", 5))
    x <- purrr::map_dfr(l, read.table,
                        sep = "\t",
                        allowEscapes = TRUE,
                        header = TRUE,
                        skipNul = TRUE,
                        colClasses = col_class,
                        fileEncoding = "UCS-2LE") |>
        tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") |>
        tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") |>
        tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") |>
        tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") |>
        dplyr::mutate(Localidad = stringr::str_to_title(Localidad)) |>
        dplyr::mutate(Municipio = stringr::str_trim(Municipio, side = "both"),
                      Jurisdiccion = stringr::str_trim(Jurisdiccion, side = "both")) |>
        janitor::clean_names()

    l_files <- purrr::map(list.dirs(path = path,
                                    full.names = TRUE),
                          list.files,
                          pattern = ".csv",
                          full.names = TRUE)
    l <- unlist(purrr::map(l_files, stringr::str_subset, c("Control")))

    col_class <- c(rep("character", 12),
                   "integer", "character",
                   rep("numeric", 3), "character", rep("numeric", 2),
                   rep("character", 3),

                   rep("numeric", 17), rep("character", 5))
    y <- purrr::map_dfr(.x = l,
                        .f = data.table::fread,
                        #fill=TRUE,
                        colClasses = col_class) |>
        tidyr::separate(Entidad, into = c("cve_ent", "Entidad"), extra = "merge") |>
        tidyr::separate(Municipio, into = c("cve_mpo", "Municipio"), extra = "merge") |>
        tidyr::separate(Localidad, into = c("cve_loc","Localidad"), extra = "merge") |>
        tidyr::separate(Jurisdiccion, into = c(NA, "Jurisdiccion"), extra = "merge") |>
        dplyr::mutate(Localidad = stringr::str_to_title(Localidad)) |>
        dplyr::mutate(Municipio = stringr::str_trim(Municipio, side = "both"),
                      Jurisdiccion = stringr::str_trim(Jurisdiccion, side = "both")) |>
        janitor::clean_names()

    cl <- dplyr::bind_rows(x, y) |>
        dplyr::mutate(clave = paste0(clave, manzana))

    # Step 2. load the blocks ine 2020 ####

    if(cve_edo %in% c("01", "02", "03", "04", "05", "06", "07",
                      "08", "09", "10")){
        blocks <- rgeomex::blocks_ine20_mx_a |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
            sf::st_make_valid()
    }

    if(cve_edo %in% c("11", "12", "13", "14")){
        blocks <- rgeomex::blocks_ine20_mx_b |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
            sf::st_make_valid()
    }

    if(cve_edo %in% c("15", "16", "17", "18", "19")){
        blocks <- rgeomex::blocks_ine20_mx_c |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
            sf::st_make_valid()
    }

    if(cve_edo %in% c("20", "21", "22",
                      "23", "24", "25")){
        blocks <- rgeomex::blocks_ine20_mx_d |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
            sf::st_make_valid()
    }

    if(cve_edo %in% c("26", "27", "28", "29", "30", "31", "32")){
        blocks <- rgeomex::blocks_ine20_mx_e |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
            sf::st_make_valid()
    }


    blocks <- blocks |>
        dplyr::mutate(cve_geo = paste(stringr::str_pad(entidad, pad = "0", width = 2, side = "left"),
                                      stringr::str_pad(municipio, pad = "0", width = 3, side = "left"),
                                      stringr::str_pad(localidad, pad = "0", width = 4, side = "left"),
                                      stringr::str_pad(seccion, pad = "0", width = 4, side = "left"),
                                      stringr::str_pad(manzana, pad = "0", width = 4, side = "left"),
                                      sep = ""))

    # Step 3. left joint control larvario and blocks ine 2020 datasets #### 10542
    dplyr::left_join(x = blocks |> dplyr::select(geometry, id, cve_geo),
                     y = cl,
                     by  = c("cve_geo" = "clave")) |>
        dplyr::filter(!is.na(cve_ent))  |>
        dplyr::mutate(coverage = dplyr::case_when(cobertura_en_manzana < 51 ~ "Deficiente",
                                                  cobertura_en_manzana >= 51 & cobertura_en_manzana < 70 ~ "Regular",
                                                  cobertura_en_manzana >= 70 & cobertura_en_manzana < 85 ~ "Bueno",
                                                  cobertura_en_manzana >= 85 ~ "Óptimo",
                                                  T ~ "NA")) |>
        dplyr::mutate(coverage =  factor(coverage,
                                         levels = c("Bueno","Deficiente","Óptimo","Regular")[c(3, 1, 4, 2)],
                                         labels = c("Bueno","Deficiente","Óptimo","Regular")[c(3, 1, 4, 2)]))

}
