#' Generate a larval control table
#'
#' @param path is the path of files.
#' @param jur is the Jurisdiccion.
#' @param state is the logical value, when state = TRUE the output is statal, else jurisdiccional.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a table.
#'
#' @export
#'
#' @examples 1+1
#'
#' @references xxxxx
#'
#' @seealso \link[formattable]{formattable}
#'
#' @details xxx
tb_cl <- function(path, inf, jur = NULL, mun){

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

    x <- dplyr::bind_rows(x, y)



    if(state == TRUE){
        xa <- x |>
            dplyr::filter(semana_epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) |>
            dplyr::filter(jurisdiccion == jur) |>
            dplyr::group_by(municipio) |>
            dplyr::summarise("Casas Visitadas hasta la Semana" = sum(casas_revisadas, na.rm = TRUE),
                             "Casas Trabajadas hasta la Semana" = sum(casas_trabajadas,na.rm = TRUE))
        xb <- x |>
            dplyr::filter(jurisdiccion == jur) |>
            dplyr::filter(semana_epidemiologica %in% c((lubridate::week(Sys.time())-9):(lubridate::week(Sys.time())-1))) |>
            dplyr::group_by(municipio) |>
            dplyr::summarise("Casas Visitadas en los dos últimos meses" = sum(casas_revisadas, na.rm = TRUE),
                             "Casas Trabajadas en los dos últimos meses" = sum(casas_trabajadas,na.rm = TRUE))
        xc <- x |>
            dplyr::filter(jurisdiccion == jur) |>
            dplyr::filter(semana_epidemiologica == lubridate::week(Sys.time())-1) |>
            dplyr::group_by(municipio) |>
            dplyr::summarise("Casas Visitadas en la Semana" = sum(casas_revisadas, na.rm = TRUE),
                             "Casas Trabajadas en la Semana" = sum(casas_trabajadas,na.rm = TRUE))
        #xab <- dplyr::left_join(x = xa, y = xb, by = "Municipio")
        xabc <- dplyr::left_join(x = dplyr::left_join(x = xa,
                                                      y = xb,
                                                      by = "municipio"),
                                 y = xc,
                                 by = "municipio") |>
            tidyr::replace_na(list(`Casas Visitadas hasta la Semana` = 0,
                                   `Casas Trabajadas hasta la Semana` = 0,
                                   `Casas Visitadas en los dos últimos meses` = 0,
                                   `Casas Trabajadas en los dos últimos meses` = 0,
                                   `Casas Visitadas en la Semana` = 0,
                                   `Casas Trabajadas en la Semana` = 0)) |>
            dplyr::arrange(desc(`Casas Trabajadas hasta la Semana`))
        formattable::formattable(xabc,
                                 align = c("c","c","c", "c", "c", "c", "c"),
                                 list(
                                     municipio = formattable::formatter("span", style = x ~ formattable::style(color =  "black",
                                                                                                               font.weight = "bold")),
                                     "Casas Visitadas hasta la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                                     "Casas Trabajadas hasta la Semana" = formattable::color_tile("white", "orange"),
                                     "Casas Visitadas en los dos últimos meses" = formattable::color_tile("#DeF7E9", "#71CA97"),
                                     "Casas Trabajadas en los dos últimos meses" = formattable::color_tile("white", "orange"),
                                     "Casas Visitadas en la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                                     "Casas Trabajadas en la Semana" = formattable::color_tile("white", "orange")))
    } else {
        xa <- x |>
            dplyr::filter(semana_epidemiologica %in% c(1:(lubridate::week(Sys.time())-1))) |>
            dplyr::group_by(jurisdiccion) |>
            dplyr::summarise("Casas Visitadas hasta la Semana" = sum(casas_revisadas, na.rm = TRUE),
                             "Casas Trabajadas hasta la Semana" = sum(casas_trabajadas,na.rm = TRUE))
        xb <- x |>
            dplyr::filter(semana_epidemiologica %in% c((lubridate::week(Sys.time())-9):(lubridate::week(Sys.time())-1))) |>
            dplyr::group_by(jurisdiccion) |>
            dplyr::summarise("Casas Visitadas en los dos últimos meses" = sum(casas_revisadas, na.rm = TRUE),
                             "Casas Trabajadas en los dos últimos meses" = sum(casas_trabajadas,na.rm = TRUE))
        xc <- x |>
            dplyr::filter(semana_epidemiologica == lubridate::week(Sys.time())-1) |>
            dplyr::group_by(jurisdiccion) |>
            dplyr::summarise("Casas Visitadas en la Semana" = sum(casas_revisadas, na.rm = TRUE),
                             "Casas Trabajadas en la Semana" = sum(casas_trabajadas,na.rm = TRUE))
        xabc <- dplyr::left_join(x = dplyr::left_join(x = xa,
                                                      y = xb,
                                                      by = "jurisdiccion"),
                                 y = xc,
                                 by = "jurisdiccion") |>
            tidyr::replace_na(list(`Casas Visitadas hasta la Semana` = 0,
                                   `Casas Trabajadas hasta la Semana` = 0,
                                   `Casas Visitadas en los dos últimos meses` = 0,
                                   `Casas Trabajadas en los dos últimos meses` = 0,
                                   `Casas Visitadas en la Semana` = 0,
                                   `Casas Trabajadas en la Semana` = 0)) |>
            dplyr::arrange(desc(`Casas Trabajadas hasta la Semana`))
        formattable::formattable(xabc,
                                 align = c("c","c","c", "c", "c", "c", "c"),
                                 list(
                                     jurisdiccion = formattable::formatter("span", style = x ~ formattable::style(color =  "black",
                                                                                                                  font.weight = "bold")),
                                     "Casas Visitadas hasta la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                                     "Casas Trabajadas hasta la Semana" = formattable::color_tile("white", "orange"),
                                     "Casas Visitadas en los dos últimos meses" = formattable::color_tile("#DeF7E9", "#71CA97"),
                                     "Casas Trabajadas en los dos últimos meses" = formattable::color_tile("white", "orange"),
                                     "Casas Visitadas en la Semana" = formattable::color_tile("#DeF7E9", "#71CA97"),
                                     "Casas Trabajadas en la Semana" = formattable::color_tile("white", "orange")))

    }
}
