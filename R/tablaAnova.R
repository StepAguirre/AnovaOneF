#' Tabla Anova
#'
#' Realiza una Tabla Anova de un factor para comparar la producción lechera de dos razas de ganado.
#'
#' @param data base de datos
#' @param a (vector) variable que representa los datos de la primera raza.
#' @param b (vector) variable que representa los datos de la segunda raza.
#' @return un dataframe con la Tabla Anova de un factor.
#' @export
#'
#' @examples
#' /dontrun{
#' ## Directorio de trabajo
#' ruta <- "~/ruta/test.csv"
#'
#' -------------------------------------
#' # Ejemplo 1:
#' df <- read.csv(ruta)
#' # Cargo la libreria
#' library(AnovaOneF)
#' Tanova(a = df$HOLSTEIN, b = df$JERSEY)
#'
#' -------------------------------------
#' # Ejemplo 2:
#' a <- c(30, 30, 30, 30, 30, 32, 30, 30, 30, 32, 34, 34, 34, 32, 31)
#' b <- c(26, 26, 26, 26, 25, 26, 26, 26, 26, 26, 25, 24, 24, 25, 26)
#'
#' Tanova(a = a, b = b)
#' }
Tanova <- function(a, b) {

  # Calcular la suma de cuadrados total sct
  media_total <- mean(c(a, b))
  sct <- sum((c(a, b) - media_total)^2)

  # Calcular la suma de cuadrados de a (SCa)
  media_a <- mean(a)
  sca <- length(a) * (media_a - media_total)^2

  # Calcular la suma de cuadrados de b (SCb)
  media_b <- mean(b)
  scb <- length(b) * (media_b - media_total)^2

  # Calcular la suma de cuadrados del error (SCE)
  sce <- sct - sca - scb

  # Calcular los grados de libertad
  df_a <- 1
  df_b <- length(unique(a)) - 1
  df_error <- length(a) + length(b) - df_a - df_b

  # Calcular los cuadrados medios (Mean Squares)
  ms_a <- sca / df_a
  ms_b <- scb / df_b
  ms_error <- sce / df_error

  # Calcular la estadística F
  f_value <- ms_b / ms_error

  # Crear la tabla ANOVA
  tab_Anova <- data.frame(Source = c("a", "b", "Error", "Total"),
                           DF = c(df_a, df_b, df_error, length(a) + length(b) - 1),
                           SS = c(sca, scb, sce, sct),
                           MS = c(ms_a, ms_b, ms_error, NA),
                           F = c(f_value, NA, NA, NA),
                           p_value = c(pf(f_value, df_a, df_error, lower.tail = FALSE), NA, NA, NA))
  return(tab_Anova)

}




