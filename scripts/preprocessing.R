
# Se descargan los datos originaes, se descomprimen y guardan en .rds. El resto se borra.
library(data.table) 
output_dir = here::here("data/temporal")
dir.create(output_dir, showWarnings = FALSE)
# 1.Descargar archivo:
download.file(url = "https://catalogodatos.gub.uy/dataset/041c38c8-41b5-4bdd-87d2-3e6752115298/resource/3bf8386c-2b3a-486c-a403-6288cb422ca5/download/v_da_ft_incautaciones_csv.zip",
              destfile = paste0(output_dir, "/archivo.zip"))
# 2. Descomprimir
unzip(zipfile = paste0(output_dir, "/archivo.zip"), exdir = output_dir)
# 3. Cargar datos
data = fread(list.files(path = output_dir, pattern = ".csv", full.names = TRUE), encoding = "Latin-1")
# data <- data[, lapply(lapply(.SD, iconv, "latin1", "UTF-8"), trimws)] Opcional, chequear si funciona y hacerlo acá en vez de hacerlo en la app.
# 4. Remover directorio
unlink(output_dir, recursive = TRUE)
# 5. Guardar datos como .rds
saveRDS(data, "data/raw/datos.rds")


########### DATOS DE RECAUDACIÓN ###############

output_dir = here::here("data/temporal")
dir.create(output_dir, showWarnings = FALSE)
# 1.Descargar archivo:
download.file(url = "https://catalogodatos.gub.uy/dataset/351eab09-8ac9-4a0b-b27f-765de063f20a/resource/d10e60c1-8297-474f-a4c0-8150f4a9c0f4/download/v_da_ft_recaudacion_csv.zip",
              destfile = paste0(output_dir, "/archivo.zip"))
# # 2. Descomprimir
# unzip no descomprime más de 4gb
decompress_file <- function(directory, file, .file_cache = FALSE) {
  
  if (.file_cache == TRUE) {
    print("decompression skipped")
  } else {
    
    # Set working directory for decompression
    # simplifies unzip directory location behavior
    wd <- getwd()
    setwd(directory)
    
    # Run decompression
    decompression <-
      system2("unzip",
              args = c("-o", # include override flag
                       file),
              stdout = TRUE)
    
    # uncomment to delete archive once decompressed
    # file.remove(file) 
    
    # Reset working directory
    setwd(wd); rm(wd)
    
    # Test for success criteria
    # change the search depending on 
    # your implementation
    if (grepl("Warning message", tail(decompression, 1))) {
      print(decompression)
    }
  }
}    
decompress_file(directory = output_dir, file = here::here(output_dir, "archivo.zip"))
# OBS: Si la función decompress_file no funciona descomprimirlo a mano (por fuera de R).
# 3. Cargar datos
data = fread(list.files(path = output_dir, pattern = ".csv", full.names = TRUE), encoding = "Latin-1")
# 4. Remover directorio
unlink(output_dir, recursive = TRUE)
# 5. Guardar datos como .rds
data <- data[, lapply(lapply(.SD, iconv, "latin1", "UTF-8"), trimws)]
saveRDS(data, "data/raw/recaudacion.rds")


#### OPCIONAL. Si se quiere subir a github los archivos por si se dieran de baja subirlos en chuncks.
library(magrittr)
i = 0
split(data, 1:15) %>%
  lapply(., function(x) {
    i <<- i +1
    saveRDS(x, sprintf("data/raw/recaudacion%s.rds", i))
  })
