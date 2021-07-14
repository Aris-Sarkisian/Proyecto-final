
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
# 4. Remover directorio
unlink(output_dir, recursive = TRUE)
# 5. Guardar datos como .rds
saveRDS(data, "data/raw/datos.rds")
