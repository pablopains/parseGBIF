#' @title Seleção Otimizada de Digital Voucher em Cluster
#' @description Divide o processamento em N partes (chunks) para gerenciar memória em datasets gigantes.
#' @param n_chunks Número de divisões (padrão 48).
#' @export
select_digital_voucher_cluster_optimized <- function(occ,
                                                   occ_gbif_issue,
                                                   occ_wcvp_check_name,
                                                   occ_collectorsDictionary,
                                                   n_chunks = 48,
                                                   n_minimo = 10000) {
  
  # 1. Converter para data.table
  # Usamos setDT para não copiar a memória se já for DT
  occ_dt   <- as.data.table(occ)
  iss_dt   <- as.data.table(occ_gbif_issue)
  wcvp_dt  <- as.data.table(occ_wcvp_check_name)
  coll_dt  <- as.data.table(occ_collectorsDictionary)
  
  if (nrow(occ_dt) <= n_minimo) {
    return(select_digital_voucher_final(occ_dt, iss_dt, wcvp_dt, coll_dt, silence = TRUE))
  }
  
  # 2. Chunking por Hash (Ultra rápido)
  # Criamos o chunk_id baseando-se na chave única
  unique_keys <- data.table(key = unique(coll_dt$Ctrl_key_family_recordedBy_recordNumber))
  unique_keys[, chunk_id := as.integer(as.factor(key)) %% n_chunks + 1L]
  
  # Join para trazer o chunk_id (mais rápido que converter a coluna toda)
  coll_dt[unique_keys, chunk_id := i.chunk_id, on = .(Ctrl_key_family_recordedBy_recordNumber = key)]
  
  # 3. Processamento Paralelo (Unix) ou Sequencial (Windows)
  # Determinamos o número de núcleos
  n_cores <- if(.Platform$OS.type == "unix") parallel::detectCores() - 1 else 1
  
  if (n_cores > 1) {
    message(paste("🔥 Executando em PARALELO (Unix detected) com", n_cores, "núcleos..."))
    
    results <- parallel::mclapply(1:n_chunks, function(i) {
      idx <- which(coll_dt$chunk_id == i)
      if (length(idx) == 0) return(NULL)
      
      select_digital_voucher_final(
        occ = occ_dt[idx],
        occ_gbif_issue = iss_dt[idx],
        occ_wcvp_check_name = wcvp_dt[idx],
        occ_collectorsDictionary = coll_dt[idx][, chunk_id := NULL],
        silence = TRUE
      )
    }, mc.cores = n_cores)
    
  } else {
    message("🔄 Executando em SEQUENCIAL (Windows ou Single Core)...")
    results <- lapply(1:n_chunks, function(i) {
      idx <- which(coll_dt$chunk_id == i)
      if (length(idx) == 0) return(NULL)
      select_digital_voucher_final(occ_dt[idx], iss_dt[idx], wcvp_dt[idx], coll_dt[idx][, chunk_id := NULL], silence = TRUE)
    })
  }
  
  # 4. Combinar resultados (O ápice da performance)
  results <- results[!sapply(results, is.null)]
  
  return(list(
    occ_digital_voucher = rbindlist(lapply(results, `[[`, "occ_digital_voucher"), use.names = TRUE, fill = TRUE),
    occ_results = rbindlist(lapply(results, `[[`, "occ_results"), use.names = TRUE, fill = TRUE)
  ))
}


# select_digital_voucher_cluster_optimized <- function(occ = NA,
#                                                      occ_gbif_issue = NA,
#                                                      occ_wcvp_check_name = NA,
#                                                      occ_collectorsDictionary = NA,
#                                                      file_name_occ_issue = NA,
#                                                      file_name_occ_wcvp_check_name = NA,
#                                                      file_name_occ_collectorsDictionary = NA,
#                                                      enumOccurrenceIssue = NA,
#                                                      n_chunks = 48,
#                                                      n_minimo = 10000) {
#   
#   # --- 1. CARREGAMENTO DE DADOS ---
#   # Helper para carregar CSVs se os objetos não forem passados diretamente
#   load_input <- function(obj, path) {
#     if (all(is.na(obj)) && !is.na(path)) {
#       return(readr::read_csv(path, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE))
#     }
#     return(obj)
#   }
#   
#   occ_gbif_issue <- load_input(occ_gbif_issue, file_name_occ_issue)
#   occ_wcvp_check_name <- load_input(occ_wcvp_check_name, file_name_occ_wcvp_check_name)
#   occ_collectorsDictionary <- load_input(occ_collectorsDictionary, file_name_occ_collectorsDictionary)
#   
#   # --- 2. VERIFICAÇÃO DE TAMANHO ---
#   # Se o dado for menor que o mínimo, processa tudo de uma vez
#   if (nrow(occ) <= n_minimo) {
#     return(select_digital_voucher_final(
#       occ = occ, 
#       occ_gbif_issue = occ_gbif_issue,
#       occ_wcvp_check_name = occ_wcvp_check_name,
#       occ_collectorsDictionary = occ_collectorsDictionary,
#       enumOccurrenceIssue = enumOccurrenceIssue
#     ))
#   }
#   
#   # --- 3. LÓGICA DE DIVISÃO (CHUNKING) ---
#   # Garantimos que registros com a mesma chave de colecionador fiquem no mesmo "pedaço"
#   dict_dt <- as.data.table(occ_collectorsDictionary)
#   unique_keys <- unique(dict_dt$Ctrl_key_family_recordedBy_recordNumber)
#   n_keys <- length(unique_keys)
#   
#   # Criamos um mapeamento de Chave -> Chunk ID
#   key_map <- data.table(
#     key = unique_keys,
#     chunk_id = cut(seq_along(unique_keys), breaks = n_chunks, labels = FALSE)
#   )
#   
#   # Adiciona o chunk_id ao dicionário original
#   dict_dt[key_map, chunk_id := i.chunk_id, on = .(Ctrl_key_family_recordedBy_recordNumber == key)]
#   
#   # --- 4. PROCESSAMENTO EM LOOP ---
#   message(paste("🚀 Iniciando processamento otimizado em", n_chunks, "partes..."))
#   
#   # Lista para armazenar os resultados de cada pedaço
#   results_list <- lapply(1:n_chunks, function(i) {
#     
#     # Filtra os índices que pertencem ao chunk atual
#     idx <- which(dict_dt$chunk_id == i)
#     
#     if (length(idx) == 0) return(NULL)
#     
#     message(sprintf(" -> Processando parte %d/%d (%d registros)", i, n_chunks, length(idx)))
#     
#     # Chama a função de processamento (deve ser a versão select_digital_voucher_final)
#     res <- select_digital_voucher_final(
#       occ = occ[idx, ],
#       occ_gbif_issue = occ_gbif_issue[idx, ],
#       occ_wcvp_check_name = occ_wcvp_check_name[idx, ],
#       occ_collectorsDictionary = occ_collectorsDictionary[idx, ],
#       enumOccurrenceIssue = enumOccurrenceIssue,
#       silence = TRUE
#     )
#     return(res)
#   })
#   
#   # --- 5. CONSOLIDAÇÃO DOS DADOS ---
#   message("📦 Unindo resultados...")
#   
#   # Remove nulos caso algum chunk tenha ficado vazio
#   results_list <- results_list[!sapply(results_list, is.null)]
#   
#   # Empilha as tabelas de retorno usando a velocidade do rbindlist
#   final_occ_digital_voucher <- rbindlist(lapply(results_list, `[[`, "occ_digital_voucher"))
#   final_occ_results <- rbindlist(lapply(results_list, `[[`, "occ_results"))
#   
#   message("✅ Processamento concluído com sucesso!")
#   
#   return(list(
#     occ_digital_voucher = final_occ_digital_voucher,
#     occ_results = final_occ_results
#   ))
# }