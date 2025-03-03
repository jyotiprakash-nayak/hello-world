import_model_outputs <- function(scn_run_dir, inference, outcome, global_opt = NULL, final_opt = NULL){
  
  if(inference){ 
    if(is.null(global_opt) | is.null(final_opt)){
      stop("Inference run, must specify global_opt and final_opt")
    }else{
      inference_filepath_suffix <-paste0("/",global_opt,"/",final_opt)
      print(paste0('Assuming inference run with files in',inference_filepath_suffix))
    }
  }else{ 
    inference_filepath_suffix <-""
    print('Assuming non-inference run. Ignoring values of global_opt and final_opt if specified') 
  }
  
  subdir <- paste0(scn_run_dir,"/", outcome,"/",inference_filepath_suffix, "/")
  print(subdir)
  
  subdir_list <- list.files(subdir)
  print(subdir_list)
  
  if (length(subdir_list) == 0) {
    stop("No files found in the directory: ", subdir)
  }
  
  out <- NULL
  total <- length(subdir_list)
  print(paste0("Importing ", outcome, " files (n = ", total, "):"))
  
  for (i in 1:length(subdir_list)) {
    file_path <- paste(subdir, subdir_list[i], sep = "/")
    if (grepl("parquet", subdir_list[i])) {
      dat <- tryCatch({
        arrow::read_parquet(file_path)
      }, error = function(e) {
        warning("Failed to read parquet file: ", file_path, " - ", e$message)
        NULL
      })
    } else if (grepl("csv", subdir_list[i])) {
      dat <- tryCatch({
        read.csv(file_path)
      }, error = function(e) {
        warning("Failed to read CSV file: ", file_path, " - ", e$message)
        NULL
      })
    } else {
      warning("Unsupported file type: ", file_path)
      next
    }
    
    if (is.null(dat)) {
      next
    }
    
    if(inference == TRUE & final_opt == "intermediate"){ 
      dat$slot <- as.numeric(str_sub(subdir_list[i], start = 1, end = 9))
      dat$block <- as.numeric(str_sub(subdir_list[i], start = 11, end = 19))
      dat$iter <- as.numeric(str_sub(subdir_list[i], start = 21, end = 29))
    }else{ 
      dat$slot <- as.numeric(str_sub(subdir_list[i], start = 1, end = 9))
    }
    
    out <- rbind(out, dat)
  }
  
  if (is.null(out)) {
    stop("No valid data files were imported from the directory: ", subdir)
  }
  
  return(out)
}
