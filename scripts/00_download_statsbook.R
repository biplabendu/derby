#load the libraries
library(stringr)
library(googledrive)

#folder link to id
link = "https://drive.google.com/drive/folders/1TC1QUmpIwy9NZX9DBPUPoHjkjFbbzyYr"
folder_id = drive_get(as_id(link))

#find files in folder
files = drive_ls(folder_id)

setwd("dev")

#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  #list files
  i_dir = drive_ls(files[i, ])
  
  #mkdir
  if (!dir.exists(files$name[i])) {
    dir.create(files$name[i])
  }
  
  # fs::dir_create(files$name[i])
  #download files
  for (file_i in seq_along(i_dir$name)) {
    path <- str_c(files$name[i], "/", i_dir$name[file_i])
    # check if file empty
    info <- fs::file_info(path)
    size <- info$size
    
    if (is.na(size) | size < fs::fs_bytes("3K")) {
      #fails if already exists
      try({
        drive_download(
          as_id(i_dir$id[file_i]),
          path = path,
          overwrite = TRUE
        )
      })
      Sys.sleep(5)
    }
  }
}
