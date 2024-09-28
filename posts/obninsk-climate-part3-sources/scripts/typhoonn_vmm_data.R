#1. download archives from http://typhoon-tower.obninsk.org/ru/10-minute.asp
#2. unzip them
#3. rename the files containing the Cyrillic symbols

Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale("LC_COLLATE","russian")
Sys.setlocale("LC_TIME", "russian")

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

read_typhoon_file <- function(txt_file){
  x0 <- read_lines(txt_file, skip = 0, skip_empty_rows = F, n_max = 20) |> str_squish()
  # check where the start 
  k <- which(str_detect(x0, "F121[:blank:]*F301"))
  # how many columns
  ncols <- str_count(x0[k], "\\s") + 1
  
  if(ncols == 15){
    xnames <- c("year","month","day","hour", "wind_speed_8m", "wind_speed_121m", "wind_speed_301m", 
                "wind_dir_8m", "wind_dir_121m", "wind_dir_301m",  
                "temp_2m", "temp_121m", "temp_301m", "humidity_2m", "air_press_2m")
  } else {
    xnames <- c("year","month","day","hour", "hour_utc",
                "wind_speed_8m", "wind_speed_121m", "wind_speed_301m", 
                "wind_dir_8m", "wind_dir_121m", "wind_dir_301m",  
                "temp_2m", "temp_121m", "temp_301m", "humidity_2m", "air_press_2m")
  }
  
  x <- read_lines(txt_file, skip = k, skip_empty_rows = F) |>
    tibble::enframe(name = NULL) |> 
    mutate(value = str_squish(value)) |> 
    filter(nchar(value)>20) |> 
    separate(value, into = xnames, sep = "\\s")
  return(x)
}

clean_typhoon_data <- function(typhoon_data){
  typhoon_data |> 
    mutate(across(starts_with("wind_speed"), ~ifelse(.x %in% c("99.9","99.0"), 
                                                     NA_real_, parse_double(.x)))) |> 
    mutate(across(starts_with("wind_dir"), ~ifelse(.x == "999", 
                                                   NA_integer_, parse_integer(.x)))) |> 
    mutate(across(starts_with("temp"), ~ifelse(.x %in% c("99.9","99.0"), 
                                               NA_real_, parse_double(.x)))) |> 
    mutate(across(starts_with("humidity"), ~ifelse(.x == "5", 
                                                   NA_integer_, parse_integer(.x)))) |> 
    mutate(across(starts_with("air_pres"), ~ifelse(.x %in% c("999.9", "9999.0"), 
                                                   NA_real_, parse_double(.x)))) |> 
    mutate(across(any_of(c("year", "month", "day", "hour", "hour_utc")), ~parse_integer(.x))) |> 
    distinct()
}


# ------------- reading the files ---------------------
dir_vmm <- paste0(dir, "/vmm/") # <- set the folder where the vmm files are stored 

fs <- list.files(dir_vmm, full.names = T, pattern = "txt") 

all_data <- list() 

for (i in 1:length(fs)){ 
  all_data[[i]] <- read_typhoon_file(fs[i]) 
  }

all_data_df <- all_data |> map_df(~.x) |> clean_typhoon_data() |> distinct()

arrow::write_parquet(all_data_df, paste0(dir_vmm, "typhoon_data_2008_2023.parquet"), compression = "gz") 

