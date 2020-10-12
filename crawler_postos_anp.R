library("RSelenium")
library(stringi)
library(rvest)
library(dplyr)
library(data.table)
library(yaml)

if(teste["sysname"] == "Windows"){
  parameter_file = choose.files(default = "./parameters.yaml", caption = "Choose parameters YAML file", multi = F)
} else if(teste["sysname"] == "Linux"){
  parameter_file = file.choose()
}

parameters = read_yaml(parameter_file)
output_path = parameters$output_path
navigator = parameters$navigator

output_path = ifelse(endsWith(output_path, "/"), output_path, paste0(output_path, "/"))  

remDr = RSelenium::rsDriver(browser = navigator, check = F, version = "3.9.1")
client = remDr$client

baseURL = "http://postos.anp.gov.br"
client$navigate(baseURL)

date_header = NULL
while (is.null(date_header)) {
  date_header = tryCatch({client$findElement(using = 'xpath', value = "/html/body/table/tbody/tr/td[2]/span")},
                error = function(e){
                  NULL
                })  
}

full_date = date_header$getElementText()[[1]]
date = stri_split_regex(full_date,"\\s+")[[1]][2]
date = paste(stri_split_fixed(date,"/")[[1]][3:1],collapse = "")
hour = stri_split_regex(full_date,"\\s+")[[1]][4]
hour = stri_replace_all_fixed(hour,":","")
hour = substr(hour,1,4)
full_date = paste0(date,hour,"00")

old_uf_page_table = ""
uf_page_table = ""

lapply(2:28, function(uf_idx){
  
  while(old_uf_page_table == uf_page_table){
    uf_page_table = tryCatch({
      select_uf_button = client$findElement(using = "xpath", value = paste0("/html/body/table/tbody/tr/td[2]/table[2]/tbody/tr/td[3]/table[2]/tbody/tr/td/form/table/tbody/tr[4]/td[2]/select[1]/option[",uf_idx,"]"))
      select_uf_button$clickElement()
      
      search_button = client$findElement(using = "xpath", value ="/html/body/table/tbody/tr/td[2]/table[2]/tbody/tr/td[3]/table[2]/tbody/tr/td/form/table/tbody/tr[7]/td[3]/input[3]")
      search_button$clickElement()
      
      table_uf = NULL
      while(is.null(table_uf)){
        table_uf = tryCatch({client$findElement(using = "xpath", value ="/html/body/table/tbody/tr/td[2]/table[3]/tbody/tr[3]/td[4]/font")},
                            error = function(e){
                              NULL
                            })
      }
      
      table_uf$getElementText()[[1]]
    },
    error = function(e){
      ""
    })
  }
  
  export_button = client$findElement(using = "xpath", value ="//*[@id='Submit2']")
  export_button$clickElement()

  old_uf_page_table = uf_page_table
})

download_complete = F

while(!download_complete){
  tryCatch({
    download_complete = ifelse(
      sum(
        grepl("ANP \\(\\d{1,2}\\).xls$",
              list.files(output_path)
              )) == 26, T, F)
  }, error = function(e){
    print("Last file download is not finished")
    Sys.sleep(1)
  })
}

client$close()
rm(remDr)
gc()

files_full_path = list.files(output_path, full.names = T)[grepl("ANP.*xls", list.files(output_path))]

treat_table = function(file){
  tabela = read_html(file) %>% 
    html_table(fill = T)
  
  tabela = data.table(tabela[[2]])
  col_names = as.character(tabela[1])
  tabela = tabela[!1]
  names(tabela) = col_names
  
  tabela[, `Data de Extração dos Dados` := full_date]
  return(tabela)
}

bases = lapply(files_full_path, treat_table)
data = rbindlist(bases)

write.table(data, paste0(output_path, "anp_consolidada.txt"), sep="\t", row.names = F, quote = F, col.names = T)

file.remove(files_full_path)