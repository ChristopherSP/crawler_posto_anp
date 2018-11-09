library("RSelenium")
library(stringi)
library(readxl)
library(rvest)
library(dplyr)
library(data.table)

remDr = RSelenium::rsDriver(browser = "chrome")
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

files_full_path = list.files("~/Downloads/",full.names = T)[grepl("ANP.*xls",list.files("~/Downloads/"))]

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

bases = lapply(files_full_path[1:2], treat_table)
data = rbindlist(bases)

write.table(data,"~/Downloads/anp_consolidada.txt",sep="\t",row.names = F, quote = F, col.names = T)
