library(rvest)

url <- read_html("https://www.nvsos.gov/SOSelectionPages/results/2022StateWideGeneral/ElectionIndex.aspx")

url |> 
  html_nodes(".tableData a") |> 
  html_text()
