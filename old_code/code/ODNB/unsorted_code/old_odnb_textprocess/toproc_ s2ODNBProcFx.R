##@S This codefile contains stuff that *should* be deleted soon. Make sure this isn't used anywhere... 

## 9/2/2014 -- Are these functions even used anymore? in a different form?
get.subject <- function(text) {
  ## Input      text = char vector (format = processed through dnb.grab.main)
  ## Output     char singleton
  ## 
  ## Returns an entry containing name that bio is about 
  
  b = gregexpr(text = text[1], pattern = "span")[[1]]
  tx = substr(text[1], start=b[1],stop=b[2])
  tx = strsplit(tx, split = ">")[[1]][2]
  tx = strsplit(tx, split = "<")[[1]][1]
  return(tx)
}

listify.text <- function(main.text, num) {
  ## Input      main.text = char vector (format = dnb) 
  ##            num = file number for text
  ## Output     named list: $about [person name]
  ##                        $num [file id]
  ##                        $len [number lines]
  ##                        $dates [dates for person]
  ##                        $text [dnb.grab.main result]
  ## 
  ## Returns a list of entries above, for each file
  
  ab = get.subject(main.text) 
  da = get.dates(main.text)
  len = length(main.text)
  temp = list(about = ab, num = num, len = len, dates = da, text = main.text)
  return(temp)
}
#################################################################################################



