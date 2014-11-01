##@S Ths file contains all helper functions.



#' Removes all elements which only have spaces
#' 
#' @param raw_htmltext Character vector corresponding to lines in a document (HTML code)
#' 
#' @return Character vector without empty lines
#' 
#' @export
#' 
rm_emptylines = function(raw_htmltext) {   
  a = grep(pattern = "^[[:space:]]*$", raw_htmltext)
  return(raw_htmltext[-a])
}


#' This extracts the pertinent part of the html code and outputs that.
#' 
#' @param raw_htmltext Character vector corresponding to lines in a document (HTML code)
#' 
#' @return Character vector contains biography section of code
#' 
#' @export
#' 
dnb_grab_main = function(raw_htmltext) {
  base_article = rm_emptylines(raw_htmltext)
  sta = min(grep(pattern = "<div class=\"para\">" , base_article))
  end = grep(pattern = "<div id=\"references\">" , base_article)
  return(base_article[(sta+1):(end-2)])
}


#' Checks if the HTML file is a biography
#' 
#' @param raw_htmltext Character vector corresponding to lines in a document (HTML code)
#' 
#' @return TRUE if file is not a biography
#' 
#' @export
#' 
is_nobio = function(raw_htmltext) {
  if (length(grep(pattern = "Server Error", raw_htmltext[6])) > 0) {
    return(TRUE)
  } else { 
    return(FALSE)
  }
}



#' Returns all cosubjects listed in this biography
#' 
#' @param htmltext Character vector : only section pertaining to biography
#' 
#' @return vector of cosubjects in biography
#' 
#' @export
#' 
check_cosubject = function(htmltext) {
  co = grep(pattern = "cosubject_", x = htmltext)
  if (length(co) == 0) { return(0) }
  else {
    toreturn = 0
    for(i in 1:length(co)) {
      temp = regexpr(pattern = "[[:digit:]]+", text = htmltext[co[i]]) 
      toreturn[i] = substr(htmltext[co[i]], start = 20, stop = 19 + attr(temp, "match.length")[1])
    }
    return(as.numeric(toreturn))
  }
}


#' Detects if this there are cosubject mentions in this biography
#' 
#' @param htmltext Character vector : only section pertaining to biography
#' 
#' @return T/F: True if 'cosubject' appears in document'
#' 
#' @export
#' 
exists_cosubject = function(htmltext) {
  if (length(grep("cosubject", htmltext)) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Split cosubject biographies into portions
#' 
#' This function determines the type of cosubject biography, 
#' and consequently splits it (and returns a list of texts and id numbers for the texts)
#' 
#' REQUIRES ODNB_rawHTML to exist in global environment. 
#' 
#' @param main_id ID number for person the document is mainly about (ignored if group bio)
#' @param all_ids Vector of numbers that refer to the same document
#' 
#' @return list of cosubject biographies
#' 
#' @export
#' 
process_cosubject = function(main_id, all_ids) {
  
  if (FALSE) {
    main_id = 54525
    all_ids = alt_cosub[[main_id]]
  }
  ## These are names that identify group cosubjects. 
  group_names = c("family", "Society", "emperors", "school", "Crazy Gang", "Castilians", "group", "brothers", "Corps", "Club", "Boys", "scholars", "Queen's", "club", "Edinburgh Seven", "sisters", "Vorticists", "knights", "colourists", "kings", "conspirators", "Group", "Martyrs", "witches", "officers", "wives", "Pioneers", "artists", "visitors", "painters", "Kings", "Girls", "women", "Babes", "Gruffudd ap Rhys", "Moneyers", "officials", "Women", "spies", "American Indians", "Servants")
  
  text_list = list()
  id_list = list()
  ## Extract base document
  doc = dnb_grab_main(ODNB_rawHTML[[main_id]])
  id_vec = rep(NA, times = length(all_ids))
  
  ## Check if this is a group biography
  group_cosub = FALSE
  name = ODNB_extract_name_segment(doc)
  for(k in 1:length(group_names)) {
    if (length(grep(group_names[k], name$text)) > 0) { group_cosub = TRUE }
  }
  
  
  if (group_cosub) {
    comb_doc = paste(doc, collapse = "#*#*#")
    
    ## Find locations of cosubjects and split these from the rest of the document. 
    match_locs = gregexpr("<a name=\"cosubject_[0-9]+\"></a>", comb_doc)
    matchs = regmatches(comb_doc, match_locs)[[1]]
    match_ids = as.numeric(gsub("[^0-9]", "", matchs))
    matchs_inv = regmatches(comb_doc, match_locs, invert = TRUE)[[1]]
    
    ## Split at every cosubjects
    text_list[[1]] = strsplit(x = matchs_inv[1], split = "#*#*#", fixed = TRUE)[[1]]
    id_vec[1] = ifelse(test = !(any(main_id == match_ids)), yes = main_id, no = NA)
    for(k in seq_along(match_ids)) {
      text_list[[k+1]] = strsplit(matchs_inv[k+1], "#*#*#", fixed = TRUE)[[1]]
      id_vec[k+1] = match_ids[k]
    }
    
    id_status = c("main_group", rep("cosub_group", times = length(id_vec) - 1))
    
  } else {
    ## The main subject takes most of the document. All other subjects only last until next cosubject OR until the end of the paragraph. 
    
    match_locs = gregexpr("<a name=\"cosubject_[0-9]+\"></a>", doc)
    matchs = regmatches(doc, match_locs)
    matchs_inv = regmatches(doc, match_locs, invert = TRUE)
    
    text_list[[1]] = sapply(matchs_inv, function(x) {x[1]})
    id_vec[1] = main_id
    k = 2
    for(m in seq_along(matchs)) {
      for(n in seq_along(matchs[[m]])) {
        match_id = as.numeric(gsub("[^0-9]", "", matchs[[m]][n]))
        text_list[[k]] = matchs_inv[[m]][n+1]
        id_vec[k] = match_id
        k = k + 1
      }
    }
    
    id_status = c("main_subj", rep("cosub_subj", times = length(id_vec) - 1))
  }
  
  ## Drop non-identified OR large IDs. 
  todrop = which(is.na(id_vec) | id_vec >= 100000)
  if (length(todrop) > 0) {
    text_list = text_list[-todrop]
    id_vec = id_vec[-todrop]
  }
    
  return(list(text = text_list, ids = id_vec, cosub_status = id_status))
}


#' This function returns a list of two elements: the substring in the text that matches the name segment 
#' (including html), and the character number that signifies the end of the string.
#' 
#' @param text odnb text
#' 
#' @return FIX THIS
#' 
#' @export
#' 
ODNB_extract_name_segment = function(text) {
  if(is.null(text)) { return(list(text = "", end = NA)) }
  if (length(text) == 0) { return(list(text = "", end = NA)) }
  
  starts = gregexpr("<span", text)[[1]]
  ends = gregexpr("</span>", text)[[1]]
  
  start_i = 2
  end_i = 1
  embed = 1
  
  if (length(starts == 1) & length(ends) == 1) {
    y = ends[1] + attr(ends,"match.length")[1] - 1
    return(list(text = substr(text[[1]], start =1, stop = y),
                end = y))
  } else {
    while(embed > 0) {
      if (starts[start_i] > ends[end_i]) {
        embed = embed - 1
        end_i = end_i + 1
        if (end_i > length(ends)) {
          return(list(text = "!!ERROR!!", end = "!!ERROR"))
        }
      } else {
        embed = embed + 1
        start_i = start_i + 1
        if (start_i > length(starts)) {
          return(list(text = "!!ERROR!!", end = "!!ERROR"))
        }
      }
    }
    if (start_i == end_i) {
      y = ends[end_i - 1] + attr(ends,"match.length")[end_i - 1] - 1
      return(list(text = substr(text[[1]], start =1, stop = y),
                  end = y))
    } else {
      return(list(text = "!!ERROR!!", end = "!!ERROR"))
    }
  }
  
}


#' This removes/fixes all sorts of HTML things. 
#' 
#' @param text  char vec (ODNB)
#' 
#' @return char vec
#' 
#' @export
#' 
ODNB_fix_accent_html =  function(text) {
  temp = text
  
  ## Removes/Fixes HTML expressions
  temp = gsub("&#130;", ",", temp)
  temp = gsub("&#145;", "\'", temp)
  temp = gsub("&#146;", "\'", temp)
  temp = gsub("&#147;", "\'", temp)
  temp = gsub("&#148;", "\'", temp)
  temp = gsub("&#150;", "-", temp)
  temp = gsub("&#151;", " -- ", temp)
  temp = gsub("%20", " ", temp)
  temp = gsub("%2C", ",", temp)
  temp = gsub("%3F", "-", temp)
  temp = gsub("&lt;", "[", temp)
  temp = gsub("&gt;", "]", temp)
  temp = gsub("&frac14;", "1/4", temp)
  temp = gsub("&frac34;", "3/4", temp)
  temp = gsub("&frac12;", "1/2", temp)
  temp = gsub("&times;", "x", temp) # check if this works later
  temp = gsub("&pound;", "$", temp) # check if this works later
  temp = gsub("&#9839;", "sharp", temp)
  temp = gsub("&infin;", "&infin;", temp) #did not change this
  temp = gsub("&asymp;", "&asymp;", temp) #did not change this
  temp = gsub("&rarr;", "&rarr;", temp) #did not change this
  temp = gsub("&Dagger;", "&Dagger;", temp) #did not change this
  temp = gsub("&para;", "&para;", temp) # did not change this
  temp = gsub("&sect;", "&sect;", temp) # did not change this
  temp = gsub("&int;", "&int;", temp) #did not change this
  temp = gsub("&radic;", "sqrt", temp)
  temp = gsub("&plusmn;", "+-", temp)
  temp = gsub("&#772;", "-", temp)
  temp = gsub("&#914;", "greekLetterBeta", temp)
  temp = gsub("&#915;", "greekLetterGamma", temp)
  temp = gsub("&#916;", "greekLetterDelta", temp)
  temp = gsub("&#923;", "greekLetterLambda", temp)
  temp = gsub("&#921;", "greekLetterIota", temp)
  temp = gsub("&#928;", "greekLetterPi", temp)
  temp = gsub("&#934;", "greekLetterPhi", temp)
  temp = gsub("&#933;", "greekLetterUpsilon", temp)
  temp = gsub("&#936;", "greekLetterPsi", temp)
  temp = gsub("&#945;", "greekLetteralpha", temp)
  temp = gsub("&#946;", "greekLetterbeta", temp)
  temp = gsub("&#947;", "greekLettergamma", temp)
  temp = gsub("&#948;", "greekLetterdelta", temp)
  temp = gsub("&#950;", "greekLetterzeta", temp)
  temp = gsub("&#951;", "greekLettereta", temp)
  temp = gsub("&#952;", "greekLettertheta", temp)
  temp = gsub("&#954;", "greekLetterkappa", temp)
  temp = gsub("&#956;", "greekLettermu", temp)
  temp = gsub("&#960;", "greekLetterpi", temp)
  temp = gsub("&#963;", "greekLettersigma", temp)
  temp = gsub("&#966;", "greekLetterphi", temp)
  temp = gsub("&#955;", "greekLetterlambda", temp)
  temp = gsub("&#959;", "greekLetteromicron", temp)
  temp = gsub("&amp;", "&", temp)
  temp = gsub("&deg;", " degree ", temp)
  temp = gsub("&#322;", "L", temp)
  temp = gsub("&#321;", "L", temp)
  temp = gsub("&#345;", "r", temp)
  temp = gsub("&#380;", "z", temp)
  temp = gsub("&#338;", "OE", temp)
  temp = gsub("&#339;", "oe", temp)
  temp = gsub("&#288;", "G", temp)
  temp = gsub("&#289;", "g", temp)
  temp = gsub("&#373;", "w", temp)
  temp = gsub("&#372;", "W", temp)
  temp = gsub("&#375;", "y", temp)
  temp = gsub("&yacute;", "y", temp)
  temp = gsub("&yuml;", "y", temp)
  temp = gsub("&#382;", "z", temp)
  temp = gsub("&#381;", "Z", temp)
  temp = gsub("&#379;", "Z", temp)
  temp = gsub("&#378;", "z", temp)
  temp = gsub("&#369;", "U", temp) 
  temp = gsub("&eth;", "d", temp)
  temp = gsub("&#7695;", "d", temp)
  temp = gsub("&#263;", "c", temp)
  temp = gsub("&#231;", "c", temp)
  temp = gsub("&#269;", "c", temp)
  temp = gsub("&#268;", "C", temp)
  temp = gsub("&ccedil;", "c", temp)
  temp = gsub("&Ccedil;", "C", temp)
  temp = gsub("&thorn;", "p", temp) #?? . p?
  temp = gsub("&THORN;", "P", temp)
  temp = gsub("&ntilde;", "n", temp)
  temp = gsub("&#328;", "n", temp)
  temp = gsub("&#324;", "n", temp)
  temp = gsub("&#327;", "N", temp)
  temp = gsub("&#351;", "s", temp)
  temp = gsub("&#347;", "s", temp)
  temp = gsub("&#346;", "S", temp)
  temp = gsub("&#353;", "s", temp)
  temp = gsub("&#352;", "S", temp)
  temp = gsub("&#355;", "t", temp)
  temp = gsub("&hellip;", "...", temp)
  temp = gsub("&uuml;", "u", temp)
  temp = gsub("&Uuml;", "U", temp)
  temp = gsub("&ucirc;", "u", temp)
  temp = gsub("&ugrave;", "u", temp)
  temp = gsub("&uacute;", "u", temp)
  temp = gsub("&Uacute;", "U", temp)
  temp = gsub("&#363;", "u", temp)
  temp = gsub("&#365;", "u", temp)
  temp = gsub("&#250;", "u", temp)
  temp = gsub("&#252;", "u", temp)
  temp = gsub("&#367;", "u", temp)
  temp = gsub("&ouml;", "o", temp)
  temp = gsub("&Ouml;", "O", temp)
  temp = gsub("&otilde;", "o", temp)
  temp = gsub("&#333;", "o", temp)
  temp = gsub("&#243;", "o", temp)
  temp = gsub("&#244;", "o", temp)
  temp = gsub("&#337;", "o", temp)
  temp = gsub("&#332;", "O", temp)
  temp = gsub("&#211;", "O", temp)
  temp = gsub("&Oacute;", "O", temp)
  temp = gsub("&Oslash;", "O", temp)
  temp = gsub("&Ograve;", "O", temp)
  temp = gsub("&oacute;", "o", temp)
  temp = gsub("&ograve;", "o", temp)
  temp = gsub("&oslash;", "o", temp)
  temp = gsub("&ocirc;", "o", temp)
  temp = gsub("&Ocirc;", "O", temp)
  temp = gsub("&#246;", "o", temp)
  temp = gsub("&#7885;", "o", temp)
  temp = gsub("&#205;", "I", temp)
  temp = gsub("&iacute;", "i", temp)
  temp = gsub("&Iacute;", "I", temp)
  temp = gsub("&igrave;", "i", temp)
  temp = gsub("&icirc;", "i", temp)
  temp = gsub("&Icirc;", "I", temp)
  temp = gsub("&iuml;", "i", temp)
  temp = gsub("&#305;", "i", temp)
  temp = gsub("&#237;", "i", temp)
  temp = gsub("&#238;", "i", temp)   
  temp = gsub("&#239;", "i", temp)
  temp = gsub("&#299;", "i", temp)
  temp = gsub("&#235;", "e", temp)
  temp = gsub("&#234;", "e", temp)
  temp = gsub("&#233;", "e", temp)
  temp = gsub("&#232;", "e", temp)
  temp = gsub("&eacute;", "e", temp)
  temp = gsub("&Eacute;", "E", temp)
  temp = gsub("&egrave;", "e", temp)
  temp = gsub("&Egrave;", "E", temp)
  temp = gsub("&#283;", "e", temp)
  temp = gsub("&#275;", "e", temp)
  temp = gsub("&#281;", "e", temp)
  temp = gsub("&#277;", "e", temp)
  temp = gsub("&#274;", "E", temp)
  temp = gsub("&#201;", "E", temp)
  temp = gsub("&ecirc;", "e", temp)
  temp = gsub("&Ecirc;", "E", temp)
  temp = gsub("&euml;", "e", temp)
  temp = gsub("&auml;", "a", temp)
  temp = gsub("&Auml;", "A", temp)
  temp = gsub("&Acirc;", "A", temp)
  temp = gsub("&aring;", "a", temp)
  temp = gsub("&Aring;", "A", temp)
  temp = gsub("&aacute;", "a", temp)
  temp = gsub("&atilde;", "a", temp)
  temp = gsub("&#193;", "A", temp)
  temp = gsub("&#261;", "a", temp)
  temp = gsub("&#259;", "a", temp)
  temp = gsub("&#225;", "a", temp)
  temp = gsub("&#226;", "a", temp)
  temp = gsub("&#257;", "a", temp)
  temp = gsub("&#259;", "a", temp)
  temp = gsub("&#228;", "a", temp)
  temp = gsub("&Aacute;", "A", temp)
  temp = gsub("&acirc;", "a", temp)
  temp = gsub("&#224;", "a", temp) ## Same?
  temp = gsub("&#192;", "A", temp)   ## Same. 
  temp = gsub("&agrave;", "a", temp)
  temp = gsub("&Agrave;", "A", temp) ## THIS SHOULD BE TREATED BETTER?
  temp = gsub("&#256;", "A", temp)
  temp = gsub("&AElig;", "AE", temp)
  temp = gsub("&aelig;", "ae", temp)
  temp = gsub("&#198;", "AE", temp)
  temp = gsub("&#230;", "ae", temp)
  temp = gsub("&para;", "PP", temp)
  temp = gsub("&sect;", "SS", temp)
  
  
  temp = gsub("<br>", " ", temp)
  temp = gsub("<blockquote>", " '", temp)
  temp = gsub("</blockquote>", "' ", temp)
  temp = gsub("</*em>", "", temp)
  temp = gsub("</*sup>", "", temp)
  temp = gsub("</*sub>", "", temp)
  temp = gsub("</*b>", "", temp)
  temp = gsub("</*table>", "", temp)
  temp = gsub("</*tr>", "", temp)
  temp = gsub("</*td.*?>", "", temp)
  temp = gsub("</*div.*?>", "", temp)
  temp = gsub("<span class=\"st\">[^<]*</span>", " ", temp)
  temp = gsub("<a.*?</h2>", " ", temp)
  temp = gsub("<h2>", " ", temp) ### WHY DOES THIS NOT WORK!?!??!
  # temp = gsub("<.*?>", " ", temp)
  # temp = gsub("\\( d .", "\\(died", temp)
  temp = gsub("<i>d</i>.", "died", temp)
  
  replace.italics <- function(row.text, start.text, stop.text, rep = "\'") { #replaces with quotes
    loc.a = gregexpr(pattern = start.text, text = row.text)[[1]]
    loc.b = gregexpr(pattern = stop.text, text = row.text)[[1]]
    toreturn = row.text
    if (loc.a[1] != -1) {
      for(j in length(loc.a):1) {
        use = loc.b[loc.b > loc.a[j]][1]
        toreturn = paste(substr(toreturn,start = 1, stop = use - 1), rep, 
                         substr(toreturn,start = use + attr(loc.b, "match.length")[which(loc.b == use)], stop = nchar(toreturn)), sep = "")
        toreturn = paste(substr(toreturn,start = 1, stop = loc.a[j] - 1), rep, 
                         substr(toreturn,start = loc.a[j] + attr(loc.a, "match.length")[j], stop = nchar(toreturn)), sep = "")
      }
    }
    return(toreturn)
  }
  
  #<script>writeSeealsoLink('../view/article/16389/', \"Richard Leigh\")</script>
  remove.script <- function(text) {
    a = grep("<script>writeSeealsoLink",text)
    if(length(a) == 0) { 
      return(text)
    } else {
      for(i in 1:length(a)) {
        b = gregexpr("<script>.*?</script>", text[a[i]])[[1]]
        for(j in length(b):1) {
          temp = substr(text[a[i]], b[j], b[j] + attr(b,"match.length")[j] - 1)
          d = strsplit(temp, split = "\"")[[1]]
          text[a[i]] = paste(substr(text[a[i]], start = 1, stop = b[j]-1), 
                             d[2],
                             substr(text[a[i]], start = b[j] + attr(b,"match.length")[j], stop = nchar(text[a[i]])), sep = "")
        }
      }
    }
    return(text) 
  }
  
  for(j in 1:length(temp)) {
    temp[j] = replace.italics(temp[j], "<span class=\"roman\">", "</span>")
    temp[j] = replace.italics(temp[j], "<span class=\"italic\">", "</span>")
    temp[j] = replace.italics(temp[j], "<span class=\"headword\">", "</span>", rep = "")
    temp[j] = replace.italics(temp[j], "<span class=\"occ\">", "</span>", rep = "")
    temp[j] = replace.italics(temp[j], "<span class=\"smallcap\">", "</span>", rep = "")
    temp[j] = replace.italics(temp[j], "<span class=\"st\">", "</span>", rep = "")
    temp[j] = replace.italics(temp[j], "<a target=\"_top\".*?>", "</a>", rep = "")
  }
  
  temp = remove.script(temp)
  temp = gsub("<i>", "'", temp)
  temp = gsub("</i>", "'", temp)
  # temp = gsub(pattern = "\\(.*?\\)", replacement = "", temp)
  temp = gsub("^ *", "", temp)
  temp = gsub(" +"," ", temp) 
  temp = gsub(" ,", ",", temp)
  temp = gsub(" \\.", "\\.", temp)
  temp = gsub("-'", "- '", temp)
  temp = gsub("</span>", "", temp) #This means that there are span mistakes... find?
  
  return(temp)
}
