# This script downloads the raw ODNB files. Depending on whether they have changed their format, this script might not be as up to date... 
# This script is intended to be run from this directory. 

curl -o ../../../../data/ODNB_raw/HTML/file_#1.txt "http://www.oxforddnb.com/templates/article.jsp?articleid=[1-99999]"

tar -zcvf ../../../../data/ODNB_raw/HTML_ZIP/ODNB_text.tar.gz ../../../../data/ODNB_raw/HTML

