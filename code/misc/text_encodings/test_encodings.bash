#!bin/bash

number=0
while [$number -lt 1173]; do
    echo $number
    echo **$number** \n >> outfile.out
    enc = sed -n '$(number)p' allData.out
    iconv -f enc -t UTF-8 >> outfile.out
    number=$((number+1))
    echo ------------------------ >> outfile.out
done