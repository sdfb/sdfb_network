#!/usr/bin/env python
#
# -*- coding: utf-8; -*-
 
import sys
import chardet
 
file_handle = file(sys.argv[1])
content = file_handle.read()
file_handle.close()
result = chardet.detect(content)
print(repr(result))
