module Zcode(zencode, zdecode) where 

zencode ('z':s) = 'z':'z':(zencode s)
zencode ('Z':s) = 'Z':'Z':(zencode s)
zencode (c:s) = c:(zencode s)
zencode [] = []

zdecode ('z':'p':s) = '+':(zdecode s)
zdecode ('z':'z':s) = 'z':(zdecode s)
zdecode ('z':'u':s) = '_':(zdecode s)
zdecode ('Z':'Z':s) = 'Z':(zdecode s)
zdecode ('Z':'M':s) = '[':(zdecode s)
zdecode ('Z':'N':s) = ']':(zdecode s)
zdecode ('Z':'C':s) = ':':(zdecode s)
zdecode (a:s) = a:(zdecode s)
zdecode [] = []

