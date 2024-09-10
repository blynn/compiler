#!/usr/bin/env bash
cat singularity | sed '
/^-/d
s/[a-z]*\(.* \)=/\\\1->/
s/\<[dxv]s\>/p/g
s/\<ys\>/q/g
s/\<tab\>/a/g
s/\<xt\>/t/g
s/\<yt\>/u/g
s/#@/_/g
s/@//g
s/_/#@/g
'"$(cat singularity | sed  -n '/.* =/{s/ .*//;p}' | awk '{printf "s_\\<" $0 "\\>_@%c_g\n",NR+31 }' | sed 's/&/\\&/g')"'
s/\(\\[a-z ]*\)\([a-z]\) *->/\1->\\\2./g
s/\(\\[a-z ]*\)\([a-z]\) *->/\1->\\\2./g
s/\(\\[a-z ]*\)\([a-z]\) *->/\1->\\\2./g
s/\(\\[a-z ]*\)\([a-z]\) *->/\1->\\\2./g
s/\(\\[a-z ]*\)\([a-z]\) *->/\1->\\\2./g
s/\\ *->//g' | sed -z '
s/# /#_/g
s/@ /@_/g
s/ //g
s/_/ /g
s/\([@#][@#]\) /\1/g
s/#\n/#_/g
s/\n//g
s/_/\n/g
'
