cut -d'~' -f1 INF |sed 's///g' |sed 's/\t//g'|sed 's//@@/g'|grep -v '^$' >inf0
cut -d'~' -f1 BEL |sed 's///g' |sed 's/\t//g'|grep -v '^$' >bel0
sed 's/~/;/' RDATA >rdata.lisp


