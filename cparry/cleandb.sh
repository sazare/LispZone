cut -d'~' -f1 INF |sed 's///g' |grep -v '^$' |sed 's//@@/g'>inf0
cut -d'~' -f1 BEL |grep -v '^$' >bel0

