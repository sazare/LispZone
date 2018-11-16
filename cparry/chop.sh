for f in diaapp opar3 pmem pmem2 pmem3 pmem4 pmem5 win
do
  echo $f
  sed -e 's// >= /g;s// ⇦  /g;s// AND /g;s// or /g;s// not /g;s//car /g;s//cdr /g;s// set0 /g;s// cons /g;s// then /g;s// storebyte /g;s///g;s///g;s//@@/g;s//^B/g' $f >$f.mlisp
done

