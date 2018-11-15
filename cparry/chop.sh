for f in DIAAPP OPAR3 PMEM PMEM2 PMEM3 PMEM4 PMEM5 WIN
do
  echo $f
  sed -e 's// >= /g;s// ⇦  /g;s// \& /g;s// \&\& /g;s// not/g;s//car /g;s//cdr /g;s// set0 /g;s// cons /g;s//then/g;s// storebyte /g' $f >$f.mlisp
done

