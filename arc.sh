p=symm
tar -X ./xclude.lst -cv ../${p} | bzip2 -z > ../${p}_`date +%y%m%d`.tar.bz2

