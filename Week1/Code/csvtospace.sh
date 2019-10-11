#/bin/bash
cat /dev/null > $2
cat $1 | tr -s "," "\t"  >>$2
echo "Done!" 
exit