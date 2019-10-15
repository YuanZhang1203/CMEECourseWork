#/bin/bash
cat $1 | tr -s "," " "  >>$1.txt
echo "Done!" 
exit