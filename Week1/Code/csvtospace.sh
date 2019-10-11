# Shell exercise: echo "Convert file-$1 ..."
cat $1 | tr -s "." "\t" >> res.csv
echo "Done!"
exit