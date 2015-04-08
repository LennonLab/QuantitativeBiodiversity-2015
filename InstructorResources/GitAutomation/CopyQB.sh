while read EachLine
do
echo $EachLine
cp $1 ./$EachLine/
done < $2
