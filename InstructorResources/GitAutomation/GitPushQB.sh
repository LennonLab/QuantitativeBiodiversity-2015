while read EachLine
do
echo $EachLine
cd ./$EachLine
git push origin master
cd ../
done < $1
