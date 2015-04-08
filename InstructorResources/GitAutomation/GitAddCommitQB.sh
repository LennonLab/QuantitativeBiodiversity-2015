while read EachLine
do
echo $EachLine
cd ./$EachLine
git add $1
git commit -m "$2"
cd ..
done < $3
