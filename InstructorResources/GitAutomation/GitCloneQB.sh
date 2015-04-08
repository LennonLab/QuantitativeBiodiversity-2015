while read EachLine
do
echo $EachLine
git clone https://github.iu.edu/2015-QuantitativeBiodiversity/$EachLine
done < $1
