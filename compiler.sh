for FILE in *
do
	
	if [[ $FILE == *.erl ]]
	then
		erlc -W0 $FILE
	fi

done

echo "TPLS compiled"
