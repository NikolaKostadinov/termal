for FILE in *
do
	# delete old .beam files
	if [[ $FILE == *.beam ]]
	then
		rm $FILE
	fi

	# compile .erl files
	if [[ $FILE == *.erl ]]
	then
		erlc -W0 $FILE
	fi

done

echo "TPLS compiled"
