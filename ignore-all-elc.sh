for i in $( find plugins/ -wholename \*git/info/exclude -print ); do
	if [ -z `grep \*.elc $i` ]; then
		echo "no pattern"
		echo \*.elc >> $i
	fi
done
