all:
	echo "Dummy command"

clean:
	find problem-* \
		-type f \
		\( -iname "*.hi" -or -iname "*.o" -or -executable \) \
		-exec \
			rm {} \;

