
       SUBROUTINE initCommonBlocksPin(FILENAME)

	CHARACTER *1024 FILENAME

	COMMON/IONUM/IN,IPR,IPU

	IN = 1
	open(IN,file=TRIM(FILENAME),status='old')
	
	LEFTP =0
	IUSEP = 0
	LEFTC = 0
	IUSEC = 0

	RETURN
	END
