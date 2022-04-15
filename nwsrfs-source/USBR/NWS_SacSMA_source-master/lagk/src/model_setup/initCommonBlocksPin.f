
       SUBROUTINE initCommonBlocksPin(FILENAME)

	CHARACTER*1024 FILENAME
	COMMON/IONUM/IN,IPR,IPU
	COMMON /FATLGK/ IATL,C1,C2
	
	DATA  IATL, C1, C2 / 1, 12.0, 100.0 /
	

	IN = 1
	IPR = 0
	IPU = 0

	open(IN,file=TRIM(FILENAME),status='old')
	

	RETURN
	END
