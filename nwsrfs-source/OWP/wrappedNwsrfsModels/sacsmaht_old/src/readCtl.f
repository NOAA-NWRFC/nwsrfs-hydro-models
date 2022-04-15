c readCtl read the control file, such as amen8.ctl
c
      SUBROUTINE readCtl(basinName,startDate,endDate,inputDir)

c Declaration
      integer startDate, endDate
	  character*80 inputDir
	  character ctlFile*200
	  character*8 basinName

c Code
      call getarg(1,ctlFile)
      if(ctlFile .eq. ' ') then
       write(*,'(A)') ' ENTER CONTROL FILE NAME'
       read(*,'(A)') ctlFile
      endif
      open(1,file=ctlFile)
c need error check after open.
c
c
      read(1,*) basinName
      read(1,*) startDate
      read(1,*) endDate
	  read(1,'(A)')inputDir
      close (1) 

      write(*,*) 'basin name=',basinName
      write(*,*) 'start date=',startDate
      write(*,*) 'end date =',endDate
      write(*,'(2A)') 'inputDir=',TRIM(inputDir)
  
      RETURN
      END
