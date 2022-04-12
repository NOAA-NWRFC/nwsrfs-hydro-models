c**********************************************************************
c This subroutine will remove leading blank space(s) for string 8-char
c Cham Pham          03/12/09
c**********************************************************************
      subroutine rmleadblk8(oldstr, newstr)
      
      character oldstr(2)*4, newstr(2)*4
      character str*8
   
      str = oldstr(1)//oldstr(2)
      
      if (str(8:8) .eq. ' ' ) then
         write(newstr,'(a)')
         return
      end if

 10   if ( str(1:1) .eq. ' ' ) then
        str = str(2:)
        go to 10
      end if

      read(str,'(2a4)') newstr

      return
      end

c**********************************************************************
c This subroutine will remove leading blank space(s) for string 4-char
c**********************************************************************
      subroutine rmleadblk4(oldstr, newstr)
      character*4 oldstr, newstr

      if ( oldstr(4:4) .eq. ' ' ) then
        write(newstr,'(a)') 
        return
      end if   

 10   if ( oldstr(1:1) .eq. ' ' ) then
        oldstr = oldstr(2:) 
        go to 10
      end if

      read(oldstr,'(a4)') newstr

      return 
      end
