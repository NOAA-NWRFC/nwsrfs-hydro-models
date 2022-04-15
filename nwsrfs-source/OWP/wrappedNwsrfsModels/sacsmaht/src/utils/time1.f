c *********************************************************************
c Subroutine computes year, month, day and hour.
c version Apr 06, 2003 by D.-J. Seo at NWS/HL
c**********************************************************************
      subroutine time1(itime,iyr,mon,iday,ihr)

      iyr=itime/1000000
      mon=itime-iyr*1000000
      mon=mon/10000
      iday=itime-iyr*1000000-mon*10000
      iday=iday/100
      ihr=itime-iyr*1000000-mon*10000-iday*100

      return
      end

