c**********************************************************************
c Subroutine performs range check on SAC parameters, adjustment factors
c to precip and pe, empirical UH and SAC states
c version Apr 06, 2003 by D.-J. Seo at NWS/HL
c**********************************************************************
      subroutine check_range(par,adj,state)

      dimension par(20), adj(2), state(6)
c
c     Parameter checks -- to eliminate impossible values.
c
      ichge=0
      if (par(4).gt.1.0) then
      par(4)=1.0
      ichge=ichge+1
      endif
      if (par(5).gt.(1.0-par(4))) then
      par(5)=1.0-par(4)
      ichge=ichge+1
      endif
      if (par(6).gt.1.0) then
      par(6)=1.0
      ichge=ichge+1                    
      endif
      if (par(17).gt.1.0) then
      par(17)=1.0
      ichge=ichge+1
      endif
c
c     Insure that capacities are not equal to 0.0
c
      if (par(1).lt.0.1) par(1)=0.1
      if (par(2).lt.0.1) par(2)=0.1
      if (par(9).lt.0.1) par(9)=0.1
      if (par(10).lt.0.1) par(10)=0.1
      if (par(11).lt.0.1) par(11)=0.1
c
c     parameter checks -- to eliminate impossible values.
c
      if (par(3).gt.1.0) then
      par(3)=1.0
      ichge=ichge+1
      endif
      if (par(12).gt.1.0) then   
      par(12)=1.0
      ichge=ichge+1
      endif
      if (par(13).gt.1.0) then
      par(13)=1.0
      ichge=ichge+1
      endif
      if (par(14).gt.1.0) then
      par(14)=1.0
      ichge=ichge+1
      endif
      if (par(15).gt.1.0) then
      par(15)=1.0
      ichge=ichge+1
      endif
      if (ichge.ne.0) then
      endif                                

      if (state(1).lt.0.) state(1)=0.01
      if (state(2).lt.0.) state(2)=0.01
      if (state(3).lt.0.) state(3)=0.01
      if (state(4).lt.0.) state(4)=0.01
      if (state(5).lt.0.) state(5)=0.01
      if (state(6).lt.0.) state(6)=0.01

      if (state(1).gt.par(1)) state(1)=par(1)
      if (state(2).gt.par(2)) state(2)=par(2)
      if (state(3).gt.par(9)) state(3)=par(9)
      if (state(4).gt.par(10)) state(4)=par(10) 
      if (state(5).gt.par(11)) state(5)=par(11) 
      if (state(6).gt.(par(1)+par(9))) state(6)=par(1)+par(9)
      if (state(6).lt.state(1)) state(6)=state(1)

      return               
      end

