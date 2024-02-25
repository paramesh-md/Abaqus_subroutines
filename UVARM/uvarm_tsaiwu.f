C-----------------------------------------------------------------------
C Subroutine: UVARM
C
C This subroutine is called at all material calculation points of the 
C elements for which the material definition includes the specification 
C of user-defined output variables (UVARM)
C
C-----------------------------------------------------------------------
C
C Output:
C
C UVAR(1): Tsai-Hill failure index
C
C-----------------------------------------------------------------------
      SUBROUTINE UVARM(UVAR,DIRECT,T,TIME,DTIME,CMNAME,ORNAME,
     1 NUVARM,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,NDI,NSHR,COORD,
     2 JMAC,JMATYP,MATLAYO,LACCFLA)
      
      INCLUDE 'ABA_PARAM.INC'
!
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3 FLGRAY(15)
      DIMENSION UVAR(NUVARM),DIRECT(3,3),T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)
      
      integer pinc, count
      real XT, XC, YT, YC, S, X, Y, FTSWU
      real F1, F2, F12, F11, F22, F66
      PARAMETER (XT=2106, XC=1676, YT=74, YC=322, S=110)
      save pinc, count
      data pinc, count /-1, 0/
      
      F1 = (1/XT) - (1/XC)
      F2 = (1/YT) - (1/YC)
      F11 = (1/(XT*XC))
      F22 = (1/(YT*YC))
      F66 = (1/S**2)
      F12 = sqrt(F11)*sqrt(F22)/2

C     Count executions of this subroutine   
      count=count+1 
      
C     Check for new increment      
      if (kinc .ne. pinc) then
          print*, '-----Increment:', kinc
          print*, '-----Count:', count
          pinc = kinc
      endif
 
      CALL GETVRM('S', ARRAY, JARRAY, FLGRAY, JRCD,
     &            JMAC, JMATYP, MATLAYO, LACCFLA)
      
  
      FTSWU = F1*ARRAY(1) + F2*ARRAY(2) + 2*F12*ARRAY(1)*ARRAY(2)
     &    + F11*ARRAY(1)**2 + F22*ARRAY(2)**2 + F66*ARRAY(4)**2

      UVAR(1) = FTSWU

      RETURN
      END