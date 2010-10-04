       SUBROUTINE interp_tgt(ii, n1, n2, m1, m2, u1, u2, u3,
     &                       A, T, Px, Py, Pz,
     &                       AA, TT, PPx, PPy, PPz, AMPMIN,
     &                       OUT_A, OUT_T, OUT_UX, OUT_UY, OUT_UZ, SSHOT)
      implicit none

* Variabili in input
      integer*4 n1,n2

      real*4    A(n1,n2,2), T(n1,n2,2)
      real*4    Px(n1,n2,2), Py(n1,n2,2), Pz(n1,n2,2) 

      real*4    AA(*), TT(*), PPx(*), PPy(*), PPz(*)

      integer*4 m1,m2,ii
      integer*4 OUT_A,OUT_T,OUT_UX,OUT_UY,OUT_UZ
      real*4    u1,u2,u3
      real*4    v1,v2
      real*4    AMPMIN
      real*4    SLOWTEMP,SSHOT

      IF ((A(m1,m2,1).GT.AMPMIN)    .AND.(A(m1+1,m2,1).GT.AMPMIN).AND.
     &    (A(m1+1,m2+1,1).GT.AMPMIN).AND.(A(m1,m2+1,1).GT.AMPMIN).AND.
     &    (A(m1,m2,2).GT.AMPMIN)    .AND.(A(m1+1,m2,2).GT.AMPMIN).AND.
     &    (A(m1+1,m2+1,2).GT.AMPMIN).AND.(A(m1,m2+1,2).GT.AMPMIN).AND.
     &    (T(m1,m2,1).GT.0.0E0)     .AND.(T(m1+1,m2,1).GT.0.0E0) .AND.
     &    (T(m1+1,m2+1,1).GT.0.0E0) .AND.(T(m1,m2+1,1).GT.0.0E0) .AND.
     &    (T(m1,m2,2).GT.0.0E0)     .AND.(T(m1+1,m2,2).GT.0.0E0) .AND.
     &    (T(m1+1,m2+1,2).GT.0.0E0) .AND.(T(m1,m2+1,2).GT.0.0E0) ) then

       IF (OUT_A.EQ.1) THEN
        v1 = (1.0E0-u1)*(1.0E0-u2)*A(m1,m2,1) + u1*(1.0E0-u2)*A(m1+1,m2,1) +
     &       u1*u2*A(m1+1,m2+1,1) + (1.0E0-u1)*u2*A(m1,m2+1,1)      
        v2 = (1.0E0-u1)*(1.0E0-u2)*A(m1,m2,2) + u1*(1.0E0-u2)*A(m1+1,m2,2) +
     &       u1*u2*A(m1+1,m2+1,2) + (1.0E0-u1)*u2*A(m1,m2+1,2)
        AA(ii) = (1.0E0-u3)*v1 + u3*v2
       ENDIF

       IF (OUT_T.EQ.1) THEN
        v1 = (1.0E0-u1)*(1.0E0-u2)*T(m1,m2,1) + u1*(1.0E0-u2)*T(m1+1,m2,1) +
     &       u1*u2*T(m1+1,m2+1,1) + (1.0E0-u1)*u2*T(m1,m2+1,1)      
        v2 = (1.0E0-u1)*(1.0E0-u2)*T(m1,m2,2) + u1*(1.0E0-u2)*T(m1+1,m2,2) +
     &       u1*u2*T(m1+1,m2+1,2) + (1.0E0-u1)*u2*T(m1,m2+1,2)
        TT(ii) = (1.0E0-u3)*v1 + u3*v2
       ENDIF

       IF ((OUT_UX.EQ.1).OR.(OUT_UY.EQ.1).OR.(OUT_UZ.EQ.1)) THEN
        v1 = (1.0E0-u1)*(1.0E0-u2)*Px(m1,m2,1)+u1*(1.0E0-u2)*Px(m1+1,m2,1)+
     &       u1*u2*Px(m1+1,m2+1,1) + (1.0E0-u1)*u2*Px(m1,m2+1,1)
        v2 = (1.0E0-u1)*(1.0E0-u2)*Px(m1,m2,2)+ u1*(1.0E0-u2)*Px(m1+1,m2,2)+
     &       u1*u2*Px(m1+1,m2+1,2)+(1.0E0-u1)*u2*Px(m1,m2+1,2)
        PPx(ii) = (1.0E0-u3)*v1 + u3*v2
       

       
        v1 = (1.0E0-u1)*(1.0E0-u2)*Py(m1,m2,1)+u1*(1.0E0-u2)*Py(m1+1,m2,1)+
     &       u1*u2*Py(m1+1,m2+1,1) + (1.0E0-u1)*u2*Py(m1,m2+1,1)
        v2 = (1.0E0-u1)*(1.0E0-u2)*Py(m1,m2,2)+ u1*(1.0E0-u2)*Py(m1+1,m2,2)+
     &       u1*u2*Py(m1+1,m2+1,2)+(1.0E0-u1)*u2*Py(m1,m2+1,2)
        PPy(ii) = (1.0E0-u3)*v1 + u3*v2
      

       
        v1 = (1.0E0-u1)*(1.0E0-u2)*Pz(m1,m2,1)+u1*(1.0E0-u2)*Pz(m1+1,m2,1)+
     &       u1*u2*Pz(m1+1,m2+1,1) + (1.0E0-u1)*u2*Pz(m1,m2+1,1)
        v2 = (1.0E0-u1)*(1.0E0-u2)*Pz(m1,m2,2)+ u1*(1.0E0-u2)*Pz(m1+1,m2,2)+
     &       u1*u2*Pz(m1+1,m2+1,2)+(1.0E0-u1)*u2*Pz(m1,m2+1,2)
        PPz(ii) = (1.0E0-u3)*v1 + u3*v2
       
!Clara7 uscita in coseni direttori e non in slowness      
      SLOWTEMP = SQRT(PPx(ii)**2+PPy(ii)**2+PPz(ii)**2)
      
      IF (SLOWTEMP.eq.0.0) THEN
         SLOWTEMP=SSHOT
         WRITE(6,*)"-----SLOWTEMP=0---------------------"
         PPz(ii) = SLOWTEMP
      ENDIF

      IF (SLOWTEMP.eq.1.0) THEN
         WRITE(6,*) "Sshot", SSHOT	
         WRITE(6,*)"-----Sshot=1---------------------"
      ENDIF

      PPx(ii) = PPx(ii)/SLOWTEMP
      PPy(ii) = PPy(ii)/SLOWTEMP
      PPz(ii) = PPz(ii)/SLOWTEMP
      ENDIF !IF ((OUT_UX.EQ.1).OR.(OUT_UY.EQ.1).OR.(OUT_UZ.EQ.1))
      
      ENDIF

      RETURN
      END

