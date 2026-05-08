$STORAGE:2
c     Unterprogrammsammlung FBCALGF.FOR
c
c     Schnelle Algorithmen fuer Programm FBC (FeedBackController)
c     Roether,1987
c
c
c
      subroutine parsev(v1,v2,a,b,c,n1,as,ierr)
c
c     Zweck: Berechnung der Regelflaeche nach
c     Astroem (1972). Abbruch mit ierr=1, falls Pol
c     ausserhalb des Einheitskreises der z-ebene
c     Das Programm wurde auf sehr schnelle Berechnung des
c     gemischten Regelflaechenkriteriums ausgelegt.
c
c     Eingabeparameter:
c     as   - Hilfsfeld der Dimensiion n1
c     n1   - Laenge des Polynome a
c     c    - Vektor mit Koeffizienten des Zaehlers der
c            Stelluebertragungsfunktion
c     b    - Vektor mit Koeffizienten des Zaehlers der
c            Regeluebertragungsfunktion
c     a    - Vektor mit Koeffizienten des Gesamtnenners
c
c     Ausgabeparameter:
c
c     v1   - Wert der ersten Verlustfunktion b(z)/a(z)
c     v2   - Wert der zweiten Verlustfunktion  c(z)/a(z)
c     ierr - besitzt der Wert 1, wenn die Strecke nicht
c            asymptotisch stabil ist
c
c
c     Autor:Friedemann Roether
c     nach Astr”m: Introduction to stochastic control theory,1972
c     Erste Version:   21.2.82
c     Letzte Aenderung:15.5.87
c
      real*8 a(n1),b(n1),c(n1),as(n1),v1,v2,a0,alfa,beta,gama
      n=n1-1
      a0=a(1)
      ierr=0
      v1=0.d00
      v2=0.d00
c
c     Beginn der sukzessiven Berechnungsschleife
c
      do 10 k=1,n
         l=n+1-k
         l1=l+1
         alfa=a(l1)/a(1)
         beta=b(l1)/a(1)
         gama=c(l1)/a(1)
         v1=v1+beta*b(l1)
         v2=v2+gama*c(l1)
         do 20 i=1,l
            m=l+2-i
            as(i)=a(i)-alfa*a(m)
            b(i)=b(i)-beta*a(m)
            c(i)=c(i)-gama*a(m)
 20      continue
         if(as(1)) 50,50,30
 30      continue
            do 40 i=1,l
               a(i)=as(i)
 40         continue
 10   continue
c
c     Ende der sukzessiven Berechnungsschleife
c     Berechnung der Verlustfunktionen
c
      v1=(v1+b(1)*b(1)/a(1))/a0
      v2=(v2+c(1)*c(1)/a(1))/a0
      return
 50   continue
      ierr=1
      return
      end
c
c
c
      subroutine padd(z,idimz,x,idimx,y,idimy)
c
c     Zweck: Addition zweier Polynome z:=x+y
c
c     Eingabeparameter:
c     y    - Polynomparametervektor y
c     idimy- Laenge des Vektors y
c     x    - Polynomparametervektor x
c     idimx- Laenge des Vektors x
c
c     Ausgabeparameter:
c     z    - Ergebnispolynom z
c     idimz- Laenge des Vektors z
c
c     Autor: IBM-SSP (IBM-Scientific-Program)
c
      real*8 z(idimz),x(idimx),y(idimy)
      ndim=idimx
      if(idimx-idimy) 10,10,20
 10   ndim=idimy
 20   if(ndim) 90,90,30
 30   do 80 i=1,ndim
      if(i-idimx) 40,40,60
 40   if(i-idimy) 50,50,70
 50   z(i)=x(i)+y(i)
      go to 80
 60   z(i)=y(i)
      go to 80
 70   z(i)=x(i)
 80   continue
 90   idimz=ndim
      return
      end
c
c
c
      subroutine pmp(z,idimz,x,idimx,y,idimy)
c
c     Zweck: Multiplikation der Polynome z:=x*y
c
c     Eingabeparameter:
c     y    - Polynomparametervektor y
c     idimy- laenge des Vektors y
c     x    - Polynomparametervektor x
c     idimx- Laenge des Vektors x
c
c     Ausgabeparameter:
c     z    - Polynomparametervektor z
c     idimz- Laenge des Vektors z
c
c     Autor: IBM-SSP (IBM Scientific Program Package)
c
      real*8 z(idimz),x(idimx),y(idimy)
      if(idimx*idimy) 10,10,20
 10   ndim=0
      go to 50
 20   ndim=idimx+idimy-1
      do 30 i=1,ndim
 30   z(i)=0.d00
      do 40 i=1,idimx
         do 45 j=1,idimy
            k=i+j-1
            z(k)=x(i)*y(j)+z(k)
 45      continue
 40   continue
      idimz=ndim
 50   return
      end
c
c
c
      SUBROUTINE SCHUD(R,LDR,P,X,Z,LDZ,NZ,Y,RHO,C,S)
C
C     ZWECK:ONLINE-QR-ZERLEGUNG EINES FELDES R
C     (NACH LINPACK,1979,SIAM)
C
C     EINGABEPARAMETER:
C     S    - VEKTOR MIT SIN-WERTEN DER DREHUNG
C     C    - VEKTOR MIT COS-WERTEN DER DREHUNG
C     RHO  - FEHLERVEKTOR
C     Y    - VEKTOR QT*Y, AUF DEN DREHUNG ANGEWENDET WIRD
C     NZ   - SPALTENZAHL DES FELDES Z
C     LDZ  - FUEHRENDE DIMENSION VON Z
C     Z    - RECHTE SEITE QT*Y, DIE BEI DREHUNG VON Y ENTSTEHT
C     X    - NEU HINZUKOMMENDER DATENVEKTOR
C     P    - ZEILEN- UND SPALTENZAHL VON R
C     LDR  - FUEHRENDE DIMENSION VON R
C     R    - MATRIX R DER ZERLEGUNG
C
C     AUSGABEPARAMETER:
C     R    - AKTUALISIERTER WERT VON R
C     Z    - AKTUALISIERTER WERT VON Z
C     C    - NEUER VEKTOR MIT SIN-WERTEN
C     S    - NEUER VEKTOR MIT COS-WERTEN
C
C     AUTOR:LINPACK USERS GUIDE
C     SIAM,1979
C
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER LDR,P,LDZ,NZ
      DIMENSION RHO(NZ),C(P)
      DIMENSION R(LDR,P),X(P),Z(LDZ,NZ),Y(NZ),S(P)
C
C     UPDATES A COLUMN X - SEE LINPACK USERS GUIDE FOR DETAILS
C
      INTEGER I,J,JM1
C
C
C     UPDATE R
C
      DO 30 J=1,P
      XJ=X(J)
C
C     APPLY THE PREVIOUS ROTATIONS
C
      JM1=J-1
      IF(JM1.LT.1) GO TO 20
      DO 10 I=1,JM1
      T=C(I)*R(I,J)+S(I)*XJ
      XJ=C(I)*XJ-S(I)*R(I,J)
      R(I,J)=T
 10   CONTINUE
 20   CONTINUE
C
C     COMPUTE THE NEXT ROTATION
C
      CALL SROTG(R(J,J),XJ,C(J),S(J))
 30   CONTINUE
C
C     IF REQUIRED, UPDATE Z AND RHO
C
      IF(NZ.LT.1) GO TO 70
      DO 60 J=1,NZ
      ZETA=Y(J)
      DO 40 I=1,P
      T=C(I)*Z(I,J)+S(I)*ZETA
      ZETA=C(I)*ZETA-S(I)*Z(I,J)
      Z(I,J)=T
 40   CONTINUE
      AZETA=DABS(ZETA)
      IF(AZETA.EQ.0.D00.OR.RHO(J).LT.0.D00) GO TO 50
      SCALE=AZETA+RHO(J)
C
C     MODIFYING ORIGINAL LINPACK-PROGRAM:NEXT ROW
C     RHO(J)=SQRT(AZETA*AZETA+RHO(J)*RHO(J))
C
      ARG=(AZETA/SCALE)*(AZETA/SCALE)+(RHO(J)/SCALE)*(RHO(J)/SCALE)
      RHO(J)=SCALE*DSQRT(ARG)
 50   CONTINUE
 60   CONTINUE
 70   CONTINUE
      RETURN
      END
c
c
c
      SUBROUTINE SROTG(SA,SB,C,S)
C
C     ZWECK: BERECHNET EINE GIVENS-ROTATION
C     QUELLE:LINPACK,1979,SIAM
C
C     EINGABEPARAMETER:
C     S    - SINUS DER DREHUNG
C     C    - COSINUS DER DREHUNG
C     SA   - 1.WERT
C     SB   - 2.WERT
C
C     AUSGABEPARAMETER:
C     S,C  - AKTUALISIERTE GROESSEN
C
C     AUTOR: JACK DONGARRA,SIAM,1979
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DABSSA=DABS(SA)
      DABSSB=DABS(SB)
      ROE=SB
      IF(DABSSA.GT.DABSSB) ROE=SA
      SCALE=DABSSA+DABSSB
      IF(SCALE.NE.0.D0) GO TO 10
      C=1.0D0
      S=0.0D0
      R=0.0D0
      GO TO 20
 10   SASC=SA/SCALE
      SBSC=SB/SCALE
      ARG=SASC*SASC+SBSC*SBSC
      R=SCALE*DSQRT(ARG)
      R=DSIGN(1.0D0,ROE)*R
      C=SA/R
      S=SB/R
 20   Z=1.0D0
      IF(DABSSA.GT.DABSSB) Z=S
      IF(DABSSB.GE.DABSSA.AND.C.NE.0.0D0) Z=1.0D0/C
      SA=R
      SB=Z
      RETURN
      END
