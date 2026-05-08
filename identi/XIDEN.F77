      SUBROUTINE XIDEN
C
C     VARIABLEN-VEREINBARUNGEN
C
      PARAMETER(NDAMA=2,NADMA=8,NADDAX=10,LENMA=1000)
      PARAMETER(LTH=30,LTH2=61,LTHLM=38,LU=10,NPWMA=400)
      REAL FPAR
      INTEGER*4 IZ,IZSAVE,IZMOLD,IMW(NADDAX)        
      INTEGER*2 ITIME,ICNEND,NADU,NDAU,LEN,ITYPMW,IVMA(NDAMA),    
     *NRDM(NDAMA),NSEQ(NDAMA),NRSB(NDAMA),IPL(NADDAX),NPL
      INTEGER*2 JOBPL,ISCALE(NADMA)
      INTEGER*2 ICNT,NPOINT,IONLFL
      INTEGER*2 IDAT(NADDAX,LENMA)
      INTEGER*2 NADMAX,NDAMAX,IADMIN,IADMAX,IDAMIN,IDAMAX,LENMAX
      INTEGER*2 NADRES,ILAUF,IPOS0(LTH,NADMA)
      INTEGER*2 NYI(NADMA),IAD(NADMA),IDA(NADDAX)
      REAL*4 W(NPWMA),W1(LTH,LTHLM),W2(LTH,LTH),W3(LTH,LU)
      REAL*4 WKSP(LTH,LTH2)
      CHARACTER*27 CMETHO(4),CMETH 
C
C     COMMON-VEREINBARUNGEN
C
      COMMON/KONFIG/ITIME,ICNEND,NADU,NDAU,LEN,ITYPMW,IMW,IVMA,   
     *NRDM,NSEQ,NRSB,IPL,NPL,JOBPL,ISCALE,FPAR
      COMMON/STATUS/IZ,ICNT,NPOINT,IONLFL,IZMOLD,NADRES,ILAUF
      COMMON/ROHDAT/IDAT          
      COMMON/GRENZW/NADMAX,NDAMAX,IADMIN,IADMAX,IDAMIN,IDAMAX,LENMAX,
     *NPMAX
      COMMON/MODELL/JOBID,NANF,IANZ,N,NM,NU,IGLW,NYI,IAD,IDA,W
      DATA NOUT,NDR/6,10/
      DATA CMETHO/'Kleinste Fehlerquadrate    ',      
     *'Kleinste Fehlerquadrate  QR','Instrumentelle Variablen   ',
     *'Instrumentelle Variablen QR'/
      DATA CMETH /'Bisher keine Spezifikation '/
C
C
C
 100  CONTINUE
      WRITE(*,110)
 110  FORMAT(/' Submen} Proze~identifikation'//
     *' R}ckkehr ins Hauptmen}_________________________________0'/
     *' Kanalzuordnung Systemgr|~en und L|schen Entkopplungen__1'/
     *' Festlegung der Anzahl auszuwertender Me~punkte_________2'/
     *' Festlegung bekannter Entkopplungen_____________________3'/
     *' Gleichwertsch{tzung Ein/Aus____________________________4'/
     *' Identifikation mit Standard-Least-Squares-Methode______5'/
     *' Identifikation mit QR-Least-Squares-Methode____________6'/
     *' Identifikation mit Standard-Hilfsvariablenmethode______7'/
     *' Identifikation mit QR-Hilfsvariablenmethode____________8'/
     *' Ausgabe des gesch{tzten Modells in Zustandsform________9'/ 
     *' Ausgabe des gesch{tzten Modells auf Drucker___________10'//
     *' Ihre Wahl:')
      READ(*,*,ERR=100) JOB
      IF(JOB.LE.0) GO TO 9999
      IF(JOB.GT.10) GO TO 100
      GO TO(1000,2000,2500,2800,3000,4000,5000,6000,7000,8000),JOB
C
C     FESTLEGUNG VON SYSTEMKENNGROESSEN
C
 1000 CONTINUE
      CALL SINPUT(NM,' Anzahl der Systemausg{nge: ')
      CALL SINPUT(NU,' Anzahl der Systemeing{nge: ')
      DO 1020 I=1,NM  
         WRITE(*,1010) I      
 1010    FORMAT(' Welcher Kanal soll Systemausgang Nr.',I3,' sein?')
         READ(*,*,ERR=1000) IAD(I)
         WRITE(*,1015)        
 1015    FORMAT(' Welcher Strukturindex wird dem Kanal zugeordnet?')
         READ(*,*,ERR=1000) NYI(I)
         IF(NYI(I).EQ.0) GO TO 1000
 1020 CONTINUE
      DO 1040 I=1,NU   
         WRITE(*,1030) I
 1030    FORMAT(' Welcher Kanal soll Systemeingang Nr.',I3,' sein?')
         READ(*,*,ERR=1000) IDA(I)
 1040 CONTINUE
      DO 1060 I=1,NADMA
         DO 1050 J=1,LTH 
            IPOS0(J,I)=0
 1050    CONTINUE
 1060 CONTINUE
      GO TO 100
C
C     FESTLEGUNG DER ANZAHL AUSZUWERTENDER PUNKTE
C
 2000 CONTINUE
      CALL SINPUT(NANF,' Anfangspunkt der Sch{tzung:')
      CALL SINPUT(IANZ,' Anzahl der Sch{tzpunkte:')
      GO TO 100
 2500 CONTINUE
 2580 CONTINUE
      CALL SINPUT(IZEI,' Nichttriviale Zeile Nr.(0-Abbruch):')
      IF(IZEI.LE.0) GO TO 100
      CALL SINPUT(ISPA,' Spalte Nr.:')
      IPOS0(ISPA,IZEI)=1
      GO TO 2580
C
C     GLEICHWERTIDENTIFIKATION
C
 2800 CONTINUE
      CALL SINPUT(IGLW,' Ohne (0) oder mit (1) Gleichwertsch{tzung?:')
      GO TO 100
C
C     IDENTIFIKATION
C
 3000 JOBID=1
      GO TO 6100
 4000 JOBID=2
      GO TO 6100
 5000 JOBID=3
      GO TO 6100
 6000 JOBID=4
 6100 CONTINUE
      N=0
      DO 6105 I=1,NM
         N=N+NYI(I)
 6105 CONTINUE
      NPW=N*(NU+NM)
      NPS=NPOINT
      IERR=JOBID
      CALL XIDENT(W,IDAT,IDA,IAD,LEN,NANF,IANZ,NPS,NYI,NU,NM,W1,W2,W3,
     *IPOS0,IGLW,WKSP,IERR) 
      IF(IERR.EQ.1) GO TO 9910
      IF(IERR.EQ.2) GO TO 9920
      DIAG=0.
      DO 6110 I=1,N
         DIAG=DIAG+W2(I,I)
 6110 CONTINUE
      IF(IGLW.EQ.1) THEN
         CALL XMDECD(W1,LTH,N,W3,LTH,NU,W2,LTH,NM,W,NPW,NYI,IERR)
         IF(IERR.NE.0) GO TO 9930
         DO 6130 I=1,N
            DO 6120 J=1,N
               W1(I,J)=-W1(I,J)
 6120       CONTINUE
            W1(I,I)=W1(I,I)+1.
 6130    CONTINUE
         CALL MIMPRD(W(NPW+1),N,W1,LTH,N,W(NPW+1),N,1,W2,1,INFO)
         IF(INFO.NE.0) GO TO 9930
      END IF
      GO TO 100
C
C     AUSGABE VON MODELLDATEN
C
 7000 CONTINUE
      NOU=NOUT
      GO TO 8500
 8000 CONTINUE
      NOU=NDR
      OPEN(UNIT=NDR,ACCESS='SEQUENTIAL',FILE='LST:',FORM='FORMATTED')
 8500 CONTINUE
      IF(JOBID.GT.0) CMETH=CMETHO(JOBID)
      WRITE(NOU,8510) CMETH,NANF,IANZ,(IDA(I),I=1,NU) 
 8510 FORMAT(/' Ausgabe identifizierter Modellparameter:'//
     *' Verwendete Methode:           ',A27,/ 
     *' Beginn der Sch{tzung an Punkt:',I5,/
     *' Anzahl der Abtastungen:       ',I5,/
     *' Modelleing{nge an Me~kan{len: ',8I3)
      WRITE(NOU,8520) (IAD(I),I=1,NM)
 8520 FORMAT(' Modellausg{nge an Me~kan{len: ',8I3)
      WRITE(NOU,8530) (NYI(I),I=1,NM)
 8530 FORMAT(' Angenommene Strukturindizes:  ',8I3)
      NPW=N*(NM+NU)
      CALL XMDECD(W1,LTH,N,W3,LTH,NU,W2,LTH,NM,W,NPW,NYI,IERR)
      IF(IERR.NE.0) GO TO 9930
      CALL MWRITE(W1,LTH,N,N,NOU,'A   ',4)
      CALL MWRITE(W3,LTH,N,NU,NOU,'B   ',4)
      CALL MWRITE(W2,LTH,NM,N,NOU,'C   ',4)
      DO 8535 I=1,N
         DO 8532 J=1,N
            W1(I,J)=-W1(I,J)
 8532    CONTINUE
         W1(I,I)=W1(I,I)+1.
 8535 CONTINUE
      CALL MIMPRD(W3,LTH,W1,LTH,N,W3,LTH,NU,W2,1,IERR)
      IF(IERR.NE.0) GO TO 9930
      INDEX=1
      DO 8538 I=1,NM
         DO 8537 J=1,NU
            W3(I,J)=W3(INDEX,J)
 8537    CONTINUE
         INDEX=INDEX+NYI(I)
 8538 CONTINUE
      WRITE(NOU,8539)
 8539 FORMAT(' Matrix der stationaeren Verstaerkungen:')
      CALL MWRITE(W3,LTH,NM,NU,NOU,'Kend',4)
      IF(IGLW.EQ.1) THEN
         IPW=1
         DO 8545 I=1,NM
            WRITE(NOU,8540) I,W(NPW+IPW)
 8540       FORMAT(' Gleichwert an Ausgang ',I2,':',G12.4)
            IPW=IPW+NYI(I)
 8545    CONTINUE
      END IF
      IF(JOBID.EQ.2.OR.JOBID.EQ.4) THEN
         WRITE(NOU,8548) DIAG
 8548    FORMAT(' Determinante des Gleichungssystems:',G12.4)
      END IF
      IF(NOU.EQ.NDR) THEN
         CLOSE(NDR)
      ELSE
 8550    CONTINUE
         IF(IRAWIO().EQ.0) GO TO 8550
      END IF
      GO TO 100
C
C     FEHLERMELDUNGEN
C
 9910 CONTINUE
      WRITE(*,9915)
 9915 FORMAT(' *Fehlermeldung: Singulaere Datenmatrix*')
      GO TO 100
 9920 CONTINUE
      WRITE(*,9925)
 9925 FORMAT(' *Fehlermeldung: Instabiles Hilfsmodell*')
      GO TO 100 
 9930 CONTINUE
      WRITE(*,9935)
 9935 FORMAT(' *Fehlermeldung: Inkonsistente Strukturangaben*')
      GO TO 100
 9999 CONTINUE
      RETURN
      END
