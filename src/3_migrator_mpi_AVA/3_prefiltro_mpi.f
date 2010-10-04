************************************************************************
*     FILE prefiltro.f
*     Contiene la subroutine prefiltro che esegue la convoluzione della traccia
*     sismica prima con scalino causale e poi con scalino antcausale
*
*     INPUT: la traccia da trattare
*
*     OUTPUT: la traccia prefiltrata
*
*     Fabrizio & Mirco
*
*     Luglio 1999
*
************************************************************************
      SUBROUTINE PREFILTRO( TRACCIA, NCAMP, NCAMPFILT, BORDF,
     $                      TRACCIAOUT, TRACCIA1, TRACCIA2 )

      IMPLICIT NONE

*     variabili di ingresso
      INTEGER*4  NCAMP, NCAMPFILT, X, BORDF
      REAL*4     TRACCIA(NCAMP)

*     variabili di uscita
      REAL*4 TRACCIAOUT(NCAMPFILT)

*     variabili locali
      REAL*4 TRACCIA1(NCAMP+BORDF),TRACCIA2(NCAMP+BORDF)

*     convoluziione con scalino causale
      TRACCIA1(1) = TRACCIA(1)
      DO 10 X=2,NCAMP
         TRACCIA1(X) = TRACCIA1(X-1)+TRACCIA(X)
 10   CONTINUE

      DO 20 X=1,BORDF
         TRACCIA1(NCAMP+X) = TRACCIA1(NCAMP)
 20   CONTINUE


*     convoluzione con scalino antiacusale
      TRACCIA2(NCAMP+BORDF) = TRACCIA1(NCAMP+BORDF)
      DO 30 X=1,NCAMP+BORDF-1
         TRACCIA2(NCAMP+BORDF-X) = TRACCIA1(NCAMP+BORDF-X) +
     $                             TRACCIA2(NCAMP+BORDF-X+1)
 30   CONTINUE

      DO 40 X=1,BORDF
         TRACCIAOUT(X) = TRACCIA2(1)
 40   CONTINUE

      DO 50 X=1,NCAMP+BORDF
         TRACCIAOUT(X+BORDF)=TRACCIA2(X)
 50   CONTINUE

      RETURN
      END




