      character*(*) function itostr (n)
CCC integer --> ASCII string

      implicit none

CCC Parameters
CCC integer n - See above
CCC             n is not changed by itostr

      integer n

CCC Local variables
CCC integer k - = ABS(n) Operations are done on k, so n doesn't change 
CCC integer j - Number of figures of n
CCC integer kn - Ith figure of n 
CCC character c - kn figure translated in ASCII character

      integer i, j, k, fk, kn
      character c

CCC Control if n = 0
      k  = ABS (n)
      if (k .EQ. 0) then
         itostr = '0'
      else
         itostr = ' '
         j = INT (LOG10 (FLOAT(k)+1.E-10)) + 1
         do 10 i = j, 1, -1
            fk = FLOAT (k) + 1.E-10
            kn = INT (fk / 10**(i-1))
CCC In the next three rows: translate kn into c ASCII, insert c in the right
CCC position in itostr string and delete from k the converted figure
CCC After this it restarts            
            c  = CHAR (kn+48)
            itostr(j-i+1:j-i+1) = c
            k = k - kn * 10**(i-1)
 10      continue
      endif

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      integer function strlen (string)
CCC Count string's elements different from the final blank
CCC string must not contains any blank between its characters

      implicit none

CCC Parameters
CCC character*(*) string - See above
CCC                        string is not changed by strlen

      character*(*) string

CCC Local variables
CCC integer l - string length

      integer l, i

      l = len (string)
      i = 1
CCC Control if examined character is a blank ( = 32 ASCII )
 10    if (ICHAR (string(i:i)) .EQ. 32) then          
         strlen = i - 1
         goto 20
      else
         i = i + 1
         if (i .LE. l) then
            goto 10
         else
            strlen = l
         endif
      endif

 20   return
      end

