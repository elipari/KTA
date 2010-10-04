/****************************************************/
/*                    tools.c                       */
/****************************************************/
#include "../0_include/0_common_defs.h"
#include "../0_include/0_global_vars.h"

/*-------------------      ReadParams     -------------------*/
/*-----------------------------------------------------------*/
int ReadParams(FILE *fp, char *ident[], char *val[], int str_len, int vect_len)
{
   char *temp, *line;
   signed char test;
   int i, j, k, sp, imax;

   rewind(fp);
   imax = 0;
   temp = calloc(str_len, sizeof(char));
   line = calloc(str_len, sizeof(char));

   i=0;
   while (feof(fp) == 0)
     {
        ident[i][0]='\0';
        val[i][0]='\0';

        test = fgetc(fp);
        if ((test != REMARK) && (test != '\n') && (test != EOF))
          {
            ungetc(test, fp);
            fscanf(fp," %[^\n]\n", line);
            if (strstr(line, "=") != NULL )
              {
                sscanf(line," %[^=]=%[^\n]\n", ident[i], val[i]);
                i++;
              }
          }
        else if (test == REMARK)
          {
            fscanf(fp,"%*[^\n]\n");
          }
      }

   imax=i;
   for (i=0; i<imax; i++)
     {
       j=0; k=0;
       sp=0;
       while (ident[i][j] != '\0')
         {
           if (isspace(ident[i][j]) == 0)
             {
               temp[k]=toupper(ident[i][j]);
               k++;
               sp=1;
              }
           else
             {
               if (sp == 1)
                 {
                   temp[k]=' ';
                   k++;
                   sp=2;
                  }
              }
           j++;
         }
       if (sp == 2)
         { k--; temp[k]='\0'; }
       else
         temp[k]='\0';


       j=0;
       while (temp[j] != '\0')
         {
           ident[i][j] = temp[j];
           j++;
          }
       ident[i][j]='\0';


       j=0; k=0;
       sp=0;
       while (val[i][j] != '\0')
         {
           if ( (val[i][j] == '\"') || (val[i][j] == '\'') )
             { if (sp == 0)
                  sp=1;
               else
                  sp=0;
              }
           else if ( (isspace(val[i][j]) == 0) || (sp==1) )
             {
               temp[k]=val[i][j];
               k++;
              }
           j++;
         }
       temp[k]='\0';

       j=0;
       while (temp[j] != '\0')
         {
           val[i][j] = temp[j];
           j++;
          }
       val[i][j]='\0';

     }

     free(line);
     free(temp);

     return(imax);
}


/*------------------- format_f772c_string -------------------*/
/*                                                           */
/* long int format_f772c_string(char* source, char *dest,    */
/*                              long int source_len)         */
/*                                                           */
/* Takes the string pointed by (char* source) and copies it  */
/* in (char* dest) appending a '\0' to the end of the string */
/*                                                           */
/* Returns the lenght of the C string, that is the number of */
/* bytes from start to the '\0'                              */
/*                                                           */
/* INPUT                                                     */
/* char* source : pointer to the string passed by the F77    */
/*                program                                    */
/* char* dest   : pointer to the space allocated for the     */
/*                local copy of the original string          */
/* long int source_len : size in bytes of the space allocated*/
/*                for the source string                      */
/* N.B.: the space allocated for (char* dest) must be the    */
/*       of (long int source_len) bytes                      */
/*                                                           */
/* OUTPUT                                                    */
/* long int     : the number of bytes of the C string        */
/*-----------------------------------------------------------*/
long int format_f772c_string(char* source, char *dest, long int source_len)
{
   int i, l;

   i = source_len-1;
   l = 0;
   while ( (i >= 0) && (l==0) )
     {
       if ( isspace(source[i]) == 0 ) l=i;

       i--;
      }

   for (i=0; i<=l; i++)
     {
       dest[i]=source[i];
     }
   dest[l+1]='\0';

   return (l+1);

 }


/*------------------- format_c2f77_string -------------------*/
/*                                                           */
/* long int format_c2f77_string(char* string, long int len)  */
/*                                                           */
/* Takes the string pointed by (char* string) and padds the  */
/* space from '\0' to (long int len) with white spaces.      */
/*                                                           */
/* Returns the lenght of the original C string, that is the  */
/* number of bytes from start to the '\0'                    */
/*                                                           */
/* INPUT                                                     */
/* char* string : pointer to string                          */
/* long int len : size in bytes of the space allocated to    */
/*                the string                                 */
/*                                                           */
/* OUTPUT                                                    */
/* long int     : the number of bytes of the original string */
/*-----------------------------------------------------------*/
long int format_c2f77_string(char* string, long int len)
{
   int i, l;

   i = 0;
   while ( string[i] != '\0') i++;

   for (l=i; l<len; l++) string[l]=' ';

   return (i);

 }


/*-------------------        round        -------------------*/
/*-----------------------------------------------------------*/
double round(double x)
{
  double y;

  y = floor(x);
  if (x-y > 0.5)
     return (y+1);
  else
     return y;

 }

/*Clara5 Funzioni per la lettura dei parametri di mute*/

void CountMutePar (FILE *pt_filein, int *nElemToRead, int *errCode)
{
	
	int n, i1, i2, old_i1, old_i2, contLinee;
       
       n = 0;
       contLinee = 0;
       *errCode  = 0;
       old_i1 = -1;
       old_i2 = -1;
       
       while ( n > -1)
       {
	       n = fscanf( pt_filein, "%d %d \n", &i1, &i2);
	       
/*	       printf(" n %i i1 %i i2 %i \n", n, i1, i2);*/
	       if ( n > 1 )
	       {
	          contLinee++;
                  /* Controlla che profondita' e offset siano monotone crescenti */
                  if ( i1 <= old_i1 || i2 < old_i2)
                  {
                  	  *errCode = 1;
                  	  return;
                  }
                  old_i1 = i1;
                  old_i2 = i2;
	          n = 0;
	       }
	       else if ( n > -1)
	       {
	       	  *errCode = 1;
	       	  return;
	       }
	       
	       
/*	       printf( " contLinee %i \n", contLinee);*/
       }
       
     *nElemToRead = contLinee;


}
void ReadMutePar (FILE *pt_filein, int nElemToRead, int *vett_depth, int *vett_off)
{
	
	int n;
       
       
       for ( n = 0; n < nElemToRead; n++)
       {
	       fscanf( pt_filein, "%d %d \n", &vett_depth[n], &vett_off[n]);
       }
       
}
/*****************************************************************************************/
/* Funzione di conversione REAL*4 --> INTEGER*4 */
/*
Funzione INT() modificata:                                                                                      
INT_KTA(12.0)     = 12                                                                                          
INT_KTA(12.5)     = 12                                                                                          
INT_KTA(12.9)     = 12                                                                                          
INT_KTA(12.95)    = 12                                                                                          
INT_KTA(12.99)    = 13                                                                                          
INT_KTA(12.995)   = 13                                                                                          
*/

#include <math.h>
int int_kta_(float* in_num)
{
      int toller=100;
      int difet, ecces;

      difet = (int)(*in_num * (float)toller);
      ecces = (int)rint((double)*in_num) * toller;
      if(abs(ecces-difet) == 1)
         return (int)rint((double)*in_num);
      else
         return (int)(*in_num);
}

void callocKTAFloat ( float **pt, int nelem, int err, int myrank, char *argv[])
{
         *pt=calloc( nelem, sizeof(float));
         if(*pt == NULL)
            err_and_exit(err, myrank, argv);
}

void callocKTAInt ( int **pt, int nelem, int err, int myrank, char *argv[])
{
         *pt=calloc( nelem, sizeof(int));
         if(*pt == NULL)
            err_and_exit(err, myrank, argv);
}

void callocKTAShortInt ( short int **pt, int nelem, int err, int myrank, char *argv[])
{
         *pt=calloc( nelem, sizeof(short int));
         if(*pt == NULL)
            err_and_exit(err, myrank, argv);
}

void callocKTAChar ( char **pt, int nelem, int err, int myrank, char *argv[])
{
         *pt=calloc( nelem, sizeof(char));
         if(*pt == NULL)
            err_and_exit(err, myrank, argv);
}

void callocKTAInt64 ( int64 **pt, int nelem, int err, int myrank, char *argv[])
{
         *pt=calloc( nelem, sizeof(int64));
         if(*pt == NULL)
            err_and_exit(err, myrank, argv);
}