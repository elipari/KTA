#include "../0_include/0_common_defs.h"

/*************************************************/
/*               Local Function                  */
/*************************************************/
SEGYptr check_file(SEGYptr root, int index);
SEGYptr add_file(SEGYptr *root, int index);

TGTptr TargetParams2(char* ident[], char * val[], int imax, char *filename);
TGTptr TargetParams3(char* ident[], char * val[], int imax, char *filename);

TGTptr check_tgt(TGTptr root, int index);
TGTptr add_tgt(TGTptr *root, int index);


/*************************************************/
/*                TracesParams                   */
/*************************************************/
SEGYptr TracesParams(char *filename)
{

   FILE *fp;
   char* ident[MAX_PARAM];
   char* val[MAX_PARAM];
   char* ident_s;
   int ident_n;
   int i, j, imax;

   SEGYptr startsgy, cursgy;

   /* Variables initialization */
   for (i=0; i< MAX_PARAM; i++)
      {
       ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
       val[i] = calloc(MAX_STRING_LEN, sizeof(char));
      }
   ident_s=calloc(MAX_STRING_LEN,sizeof(char));

   fp=fopen(filename, "r");
   if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", filename);
          exit(EXIT_FAILURE);
        }

   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );

   fclose(fp);


   /* assegnazione delle variabili */
   j=0;
   startsgy = NULL;
   cursgy = NULL;


   for (i=0; i<imax; i++)
     {

       sscanf(ident[i],"%[^_]_%d", ident_s, &ident_n);

       /* nome del file  */
       if (strcmp("FILENAME", ident_s) == 0 )
         {
            cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%s", cursgy->filename);
           j++;
         }

       /* offset e num.bytes identificativo shot  */
       else if (strcmp("OFFSET-NUMSHOT", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->nshotoff));
           j++;
         }

       else if (strcmp("NBYTES-NUMSHOT", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->nshotnbytes));
           j++;
         }

       /* offset e num.bytes posizione sorgente  */
       else if (strcmp("OFFSET-XSOURCE", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->xsoff));
           j++;
         }

       else if (strcmp("NBYTES-XSOURCE", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->xsnbytes));
           j++;
         }
       else if (strcmp("OFFSET-YSOURCE", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->ysoff));
           j++;
         }

       else if (strcmp("NBYTES-YSOURCE", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->ysnbytes));
           j++;
         }

       else if (strcmp("OFFSET-ZSOURCE", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->zsoff));
           j++;
         }

       else if (strcmp("NBYTES-ZSOURCE", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->zsnbytes));
           j++;
         }

       /* offset e num.bytes posizione ricevitore  */
       else if (strcmp("OFFSET-XRECEIVER", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->xroff));
           j++;
         }

       else if (strcmp("NBYTES-XRECEIVER", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->xrnbytes));
           j++;
         }
       else if (strcmp("OFFSET-YRECEIVER", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->yroff));
           j++;
         }

       else if (strcmp("NBYTES-YRECEIVER", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->yrnbytes));
           j++;
         }

       else if (strcmp("OFFSET-ZRECEIVER", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->zroff));
           j++;
         }

       else if (strcmp("NBYTES-ZRECEIVER", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->zrnbytes));
           j++;
         }

       /* numero prima traccia da migrare */
       else if (strcmp("TRACE-OFFSET", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->traceoff));
           j++;
         }

       else if (strcmp("NTRACCETOT", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->ntraccetot));
           j++;
         }

       /* parametri descrittivi traccia */
       else if (strcmp("NCAMPIONI", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->ncamp));
           j++;
         }

       else if (strcmp("FORMATO", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%d", &(cursgy->formato));
           j++;
         }
       else if (strcmp("DT", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->dt));
           j++;
         }
       else if (strcmp("DX", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->dx));
           j++;
         }
       else if (strcmp("TSHIFT", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->tshift));
           j++;
         }


       /* rotraslazione piano acquisizione  */
       else if (strcmp("A11", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->a11));
           j++;
         }
       else if (strcmp("A12", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->a12));
           j++;
         }
       else if (strcmp("A21", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->a21));
           j++;
         }
       else if (strcmp("A22", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->a22));
           j++;
         }
       else if (strcmp("B1", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->b1));
           j++;
         }
       else if (strcmp("B2", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%f", &(cursgy->b2));
           j++;
         }


       /* file pesi per equalizzazione  */
       else if (strcmp("FILEPESI", ident_s) == 0 )
         {
           cursgy = check_file(startsgy, ident_n);
           if (cursgy == NULL)
               cursgy = add_file(&startsgy, ident_n);

           sscanf(val[i], "%s", cursgy->filepesi);
           j++;
         }

     }

   /* controllo parametri obbligatori */
   cursgy = startsgy;
   while (cursgy != NULL)
     {
        if (strcmp(cursgy->filename, "\0") == 0) {
           fprintf(stderr, "ERROR: missing parameter filename_%d in file %s\n",
                   cursgy->seq, filename);
           exit(EXIT_FAILURE); }

        if (cursgy->ntraccetot == -1) {
           fprintf(stderr, "ERROR: missing parameter ntracceshot_%d in file %s\n",
                   cursgy->seq, filename);
           exit(EXIT_FAILURE); }

        if (cursgy->ncamp == -1) {
           fprintf(stderr, "ERROR: missing parameter ncamp_%d in file %s\n",
                   cursgy->seq, filename);
           exit(EXIT_FAILURE); }

        if (cursgy->formato == -1) {
           fprintf(stderr, "ERROR: missing parameter formato_%d in file %s\n",
                   cursgy->seq, filename);
           exit(EXIT_FAILURE); }

        if (cursgy->dt == -1) {
           fprintf(stderr, "ERROR: missing parameter dt_%d in file %s\n",
                   cursgy->seq, filename);
           exit(EXIT_FAILURE); }

        if (cursgy->dx == -1) {
           fprintf(stderr, "ERROR: missing parameter dx_%d in file %s\n",
                   cursgy->seq, filename);
           exit(EXIT_FAILURE); }

        cursgy = cursgy->nextsgy;

     }

   for (i=0; i< MAX_PARAM; i++)
     {
       free(ident[i]);
       free(val[i]);
     }
   free(ident_s);

   return startsgy;

}



/*************************************************/
/*                TargetParams                   */
/*************************************************/
TGTptr TargetParams(char *filename, int *ptr_settype)
{

   FILE *fp;
   char* ident[MAX_PARAM];
   char* val[MAX_PARAM];
   int i, j, imax;
   TGTptr starttgt;

   /* Variables initialization */
   for (i=0; i< MAX_PARAM; i++)
      {
       ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
       val[i] = calloc(MAX_STRING_LEN, sizeof(char));
      }

   fp=fopen(filename, "r");
   if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", filename);
          exit(EXIT_FAILURE);
        }

   imax = 0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );

   fclose(fp);

   *ptr_settype=0;
   j=0;
   for (i=0; i<imax; i++)
     {
        if (strcmp("SETTYPE", ident[i]) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {
                 sscanf(val[i], "%d", ptr_settype);
                 j++;
              }
         }
     }

   if (j==0)
     {
       fprintf(stderr, "ERROR in %s : SETTYPE must be specified.\n", filename);
       exit(EXIT_FAILURE);
      }
   if ((*ptr_settype < 1) || (*ptr_settype > 3))
     {
       fprintf(stderr, "ERROR in %s : SETTYPE must be 1,2 or 3.\n", filename);
       exit(EXIT_FAILURE);
      }

   starttgt = NULL;

   if (*ptr_settype == 2)
     {
        starttgt = TargetParams2(ident, val, imax, filename);
      }
   else if (*ptr_settype == 3)
     {
        starttgt = TargetParams3(ident, val, imax, filename);
      }

   for (i=0; i< MAX_PARAM; i++)
     {
       free(ident[i]);
       free(val[i]);
     }

   return starttgt;
 }

/*----------------------------------------------------------*/
/*----------------------------------------------------------*/
/*----------------------------------------------------------*/
TGTptr TargetParams2(char* ident[], char * val[], int imax, char *filename)
{
   char* ident_s;
   int ident_n;
   int i, j;
   TGTptr starttgt, curtgt;

   float test1;
   float theta, phi, psi;

   ident_s=calloc(MAX_STRING_LEN,sizeof(char));

   /* assegnazione delle variabili */
   starttgt = NULL;
   curtgt = NULL;
   j=0;
   for (i=0; i<imax; i++)
     {

       sscanf(ident[i],"%[^_]_%d", ident_s, &ident_n);

       if (strcmp("O1", ident_s) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {

                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->o1));
                j++;
              }
         }
       else if (strcmp("O2", ident_s) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {

                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->o2));
                j++;
              }
         }
       else if (strcmp("O3", ident_s) == 0 ) /* 03 */
         {
            if (val[i][0] != '\0')
              {

                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->o3));
                j++;
              }
         }
       else if (strcmp("A1", ident_s) == 0 ) /* 04 */
         {
           if (val[i][0] != '\0')
              {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->a1));
                j++;
              }
         }
       else if (strcmp("A2", ident_s) == 0 ) /* 05 */
         {
           if (val[i][0] != '\0')
              {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->a2));
                j++;
              }
         }
       else if (strcmp("A3", ident_s) == 0 ) /* 06 */
         {
           if (val[i][0] != '\0')
              {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->a3));
                j++;
              }
         }
       else if (strcmp("B1", ident_s) == 0 ) /* 07 */
         {
           if (val[i][0] != '\0')
              {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->b1));
                j++;
              }
         }
       else if (strcmp("B2", ident_s) == 0 ) /* 08 */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->b2));
                j++;
              }
         }
       else if (strcmp("B3", ident_s) == 0 ) /* 09  */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->b3));
                j++;
              }
         }
       else if (strcmp("LA", ident_s) == 0 ) /* 10 */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->la));
                j++;
              }
         }
       else if (strcmp("LB", ident_s) == 0 ) /* 11 */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->lb));
                j++;
              }
         }
       else if (strcmp("THETA", ident_s) == 0 ) /* 12 */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->theta));
                j++;
              }
         }
       else if (strcmp("PHI", ident_s) == 0 ) /* 13 */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->phi));
                j++;
              }
         }
       else if (strcmp("PSI", ident_s) == 0 ) /* 13 */
         {
           if (val[i][0] != '\0')
             {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%f", &(curtgt->psi));
                j++;
              }
         }
       else if (strcmp("NA", ident_s) == 0 ) /* 14 */
         {
           if (val[i][0] != '\0')
           {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%d", &(curtgt->na));
                j++;
              }
         }
       else if (strcmp("NB", ident_s) == 0 ) /* 15 */
         {
           if (val[i][0] != '\0')
           {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%d", &(curtgt->nb));
                j++;
              }
         }
       else if (strcmp("NNA", ident_s) == 0 ) /* 16 */
         {
           if (val[i][0] != '\0')
           {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%d", &(curtgt->nna));
                j++;
              }
         }
       else if (strcmp("NNB", ident_s) == 0 ) /* 17 */
         {
           if (val[i][0] != '\0')
           {
                curtgt = check_tgt(starttgt, ident_n);
                if (curtgt == NULL)
                    curtgt = add_tgt(&starttgt, ident_n);

                sscanf(val[i], "%d", &(curtgt->nnb));
                j++;
              }
         }
     }

     if ( j < 12)
       {
          fprintf(stderr, "ERROR: missing parameters in file %s\n", filename);
          exit(EXIT_FAILURE);
        }


  /* ------------------ target parameters check ------------------------ */

    curtgt = starttgt;
    while (curtgt != NULL)
      {
        if ( (curtgt->o1==FLT_MAX) || (curtgt->o2==FLT_MAX) || (curtgt->o3==FLT_MAX) )
          {
             fprintf(stderr, "ERROR in O(o1, o2, o3)\n");
             exit(EXIT_FAILURE);
           }


        if ( ( ((curtgt->nna)-1) % ((curtgt->na)-1) ) != 0 )
          {
             fprintf(stderr, "ERROR: parameters file %s\n", filename);
             fprintf(stderr, "Target n. %d\n", curtgt->seq);
             fprintf(stderr, "nna-1 must be an integral multiple of na-1.\n");
             exit(EXIT_FAILURE);
           }


        if ( ( ((curtgt->nnb)-1) % ((curtgt->nb)-1) ) != 0 )
          {
             fprintf(stderr, "ERROR: parameters file %s\n", filename);
             fprintf(stderr, "Target n. %d\n", curtgt->seq);
             fprintf(stderr, "nnb-1 must be an integral multiple of nb-1.\n");
             exit(EXIT_FAILURE);
           }

        if ( (curtgt->a1==FLT_MAX) || (curtgt->a2==FLT_MAX) || (curtgt->a3==FLT_MAX) ||
             (curtgt->b1==FLT_MAX) || (curtgt->b2==FLT_MAX) || (curtgt->b3==FLT_MAX)    )
           {
             if ( (curtgt->la==FLT_MAX) || (curtgt->theta==FLT_MAX) || (curtgt->phi==FLT_MAX) ||
                  (curtgt->lb==FLT_MAX) || (curtgt->psi==FLT_MAX)   )
                {
                  fprintf(stderr, "ERROR: either A and B coords or (la, lb) and (theta, phi, psi) must be specified.\n");
                  exit(EXIT_FAILURE);
                }
             else
                {
                  curtgt->settype = 2;
                  theta = curtgt->theta*PI/180;
                  psi = curtgt->psi*PI/180;
                  phi = curtgt->phi*PI/180;

                  curtgt->ca1 = - sin(psi)*cos(theta)*sin(phi) + cos(psi)*cos(phi) ;
                  curtgt->ca2 =   sin(psi)*cos(theta)*cos(phi) + cos(psi)*sin(phi) ;
                  curtgt->ca3 =   sin(psi)*sin(theta);

                  curtgt->cb1 = - cos(psi)*cos(theta)*sin(phi) - sin(psi)*cos(phi) ;
                  curtgt->cb2 =   cos(psi)*cos(theta)*cos(phi) - sin(psi)*sin(phi) ;
                  curtgt->cb3 =   cos(psi)*sin(theta);

                  curtgt->cc1 =   sin(phi)*sin(theta);
                  curtgt->cc2 = - cos(phi)*sin(theta);
                  curtgt->cc3 =   cos(theta);


                  curtgt->da = curtgt->la/(curtgt->na - 1);
                  curtgt->db = curtgt->lb/(curtgt->nb - 1);

                  curtgt->dda = curtgt->la/(curtgt->nna - 1);
                  curtgt->ddb = curtgt->lb/(curtgt->nnb - 1);

                  curtgt->lc = 0;
                  curtgt->nc = 1;
                  curtgt->nnc = 1;
                  curtgt->dc = 0;
                  curtgt->ddc = 0;

                  curtgt->nt = curtgt->na*curtgt->nb;
                  curtgt->nnt = curtgt->nna*curtgt->nnb;

                 }
          }
      else
          {
              curtgt->la = sqrt(
                                (curtgt->a1-curtgt->o1)*(curtgt->a1-curtgt->o1) +
                                (curtgt->a2-curtgt->o2)*(curtgt->a2-curtgt->o2) +
                                (curtgt->a3-curtgt->o3)*(curtgt->a3-curtgt->o3)
                               );
              curtgt->lb = sqrt(
                                (curtgt->b1-curtgt->o1)*(curtgt->b1-curtgt->o1) +
                                (curtgt->b2-curtgt->o2)*(curtgt->b2-curtgt->o2) +
                                (curtgt->b3-curtgt->o3)*(curtgt->b3-curtgt->o3)
                               );

              curtgt->ca1=(curtgt->a1-curtgt->o1)/curtgt->la;
              curtgt->ca2=(curtgt->a2-curtgt->o2)/curtgt->la;
              curtgt->ca3=(curtgt->a3-curtgt->o3)/curtgt->la;

              curtgt->cb1=(curtgt->b1-curtgt->o1)/curtgt->lb;
              curtgt->cb2=(curtgt->b2-curtgt->o2)/curtgt->lb;
              curtgt->cb3=(curtgt->b3-curtgt->o3)/curtgt->lb;

              curtgt->cc1 =   (curtgt->ca2 * curtgt->cb3) - (curtgt->ca3 * curtgt->cb2) ;
              curtgt->cc2 =   (curtgt->ca3 * curtgt->cb1) - (curtgt->ca1 * curtgt->cb3) ;
              curtgt->cc3 =   (curtgt->ca1 * curtgt->cb2) - (curtgt->ca2 * curtgt->cb1) ;

              test1 = abs(curtgt->ca1*curtgt->cb1 + curtgt->ca2*curtgt->cb2
                        + curtgt->ca3*curtgt->cb3);

              if ( test1 > EPS )
               {
                 fprintf(stderr,"ERROR angle between edges of panel n.%d must be 90 degrees\n", curtgt->seq);
                 exit(EXIT_FAILURE);
                }

              curtgt->da = curtgt->la/(curtgt->na - 1);
              curtgt->db = curtgt->lb/(curtgt->nb - 1);
              curtgt->dda = curtgt->la/(curtgt->nna - 1);
              curtgt->ddb = curtgt->lb/(curtgt->nnb - 1);

              curtgt->lc = 0;
              curtgt->nc = 1;
              curtgt->nnc = 1;
              curtgt->dc = 0;
              curtgt->ddc = 0;

              curtgt->nt = curtgt->na*curtgt->nb;
              curtgt->nnt = curtgt->nna*curtgt->nnb;

          }

      curtgt = curtgt->next_tgt;

      }



    free(ident_s);

    return starttgt;

}


/*----------------------------------------------------------*/
/*----------------------------------------------------------*/
/*----------------------------------------------------------*/

TGTptr TargetParams3(char* ident[], char * val[], int imax, char *filename)
{
   int i, j, k;
   TGTptr starttgt, curtgt;

   float o1, o2, o3;

   float a1, a2, a3;
   float b1, b2, b3;
   float c1, c2, c3;

   float la, lb, lc;
   float psi, theta, phi;

   int na, nb, nc, nna, nnb, nnc;

   float da, db, dc;
   float dda, ddb, ddc;
   float test1, test2, test3;
   float ca1, ca2, ca3;
   float cb1, cb2, cb3;
   float cc1, cc2, cc3;

   /* assegnazione delle variabili */
   o1=FLT_MAX;
   o2=FLT_MAX;
   o3=FLT_MAX;

   a1=FLT_MAX;
   a2=FLT_MAX;
   a3=FLT_MAX;
   b1=FLT_MAX;
   b2=FLT_MAX;
   b3=FLT_MAX;
   c1=FLT_MAX;
   c2=FLT_MAX;
   c3=FLT_MAX;

   la=FLT_MAX;
   lb=FLT_MAX;
   lc=FLT_MAX;
   theta=FLT_MAX;
   phi=FLT_MAX;
   psi = 0;

   j=0;
   for (i=0; i<imax; i++)
     {
       if (strcmp("O1", ident[i]) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &o1);
                j++;
              }
         }
       else if (strcmp("O2", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &o2);
                j++;
              }
         }
       else if (strcmp("O3", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &o3);
                j++;
              }
         }
       else if (strcmp("A1", ident[i]) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &a1);
                j++;
              }
         }
       else if (strcmp("A2", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &a2);
                j++;
              }
         }
       else if (strcmp("A3", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &a3);
                j++;
              }
         }
       else if (strcmp("B1", ident[i]) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &b1);
                j++;
              }
         }
       else if (strcmp("B2", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &b2);
                j++;
              }
         }
       else if (strcmp("B3", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &b3);
                j++;
              }
         }
       else if (strcmp("C1", ident[i]) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &c1);
                j++;
              }
         }
       else if (strcmp("C2", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &c2);
                j++;
              }
         }
       else if (strcmp("C3", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &c3);
                j++;
              }
         }
       else if (strcmp("LA", ident[i]) == 0 ) /* 01 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &la);
                j++;
              }
         }
       else if (strcmp("LB", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &lb);
                j++;
              }
         }
       else if (strcmp("LC", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &lc);
                j++;
              }
         }
       else if (strcmp("THETA", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &theta);
                j++;
              }
         }
       else if (strcmp("PHI", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &phi);
                j++;
              }
         }
       else if (strcmp("PSI", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%f", &psi);
                j++;
              }
         }
       else if (strcmp("NA", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%d", &na);
                j++;
              }
         }
       else if (strcmp("NB", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%d", &nb);
                j++;
              }
         }
       else if (strcmp("NC", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%d", &nc);
                j++;
              }
         }
       else if (strcmp("NNA", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%d", &nna);
                j++;
              }
         }
       else if (strcmp("NNB", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%d", &nnb);
                j++;
              }
         }
       else if (strcmp("NNC", ident[i]) == 0 ) /* 02 */
         {
           if (val[i][0] != '\0')
              {
                sscanf(val[i], "%d", &nnc);
                j++;
              }
         }
     }  /* end for i */

     if ( j < 15)
       {
          fprintf(stderr, "ERROR: missing parameters in file %s\n", filename);
          exit(EXIT_FAILURE);
        }


  /* ------------------ target parameters check ------------------------ */
     if ( (o1==FLT_MAX) || (o2==FLT_MAX) || (o3==FLT_MAX) )
       {
         fprintf(stderr, "ERROR in O(o1, o2, o3)\n");
         exit(EXIT_FAILURE);
       }


    if ( ( (nna-1) % (na-1) ) != 0 )
       {
         fprintf(stderr, "ERROR: parameters file %s\n", filename);
         fprintf(stderr, "nna-1 must be an integral multiple of na-1.\n");
         exit(EXIT_FAILURE);
        }
    if ( ( (nnb-1) % (nb-1) ) != 0 )
       {
         fprintf(stderr, "ERROR: parameters file %s\n", filename);
         fprintf(stderr, "nnb-1 must be an integral multiple of nb-1.\n");
         exit(EXIT_FAILURE);
        }
    if ( ( (nnc-1) % (nc-1) ) != 0 )
       {
         fprintf(stderr, "ERROR: parameters file %s\n", filename);
         fprintf(stderr, "nnc-1 must be an integral multiple of nc-1.\n");
         exit(EXIT_FAILURE);
        }

    if ( (a1==FLT_MAX) || (a2==FLT_MAX) || (a3==FLT_MAX) ||
         (b1==FLT_MAX) || (b2==FLT_MAX) || (b3==FLT_MAX) ||
         (c1==FLT_MAX) || (c2==FLT_MAX) || (c3==FLT_MAX)  )
        {
          if ( (la==FLT_MAX) || (lb==FLT_MAX) || (lc==FLT_MAX) ||
               (theta==FLT_MAX) || (phi==FLT_MAX) || (psi==FLT_MAX) )
            {
              fprintf(stderr, "ERROR: either A,B and C coords or (la,lb,lc) and (theta,phi, psi) must be specified.\n");
              exit(EXIT_FAILURE);
             }
          else
            {
              theta = theta * PI / 180;
              phi   = phi   * PI / 180;
              psi   = psi   * PI / 180;

              ca1 = - sin(psi)*cos(theta)*sin(phi) + cos(psi)*cos(phi) ;
              ca2 =   sin(psi)*cos(theta)*cos(phi) + cos(psi)*sin(phi) ;
              ca3 =   sin(psi)*sin(theta);

              cb1 = - cos(psi)*cos(theta)*sin(phi) - sin(psi)*cos(phi) ;
              cb2 =   cos(psi)*cos(theta)*cos(phi) - sin(psi)*sin(phi) ;
              cb3 =   cos(psi)*sin(theta);

              cc1 =   sin(phi)*sin(theta);
              cc2 = - cos(phi)*sin(theta);
              cc3 =   cos(theta);


              da = la/(na - 1);
              db = lb/(nb - 1);
              dc = lc/(nc - 1);
              dda = la/(nna - 1);
              ddb = lb/(nnb - 1);
              ddc = lc/(nnc - 1);

             }
          }
      else
          {
              la = sqrt( (a1-o1)*(a1-o1) + (a2-o2)*(a2-o2) + (a3-o3)*(a3-o3) );
              lb = sqrt( (b1-o1)*(b1-o1) + (b2-o2)*(b2-o2) + (b3-o3)*(b3-o3) );
              lc = sqrt( (c1-o1)*(c1-o1) + (c2-o2)*(c2-o2) + (c3-o3)*(c3-o3) );

              ca1=(a1-o1)/la;
              ca2=(a2-o2)/la;
              ca3=(a3-o3)/la;

              cb1=(b1-o1)/lb;
              cb2=(b2-o2)/lb;
              cb3=(b3-o3)/lb;

              cc1=(c1-o1)/lc;
              cc2=(c2-o2)/lc;
              cc3=(c3-o3)/lc;

              test1 = abs(ca1*cb1 + ca2*cb2 + ca3*cb3);
              test2 = abs(ca1*cc1 + ca2*cc2 + ca3*cc3);
              test3 = abs(cc1*cb1 + cc2*cb2 + cc3*cb3);

              if ( test1 > EPS || test2 > EPS || test3 > EPS)
               {
                 fprintf(stderr, "ERROR angle between edges of volume must be 90 degrees\n");
                 exit(EXIT_FAILURE);
                }

              da = la/(na - 1);
              db = lb/(nb - 1);
              dc = lc/(nc - 1);

              dda = la/(nna - 1);
              ddb = lb/(nnb - 1);
              ddc = lc/(nnc - 1);
          }


    starttgt = NULL;
    curtgt = NULL;
    for (k=1; k<=nc; k++)
      {
        curtgt = add_tgt(&starttgt, k);

        curtgt->settype = 3;

        curtgt->o1 = o1 + (k-1)*dc*cc1;
        curtgt->o2 = o2 + (k-1)*dc*cc2;
        curtgt->o3 = o3 + (k-1)*dc*cc3;

        curtgt->la = la;
        curtgt->lb = lb;
        curtgt->lc = lc;

        curtgt->ca1 = ca1;
        curtgt->ca2 = ca2;
        curtgt->ca3 = ca3;

        curtgt->cb1 = cb1;
        curtgt->cb2 = cb2;
        curtgt->cb3 = cb3;

        curtgt->cc1 = cc1;
        curtgt->cc2 = cc2;
        curtgt->cc3 = cc3;

        curtgt->da = da;
        curtgt->db = db;
        curtgt->dc = dc;
        curtgt->dda = dda;
        curtgt->ddb = ddb;
        curtgt->ddc = ddc;

        curtgt->na = na;
        curtgt->nb = nb;
        curtgt->nc = nc;
        curtgt->nna = nna;
        curtgt->nnb = nnb;
        curtgt->nnc = nnc;

        curtgt->nt = curtgt->na*curtgt->nb;
        curtgt->nnt = curtgt->nna*curtgt->nnb;
        }

    return starttgt;

}




/****************************************************/


SEGYptr check_file(SEGYptr root, int index)
{
   SEGYptr cur, check;

   check = NULL;

   cur = root;
   while (cur != NULL)
     {
        if (cur->seq == index)
          {
            check = cur;
            cur = NULL;
          }
        else
           cur = cur->nextsgy;
      }

   return check;
}


SEGYptr add_file(SEGYptr *root, int index)
{
   SEGYptr last,cur, new;

   if (*root == NULL)
     {
       new = malloc(sizeof(SEGY));

       new->seq = index;

       strcpy(new->filename, "\0"); /* parametro obbligatorio */

       new->nshotoff    = 9;
       new->nshotnbytes = 4;

       new->xsoff    = 73;
       new->xsnbytes = 4;
       new->ysoff    = 77;
       new->ysnbytes = 4;
       new->zsoff    = 45;
       new->zsnbytes = 4;

       new->xroff    = 81;
       new->xrnbytes = 4;
       new->yroff    = 85;
       new->yrnbytes = 4;
       new->zroff    = 41;
       new->zrnbytes = 4;

       new->traceoff    = 1;
       new->ntraccetot = -1;  /* parametro obbligatorio */

       new->ncamp     = -1;   /* parametro obbligatorio */
       new->ncampfilt = -1;   /* parametro calcolato */
       new->formato   = -1;   /* parametro obbligatorio */
       new->dt        = -1;   /* parametro obbligatorio */
       new->dx        = -1;   /* parametro obbligatorio */
       new->tshift    = 0;

       new->a11 = 1;
       new->a12 = 0;
       new->a21 = 0;
       new->a22 = 1;
       new->b1  = 0;
       new->b2  = 0;

       strcpy(new->filepesi,"\0");

       new->nextsgy = NULL;

       *root = new;

     }
   else
     {
       last = *root;
       while (last != NULL)
        {
         cur = last;
         last = cur->nextsgy;
         }

       new = malloc(sizeof(SEGY));
       cur->nextsgy = new;

       new->seq = index;

       strcpy(new->filename, "\0"); /* parametro obbligatorio */

       new->nshotoff    = 9;
       new->nshotnbytes = 4;

       new->xsoff    = 73;
       new->xsnbytes = 4;
       new->ysoff    = 77;
       new->ysnbytes = 4;
       new->zsoff    = 45;
       new->zsnbytes = 4;

       new->xroff    = 81;
       new->xrnbytes = 4;
       new->yroff    = 85;
       new->yrnbytes = 4;
       new->zroff    = 41;
       new->zrnbytes = 4;

       new->traceoff    = 1;
       new->ntraccetot = -1;  /* parametro obbligatorio */

       new->ncamp     = -1;   /* parametro obbligatorio */
       new->ncampfilt = -1;   /* parametro calcolato */
       new->formato   = -1;   /* parametro obbligatorio */
       new->dt        = -1;   /* parametro obbligatorio */
       new->dx        = -1;   /* parametro obbligatorio */
       new->tshift    = 0;

       new->a11 = 1;
       new->a12 = 0;
       new->a21 = 0;
       new->a22 = 1;
       new->b1  = 0;
       new->b2  = 0;

       strcpy(new->filepesi,"\0");

       new->nextsgy = NULL;

     }

   return new;
}




TGTptr check_tgt(TGTptr root, int index)
{
   TGTptr cur, check;

   check = NULL;

   cur = root;
   while (cur != NULL)
     {
        if (cur->seq == index)
          {
            check = cur;
            cur = NULL;
          }
        else
           cur = cur->next_tgt;
      }

   return check;
}



TGTptr add_tgt(TGTptr *root, int index)
{
   TGTptr last,cur, new;

   if (*root == NULL)
     {
       new = malloc(sizeof(TGT));
       new->next_tgt = NULL;
       new->seq = index;
       *root = new;

        new->o1=FLT_MAX;
        new->o2=FLT_MAX;
        new->o3=FLT_MAX;

        new->a1=FLT_MAX;
        new->a2=FLT_MAX;
        new->a3=FLT_MAX;

        new->b1=FLT_MAX;
        new->b2=FLT_MAX;
        new->b3=FLT_MAX;

        new->c1=FLT_MAX;
        new->c2=FLT_MAX;
        new->c3=FLT_MAX;

        new->la=FLT_MAX;
        new->lb=FLT_MAX;
        new->lc=FLT_MAX;

        new->theta=FLT_MAX;
        new->phi=FLT_MAX;
        new->psi=FLT_MAX;

     }
   else
     {
       last = *root;
       while (last != NULL)
        {
         cur = last;
         last = cur->next_tgt;
         }

        new = malloc(sizeof(TGT));
        cur->next_tgt = new;

        new->next_tgt = NULL;
        new->seq = index;

        new->o1=FLT_MAX;
        new->o2=FLT_MAX;
        new->o3=FLT_MAX;

        new->a1=FLT_MAX;
        new->a2=FLT_MAX;
        new->a3=FLT_MAX;

        new->b1=FLT_MAX;
        new->b2=FLT_MAX;
        new->b3=FLT_MAX;

        new->c1=FLT_MAX;
        new->c2=FLT_MAX;
        new->c3=FLT_MAX;

        new->la=FLT_MAX;
        new->lb=FLT_MAX;
        new->lc=FLT_MAX;

        new->theta=FLT_MAX;
        new->phi=FLT_MAX;
        new->psi=FLT_MAX;

     }

   return new;
}

