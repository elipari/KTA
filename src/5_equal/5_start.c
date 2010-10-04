/****************************************************/
/*                   start.be.c                     */
/****************************************************/
#include "../0_include/0_common_defs.h"


/*************************************************/
/*              External Function                */
/*************************************************/
extern void execute_();


/*************************************************/
/*              Global variables                 */
/*************************************************/
FILE* glob_fp[MAX_FILE_NUM];


/*************************************************/
/*                    MAIN                       */
/* argv[0] = program name                        */
/* argv[1] = traces parameters                   */
/* argv[2] = equalization parameters             */
/* argv[3] = output directory                    */
/*************************************************/
int main(int argc, char* argv[])
{

    FILE *fp;
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];

    int i, j, imax, lstr1, lstr2;
    int fdesc, fout;
    char *mode1;
    char *mode2;
    int file_error;

/*-------GRIGLIA DI EQUALIZZAZIONE---*/
    float eq_dx;
    float eq_dy;

    int verbose;

/*------------TRACCE--------------*/
    int max_ncamp;
    int max_nshot, max_ntracceshot;

    SEGYptr startsgy, cursgy, freesgy;
    int nf,nshot;
    int ntracceshot, ncamp;
    float a11, a12, a21, a22;
    float b1,b2;


/* ------- Ricevitori -------- */
    float* xr;
    float* yr;
    float* weight;


  for (i=0; i< MAX_PARAM; i++)
     {
       ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
       val[i] = calloc(MAX_STRING_LEN, sizeof(char));
     }


  if (argc < 2)
       {
         fprintf(stderr,"ERROR : Missing command line parameter.\n");
         printf("*** EQUAL v. %s ***\n",VERSION);
         fprintf(stderr,"equal_sol.x (traces <.trs>) (equalization <.eql>)\n\n");
         exit(EXIT_FAILURE);
         }

  if (strcmp("--help", argv[1]) == 0 )
       {
         printf("*** EQUAL v. %s ***\n",VERSION);
         printf("Command line template :\n");
         printf("equal_sol.x (traces <.trs>) (equalization <.eql>)\n\n");
         exit(EXIT_SUCCESS);
        }




  /*************************** LETTURA FILE PARAMETRI  ************************/

  /* ------------------- start processing traces parameters ----------------- */

    startsgy =TracesParams(argv[1]);

    max_nshot = 0;
    max_ntracceshot = 0;
    max_ncamp = 0;
    cursgy = startsgy;
    while (cursgy != NULL)
      {

        if (cursgy->nshot > max_nshot)
          max_nshot = cursgy->ntracceshot;

        if (cursgy->ntracceshot > max_ntracceshot)
          max_ntracceshot = cursgy->ntracceshot;

        if (cursgy->ncamp > max_ncamp)
          max_ncamp = cursgy->ncamp;

        cursgy = cursgy->nextsgy;
      }

  /* ------------------- end processing traces parameters --------------*/


  /* ----------------- start processing equalization parameters ----------------- */
   fp=fopen(argv[2], "r");
   if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", argv[2]);
          exit(EXIT_FAILURE);
        }

   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );

   fclose(fp);


   /* assegnazione delle variabili */
   eq_dx = FLT_MAX;
   eq_dy = FLT_MAX;

   j=0;
   for (i=0; i<imax; i++)
     {

       if (strcmp("EQ_D1", ident[i]) == 0 ) /* 1 */
         {
           sscanf(val[i], "%f", &eq_dx);
           j++;
         }
       else if (strcmp("EQ_D2", ident[i]) == 0 ) /* 2 */
         {
           sscanf(val[i], "%f", &eq_dy);
           j++;
         }
       else if (strcmp("VERBOSE", ident[i]) == 0 ) /* 2 */
         {
           sscanf(val[i], "%d", &verbose);
           j++;
         }
     }


   /* check dei parametri */
   if ( eq_dx == FLT_MAX ) {
       fprintf(stderr, "ERROR: missing parameter EQ_D1 in file %s\n", argv[2]);
       exit(EXIT_FAILURE); }
   if ( eq_dx <= 0 ) {
       fprintf(stderr, "ERROR: parameter value EQ_D1 in file %s\n", argv[2]);
       exit(EXIT_FAILURE); }

   if ( eq_dy == FLT_MAX ) {
       fprintf(stderr, "ERROR: missing parameter EQ_D2 in file %s\n", argv[2]);
       exit(EXIT_FAILURE); }
   if ( eq_dy <= 0 ) {
       fprintf(stderr, "ERROR: parameter value EQ_D2 in file %s\n", argv[2]);
       exit(EXIT_FAILURE); }

   /* ------------------- end processing equalization parameters --------------*/




  /*------------------------------ALLOCAZIONE DINAMICA DELLA MEMORIA--------------------*/

    xr=calloc(max_ntracceshot, sizeof(float));
    if (xr == NULL)
      {
        fprintf(stderr, "ERROR: cannot allocate memory.\n");
        exit(EXIT_FAILURE);
      }

    yr=calloc(max_ntracceshot, sizeof(float));
    if (yr == NULL)
      {
        fprintf(stderr, "ERROR: cannot allocate memory.\n");
        exit(EXIT_FAILURE);
      }

    weight=calloc(max_ntracceshot, sizeof(float));
    if (weight == NULL)
      {
        fprintf(stderr, "ERROR: cannot allocate memory.\n");
        exit(EXIT_FAILURE);
      }

    printf("Calling main program ....\n\n");


    mode1 = calloc(5, sizeof(char));
    mode2 = calloc(5, sizeof(char));

    if (verbose >0)
    printf("\n*** EQUAL v. %s ***\n",VERSION);

    cursgy = startsgy;
    while (cursgy != NULL)
      {

        if (verbose > 0) {
         printf("\n FILE SEG-Y N: %d\n",cursgy->seq);
         printf("%d - filename    : %s\n",cursgy->seq,cursgy->filename);
         printf("--- Binary Trace Header ---\n");
         printf("%d - offset-numshot: %d\n",cursgy->seq,cursgy->nshotoff);
         printf("%d - nbytes-numshot: %d\n",cursgy->seq,cursgy->nshotnbytes);
         printf("%d - offset-xsource: %d\n",cursgy->seq,cursgy->xsoff);
         printf("%d - nbytes-xsource: %d\n",cursgy->seq,cursgy->xsnbytes);
         printf("%d - offset-ysource: %d\n",cursgy->seq,cursgy->ysoff);
         printf("%d - nbytes-ysource: %d\n",cursgy->seq,cursgy->ysnbytes);
         printf("%d - offset-zsource: %d\n",cursgy->seq,cursgy->zsoff);
         printf("%d - nbytes-zsource: %d\n",cursgy->seq,cursgy->zsnbytes);
         printf("%d - offset-xreceiver: %d\n",cursgy->seq,cursgy->xroff);
         printf("%d - nbytes-xreceiver: %d\n",cursgy->seq,cursgy->xrnbytes);
         printf("%d - offset-yreceiver: %d\n",cursgy->seq,cursgy->yroff);
         printf("%d - nbytes-yreceiver: %d\n",cursgy->seq,cursgy->yrnbytes);
         printf("%d - offset-zreceiver: %d\n",cursgy->seq,cursgy->zroff);
         printf("%d - nbytes-zreceiver: %d\n",cursgy->seq,cursgy->zrnbytes);
         printf("--- Numero tracce ---\n");
         printf("%d - trace-offset: %d\n",cursgy->seq,cursgy->traceoff);
         printf("%d - nshot       : %d\n",cursgy->seq,cursgy->nshot);
         printf("%d - ntracceshot : %d\n",cursgy->seq,cursgy->ntracceshot);
         printf("--- Formato tracce ---\n");
         printf("%d - ncampioni   : %d\n",cursgy->seq,cursgy->ncamp);
         printf("%d - ncampfilt   : %d\n",cursgy->seq,cursgy->ncampfilt);
         printf("%d - formato     : %d\n",cursgy->seq,cursgy->formato);
         printf("%d - dt          : %f\n",cursgy->seq,cursgy->dt);
         printf("%d - dx          : %f\n",cursgy->seq,cursgy->dx);
         printf("%d - tshift      : %f\n",cursgy->seq,cursgy->tshift);
         printf("--- Cambiamento riferimento ---\n");
         printf("%d - a11 a12     : %f %f\n",cursgy->seq,cursgy->a11,cursgy->a12);
         printf("%d - a21 a22     : %f %f\n",cursgy->seq,cursgy->a21,cursgy->a22);
         printf("%d - b1  b2      : %f %f\n",cursgy->seq,cursgy->b1,cursgy->b2);
         printf("\n");
        }


         if (strlen(cursgy->filepesi) != 0)
           {

             fdesc = 7;
             strcpy(mode1,"rb");
             lstr1 = strlen(cursgy->filename);
             glob_fopen_(&fdesc, cursgy->filename, mode1, &file_error, lstr1,2);

             fout = 8;
             strcpy(mode2,"wb");
             lstr2 = strlen(cursgy->filepesi);
             glob_fopen_(&fout,cursgy->filepesi, mode2, &file_error, lstr2,2);

             nf = cursgy->seq;
             nshot = cursgy->nshot;
             ntracceshot = cursgy->ntracceshot;
             ncamp = cursgy->ncamp;
             a11 = cursgy->a11;
             a12 = cursgy->a12;
             a21 = cursgy->a21;
             a22 = cursgy->a22;
             b1 = cursgy->b1;
             b2 = cursgy->b2;

             execute_(&fdesc, &nf, &nshot, &ntracceshot,&max_ntracceshot,&ncamp, &a11,
                      &a12, &a21, &a22, &b1, &b2, &eq_dx, &eq_dy,
                      xr, yr, weight, &fout);

             glob_fclose_(&fdesc, &file_error);
             glob_fclose_(&fout, &file_error);

            }

         if (verbose >0) {
         printf("Generato file pesi :\n");
         printf("%d - filepesi    : %s\n",cursgy->seq,cursgy->filepesi);
         }

         cursgy = cursgy->nextsgy;

  }


/* Disallocazione della memoria     */
   cursgy = startsgy;
   while (cursgy != NULL)
      {
         freesgy = cursgy;
         cursgy = freesgy->nextsgy;
         free(freesgy);
      }

   for (i=0; i< MAX_PARAM; i++)
      {
       free(ident[i]);
       free(val[i]);
      }

   free(mode1);
   free(mode2);

   free(xr);
   free(yr);
   free(weight);

   if (verbose >0)
   printf("That's all folks!\n");

   exit(EXIT_SUCCESS);
}


