/****************************************************/
/*                   start.be.c                     */
/****************************************************/
#include "../0_include/0_common_defs.h"

/*************************************************/
/*              External Function                */
/*************************************************/
extern void execute_();
extern void reverse(char *Hfile, char *Dfile, char *outHfile, char *outDfile, char *name, int sorting, int updown, int nshot, int ntarget, int isrestart, int *sortList);

/*************************************************/
/*              Global variables                 */
/*************************************************/
FILE* glob_fp[MAX_FILE_NUM];

/********************************************************************/
/*                    MAIN                                          */
/* argv[0] = program name                                           */
/* argv[1] = directory containing eikonal.x output                  */
/* argv[2] = parameters file of dbmaker (<>.dbm)                    */
/* argv[3] = output directory                                       */
/********************************************************************/
int main(int argc, char* argv[])
{
    FILE *fp;
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    int i, j, imax;
    long len;
    char *string, *string1, *string2;
    char *st;


    char *file, *file2, *file3, *file4;
    char *mode, *mode2;

    int verbose;

    /*  Input Data Files    */
    char *HeaderFile;
    char *DataFile;
    char *InDir;
    long lenInDir;
    int attrib[8];
    char *filenames[16];
    char unix_command[150];
    int updown, ntarget, nshot;

    /*  Acquisition  */
    char  *bmFile;
    int bmInterp;

    /*  Target  */
    char  *tgFile;
    int tgInterp;

    char *dbDir;
    long LendbDir;
    int sorting;
    int isrestart;
    int *sortList;
    FILE *ptFile;
    char sortFile[100];
    
/* NICOLA: processing di un solo file */
/*====================================*/
    int dbnum;
     
    
    
  if ( (argc > 1 ) && (strcmp("--help", argv[1]) == 0) )
       {
         printf("*** DBSWAPPER v. %s ***\n",VERSION);
         printf("Command line template :\n");
         printf("dbswapper.x (eikonal output dir) (dbswapper par <dbs.p>) (output dir)\n\n");
         exit(EXIT_SUCCESS);
        }

  if (argc < 3)
       {
         fprintf(stderr,"ERROR : Missing command line parameter.\n");
         fprintf(stderr,"*** DBSWAPPER v. %s ***\n",VERSION);
         printf("dbswapper.x (eikonal output dir) (dbswapper par <dbs.p>) (output dir)\n\n");
         exit(EXIT_FAILURE);
       }

  printf("\n*** DBSWAPPER v. %s ***\n\n",VERSION);

  /* Allocazione variabili di servizio */
  file  = calloc(MAX_STRING_LEN, sizeof(char));
  file2 = calloc(MAX_STRING_LEN, sizeof(char));
  file3 = calloc(MAX_STRING_LEN, sizeof(char));
  file4 = calloc(MAX_STRING_LEN, sizeof(char));

  mode  = calloc(MAX_STRING_LEN, sizeof(char));
  mode2 = calloc(MAX_STRING_LEN, sizeof(char));

  string  = calloc(MAX_STRING_LEN, sizeof(char));
  string1 = calloc(MAX_STRING_LEN, sizeof(char));
  string2 = calloc(MAX_STRING_LEN, sizeof(char));
  st = calloc(MAX_STRING_LEN, sizeof(char));

  for (i=0; i< MAX_PARAM; i++)
     {
       ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
       val[i]   = calloc(MAX_STRING_LEN, sizeof(char));
     }


  for (i=0; i<16; i++)
     {
       filenames[i] = calloc(MAX_STRING_LEN, sizeof(char));
     }

   strcpy(filenames[0], "DBa.H");
   strcpy(filenames[1], "DBa.H@");
   strcpy(filenames[2], "DBt.H");
   strcpy(filenames[3], "DBt.H@");
   strcpy(filenames[4], "DBux.H");
   strcpy(filenames[5], "DBux.H@");
   strcpy(filenames[6], "DBuy.H");
   strcpy(filenames[7], "DBuy.H@");
   strcpy(filenames[8], "DBuz.H");
   strcpy(filenames[9], "DBuz.H@");
   strcpy(filenames[10], "DBvx.H");
   strcpy(filenames[11], "DBvx.H@");
   strcpy(filenames[12], "DBvy.H");
   strcpy(filenames[13], "DBvy.H@");
   strcpy(filenames[14], "DBvz.H");
   strcpy(filenames[15], "DBvz.H@");


  /* --------- start processing input directory ----------- */
   InDir = calloc(MAX_STRING_LEN, sizeof(char));
   strcpy(InDir, argv[1]);
   lenInDir = strlen(InDir);
   if (InDir[lenInDir-1] != '/')
     {
       if (lenInDir < (MAX_STRING_LEN-2))
         {
           InDir[lenInDir]='/';
           InDir[lenInDir+1]='\0';
          }
       else
         {
            fprintf(stderr, "ERROR: Input directory name too long : %s\n", InDir);
            exit(EXIT_FAILURE);
          }
     }
   lenInDir = strlen(InDir);
  /* --------- end processing input directory ----------- */



  /* --------- start processing output directory ----------- */
   dbDir = calloc(MAX_STRING_LEN, sizeof(char));
   strcpy(dbDir, argv[3]);
   LendbDir = strlen(dbDir);
   if (dbDir[LendbDir-1] != '/')
     {
       if (LendbDir < (MAX_STRING_LEN-2))
         {
           dbDir[LendbDir]='/';
           dbDir[LendbDir+1]='\0';
          }
       else
         {
            fprintf(stderr, "ERROR: Output directory name too long : %s\n", dbDir);
            exit(EXIT_FAILURE);
          }
     }
    LendbDir = strlen(dbDir);
  /* --------- end processing output directory ----------- */




  /* --------- start processing parameters file ----------- */
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
   bmFile = calloc(MAX_STRING_LEN, sizeof(char));
   tgFile = calloc(MAX_STRING_LEN, sizeof(char));

   strcpy(bmFile, "\0");
   strcpy(tgFile, "\0");
   isrestart=0;
   sorting = 1;
   bmInterp = 0;
   tgInterp = 0;
   for (i=0; i<8; i++)
       attrib[i]=1;
   verbose = 1;

   j=0;
   for (i=0; i<imax; i++)
     {

       if (strcmp("ACQUISITION", ident[i]) == 0 ) /* 01 */
         {
           sscanf(val[i], "%s", bmFile);
           j++;
         }
       else if (strcmp("TARGET", ident[i]) == 0 ) /* 02 */
         {
           sscanf(val[i], "%s", tgFile);
           j++;
         }
       else if (strcmp("SORTING", ident[i]) == 0 ) /* 03 */
         {
           sscanf(val[i], "%d", &sorting);
           j++;
         }
       else if (strcmp("ACQ-INTERP", ident[i]) == 0 ) /* 03 */
         {
           sscanf(val[i], "%d", &bmInterp);
           j++;
         }
       else if (strcmp("TGT-INTERP", ident[i]) == 0 ) /* 03 */
         {
           sscanf(val[i], "%d", &tgInterp);
           j++;
         }
       else if (strcmp("AMPLITUDES", ident[i]) == 0 ) /* 04 */
         {
           sscanf(val[i], "%d", &(attrib[0]));
           j++;
         }
       else if (strcmp("TRAVELTIMES", ident[i]) == 0 ) /* 05 */
         {
           sscanf(val[i], "%d", &(attrib[1]));
           j++;
         }
       else if (strcmp("UX", ident[i]) == 0 ) /* 06 */
         {
           sscanf(val[i], "%d", &(attrib[2]));
           j++;
         }
       else if (strcmp("UY", ident[i]) == 0 ) /* 07 */
         {
           sscanf(val[i], "%d", &(attrib[3]));
           j++;
         }
       else if (strcmp("UZ", ident[i]) == 0 ) /* 08 */
         {
           sscanf(val[i], "%d", &(attrib[4]));
           j++;
         }
       else if (strcmp("VX", ident[i]) == 0 ) /* 09 */
         {
           sscanf(val[i], "%d", &(attrib[5]));
           j++;
         }
       else if (strcmp("VY", ident[i]) == 0 ) /* 10 */
         {
           sscanf(val[i], "%d", &(attrib[6]));
           j++;
         }
       else if (strcmp("VZ", ident[i]) == 0 ) /* 11 */
         {
           sscanf(val[i], "%d", &(attrib[7]));
           j++;
         }
       else if (strcmp("VERBOSE", ident[i]) == 0 ) /* 11 */
         {
           sscanf(val[i], "%d", &verbose);
           j++;
         }
/* NICOLA: processing di un solo file */
/*====================================*/
       else if ( strcmp("DBNUM", ident[i]) == 0 ) /* 11 */
         {
           sscanf(val[i], "%d", &dbnum);
           j++;
         }     
       else if ( strcmp("ISRESTART", ident[i]) == 0 ) /* 11 */
         {
           sscanf(val[i], "%d", &isrestart);
           j++;
         }        
     }


     /* check dei parametri  */
     if ( strlen(bmFile) == 0 ) {
          fprintf(stderr, "ERROR: missing parameter ACQUISITION in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     if ( strlen(tgFile) == 0 ) {
          fprintf(stderr, "ERROR: missing parameter TARGET in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     if ( (sorting != 1) && (sorting != 2) ) {
          fprintf(stderr, "ERROR: parameter value SORTING in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     if ( (verbose < 0) && (verbose >2) ) {
          fprintf(stderr, "ERROR: parameter value VERBOSE in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }
     
     if ( (isrestart < 0) && (isrestart >2) ) {
          fprintf(stderr, "ERROR: parameter value ISRESTART in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     for (i=0; i<8; i++) {
       if ( (attrib[i] != 0) && (attrib[i] != 1) ) {
          fprintf(stderr, "ERROR: attribute value in file %s\n", argv[2]);
          exit(EXIT_FAILURE); } }

  /* --------- end processing parameters file ----------- */


  if (verbose >0)
    {
      printf("---- Paramentri del programma ----\n");
      if (isrestart == 1)
      printf("RESTART \n");
      printf("File descrizione superficie acquisizione      : %s\n", bmFile);
      printf("File descrizione target                       : %s\n", tgFile);
      
      if (sorting == 1)
      printf("Ordinamento dati                              : [acq , tgt]\n");
      if (sorting == 2)
      printf("Ordinamento dati                              : [tgt , acq]\n");
      printf("Attributi di ogni raggio : \n");
      if (attrib[0] == 1)
         printf("- ampiezze -");
      if (attrib[1] == 1)
         printf("- travel times -");
      if (attrib[2] == 1)
         printf("- ux -");
      if (attrib[3] == 1)
         printf("- uy -");
      if (attrib[4] == 1)
         printf("- uz -");
      if (attrib[5] == 1)
         printf("- vx -");
      if (attrib[6] == 1)
         printf("- vy -");
      if (attrib[7] == 1)
         printf("- vz -");
      printf("\n\n");
      
/* NICOLA: processing di un solo file */
/*====================================*/
      printf("DBNUM = %i => %s \n\n", dbnum, filenames[ (dbnum-1)*2]);      
      
    }

  /* ----------------- reversing files ------------------ */
   HeaderFile = calloc(MAX_STRING_LEN, sizeof(char));
   DataFile = calloc(MAX_STRING_LEN, sizeof(char));

/* NICOLA: processing di un solo file */
/*====================================*/
/*   for (i=0; i<8; i++) */
    for (i=dbnum-1; i<dbnum; i++)
    {
      if (attrib[i] == 1)
       {
          strcpy(HeaderFile, filenames[i*2]);

          strcpy(DataFile, filenames[i*2+1]);

          strcpy(file, InDir);
          strcat(file, HeaderFile);
          len = strlen(file);

          strcpy(file2, InDir);
          strcat(file2, DataFile);

          strcpy(file3, dbDir);
          strcat(file3, HeaderFile);

          strcpy(file4, dbDir);
          strcat(file4, DataFile);

          strcpy(string, "UPDOWN");
          seph_get_int_(file, string, &updown, len, 6);
          strcpy(string, "NSHOT");
          seph_get_int_(file, string, &nshot, len, 5);
          strcpy(string, "NTARGET");
          seph_get_int_(file, string, &ntarget, len, 7);

          sortList = calloc( nshot, sizeof(int));
          strcpy( sortFile, InDir);
          strcat( sortFile, "sortGreenF.sht");
          ptFile = fopen( sortFile, "r");
          if ( ptFile == NULL)
          {
          	printf("Cannot open %s \n", sortFile);
          	exit(0);
          }
          fread( sortList, sizeof(int), nshot, ptFile);
          fclose( ptFile);
          
          if (verbose > 1)
            {
              if (sorting != updown)
                 printf("Copying and reversing file %s ...\n",file);
              else
                 printf("Copying file %s ...\n",file);
            }
          printf("sorting updown nshot ntarget %d %d %d %d...\n",sorting,updown,nshot,ntarget);
          reverse(file, file2, file3, file4, DataFile, sorting, updown, nshot, ntarget, isrestart, sortList); 
          
          /* Copia i file contenenti i range dei database */
          strcpy( unix_command, "cp ");
          strcat( unix_command, InDir);
          if ( dbnum == 1)
          {
             strcat( unix_command, "rangeA.sht ");
          }
          else if ( dbnum == 2)
          {
             strcat( unix_command, "rangeT.sht ");
          }
          else if ( dbnum == 5)
          {
             strcat( unix_command, "rangeCz.sht ");
          }   
          strcat( unix_command, dbDir);
          system( unix_command);


        }
     }
   printf("\n");
  /* --------------- end reverting files ---------------- */


  /* ---------- freee memory ----------- */

  free(file);
  free(file2);
  free(file3);
  free(file4);
  free(mode);
  free(mode2);
  free(string);
  free(string1);
  free(string2);
  free(st);

  for (i=0; i<MAX_PARAM-1; i++)
     {
       free(ident[i]);
       free(val[i]);
     }

  free(InDir);
  free(dbDir);
  free(bmFile);
  free(tgFile);


  free(HeaderFile);
  free(DataFile);

  for (i=0; i<16; i++)
     {
       free(filenames[i]);
     }

  if (verbose > 0)
     printf("\nThat's all folks!\n");

  exit(EXIT_SUCCESS);

}

