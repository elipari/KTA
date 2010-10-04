/****************************************************/
/*                   start.be.c                     */
/****************************************************/
#include "../0_include/0_common_defs.h"

/*************************************************/
/*              External Function                */
/*************************************************/
extern void execute_();
extern void reverse(char *Hfile, char *Dfile,
              int updown, int nshot, int ntarget);


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
    FILE *fp, *fp2;
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    int i, j, k, imax;
    long len, len1;
    char *string, *string1, *string2;
    char *st;
    char c, test;

    char *file, *file2;
    int  fdesc, fdesc2;
    char *mode, *mode2;
#ifdef TEST_DEBUG
    int  dim;
    int  num;
#else
    int  dim, dim2;
    int  num, num2;
#endif
    long offset, offset2;
    int  origin, origin2;
    int  file_error;

    int verbose;

    /*  Input Data Files    */
    char *HeaderFile;
    char *DataFile;
    char *InDir;
    long lenInDir;
    int attrib[8];
    char *filenames[16];
    int updown, ntarget, nshot;

    /*  Acquisition  */
    char  *bmFile;
    int bmInterp;

    /*  Target  */
    char  *tgFile;
    int tgInterp;

    /*  DataBase              */
    /* ------------ Key1 ------------- */
    char  *key1_file;
    int   key1_type, key1_interp;
    TGTptr start_key1, cur_key1, free_key1;

    int   key1_npan,    key1_mpan;
    int   key1_nt_tot,  key1_nnt_tot;
    int   *key1_pan1,   *key1_pan2,   key1_pan3;
    int   key1_na_max,  key1_nb_max,  key1_nc_max;
    int   key1_nna_max, key1_nnb_max, key1_nnc_max;
    int   key1_rra,     key1_rrb,     key1_rrc;
    int   key1_rra_max, key1_rrb_max, key1_rrc_max;

    /* cambiamento sistema riferimento */
    float *key1_o1,  *key1_o2,  *key1_o3;
    float *key1_ca1, *key1_ca2, *key1_ca3;
    float *key1_cb1, *key1_cb2, *key1_cb3;
    float *key1_cc1, *key1_cc2, *key1_cc3;

    /* bassa risoluzione */
    float *key1_da, *key1_db, key1_dc;
    int   *key1_na, *key1_nb, key1_nc;
    int   *key1_nt;

    /* alta risoluzione */
    float *key1_dda, *key1_ddb, key1_ddc;
    int   *key1_nna, *key1_nnb, key1_nnc;
    int   *key1_nnt;

    /* dimensione degli indici delle matrici dei pesi*/
    int   key1_w1, key1_w2, key1_w3, key1_w4;

    /* matrici pesi per interpolazione */
    float *key1_weights;


    /* ------------ Key2 ------------- */
    char  *key2_file;
    int   key2_type, key2_interp;
#ifdef TEST_DEBUG
    TGTptr start_key2, cur_key2;
#else
    TGTptr start_key2, cur_key2, free_key2;
#endif

    int   key2_npan,    key2_mpan;
    int   key2_nt_tot,  key2_nnt_tot;
    int   *key2_pan1,   *key2_pan2,   key2_pan3;
    int   key2_na_max,  key2_nb_max,  key2_nc_max;
    int   key2_nna_max, key2_nnb_max, key2_nnc_max;
    int   key2_rra,     key2_rrb,     key2_rrc;
    int   key2_rra_max, key2_rrb_max, key2_rrc_max;

    /* cambiamento sistema riferimento */
    float *key2_o1,  *key2_o2,  *key2_o3;
    float *key2_ca1, *key2_ca2, *key2_ca3;
    float *key2_cb1, *key2_cb2, *key2_cb3;
    float *key2_cc1, *key2_cc2, *key2_cc3;

    /* bassa risoluzione */
    float *key2_da, *key2_db, key2_dc;
    int   *key2_na, *key2_nb, key2_nc;
    int   *key2_nt;

    /* alta risoluzione */
    float *key2_dda, *key2_ddb, key2_ddc;
    int   *key2_nna, *key2_nnb, key2_nnc;
    int   *key2_nnt;

    /* dimensione degli indici delle matrici dei pesi*/
    int   key2_w1, key2_w2, key2_w3, key2_w4;

    /* matrici pesi per interpolazione */
    float *key2_weights;

    float *key2_mtx_in, *key2_mtx_out;

    float t,u,v;

    char *dbDir;
    long LendbDir;
    int sorting;
#ifdef TEST_DEBUG
#else
    int nshot_tot;
#endif

  if (argc < 3)
       {
         fprintf(stderr,"ERROR : Missing command line parameter.\n");
         fprintf(stderr,"*** DBMAKER v. %s ***\n",VERSION);
         fprintf(stderr,"dbmaker.x (eikonal output dir) (dbmaker par <dbm>) (output dir)\n\n");
         exit(EXIT_FAILURE);
         }

  if (strcmp("--help", argv[1]) == 0 )
       {
         printf("*** DBMAKER v. %s ***\n",VERSION);
         printf("Command line template :\n");
         printf("dbmaker.x (eikonal output dir) (dbmaker par <dbm>) (output dir)\n\n");
         exit(EXIT_SUCCESS);
        }

  /* Allocazione variabili di servizio */
  file = calloc(MAX_STRING_LEN, sizeof(char));
  file2 = calloc(MAX_STRING_LEN, sizeof(char));

  mode = calloc(MAX_STRING_LEN, sizeof(char));
  mode2 = calloc(MAX_STRING_LEN, sizeof(char));

  string = calloc(MAX_STRING_LEN, sizeof(char));
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

     if ( (bmInterp != 0) && (bmInterp != 1) ) {
          fprintf(stderr, "ERROR: parameter value ACQ-INTERP in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     if ( (tgInterp != 0) && (tgInterp != 1) ) {
          fprintf(stderr, "ERROR: parameter value TGT-INTERP in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     if ( (verbose < 0) && (verbose >2) ) {
          fprintf(stderr, "ERROR: parameter value VERBOSE in file %s\n", argv[2]);
          exit(EXIT_FAILURE); }

     for (i=0; i<8; i++) {
       if ( (attrib[i] != 0) && (attrib[i] != 1) ) {
          fprintf(stderr, "ERROR: attribute value in file %s\n", argv[2]);
          exit(EXIT_FAILURE); } }

  /* --------- end processing parameters file ----------- */


  if (verbose >0)
    {
      printf("\n*** DBMAKER v. %s ***\n\n",VERSION);
      printf("---- Paramentri del programma ----\n");
      printf("File descrizione superficie acquisizione      : %s\n", bmFile);
      printf("File descrizione target                       : %s\n", tgFile);
      if (sorting == 1)
      printf("Ordinamento dati                              : [acq , tgt]\n");
      if (sorting == 2)
      printf("Ordinamento dati                              : [tgt , acq]\n");
      printf("Interpolazione sup. acquisizione (0=no, 1=si) : %d\n", bmInterp);
      printf("Interpolazione target (0=no, 1=si)            : %d\n", tgInterp);
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
    }

  /* ----------------- reversing files ------------------ */
   HeaderFile = calloc(MAX_STRING_LEN, sizeof(char));
   DataFile = calloc(MAX_STRING_LEN, sizeof(char));


   for (i=0; i<8; i++)
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

          strcpy(string, "UPDOWN");
          seph_get_int_(file, string, &updown, len, 6);
          strcpy(string, "NSHOT");
          seph_get_int_(file, string, &nshot, len, 5);
          strcpy(string, "NTARGET");
          seph_get_int_(file, string, &ntarget, len, 7);

          if (updown != sorting)
            {
               if (verbose > 1)
                  printf("Reversing file %s ...\n",file);
               reverse(file, file2, sorting, nshot, ntarget);
            }
        }
     }
   printf("\n");
  /* --------------- end reverting files ---------------- */



  /* ------ start interpolation  ------- */
   key1_file = calloc(MAX_STRING_LEN, sizeof(char));
   key2_file = calloc(MAX_STRING_LEN, sizeof(char));

   if (sorting == 1)
     {
       start_key1 = TargetParams(bmFile, &key1_type);
       key1_interp = bmInterp;
       strcpy(key1_file, bmFile);

       start_key2 = TargetParams(tgFile, &key2_type);
       key2_interp = tgInterp;
       strcpy(key2_file, tgFile);
      }
   else if (sorting == 2)
     {
       start_key1 = TargetParams(tgFile, &key1_type);
       key1_interp = tgInterp;
       strcpy(key1_file, tgFile);

       start_key2 = TargetParams(bmFile, &key2_type);
       key2_interp = bmInterp;
       strcpy(key2_file, bmFile);
      }


     /* -------------------------------------- */
     /* ---------- Key1 parameters ----------- */
     /* -------------------------------------- */
     key1_npan=0;
     key1_mpan=0;
     key1_nt_tot=0;
     key1_nnt_tot=0;
     key1_na_max=0;
     key1_nb_max=0;
     key1_nc_max=0;
     key1_nna_max=0;
     key1_nnb_max=0;
     key1_nnc_max=0;
     key1_rra_max=0;
     key1_rrb_max=0;
     key1_rrc_max=0;

     key1_dc = 0;
     key1_nc = 0;
     key1_ddc = 0;
     key1_nnc = 0;

     if (verbose > 0)
       {
         printf("\nKEY1 : ");
         printf("type : %d - ", key1_type);
         if (key1_interp == 0)
            printf("interpolazione : no\n ");
         else
            printf("interpolazione : si\n ");
       }

     if (key1_type == 1)
       {
         if (verbose > 0)
         printf("\n----- Set of points -----\n");
         fp = fopen(key1_file, "r");
         if (fp ==  NULL)
          {
             fprintf(stderr, "ERROR: unable to open set file ->  %s \n", key1_file);
             exit(EXIT_FAILURE);
           }
         rewind(fp);
         j=0;
         while (feof(fp) == 0)
          {
            test = fgetc(fp);
             if ((test != REMARK) && (test != '\n') && (test != EOF))
              {
                ungetc(test, fp);
                st[0]='\0';
                fscanf(fp," %[$-z ] \n", st);
                for(i=0; i<strlen(st); i++)
                   {
                      st[i] = toupper(st[i]);
                    }
                if ( (strstr(st, "SETTYPE") == NULL) && (strlen(st) > 0)  )
                   {
                      j++;
                    }
              }
             else if (test == REMARK)
              {
                fscanf(fp,"%*[^\n]\n");
              }
           }
         fclose(fp);

         key1_npan=j;
         key1_mpan=j;
         key1_pan3=j;
         key1_nt_tot=j;
         key1_nnt_tot=j;

         key1_w1 = 1;
         key1_w2 = 1;
         key1_w3 = 1;
         key1_w4 = 1;

         if (verbose > 0)
         printf("Total number of points : %d\n",key1_npan);

       }

     else

       {
         cur_key1 = start_key1;
         while (cur_key1 != NULL)
          {

           if ( (key1_type == 3) && (cur_key1->seq == 1) && (verbose >0) )
            {

              printf("\n----- Volume -----\n");
              printf("o1 = %f - o2 = %f - o3 = %f\n",   cur_key1->o1, cur_key1->o2, cur_key1->o3);
              printf("ca1=%f - ca2=%f - ca3=%f\n",acos(cur_key1->ca1)*180/PI,acos(cur_key1->ca2)*180/PI,
                                                        acos(cur_key1->ca3)*180/PI);
              printf("cb1=%f - cb2=%f - cb3=%f\n",acos(cur_key1->cb1)*180/PI,acos(cur_key1->cb2)*180/PI,
                                                        acos(cur_key1->cb3)*180/PI);
              printf("cc1=%f - cc2=%f - cc3=%f\n",acos(cur_key1->cc1)*180/PI,acos(cur_key1->cc2)*180/PI,
                                                        acos(cur_key1->cc3)*180/PI);
              printf("la = %f - lb = %f - lc = %f\n",   cur_key1->la,  cur_key1->lb,  cur_key1->lc);
              printf("da = %f - db = %f - dc = %f\n",   cur_key1->da,  cur_key1->db,  cur_key1->dc );
              printf("na = %d - nb = %d - nc = %d\n",   cur_key1->na,  cur_key1->nb,  cur_key1->nc);
              printf("dda = %f - ddb = %f - ddc = %f\n",cur_key1->dda, cur_key1->ddb, cur_key1->ddc );
              printf("nna = %d - nnb = %d - nnc = %d\n",cur_key1->nna, cur_key1->nnb, cur_key1->nnc);

             }
           if ( (key1_type == 2) && (verbose>0) )
            {
              printf("\n----- Panel N. %d -----\n",     cur_key1->seq);
              printf("o1 = %f - o2 = %f - o3 = %f\n",   cur_key1->o1, cur_key1->o2, cur_key1->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(cur_key1->ca1)*180/PI, acos(cur_key1->ca2)*180/PI,
                                                        acos(cur_key1->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(cur_key1->cb1)*180/PI, acos(cur_key1->cb2)*180/PI,
                                                        acos(cur_key1->cb3)*180/PI);
              printf("la = %f - lb = %f\n",             cur_key1->la, cur_key1->lb);
              printf("da = %f - db = %f\n",             cur_key1->da, cur_key1->db);
              printf("na = %d - nb = %d\n",             cur_key1->na, cur_key1->nb);
              printf("dda = %f - ddb = %f\n",           cur_key1->dda, cur_key1->ddb);
              printf("nna = %d - nnb = %d\n",           cur_key1->nna, cur_key1->nnb);
             }


           key1_npan = key1_npan +1;
           key1_nt_tot  = key1_nt_tot  + cur_key1->nt;


           if (cur_key1->na>= key1_na_max)
             key1_na_max=cur_key1->na;
           if (cur_key1->nb>= key1_nb_max)
             key1_nb_max=cur_key1->nb;

           if (cur_key1->nna>= key1_nna_max)
             key1_nna_max=cur_key1->nna;
           if (cur_key1->nnb>= key1_nnb_max)
             key1_nnb_max=cur_key1->nnb;

           if ( (cur_key1->da/cur_key1->dda) >= key1_rra_max)
             key1_rra_max=(int)(cur_key1->da/cur_key1->dda);
           if ( (cur_key1->db/cur_key1->ddb) >= key1_rrb_max)
             key1_rrb_max=(int)(cur_key1->db/cur_key1->ddb);


           if (key1_type == 2)
             {
                key1_dc = 0;
                key1_nc = 0;
                key1_ddc = 0;
                key1_nnc = 0;

                key1_w1 = (key1_rra_max+1);
                key1_w2 = (key1_rrb_max+1);
                key1_w3 = (key1_npan);
                key1_w4 = 4;

                key1_nnt_tot = key1_nnt_tot + cur_key1->nna * cur_key1->nnb;

                if (key1_interp == 1)
                  {
                    key1_mpan = key1_nnt_tot;
                    key1_pan3 = key1_npan;
                   }
                else
                  {
                    key1_mpan = key1_nt_tot;
                    key1_pan3 = key1_npan;
                   }

             }

           if (key1_type == 3)
             {
               if (cur_key1->nc>= key1_nc_max)
                   key1_nc_max=cur_key1->nc;

               if (cur_key1->nnc>= key1_nnc_max)
                   key1_nnc_max=cur_key1->nnc;

               if ( (cur_key1->dc/cur_key1->ddc) >= key1_rrc_max)
                   key1_rrc_max=(int)(cur_key1->dc/cur_key1->ddc);

                key1_dc = cur_key1->dc;
                key1_nc = cur_key1->nc;
                key1_ddc = cur_key1->ddc;
                key1_nnc = cur_key1->nnc;

                key1_w1 = (key1_rra_max+1);
                key1_w2 = (key1_rrb_max+1);
                key1_w3 = (key1_rrc_max+1);
                key1_w4 = 8;

                key1_nnt_tot = cur_key1->nna * cur_key1->nnb * cur_key1->nnc;

                if (key1_interp == 1)
                  {
                    key1_mpan = key1_nnt_tot;
                    key1_pan3 = key1_nnc;
                   }
                else
                  {
                    key1_mpan = key1_nt_tot;
                    key1_pan3 = key1_nc;
                   }



             }

           cur_key1=cur_key1->next_tgt;
          }
       }
   /* --------- end processing key1 parameters -----------*/



  /*--------- key1 -  memory allocation ----------- */
    key1_o1=calloc(key1_npan, sizeof(float));
      if(key1_o1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_o2=calloc(key1_npan, sizeof(float));
      if(key1_o2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_o3=calloc(key1_npan, sizeof(float));
      if(key1_o3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }


    key1_pan1=calloc(key1_pan3, sizeof(int));
      if(key1_pan1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_pan2=calloc(key1_pan3, sizeof(int));
      if(key1_pan2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }


    key1_ca1=calloc(key1_npan, sizeof(float));
      if(key1_ca1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_ca2=calloc(key1_npan, sizeof(float));
      if(key1_ca2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_ca3=calloc(key1_npan, sizeof(float));
      if(key1_ca3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key1_cb1=calloc(key1_npan, sizeof(float));
      if(key1_cb1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_cb2=calloc(key1_npan, sizeof(float));
      if(key1_cb2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_cb3=calloc(key1_npan, sizeof(float));
      if(key1_cb3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key1_cc1=calloc(key1_npan, sizeof(float));
      if(key1_cb1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_cc2=calloc(key1_npan, sizeof(float));
      if(key1_cb2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_cc3=calloc(key1_npan, sizeof(float));
      if(key1_cb3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key1_da=calloc(key1_npan, sizeof(float));
      if(key1_da == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_db=calloc(key1_npan, sizeof(float));
      if(key1_db == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }


    key1_na=calloc(key1_npan, sizeof(int));
      if(key1_na == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_nb=calloc(key1_npan, sizeof(int));
      if(key1_nb == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_nt=calloc(key1_npan, sizeof(int));
      if(key1_nt == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key1_dda=calloc(key1_npan, sizeof(float));
      if(key1_dda == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_ddb=calloc(key1_npan, sizeof(float));
      if(key1_ddb == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key1_nna=calloc(key1_npan, sizeof(int));
      if(key1_nna == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_nnb=calloc(key1_npan, sizeof(int));
      if(key1_nnb == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key1_nnt=calloc(key1_npan, sizeof(int));
      if(key1_nnt == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key1_weights=calloc(key1_w1*key1_w2*key1_w3*key1_w4, sizeof(float));
      if(key1_weights == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }





  /* -------- key1 - assegnazione variabili  --------- */

    if (key1_type == 1)
      {
        i=0;
        if (key1_interp == 1)
          {
            key1_pan1[i] = 1;
            key1_pan2[i] = 1;
           }
        else
          {
            key1_pan1[i] = 1;
            key1_pan2[i] = 1;
           }
      }


    if (key1_type > 1) {

    cur_key1=start_key1;
    i=0;
    while (cur_key1 != NULL)
      {
        key1_o1[i]=cur_key1->o1;
        key1_o2[i]=cur_key1->o2;
        key1_o3[i]=cur_key1->o3;

        key1_ca1[i]=cur_key1->ca1;
        key1_ca2[i]=cur_key1->ca2;
        key1_ca3[i]=cur_key1->ca3;

        key1_cb1[i]=cur_key1->cb1;
        key1_cb2[i]=cur_key1->cb2;
        key1_cb3[i]=cur_key1->cb3;

        key1_cc1[i]=cur_key1->cc1;
        key1_cc2[i]=cur_key1->cc2;
        key1_cc3[i]=cur_key1->cc3;

        key1_da[i]=cur_key1->da;
        key1_db[i]=cur_key1->db;
        key1_na[i]=cur_key1->na;
        key1_nb[i]=cur_key1->nb;
        key1_nt[i]=cur_key1->nt;

        key1_dda[i]=cur_key1->dda;
        key1_ddb[i]=cur_key1->ddb;
        key1_nna[i]=cur_key1->nna;
        key1_nnb[i]=cur_key1->nnb;
        key1_nnt[i]=cur_key1->nnt;

        if (key1_interp == 1)
          {
            key1_pan1[i] = cur_key1->nna;
            key1_pan2[i] = cur_key1->nnb;
          }
        else
          {
            key1_pan1[i] = cur_key1->na;
            key1_pan2[i] = cur_key1->nb;
           }

        cur_key1 = cur_key1->next_tgt;
        i++;
      } }



    /*-------- key1 - calcolo pesi interpolazione  --------- */

    if (key1_type == 2)
      {
        cur_key1=start_key1;
        i=0;
        while (cur_key1 != NULL)
         {

          key1_rra=(int)cur_key1->da/cur_key1->dda;
          key1_rrb=(int)cur_key1->db/cur_key1->ddb;

          for(j=0; j<=key1_rrb; j++)
            {
              for(k=0; k<=key1_rra; k++)
                {
                   t = k*cur_key1->dda/cur_key1->da;
                   u = j*cur_key1->ddb/cur_key1->db;

                   key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*0] = (1-t)*(1-u);
                   key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*1] = (t)  *(1-u);
                   key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*2] = (1-t)*(u);
                   key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*3] = (t)  *(u);

                  }
            }

          cur_key1 = cur_key1->next_tgt;
          i++;
         }
      }


    if (key1_type == 3)
      {
        cur_key1=start_key1;

        key1_rra=(int)cur_key1->da/cur_key1->dda;
        key1_rrb=(int)cur_key1->db/cur_key1->ddb;
        key1_rrc=(int)cur_key1->dc/cur_key1->ddc;

        for (i=0; i<=key1_rrc; i++)
          {
            for(j=0; j<=key1_rrb; j++)
              {
                for(k=0; k<=key1_rra; k++)
                  {
                     t = k*cur_key1->dda/cur_key1->da;
                     u = j*cur_key1->ddb/cur_key1->db;
                     v = i*cur_key1->ddc/cur_key1->dc;

                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*0] =
                          (1-t)*(1-u)*(1-v);
                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*1] =
                          (t)  *(1-u)*(1-v);
                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*2] =
                          (1-t)*(u)  *(1-v);
                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*3] =
                          (t)  *(u)  *(1-v);

                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*4] =
                          (1-t)*(1-u)*(v);
                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*5] =
                          (t)  *(1-u)*(v);
                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*6] =
                          (1-t)*(u)  *(v);
                     key1_weights[k + key1_w1*j + key1_w1*key1_w2*i + key1_w1*key1_w2*key1_w3*7] =
                          (t)  *(u)  *(v);

                   }
                }
            }

      }




     /* -------------------------------------- */
     /* ---------- Key2 parameters ----------- */
     /* -------------------------------------- */
     key2_npan=0;
     key2_nt_tot=0;
     key2_nnt_tot=0;
     key2_na_max=0;
     key2_nb_max=0;
     key2_nc_max=0;
     key2_nna_max=0;
     key2_nnb_max=0;
     key2_nnc_max=0;
     key2_rra_max=0;
     key2_rrb_max=0;
     key2_rrc_max=0;

     key2_dc = 0;
     key2_nc = 0;
     key2_ddc = 0;
     key2_nnc = 0;

     if (verbose > 0)
       {
         printf("\nKEY2 : ");
         printf("type : %d - ", key2_type);
         if (key2_interp == 0)
            printf("interpolazione : no\n ");
         else
            printf("interpolazione : si\n ");
       }

     if (key2_type == 1)
       {
         if (verbose > 0)
         printf("\n----- Set of points -----\n");
         fp = fopen(key2_file, "r");
         if (fp ==  NULL)
          {
             fprintf(stderr, "ERROR: unable to open set file ->  %s \n", key2_file);
             exit(EXIT_FAILURE);
           }
         rewind(fp);
         j=0;
         while (feof(fp) == 0)
          {
            test = fgetc(fp);
             if ((test != REMARK) && (test != '\n') && (test != EOF))
              {
                ungetc(test, fp);
                st[0]='\0';
                fscanf(fp," %[$-z ] \n", st);
                for(i=0; i<strlen(st); i++)
                   {
                      st[i] = toupper(st[i]);
                    }
                if ( (strstr(st, "SETTYPE") == NULL) && (strlen(st) > 0)  )
                   {
                      j++;
                    }
              }
             else if (test == REMARK)
              {
                fscanf(fp,"%*[^\n]\n");
              }
           }
         fclose(fp);

         key2_npan=j;
         key2_mpan=j;
         key2_pan3=j;
         key2_nt_tot=j;
         key2_nnt_tot=j;

         key2_w1 = 1;
         key2_w2 = 1;
         key2_w3 = 1;
         key2_w4 = 1;

         if (verbose > 0)
         printf("Total number of points : %d\n",key2_npan);

       }

     else

       {
         cur_key2 = start_key2;
         while (cur_key2 != NULL)
          {

           if ( (key2_type == 3) && (cur_key2->seq == 1) && (verbose >0) )
            {

              printf("\n----- Volume -----\n");
              printf("o1 = %f - o2 = %f - o3 = %f\n",   cur_key2->o1, cur_key2->o2, cur_key2->o3);
              printf("ca1=%f - ca2=%f - ca3=%f\n",acos(cur_key2->ca1)*180/PI,acos(cur_key2->ca2)*180/PI,
                                                        acos(cur_key2->ca3)*180/PI);
              printf("cb1=%f - cb2=%f - cb3=%f\n",acos(cur_key2->cb1)*180/PI,acos(cur_key2->cb2)*180/PI,
                                                        acos(cur_key2->cb3)*180/PI);
              printf("cc1=%f - cc2=%f - cc3=%f\n",acos(cur_key2->cc1)*180/PI,acos(cur_key2->cc2)*180/PI,
                                                        acos(cur_key2->cc3)*180/PI);
              printf("la = %f - lb = %f - lc = %f\n",   cur_key2->la,  cur_key2->lb,  cur_key2->lc);
              printf("da = %f - db = %f - dc = %f\n",   cur_key2->da,  cur_key2->db,  cur_key2->dc );
              printf("na = %d - nb = %d - nc = %d\n",   cur_key2->na,  cur_key2->nb,  cur_key2->nc);
              printf("dda = %f - ddb = %f - ddc = %f\n",cur_key2->dda, cur_key2->ddb, cur_key2->ddc );
              printf("nna = %d - nnb = %d - nnc = %d\n",cur_key2->nna, cur_key2->nnb, cur_key2->nnc);

             }
           if ( (key2_type == 2) && (verbose>0) )
            {
              printf("\n----- Panel N. %d -----\n",     cur_key2->seq);
              printf("o1 = %f - o2 = %f - o3 = %f\n",   cur_key2->o1, cur_key2->o2, cur_key2->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(cur_key2->ca1)*180/PI, acos(cur_key2->ca2)*180/PI,
                                                        acos(cur_key2->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(cur_key2->cb1)*180/PI, acos(cur_key2->cb2)*180/PI,
                                                        acos(cur_key2->cb3)*180/PI);
              printf("la = %f - lb = %f\n",             cur_key2->la, cur_key2->lb);
              printf("da = %f - db = %f\n",             cur_key2->da, cur_key2->db);
              printf("na = %d - nb = %d\n",             cur_key2->na, cur_key2->nb);
              printf("dda = %f - ddb = %f\n",           cur_key2->dda, cur_key2->ddb);
              printf("nna = %d - nnb = %d\n",           cur_key2->nna, cur_key2->nnb);
             }


           key2_npan = key2_npan +1;
           key2_nt_tot = key2_nt_tot + cur_key2->nt;

           if (cur_key2->na>= key2_na_max)
             key2_na_max=cur_key2->na;
           if (cur_key2->nb>= key2_nb_max)
             key2_nb_max=cur_key2->nb;

           if (cur_key2->nna>= key2_nna_max)
             key2_nna_max=cur_key2->nna;
           if (cur_key2->nnb>= key2_nnb_max)
             key2_nnb_max=cur_key2->nnb;

           if ( (cur_key2->da/cur_key2->dda) >= key2_rra_max)
             key2_rra_max=(int)(cur_key2->da/cur_key2->dda);
           if ( (cur_key2->db/cur_key2->ddb) >= key2_rrb_max)
             key2_rrb_max=(int)(cur_key2->db/cur_key2->ddb);


           if (key2_type == 2)
             {
                key2_dc = 0;
                key2_nc = 0;
                key2_ddc = 0;
                key2_nnc = 0;

                key2_w1 = (key2_rra_max+1);
                key2_w2 = (key2_rrb_max+1);
                key2_w3 = (key2_npan);
                key2_w4 = 4;

                key2_nnt_tot = key2_nnt_tot + cur_key2->nna * cur_key2->nnb;

                if (key2_interp == 1)
                  {
                    key2_mpan = key2_nnt_tot;
                    key2_pan3 = key2_npan;
                   }
                else
                  {
                    key2_mpan = key2_nt_tot;
                    key2_pan3 = key2_npan;
                   }

             }
           if (key2_type == 3)
             {
               if (cur_key2->nc>= key2_nc_max)
                   key2_nc_max=cur_key2->nc;

               if (cur_key2->nnc>= key2_nnc_max)
                   key2_nnc_max=cur_key2->nnc;

               if ( (cur_key2->dc/cur_key2->ddc) >= key2_rrc_max)
                   key2_rrc_max=(int)(cur_key2->dc/cur_key2->ddc);

                key2_dc = cur_key2->dc;
                key2_nc = cur_key2->nc;
                key2_ddc = cur_key2->ddc;
                key2_nnc = cur_key2->nnc;

                key2_w1 = (key2_rra_max+1);
                key2_w2 = (key2_rrb_max+1);
                key2_w3 = (key2_rrc_max+1);
                key2_w4 = 8;

                key2_nnt_tot = cur_key2->nna * cur_key2->nnb * cur_key2->nnc;

                if (key2_interp == 1)
                  {
                    key2_mpan = key2_nnt_tot;
                    key2_pan3 = key2_nnc;
                   }
                else
                  {
                    key2_mpan = key2_nt_tot;
                    key2_pan3 = key2_nc;
                   }

             }

           cur_key2=cur_key2->next_tgt;
          }
       }
   /* --------- end processing key2 parameters -----------*/



  /*--------- key2 -  memory allocation ----------- */
    key2_pan1=calloc(key1_pan3, sizeof(int));
      if(key2_pan1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_pan2=calloc(key1_pan3, sizeof(int));
      if(key2_pan2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }



    key2_o1=calloc(key2_npan, sizeof(float));
      if(key2_o1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_o2=calloc(key2_npan, sizeof(float));
      if(key2_o2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_o3=calloc(key2_npan, sizeof(float));
      if(key2_o3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }


    key2_ca1=calloc(key2_npan, sizeof(float));
      if(key2_ca1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_ca2=calloc(key2_npan, sizeof(float));
      if(key2_ca2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_ca3=calloc(key2_npan, sizeof(float));
      if(key2_ca3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key2_cb1=calloc(key2_npan, sizeof(float));
      if(key2_cb1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_cb2=calloc(key2_npan, sizeof(float));
      if(key2_cb2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_cb3=calloc(key2_npan, sizeof(float));
      if(key2_cb3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key2_cc1=calloc(key2_npan, sizeof(float));
      if(key2_cb1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_cc2=calloc(key2_npan, sizeof(float));
      if(key2_cb2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_cc3=calloc(key2_npan, sizeof(float));
      if(key2_cb3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key2_da=calloc(key2_npan, sizeof(float));
      if(key2_da == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_db=calloc(key2_npan, sizeof(float));
      if(key2_db == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }


    key2_na=calloc(key2_npan, sizeof(int));
      if(key2_na == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_nb=calloc(key2_npan, sizeof(int));
      if(key2_nb == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_nt=calloc(key2_npan, sizeof(int));
      if(key2_nt == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key2_dda=calloc(key2_npan, sizeof(float));
      if(key2_dda == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_ddb=calloc(key2_npan, sizeof(float));
      if(key2_ddb == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key2_nna=calloc(key2_npan, sizeof(int));
      if(key2_nna == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_nnb=calloc(key2_npan, sizeof(int));
      if(key2_nnb == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
    key2_nnt=calloc(key2_npan, sizeof(int));
      if(key2_nnt == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

    key2_weights=calloc(key2_w1*key2_w2*key2_w3*key2_w4, sizeof(float));
      if(key2_weights == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }





  /* -------- key2 - assegnazione variabili  --------- */

    if (key2_type == 1)
      {
        i=0;
        if (key1_interp == 1)
          {
            key1_pan1[i] = 1;
            key1_pan2[i] = 1;
           }
        else
          {
            key1_pan1[i] = 1;
            key1_pan2[i] = 1;
          }
      }


    if (key2_type > 1) {

    cur_key2=start_key2;
    i=0;
    while (cur_key2 != NULL)
      {
        key2_o1[i]=cur_key2->o1;
        key2_o2[i]=cur_key2->o2;
        key2_o3[i]=cur_key2->o3;

        key2_ca1[i]=cur_key2->ca1;
        key2_ca2[i]=cur_key2->ca2;
        key2_ca3[i]=cur_key2->ca3;

        key2_cb1[i]=cur_key2->cb1;
        key2_cb2[i]=cur_key2->cb2;
        key2_cb3[i]=cur_key2->cb3;

        key2_cc1[i]=cur_key2->cc1;
        key2_cc2[i]=cur_key2->cc2;
        key2_cc3[i]=cur_key2->cc3;

        key2_da[i]=cur_key2->da;
        key2_db[i]=cur_key2->db;
        key2_na[i]=cur_key2->na;
        key2_nb[i]=cur_key2->nb;
        key2_nt[i]=cur_key2->nt;

        key2_dda[i]=cur_key2->dda;
        key2_ddb[i]=cur_key2->ddb;
        key2_nna[i]=cur_key2->nna;
        key2_nnb[i]=cur_key2->nnb;
        key2_nnt[i]=cur_key2->nnt;

        if (key2_interp == 1)
          {
            key2_pan1[i] = cur_key2->nna;
            key2_pan2[i] = cur_key2->nnb;
           }
        else
          {
            key2_pan1[i] = cur_key2->na;
            key2_pan2[i] = cur_key2->nb;
           }


        cur_key2 = cur_key2->next_tgt;
        i++;
      } }



    /*-------- key2 - calcolo pesi interpolazione  --------- */

    if (key2_type == 2)
      {
        cur_key2=start_key2;
        i=0;
        while (cur_key2 != NULL)
         {

          key2_rra=(int)cur_key2->da/cur_key2->dda;
          key2_rrb=(int)cur_key2->db/cur_key2->ddb;

          for(j=0; j<=key2_rrb; j++)
            {
              for(k=0; k<=key2_rra; k++)
                {
                   t = k*cur_key2->dda/cur_key2->da;
                   u = j*cur_key2->ddb/cur_key2->db;

                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*0] =
                          (1-t)*(1-u);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*1] =
                          (t)  *(1-u);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*2] =
                          (1-t)*(u);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*3] =
                          (t)  *(u);

                  }
            }

          cur_key2 = cur_key2->next_tgt;
          i++;
         }
      }


    if (key2_type == 3)
      {
        cur_key2=start_key2;

        key2_rra=(int)cur_key2->da/cur_key2->dda;
        key2_rrb=(int)cur_key2->db/cur_key2->ddb;
        key2_rrc=(int)cur_key2->dc/cur_key2->ddc;

        for (i=0; i<=key2_rrc; i++)
          {
            for(j=0; j<=key2_rrb; j++)
              {
                for(k=0; k<=key2_rra; k++)
                  {
                     t = k*cur_key2->dda/cur_key2->da;
                     u = j*cur_key2->ddb/cur_key2->db;
                     v = i*cur_key2->ddc/cur_key2->dc;

                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*0] =
                          (1-t)*(1-u)*(1-v);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*1] =
                          (t)  *(1-u)*(1-v);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*2] =
                          (1-t)*(u)  *(1-v);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*3] =
                          (t)  *(u)  *(1-v);

                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*4] =
                          (1-t)*(1-u)*(v);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*5] =
                          (t)  *(1-u)*(v);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*6] =
                          (1-t)*(u)  *(v);
                     key2_weights[k + key2_w1*j + key2_w1*key2_w2*i + key2_w1*key2_w2*key2_w3*7] =
                          (t)  *(u)  *(v);

                   }
                }
            }

      }




   /* ------------------------------------ */
   /* ------------------------------------ */
   /* ------------------------------------ */

   printf("\n");
   for (i=0; i<8; i++)
     {

      if (attrib[i] == 1)
        {

          if (verbose > 1)
              printf("Processing file %s ... \n", filenames[i*2+1]);

          strcpy(HeaderFile, filenames[i*2]);
          strcpy(DataFile, filenames[i*2+1]);

          strcpy(file, InDir);
          strcat(file, HeaderFile);

          strcpy(file2, dbDir);
          strcat(file2, HeaderFile);

          strcpy(string, "NSHOT");

          seph_get_int_(file, string, &nshot, strlen(file), strlen(string));
          strcpy(string, "NTARGET");
          seph_get_int_(file, string, &ntarget, strlen(file), strlen(string));




          /* ==== copy files to db directory ==== */
          if ( ((key1_interp == 0) && (key2_interp == 0)) ||
               ((key1_type == 1)   && (key2_type == 1)  ) )
            {

              fp=fopen(file, "r");
              if (fp ==  NULL)
               {
                 fprintf(stderr, "ERROR: unable to open file  %s \n", file);
                 exit(EXIT_FAILURE);
                }

              fp2=fopen(file2, "w");
              if (fp ==  NULL)
               {
                 fprintf(stderr, "ERROR: unable to open file  %s \n", file2);
                 exit(EXIT_FAILURE);
                }

              c=fgetc(fp);
              while (feof(fp)==0)
                {
                  fputc(c, fp2);
                  c=fgetc(fp);
                 }

              fclose(fp);
              fclose(fp2);


              strcpy(file, InDir);
              strcat(file, DataFile);

              strcpy(file2, dbDir);
              strcat(file2, DataFile);

              /* check dimensioni file  */
              if (key1_nt_tot != nshot)
               {
                 fprintf(stderr, "ERROR: wrong dimension file  %s \n", file);
                 exit(EXIT_FAILURE);
                }

              if (key2_nt_tot != ntarget)
               {
                 fprintf(stderr, "ERROR: wrong dimension file  %s \n", file);
                 exit(EXIT_FAILURE);
                }

              key2_mtx_in = calloc(nshot*ntarget, sizeof(float));

              fdesc=0;
              strcpy(mode,"rb");
              glob_fopen_(&fdesc, file, mode, &file_error, strlen(file), strlen(mode));
              if (file_error != 0)
                {
                  fprintf(stderr, "ERROR: unable to read file ->  %s \n", file);
                  exit(EXIT_FAILURE);
                }
              offset=1;
              origin=0;
              glob_fseek_(&fdesc,&offset,&origin, &file_error);

              dim = sizeof(float);
              num = nshot*ntarget;
              glob_fread_(&fdesc, key2_mtx_in, &dim, &num, &file_error);

              glob_fclose_(&fdesc, &file_error);



              fdesc=0;
              strcpy(mode2,"wb");
              glob_fopen_(&fdesc, file2, mode2, &file_error, strlen(file2), strlen(mode2));
              if (file_error != 0)
                {
                  fprintf(stderr, "ERROR: unable to write file ->  %s \n", file2);
                  exit(EXIT_FAILURE);
                }

              offset=1;
              origin=0;
              glob_fseek_(&fdesc,&offset,&origin, &file_error);

              dim = sizeof(float);
              num = nshot*ntarget;
              glob_fwrite_(&fdesc, key2_mtx_in, &dim, &num, &file_error);

              glob_fclose_(&fdesc, &file_error);

              free(key2_mtx_in);

           }



        /* ==== key1 interpolation - key2 interpolation ====*/
        if ((key1_interp == 1) || (key2_interp == 1))
          {

              /* ........................... */
              strcpy(file, InDir);
              strcat(file, DataFile);

              strcpy(file2, dbDir);
              strcat(file2, DataFile);

              /* check dimensioni file  */
              if (key1_nt_tot != nshot)
               {
                 fprintf(stderr, "ERROR: wrong dimension file  %s \n", file);
                 exit(EXIT_FAILURE);
                }

              if (key2_nt_tot != ntarget)
               {
                 fprintf(stderr, "ERROR: wrong dimension file  %s \n", file);
                 exit(EXIT_FAILURE);
                }


              key2_mtx_in  = calloc(key1_nt_tot*key2_nt_tot, sizeof(float));
              key2_mtx_out = calloc(key2_mpan, sizeof(float));


              /* INPUT FILE */
              fdesc=0;
              strcpy(mode,"rb");
              glob_fopen_(&fdesc, file, mode, &file_error, strlen(file), strlen(mode));
              if (file_error != 0)
                {
                  fprintf(stderr, "ERROR: unable to read file ->  %s \n", file);
                  exit(EXIT_FAILURE);
                }
              offset=1;
              origin=0;
              glob_fseek_(&fdesc,&offset,&origin, &file_error);

              dim = sizeof(float);
              num = key1_nt_tot*key2_nt_tot;
              glob_fread_(&fdesc, key2_mtx_in, &dim, &num, &file_error);

              glob_fclose_(&fdesc, &file_error);


              /* OUTPUT FILE */
              fdesc2=0;
              strcpy(mode2,"wb");
              glob_fopen_(&fdesc2, file2, mode2, &file_error, strlen(file2), strlen(mode2));
              if (file_error != 0)
                {
                  fprintf(stderr, "ERROR: unable to write file ->  %s \n", file2);
                  exit(EXIT_FAILURE);
                }
              offset2=1;
              origin2=0;
              glob_fseek_(&fdesc2,&offset2,&origin2, &file_error);

              execute_(
                           &fdesc2,
                           &key1_type,   &key1_interp,
                           &key1_npan,   &key1_nt_tot, &key1_nnt_tot,
                           key1_pan1,    key1_pan2,   &key1_pan3,
                           &key1_na_max, &key1_nb_max, &key1_nc_max,
                           &key1_nna_max,&key1_nnb_max,&key1_nnc_max,
                           &key1_rra_max,&key1_rrb_max,&key1_rrc_max,
                           key1_o1,      key1_o2,      key1_o3,
                           key1_ca1,     key1_ca2,     key1_ca3,
                           key1_cb1,     key1_cb2,     key1_cb3,
                           key1_cc1,     key1_cc2,     key1_cc3,
                           key1_da, key1_db, &key1_dc,
                           key1_na, key1_nb, &key1_nc,
                           key1_nt,
                           key1_dda, key1_ddb, &key1_ddc,
                           key1_nna, key1_nnb, &key1_nnc,
                           key1_nnt,
                           &key1_w1, &key1_w2, &key1_w3, &key1_w4,
                           key1_weights,

                           &key2_type,   &key2_interp,
                           &key2_npan,   &key2_mpan, &key2_nt_tot, &key2_nnt_tot,
                           key2_pan1,    key2_pan2,   &key2_pan3,
                           &key2_na_max, &key2_nb_max, &key2_nc_max,
                           &key2_nna_max,&key2_nnb_max,&key2_nnc_max,
                           &key2_rra_max,&key2_rrb_max,&key2_rrc_max,
                           key2_o1,      key2_o2,      key2_o3,
                           key2_ca1,     key2_ca2,     key2_ca3,
                           key2_cb1,     key2_cb2,     key2_cb3,
                           key2_cc1,     key2_cc2,     key2_cc3,
                           key2_da, key2_db, &key2_dc,
                           key2_na, key2_nb, &key2_nc,
                           key2_nt,
                           key2_dda, key2_ddb, &key2_ddc,
                           key2_nna, key2_nnb, &key2_nnc,
                           key2_nnt,
                           &key2_w1, &key2_w2, &key2_w3, &key2_w4,
                           key2_weights,

                           key2_mtx_in, key2_mtx_out

                   );



              glob_fclose_(&fdesc2, &file_error);

              free(key2_mtx_in);
              free(key2_mtx_out);


              /* ................................ */

              strcpy(file, InDir);
              strcat(file, HeaderFile);

              strcpy(file2, dbDir);
              strcat(file2, HeaderFile);

              remove(file2);

              len1 = MAX_STRING_LEN;
              strcpy(string, "TITLE");
              seph_get_string_(file, string, string1, &len, strlen(file), strlen(string), len1);
              string1[len]='\0';

              strcpy(string, "IN");
              seph_get_string_(file, string, string2, &len, strlen(file), strlen(string), len1);
              string2[len]='\0';



              strcpy(string, "title");
              seph_put_string_(file2,string,string1,strlen(file2),strlen(string),strlen(string1));

              strcpy(string, "updown");
              seph_put_int_(file2,string,&updown,strlen(file2),strlen(string));

              strcpy(string, "nshot");
              seph_put_int_(file2,string,&key1_mpan,strlen(file2),strlen(string));

              strcpy(string, "ntarget");
              seph_put_int_(file2,string,&key2_mpan,strlen(file2),strlen(string));

              dim = sizeof(float);
              strcpy(string, "esize");
              seph_put_int_(file2,string, &dim, strlen(file2),strlen(string));

              strcpy(string, "in");
              seph_put_string_(file2,string,string2,strlen(file2),strlen(string),strlen(string2));

           }


        }
     }

  /* -------- end interpolation ------- */




  /* ---------- freee memory ----------- */

  free(file);
  free(file2);
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

  cur_key1=start_key1;
  while (cur_key1 != NULL)
    {
      free_key1 = cur_key1;
      cur_key1 = free_key1-> next_tgt;
      free(free_key1);
      }

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



