/****************************************************/
/*                   start.c                     */
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

/********************************************************************/
/*                    MAIN                                          */
/* argv[0] = program name                                           */
/* argv[1] = parameters file specifying target (<>.set)             */
/* argv[2] = name of the output file  (<>.sht)                      */
/* argv[3] = level of verbosity (0/1/2)                             */
/********************************************************************/
int main(int argc, char* argv[])
{

    int i, j;

    /*  Target                */
    TGTptr starttgt, curtgt, freetgt;
    int tg_nt;
    float *tg_mtx_x1, *tg_mtx_x2, *tg_mtx_x3;

    /*   Output   */
    FILE *fp;

    char *outFileName;
    FILE *outFile;
    int   fdesc;
    char* mode;
    int   dim, num;
    int   file_error;
    float dummy1, dummy2, dummy3;

    int settype;
    int verbose;

    int test;
    char *st;

  /* Variables initialization */
  outFileName = calloc(MAX_STRING_LEN, sizeof(char));
  st = calloc(MAX_STRING_LEN, sizeof(char));

  /* Check command line arguments */
  if (argc < 3)
       {
         fprintf(stderr,"ERROR : Missing command line parameter.\n");
         fprintf(stderr,"*** SAMPLER v. %s ***\n",VERSION);
         fprintf(stderr,"sampler.x (set desc <set>) (output file <sht>) [verbose=0/1/2]\n\n");
         exit(EXIT_FAILURE);
         }

  if (strcmp("--help", argv[1]) == 0 )
       {
         printf("*** SAMPLER v. %s ***\n",VERSION);
         printf("Command line template :\n");
         printf("sampler.x (set desc <set>) (output file <sht>) [verbose=0/1/2]\n\n");
         exit(EXIT_SUCCESS);
        }

  verbose = 1;
  if (argc == 4)
    {
      sscanf(argv[3]," %d ", &verbose);
      if ( (verbose<0) || (verbose>2) )
         verbose =1;
    }

  printf("\n*** SAMPLER v. %s ***\n",VERSION);

   /* ----------------- target parameters ------------------------ */
   starttgt = TargetParams(argv[1], &settype);


   /* ----------------- output file ------------------------- */
   strcpy(outFileName, argv[2]);

   outFile=fopen(outFileName,"wb");
   if (outFile == NULL)
        {
          fprintf(stderr, "ERROR: unable to open file %s \n", outFileName);
          exit(EXIT_FAILURE);
        }
   else
        {
          fclose(outFile);
          remove(outFileName);
        }


   if (settype == 1)
    {
         fp = fopen(argv[1], "r");
         if (fp ==  NULL)
          {
             fprintf(stderr, "ERROR: unable to open set file ->  %s \n", argv[1]);
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
                      sscanf(st,"%f %f %f\n", &dummy1, &dummy2, &dummy3 );
                      j++;
                    }
              }
             else if (test == REMARK)
              {
                fscanf(fp,"%*[^\n]\n");
              }

           }
         tg_nt=j;


         /* dynamic allocation of memory */
         tg_mtx_x1=calloc(tg_nt,sizeof(float));
         if (tg_mtx_x1 == NULL)
            {
              fprintf(stderr, "ERROR: cannot allocate memory.\n");
              exit(EXIT_FAILURE);
             }
         tg_mtx_x2=calloc(tg_nt,sizeof(float));
         if (tg_mtx_x2 == NULL)
            {
              fprintf(stderr, "ERROR: cannot allocate memory.\n");
              exit(EXIT_FAILURE);
             }
         tg_mtx_x3=calloc(tg_nt,sizeof(float));
         if (tg_mtx_x3 == NULL)
            {
              fprintf(stderr, "ERROR: cannot allocate memory.\n");
              exit(EXIT_FAILURE);
             }

         if (verbose>0)
         printf("\n----- Set of points -----\n");

         if (verbose>1)
         printf("Building target ... \n");

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
                      sscanf(st,"%f %f %f\n", &tg_mtx_x1[j], &tg_mtx_x2[j],
                                            &tg_mtx_x3[j] );
                      j++;
                    }
              }
             else if (test == REMARK)
              {
                fscanf(fp,"%*[^\n]\n");
              }

           }
         fclose(fp);

         if (verbose>1)
         printf("done.\n");

         if (verbose>0)
         printf("Total number of points : %d\n",j);

     }

   else

    {
      tg_nt = 0 ;
      curtgt = starttgt;
      while (curtgt != NULL)
       {
         tg_nt = tg_nt +curtgt->na*curtgt->nb;
         curtgt = curtgt->next_tgt;
        }

      /* dynamic allocation of memory */
      tg_mtx_x1=calloc(tg_nt,sizeof(float));
      if (tg_mtx_x1 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
         }
      tg_mtx_x2=calloc(tg_nt,sizeof(float));
      if (tg_mtx_x2 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }
      tg_mtx_x3=calloc(tg_nt,sizeof(float));
      if (tg_mtx_x3 == NULL)
        {
          fprintf(stderr, "ERROR: cannot allocate memory.\n");
          exit(EXIT_FAILURE);
        }

      i=0;
      curtgt = starttgt;
      while (curtgt != NULL)
       {

          if ( (settype == 3) && (curtgt->seq == 1) && (verbose>0) )
            {

              printf("\n----- Volume -----\n");
              printf("o1 = %f - o2 = %f - o3 = %f\n",curtgt->o1, curtgt->o2, curtgt->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(curtgt->ca1)*180/PI, acos(curtgt->ca2)*180/PI,
                                                        acos(curtgt->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(curtgt->cb1)*180/PI, acos(curtgt->cb2)*180/PI,
                                                        acos(curtgt->cb3)*180/PI);
              printf("cc1 = %f - cc2 = %f - cc3 = %f\n",acos(curtgt->cc1)*180/PI, acos(curtgt->cc2)*180/PI,
                                                        acos(curtgt->cc3)*180/PI);
              printf("la = %f - lb = %f - lc = %f\n",   curtgt->la,  curtgt->lb,  curtgt->lc);
              printf("da = %f - db = %f - dc = %f\n",   curtgt->da,  curtgt->db,  curtgt->dc );
              printf("na = %d - nb = %d - nc = %d\n",   curtgt->na,  curtgt->nb,  curtgt->nc);
              printf("dda = %f - ddb = %f - ddc = %f\n",curtgt->dda, curtgt->ddb, curtgt->ddc );
              printf("nna = %d - nnb = %d - nnc = %d\n",curtgt->nna, curtgt->nnb, curtgt->nnc);

             }
          if ( (settype == 2) && (verbose>0) )
            {
              printf("\n----- Panel N. %d -----\n",curtgt->seq);
              printf("o1 = %f - o2 = %f - o3 = %f\n",curtgt->o1, curtgt->o2, curtgt->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(curtgt->ca1)*180/PI, acos(curtgt->ca2)*180/PI,
                                                        acos(curtgt->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(curtgt->cb1)*180/PI, acos(curtgt->cb2)*180/PI,
                                               acos(curtgt->cb3)*180/PI);
              printf("la = %f - lb = %f\n",curtgt->la, curtgt->lb);
              printf("da = %f - db = %f\n",curtgt->da, curtgt->db);
              printf("na = %d - nb = %d\n",curtgt->na, curtgt->nb);
              printf("dda = %f - ddb = %f\n",curtgt->dda, curtgt->ddb);
              printf("nna = %d - nnb = %d\n",curtgt->nna, curtgt->nnb);
             }

          if (verbose>1)
          printf("Building target N. %d ... \n",curtgt->seq);

          execute_(&(tg_mtx_x1[i]),&(tg_mtx_x2[i]),&(tg_mtx_x3[i]),
                   &(curtgt->nt),&(curtgt->na),&(curtgt->nb),
                   &(curtgt->o1),&(curtgt->o2),&(curtgt->o3),
                   &(curtgt->da),&(curtgt->db),
                   &(curtgt->ca1),&(curtgt->ca2),&(curtgt->ca3),
                   &(curtgt->cb1),&(curtgt->cb2),&(curtgt->cb3) );

          if (verbose>1)
          printf("done.\n");

          i += curtgt->nt;
          curtgt=curtgt->next_tgt;

      }
    }

   fdesc=0;
   mode = calloc(MAX_STRING_LEN, sizeof(char));
   strcpy(mode,"wb");
   num=tg_nt;
   dim = sizeof(float);

   glob_fopen_(&fdesc, outFileName, mode, &file_error, strlen(outFileName), strlen(mode));
   if (file_error != 0)
     {
       fprintf(stderr, "ERROR: unable to open model file ->  %s \n", outFileName);
       exit(EXIT_FAILURE);
     }

   glob_fwrite_(&fdesc, tg_mtx_x1, &dim, &num, &file_error);
   glob_fwrite_(&fdesc, tg_mtx_x2, &dim, &num, &file_error);
   glob_fwrite_(&fdesc, tg_mtx_x3, &dim, &num, &file_error);

   glob_fclose_(&fdesc, &file_error);

   /* memory deallocation    */
   free(tg_mtx_x1);
   free(tg_mtx_x2);
   free(tg_mtx_x3);
   free(mode);

     free(outFileName);
     free(st);
     curtgt = starttgt;
     while (curtgt != NULL)
      {
         freetgt = curtgt;
         curtgt = freetgt->next_tgt;
         free(freetgt);
      }

     if (verbose>0)
     printf("That's all folks!\n\n");

     exit(EXIT_SUCCESS);

}



