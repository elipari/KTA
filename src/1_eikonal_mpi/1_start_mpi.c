/****************************************************/
/*                   start.be.c                     */
/****************************************************/
    #include "../0_include/0_common_defs.h"
    #include <sys/stat.h>

/*************************************************/
/*              External Function                */
/*************************************************/
    extern void execute_();
    extern void err_and_exit();

/*************************************************/
/*              Global variables                 */
/*************************************************/
    FILE* glob_fp[MAX_FILE_NUM];


/*************************************************/
/*                    MAIN                       */
/* argv[0] = program name                        */
/* argv[1] = velocity model parameters           */
/* argv[2] = parameters file                     */
/* argv[3] = source/receiver parameters          */
/* argv[4] = output directory                    */
/*************************************************/

#include <mpi.h>

int main(int argc, char* argv[])
{

    FILE *fp;
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    int i, j, k, l, imax, lstr;
    char* temp;
    long  numbyte;

    /* Modello di velocita' */
    char *HeaderFileIn, *DataFileIn;
    int   SOTTOMOD;
    float oxminimod,oyminimod,ozminimod;
    int   nxminimod,nyminimod,nzminimod;
    int   pxminimod,pyminimod,pzminimod;

    float o1,o2,o3;
    float d1,d2,d3;
    int   n1,n2,n3;
    int   esize;
    float *S, *Sx, *Sy;

    /* Parametri iconale */
    float SOGLIAX, SOGLIAY;
    int   MODALITA;
    int   APERTURA;
    float ANGOLO;
    int   INIZIO;
    int   UPDOWN;
    int   VERSIONE;
    float AMPMIN;
    float DAMP;
    int   VERBOSE;

    /* Sorgenti */
    int   Nshot;
    char *SourceFile;
    float *xshot, *yshot, *zshot;

    /* Target */
    int   TARGET;
    char  *TargetFile;
    int   Ntgt, Ntgt1;
    int NtgtAA, NtgtTT, NtgtPPX, NtgtPPY, NtgtPPZ;
    float *xtgt, *ytgt, *ztgt;

    /* Output */
    char  *OutDir;
    int   OUTPUT;
    int   OUT_A;
    int   OUT_T;
    int   OUT_UX;
    int   OUT_UY;
    int   OUT_UZ;

    /* Ampiezze, Tempi e coseni direttori*/
    float *A, *T;
    float *Px, *Py, *Pz;
    float *AA, *TT;
    float *PPx, *PPy, *PPz;
    float *xcorr, *ycorr, *Pxcorr, *Pycorr, *Acorr,*Tcorr;
    int   *Ok2, *Posx, *Posy, *Ok1;
    
    float *vettdistx, *vettdisty;
    int   maxdisty,maxdistx;                          /* distanza massima Y (XLINE)e X (ILINE) a cui si migrano i dati*/  
    int   distyiniz,distxiniz,zmaxdistx,zmaxdisty;    /*  maxdist x e y  variabile con la profondita'*/
    int   zdistyiniz;
   
    /* ROB start - allocazione contigua*/
    long AA_size, TT_size, PPx_size, PPy_size, PPz_size, AATTPP_size, N_AATTPP;           
    void *AATTPP ;
    /* ROB end */ 

    /* Variabili locali */
    int inxminimod,inyminimod,inzminimod;
    float oxmod, oymod, ozmod;
    float dxmod, dymod, dzmod;
    int   nxmod, nymod, nzmod;
    int n3ver;
    int LenOutDir;
    
    char mode[MAX_STRING_LEN];

    int QUANT_FLAG, sizeBuftemp;
    int index_T, index_A, index_UX, index_UY, index_UZ, index_cur;
        
    int fdesc;
    int dim, num;
    int64 off64;           /* R64 lug 2001 */
    int origin;
    int file_error;

/* MPI variables */
/* 31/05/2002 */
/*
   myrank = Rank PE
   size   = Numero totale PE utilizzati
   iope   = size-1 = Rank di I/O PE
*/
    int myrank, size, iope;
    int memoffset;/* memoffset = padding da inserire fra le varie matrici allocate contiguamente per evitare conflitti in memoria */
    int err;/* err   = codice d'errore */
                
    int contMem = 0; /* CAN&BN: contatore memoria allocata (in elementi da due byte) */
    int maxMem = 50000000;
    int dimWriteBuf;
    float *writeBufferFloat;    /* CAN&BN: buffer per la scrittura del database */
    short *writeBufferShort;
    int *shotSort;    /* CAN&BN: array per il sorting del database */    
    short int *shortbuf;
    float *rangeA, *rangeT, *rangeCx, *rangeCy, *rangeCz, *rangeA_recv;
    int nRangeA, nRangeT, nRangeCx, nRangeCy, nRangeCz, nRangeTot;
    float *rangeT_recv, *rangeCx_recv, *rangeCy_recv, *rangeCz_recv;
    int  elemtowrite;
    char* rangeFile;

    /* ROB start: variabili impacchettamento */
    void *Pbuffer;
    int Npackets, q, r;
    MPI_Request *requests;
    MPI_Status *statuses;
    /* ROB end: variabili impacchettamento */

/*  02/07/2002
    statbuf = utilizzato per verificare se esistono directory */
    struct stat statbuf;
void writeBinFile( char *rangeFileName, char *OutDir, int myrank, char *argv[], float *range, int elemtowrite, int err);

/* Main *********************************************************************************************/
/* Start MPI */

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    iope = size - 1;

/* Controllo argomenti linea di comando **********************************************************/
    if (argc == 2) 
    {
       if (strcmp("--help", argv[1]) == 0 )
       {
          if(myrank == iope)
          {
             printf("*** EIKONAL v. %s ***\n",VERSION);
             printf("Command line template :\n");
             printf("eikonal.x (eik1.p <vel mod file>) (eik2.p <eik par>) (eik3.p <src/rec file>) (output dir)\n\n");
          }
          MPI_Finalize();
          exit(EXIT_SUCCESS);
       }
    }
    else if (argc != 5)
    {
       if(myrank == iope)
       {
          printf("ERROR : error in command line parameter\n");
          printf("*** EIKONAL v. %s ***\n",VERSION);
          printf("eikonal.x (eik1.p <vel mod file>) (eik2.p <eik par>) (eik3.p <src/rec file>) (output dir)\n\n");
       }
       MPI_Finalize();
       exit(EXIT_FAILURE);
    }

/***************************************************************************************************/
  if (myrank == iope)
                printf("\n*** EIKONAL v. %s ***\n\n",VERSION);

  if(size <= 1)
  		err_and_exit(112, myrank, argv);
/***************************************************************************************************/
/************************** LETTURA FILE PARAMETRI  **********************/

  for (i=0; i< MAX_PARAM; i++)
     {
       callocKTAChar(  &(ident[i]), MAX_STRING_LEN, 99, myrank, argv);
       callocKTAChar(  &( val[i]), MAX_STRING_LEN, 100, myrank, argv);
       contMem += MAX_STRING_LEN*2*sizeof(char);
     }

  /* ----------------- start processing velocity model parameters --------------- */
   fp=fopen(argv[1], "r");
   if (fp ==  NULL)                                                      err_and_exit(1, myrank, argv);
   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
   fclose(fp);

   /* assegnazione delle variabili */
    callocKTAChar(  &HeaderFileIn, MAX_STRING_LEN, 101, myrank, argv);
    contMem += MAX_STRING_LEN*2*sizeof(char);
/***************************************************************************************************/
/* Lettura parametri */

   strcpy(HeaderFileIn, "\0");
   SOTTOMOD = 1;
   oxminimod = FLT_MAX;
   oyminimod = FLT_MAX;
   ozminimod = FLT_MAX;
   nxminimod = -1;
   nyminimod = -1;
   nzminimod = -1;
   pxminimod = -1;
   pyminimod = -1;
   pzminimod = -1;

   for (i=0; i<imax; i++)
     {

       if (strcmp("MODELLO", ident[i]) == 0 ) /* 01 */
           sscanf(val[i], "%s", HeaderFileIn);
       else if (strcmp("SOTTOMOD", ident[i]) == 0 ) /* 02 */
           sscanf(val[i], "%d", &SOTTOMOD);
       else if (strcmp("OXMINIMOD", ident[i]) == 0 ) /* 03 */
           sscanf(val[i], "%f", &oxminimod); 
       else if (strcmp("OYMINIMOD", ident[i]) == 0 ) /* 04 */         
           sscanf(val[i], "%f", &oyminimod);
       else if (strcmp("OZMINIMOD", ident[i]) == 0 ) /* 05 */
           sscanf(val[i], "%f", &ozminimod);
       else if (strcmp("NXMINIMOD", ident[i]) == 0 ) /* 06 */
           sscanf(val[i], "%d", &nxminimod);
       else if (strcmp("NYMINIMOD", ident[i]) == 0 ) /* 07 */
           sscanf(val[i], "%d", &nyminimod);
       else if (strcmp("NZMINIMOD", ident[i]) == 0 ) /* 08 */
           sscanf(val[i], "%d", &nzminimod);
       else if (strcmp("PXMINIMOD", ident[i]) == 0 ) /* 09 */
           sscanf(val[i], "%d", &pxminimod);
       else if (strcmp("PYMINIMOD", ident[i]) == 0 ) /* 10 */
           sscanf(val[i], "%d", &pyminimod);
       else if (strcmp("PZMINIMOD", ident[i]) == 0 ) /* 11 */
           sscanf(val[i], "%d", &pzminimod);
     }

/***************************************************************************************************/
   /* check dei parametri */

     if ( strlen(HeaderFileIn) == 0 )                                  err_and_exit(2, myrank, argv);
     if (( SOTTOMOD < 1 ) || ( SOTTOMOD > 2  ))                        err_and_exit(3, myrank, argv);
     if ( SOTTOMOD == 2 )
       {
         if ( oxminimod == FLT_MAX )                                   err_and_exit(4, myrank, argv);
         if ( oyminimod == FLT_MAX )                                   err_and_exit(5, myrank, argv);
         if ( ozminimod == FLT_MAX )                                   err_and_exit(6, myrank, argv);
         if ( nxminimod == -1 )                                        err_and_exit(7, myrank, argv);
         if ( nyminimod == -1 )                                        err_and_exit(8, myrank, argv);
         if ( nzminimod == -1 )                                        err_and_exit(9, myrank, argv);
         if ( pxminimod == -1 )                                        err_and_exit(10, myrank, argv);
         if ( pyminimod == -1 )                                        err_and_exit(11, myrank, argv);
         if ( pzminimod == -1 )                                        err_and_exit(12, myrank, argv);
        }

   /* --------------- end processing velocity model parameters ------------------*/

/***************************************************************************************************/
   /* ---------------- start processing header velocity model file ------------- */
   fp=fopen(HeaderFileIn, "r");
    if (fp ==  NULL)                                                   err_and_exit(13, myrank, argv);
    imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
   fclose(fp);
   
    callocKTAChar(  &DataFileIn, MAX_STRING_LEN, 102, myrank, argv);
    contMem += MAX_STRING_LEN;

/***************************************************************************************************/
/* Lettura dati */

   j=0;
   for (i=0; i<imax; i++)
     {
       if (strcmp("N1", ident[i]) == 0 ) /* 01 */
         {
           sscanf(val[i], "%d", &n1);
           j++;
         }
       else if (strcmp("N2", ident[i]) == 0 ) /* 02 */
         {
           sscanf(val[i], "%d", &n2);
           j++;
         }
       else if (strcmp("N3", ident[i]) == 0 ) /* 03 */
         {
           sscanf(val[i], "%d", &n3);
           j++;
         }
       else if (strcmp("ESIZE", ident[i]) == 0 ) /* 04 */
         {
           sscanf(val[i], "%d", &esize);
           j++;
         }
       else if (strcmp("O1", ident[i]) == 0 ) /* 05 */
         {
           sscanf(val[i], "%f", &o1);
           j++;
         }
       else if (strcmp("O2", ident[i]) == 0 ) /* 06 */
         {
           sscanf(val[i], "%f", &o2);
           j++;
         }
       else if (strcmp("O3", ident[i]) == 0 ) /* 07 */
         {
           sscanf(val[i], "%f", &o3);
           j++;
         }
       else if (strcmp("D1", ident[i]) == 0 ) /* 08 */
         {
           sscanf(val[i], "%f", &d1);
           j++;
         }
       else if (strcmp("D2", ident[i]) == 0 ) /* 09 */
         {
           sscanf(val[i], "%f", &d2);
           j++;
         }
       else if (strcmp("D3", ident[i]) == 0 ) /* 10 */
         {
           sscanf(val[i], "%f", &d3);
           j++;
         }
       else if (strstr(ident[i], "IN") != NULL ) /* 11 */
         {
           sscanf(val[i], "%s", DataFileIn);
           j++;
         }
     }

/***************************************************************************************************/
/* Controllo dati */
     if ( j < 11)          err_and_exit(14, myrank, argv);
/***************************************************************************************************/

     oxmod=o1;
     oymod=o2;
     ozmod=o3;
     dxmod=d1;
     dymod=d2;
     dzmod=d3;
     nxmod=n1;
     nymod=n2;
     nzmod=n3;

     if ((DataFileIn[0] == '.') && (DataFileIn[1] == '/'))
       {
         for(i=(strlen(HeaderFileIn)-1); ( (HeaderFileIn[i]!='/') && (i>=0) ); i--);
         
         callocKTAChar(  &temp, MAX_STRING_LEN, 103, myrank, argv);
         strcpy(temp, DataFileIn+2);
         strcpy(DataFileIn, HeaderFileIn);
         DataFileIn[i+1]='\0';
         strcat(DataFileIn,temp);
         free(temp);
       }

     if ((DataFileIn[0] == '.') && (DataFileIn[1] == '.'))
       {
         for(l=(strlen(HeaderFileIn)-1); ( (HeaderFileIn[l]!='/') && (l>=0) ); l--);
         for(i=(l-1); ( (HeaderFileIn[i]!='/') && (i>=0) ); i--);

         callocKTAChar(  &temp, MAX_STRING_LEN, 104, myrank, argv);
         strcpy(temp, DataFileIn+3);
         strcpy(DataFileIn, HeaderFileIn);
         DataFileIn[i+1]='\0';
         strcat(DataFileIn,temp);
         free(temp);
       }
   /* ------------------ end processing header velocity model file ------------- */

/***************************************************************************************************/
   /* ------------------------ eventuale sottomodello ---------------------------*/
      if ( SOTTOMOD == 2 ) {

      /*  Controlli iniziali sul sottomodello */
        if ( (nxminimod <= 0) || (nyminimod <= 0) || (nzminimod <= 0) ) 
          err_and_exit(15, myrank, argv);
        else if ( (pxminimod <= 0) || (pyminimod <= 0 ) || (pzminimod <= 0) ) 
          err_and_exit(16, myrank, argv);
        else if ( (oxminimod < o1) || ((oxminimod + (nxminimod - 1) * pxminimod * d1) > (o1+(n1-1)*d1)) ) 
          err_and_exit(17, myrank, argv);
        else if ( (oyminimod < o2) || ((oyminimod + (nyminimod - 1) * pyminimod * d2) > (o2+(n2-1)*d2)) ) 
          err_and_exit(18, myrank, argv);
        else if ( (ozminimod < o3) || ((ozminimod + (nzminimod - 1) * pzminimod * d3) > (o3+(n3-1)*d3)) ) 
          err_and_exit(19, myrank, argv);

       /* Settaggio variabili per il sottomodello */
       inxminimod=round((oxminimod-o1)/d1)+1;
       inyminimod=round((oyminimod-o2)/d2)+1;
       inzminimod=round((ozminimod-o3)/d3)+1;
       oxminimod = (inxminimod-1)*d1+o1;
       oyminimod = (inyminimod-1)*d2+o2;
       ozminimod = (inzminimod-1)*d3+o3;
       n1=nxminimod;
       n2=nyminimod;
       n3=nzminimod;
       o1=oxminimod;
       o2=oyminimod;
       o3=ozminimod;
       d1=pxminimod*d1;
       d2=pyminimod*d2;
       d3=pzminimod*d3;

    }
   /* --------------------------- fine sottomodello -----------------------------*/

/***************************************************************************************************/

   /* ------------------- start processing parameters file ----------------------*/
   fp=fopen(argv[2], "r");
   if (fp ==  NULL)
          err_and_exit(20, myrank, argv);

   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );

   fclose(fp);

/***************************************************************************************************/
/* Lettura parametri */

   VERSIONE = -1;
   INIZIO = -1;
   APERTURA = -1;
   MODALITA = -1;
   SOGLIAX = FLT_MAX;
   SOGLIAY = FLT_MAX;
   ANGOLO = FLT_MAX;
   AMPMIN = FLT_MAX;
   DAMP = FLT_MAX;
   VERBOSE = 1;
   Npackets  = 1;                  /* valore di default */
   memoffset = 0;                  /* valore di default */
   /*  distanza massima Y (XLINE) e X (ILINE)a cui si migrano i dati*/  
   maxdisty=-1; 
   maxdistx=-1; 
   /* maxdist x e y  variabile con la profondita'*/
   distyiniz=-1;
   distxiniz=-1;
   zmaxdisty=-1;
   zmaxdistx=-1;
   zdistyiniz=-1;
   
   for (i=0; i<imax; i++)
     {

       if (strcmp("VERSIONE", ident[i]) == 0 ) /* 01 */
           sscanf(val[i], "%d", &VERSIONE);
       else if (strcmp("INIZIO", ident[i]) == 0 ) /* 05 */
           sscanf(val[i], "%d", &INIZIO);
       else if (strcmp("APERTURA", ident[i]) == 0 ) /* 06 */
           sscanf(val[i], "%d", &APERTURA);
       else if (strcmp("MODALITA", ident[i]) == 0 ) /* 04 */
           sscanf(val[i], "%d", &MODALITA);
       else if (strcmp("SOGLIAX", ident[i]) == 0 ) /* 02 */
           sscanf(val[i], "%f", &SOGLIAX);
       else if (strcmp("SOGLIAY", ident[i]) == 0 ) /* 03 */
           sscanf(val[i], "%f", &SOGLIAY);
       else if (strcmp("ANGOLO", ident[i]) == 0 ) /* 07 */       
           sscanf(val[i], "%f", &ANGOLO);
       else if (strcmp("AMPMIN", ident[i]) == 0 ) /* 08 */
           sscanf(val[i], "%f", &AMPMIN);
       else if (strcmp("DAMP", ident[i]) == 0 ) /* 09 */
           sscanf(val[i], "%f", &DAMP);
       else if (strcmp("VERBOSE", ident[i]) == 0 ) /* 10 */
           sscanf(val[i], "%d", &VERBOSE);
       else if (strcmp("NPACKETS", ident[i]) == 0 ) /* 11 */
           sscanf(val[i], "%d", &Npackets);
       else if (strcmp("MEMOFFSET", ident[i]) == 0 ) /* 11 */
           sscanf(val[i], "%d", &memoffset);
       /* distanza massima Y (XLINE)e X (ILINE) a cui si migrano i dati*/  
       else if (strcmp("MAXDISTY", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &maxdisty);        
       else if (strcmp("MAXDISTX", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &maxdistx);        
       /* maxdist x e y variabile con la profondita'*/   
       else if (strcmp("DISTYINIZ", ident[i]) == 0 )
           sscanf(val[i], "%d", &distyiniz);
       else if (strcmp("DISTXINIZ", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &distxiniz);
       else if (strcmp("ZMAXDISTY", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &zmaxdisty);        
       else if (strcmp("ZMAXDISTX", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &zmaxdistx);        
     }

/***************************************************************************************************/
/* check dei parametri  */

     if ( VERSIONE == -1 )                                            err_and_exit(21, myrank, argv);
     if ((VERSIONE != 1) && (VERSIONE != 2))                          err_and_exit(22, myrank, argv);
     if ( INIZIO == -1 )                                              err_and_exit(23, myrank, argv);
     if ( INIZIO <= 0 )                                               err_and_exit(24, myrank, argv);
     if ( APERTURA == -1 )                                            err_and_exit(25, myrank, argv);
     if (APERTURA <= 0)                                               err_and_exit(26, myrank, argv);
     if ( MODALITA == -1 )                                            err_and_exit(27, myrank, argv);
     if ((MODALITA != 1) && (MODALITA != 2))                          err_and_exit(28, myrank, argv);
     if ( SOGLIAX == FLT_MAX )                                        err_and_exit(29, myrank, argv);
     if ( SOGLIAX < 0.0 )                                             err_and_exit(30, myrank, argv);
     if ( SOGLIAY == FLT_MAX )                                        err_and_exit(31, myrank, argv);
     if ( SOGLIAY < 0.0 )                                             err_and_exit(32, myrank, argv);
     if ( AMPMIN == FLT_MAX )                                         err_and_exit(33, myrank, argv);
     if (AMPMIN < 0)                                                  err_and_exit(34, myrank, argv);
     if ( DAMP == FLT_MAX )                                           err_and_exit(35, myrank, argv);
     if ( (DAMP < 0) || (DAMP > 1))                                   err_and_exit(36, myrank, argv);
     if ( ANGOLO == FLT_MAX )                                         err_and_exit(37, myrank, argv);
     if ((ANGOLO < 0) || (ANGOLO > 90))                               err_and_exit(38, myrank, argv);
     if ((VERBOSE < 0) || (VERBOSE > 2))                              err_and_exit(39, myrank, argv);
     if ( Npackets <= 0 )                                             err_and_exit(40, myrank, argv);
     if ( memoffset < 0 )                                             err_and_exit(41, myrank, argv);
     
/* Clara3 distanza massima Y (XLINE) e X (ILINE) a cui si migrano i dati*/  
     if (maxdisty <0 && maxdisty != -1)                               err_and_exit(129, myrank, argv);
     if (maxdistx <0 && maxdistx != -1)                               err_and_exit(130, myrank, argv);
/* Clara4 maxdist x e y variabile con la profondita'*/ 
     if ( ( (distxiniz < 0) && (distxiniz !=-1) ) || (distxiniz > maxdistx) )        err_and_exit(131, myrank, argv);
     if ( ( (distyiniz < 0) && (distyiniz !=-1) ) || (distyiniz > maxdisty) )        err_and_exit(132, myrank, argv);
     if (zmaxdisty <=0 && zmaxdisty != -1)                            err_and_exit(133, myrank, argv);
     if (zmaxdistx <=0 && zmaxdistx != -1)                            err_and_exit(134, myrank, argv);  
     if ((distxiniz ==-1) && (maxdistx !=-1)) 
                            	distxiniz=maxdistx;
     if ((distyiniz ==-1) && (maxdisty !=-1)) 
                         	distyiniz=maxdisty;
     if ((zdistyiniz > zmaxdisty) || (zdistyiniz < 0 && zdistyiniz != -1))           err_and_exit(135, myrank, argv);

  /* ------------------- end processing parameters file ------------------------*/

/***************************************************************************************************/
  /* ---------------------- start processing input/output --------------------- */
   fp=fopen(argv[3], "r");
   if (fp ==  NULL)                    err_and_exit(42, myrank, argv);
   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
   fclose(fp);

   /* assegnazione delle variabili */
   callocKTAChar(  &SourceFile, MAX_STRING_LEN, 105, myrank, argv);
   callocKTAChar(  &TargetFile, MAX_STRING_LEN, 106, myrank, argv);
   contMem += MAX_STRING_LEN*2*sizeof(char);

   strcpy(SourceFile, "\0");
   strcpy(TargetFile, "\0");

/***************************************************************************************************/
/* Lettura dei parametri */

   TARGET = -1;
   UPDOWN = -1;
   OUTPUT = -1;

   OUT_A   = 1;
   OUT_T   = 1;
   OUT_UX  = 1;
   OUT_UY  = 1;
   OUT_UZ  = 1;
   
   QUANT_FLAG=0;

   for (i=0; i<imax; i++)
     {
       if (strcmp("SOURCE-FILE", ident[i]) == 0 ) /* 01 */
           sscanf(val[i], "%s", SourceFile);
       else if (strcmp("TARGET", ident[i]) == 0 ) /* 02 */
           sscanf(val[i], "%d", &TARGET);
       else if (strcmp("TARGET-FILE", ident[i]) == 0 ) /* 03 */
           sscanf(val[i], "%s", TargetFile);
       else if (strcmp("QUANT_FLAG", ident[i]) == 0 ) /* 02 */
           sscanf(val[i], "%d", &QUANT_FLAG);
       else if (strcmp("UPDOWN", ident[i]) == 0 ) /* 04 */
           sscanf(val[i], "%d", &UPDOWN);
       else if (strcmp("OUTPUT", ident[i]) == 0 ) /* 05 */
           sscanf(val[i], "%d", &OUTPUT);
       else if ((strcmp("OUT_A", ident[i]) == 0 ) || /* 06 */
                (strcmp("AMPIEZZA", ident[i]) == 0 ))
           sscanf(val[i], "%d", &OUT_A);
       else if ((strcmp("OUT_T", ident[i]) == 0 ) || /* 07 */
                (strcmp("TRAVEL", ident[i]) == 0 ))
           sscanf(val[i], "%d", &OUT_T);
       else if ((strcmp("OUT_UX", ident[i]) == 0) || /* 08 */
                (strcmp("COSENOX", ident[i]) == 0 ))
           sscanf(val[i], "%d", &OUT_UX);
       else if ((strcmp("OUT_UY", ident[i]) == 0) || /* 09 */
                (strcmp("COSENOY", ident[i]) == 0 ))
           sscanf(val[i], "%d", &OUT_UY);
       else if ((strcmp("OUT_UZ", ident[i]) == 0) || /* 10 */
                (strcmp("COSENOZ", ident[i]) == 0 ))
           sscanf(val[i], "%d", &OUT_UZ);
     }

/***************************************************************************************************/
     /* check dei parametri  */
     if ( strlen(SourceFile) == 0 )                                  err_and_exit(43, myrank, argv);
     if ( TARGET == -1 )                                             err_and_exit(44, myrank, argv);
     if ( (TARGET != 1) && (TARGET != 2) )                           err_and_exit(45, myrank, argv);
     if ( TARGET == 2 ) 
         if ( strlen(TargetFile) == 0 )                              err_and_exit(46, myrank, argv);
     if ( UPDOWN == -1 )                                             err_and_exit(47, myrank, argv);
     if ( (UPDOWN != 1) && (UPDOWN != 2) )                           err_and_exit(48, myrank, argv);
     if ( OUTPUT == -1 )                                             err_and_exit(49, myrank, argv);
     if ((OUTPUT != 1) && (OUTPUT != 2))                             err_and_exit(50, myrank, argv);
     if ( (OUT_A != 0) && (OUT_A != 1) )                             err_and_exit(51, myrank, argv);
     if ( (OUT_T != 0) && (OUT_T != 1) )                             err_and_exit(52, myrank, argv);
     if ( (OUT_UX != 0) && (OUT_UX != 1) )                           err_and_exit(53, myrank, argv);
     if ( (OUT_UY != 0) && (OUT_UY != 1) )                           err_and_exit(54, myrank, argv);
     if ( (OUT_UZ != 0) && (OUT_UZ != 1) )                           err_and_exit(55, myrank, argv);
     if ( (QUANT_FLAG != 0) && (QUANT_FLAG != 1) )                   err_and_exit(136, myrank, argv);

     index_T = -1;  
     index_A = -1;  
     index_UX = -1;  
     index_UY = -1;  
     index_UZ = -1;  
     index_cur=0;
     if (OUT_A == 1)
          index_A = index_cur++; 
     if (OUT_T == 1)
          index_T = index_cur++; 
     if (OUT_UX == 1)
          index_UX = index_cur++;
     if (OUT_UY == 1)
          index_UY = index_cur++;
     if (OUT_UZ == 1)
          index_UZ = index_cur++;
          
     printf("index_curr %i index_T %i index_UX %i index_UY %i index_UZ %i \n", index_cur, index_T, index_UX, index_UY, index_UZ);
 /***************************************************************************************************/

      fp = fopen(SourceFile, "rb");
       if (fp ==  NULL)             err_and_exit(56, myrank, argv);
       fseek(fp, 0, SEEK_END);
       numbyte=ftell(fp);
      fclose(fp);
/***************************************************************************************************/
       Nshot =(long)(numbyte/(sizeof(float)*3));
/***************************************************************************************************/

       if (TARGET == 2)
         {
           fp = fopen(TargetFile, "rb");
            if (fp ==  NULL)                err_and_exit(57, myrank, argv);
            fseek(fp, 0, SEEK_END);
            numbyte=ftell(fp);
           fclose(fp);
/***************************************************************************************************/
           Ntgt =(long)(numbyte/(sizeof(float)*3));
/***************************************************************************************************/
         }

   /* ------------------- end processing input/output file ----------------------*/


   /* --------------------------- start processing output directory ------------------------------*/

   callocKTAChar(  &OutDir, MAX_STRING_LEN, 107, myrank, argv);
   contMem += MAX_STRING_LEN*sizeof(char);
   strcpy(OutDir, argv[4]);
   lstr = strlen(OutDir);
   if (OutDir[lstr-1] != '/')
       if (lstr < (MAX_STRING_LEN-2))
           OutDir[lstr]='\0';
       else
            err_and_exit(26, myrank, argv);

    /* Controllo esistenza dir */
    err = stat(OutDir, &statbuf);
    if(err != 0)      err_and_exit(111, myrank, argv);
    OutDir[lstr]='/';
    OutDir[lstr+1]='\0';
    LenOutDir= strlen(OutDir);
   /* ------------------- end processing output directory --------------*/

/***************************************************************************************************/

   /**************** CONTROLLI iniziali sul modello ************************************************/
      if (Nshot < 1)                                                  err_and_exit(59, myrank, argv);
      if ((d1 <= 0.0) || (d2 <= 0.0) || (d3 <= 0.0))                  err_and_exit(60, myrank, argv);
      if ((n1 <= 0.0) || (n2 <= 0.0) || (n3 <= 0.0))                  err_and_exit(61, myrank, argv);

/***************************************************************************************************/
   /***************** STAMPA A VIDEO DEI PARAMETRI  *****************/
      if ((VERBOSE >0) &&(myrank == iope))
      {
      printf("\n\t***** PARAMETRI *****\n");
      printf("Versione (1=veloce; 2=impiego minimo memoria)  : %d\n", VERSIONE);
      printf("Propagaz. verso il basso (1) o verso l'alto (2): %d\n", UPDOWN);
      printf("Apertura iniziale [# campioni]                 : %d\n",INIZIO);
      printf("Apertura fronte [# campioni]                   : %d\n",APERTURA);
      printf("Angolo massimo accettabile rispetto a z [gradi]: %f\n",ANGOLO);
      printf("Ampiezza minima                                : %f\n",AMPMIN);
      printf("Fattore smorzamento estrapolatore              : %f\n",DAMP);
      printf("Selezione fronte veloce (1) o energetico (2)   : %d\n",MODALITA);
      printf("Soglia di selezione in direzione x (fraz. dx)  : %f\n",SOGLIAX);
      printf("Soglia di selezione in direzione y (fraz. dy)  : %f\n",SOGLIAY);
      printf("- AREA ILLUMINATA \n");
      if ((maxdisty!=-1) && (maxdisty==distyiniz)){
      printf("Distanza massima Y (XLINE) a cui si migrano i dati [m]          : %d\n", maxdisty);
      } 
      else if (maxdisty!=-1) {
      printf("Distanza iniziale Y (XLINE) a cui si migrano i dati [m]         : %d\n", distyiniz);
      printf("Profondita' max a distanza y costante [m]                       : %d\n", zdistyiniz);
      printf("Distanza massima Y (XLINE) a cui si migrano i dati [m]          : %d\n", maxdisty);
      printf("Profondita' di transizione a distanza massima Y (XLINE) [m]     : %d\n", zmaxdisty);	
      }	
      if ((maxdistx!=-1) && (maxdistx==distxiniz)){
      printf("Distanza massima X (INLINE) a cui si migrano i dati [m]         : %d\n", maxdistx);
      } 
      else if (maxdistx!=-1) {
      printf("Distanza iniziale X (INLINE) a cui si migrano i dati [m]        : %d\n", distxiniz);
      printf("Distanza massima X (INLINE) a cui si migrano i dati [m]         : %d\n", maxdistx);
      printf("Profondita' di transizione a distanza massima X (INLINE) [m]    : %d\n", zmaxdistx);	
      }
      printf("File contenente punti sorgenti                 : %s\n",SourceFile);
      printf("Numero shot                                    : %d\n", Nshot);
      printf("Target (1=VOLUME; 2=SET DI PUNTI)              : %d\n",TARGET);
      if (TARGET == 2)
        {
          printf("File contenente punti del target               : %s\n",TargetFile);
          printf("Numero punti target                            : %d\n", Ntgt);
        }
      printf("Directory di uscita                            : %s\n",OutDir);
      printf("\n");
      printf("Grandezze salvate: \n");
      printf("Tipo Output (1=file per src; 2=database): %d\n",OUTPUT);
      printf("Ampiezza         (0=NO; 1=SI)           : %d\n",OUT_A);
      printf("Travel time      (0=NO; 1=SI)           : %d\n",OUT_T);
      printf("Componente x slowness (0=NO; 1=SI)      : %d\n",OUT_UX);
      printf("Componente y slowness (0=NO; 1=SI)      : %d\n",OUT_UY);
      printf("Componente z slowness (0=NO; 1=SI)      : %d\n",OUT_UZ);
  /*  Esposizione a video dei parametri descrittori del modello considerato  */
      printf("\n");
      printf("DESCRIZIONE DEL MODELLO\n");
      printf("Origine asse x [m]     : %f\n",oxmod);
      printf("Passo asse x [m]       : %f\n",dxmod);
      printf("Dimensione asse x [m]  : %f\n",(nxmod-1)*dxmod);
      printf("N. campioni asse x     : %d\n\n",nxmod);
      printf("\n");
      printf("Origine asse y  [m]    : %f\n",oymod);
      printf("Passo asse y  [m]      : %f\n",dymod);
      printf("Dimensione asse y [m]  : %f\n",(nymod-1)*dymod);
      printf("N. campioni asse y     : %d\n\n",nymod);
      printf("\n");
      printf("Origine asse z [m]     : %f\n",ozmod);
      printf("Passo asse z [m]       : %f\n",dzmod);
      printf("Dimensione asse z [m]  : %f\n",(nzmod-1)*dzmod);
      printf("N. campioni asse z     : %d\n\n",nzmod);
      printf("\n");
      printf("Sotto modello (1=NO; 2=SI): %d\n",SOTTOMOD);
      if (SOTTOMOD == 2)
        {
          printf("\tDESCRIZIONE SOTTOMODELLO\n");
          printf("\tSottocampionamento asse x [# campioni]: %d\n",pxminimod);
          printf("\tSottocampionamento asse y [# campioni]: %d\n",pyminimod);
          printf("\tSottocampionamento asse z [# campioni]: %d\n",pzminimod);
          printf("\tOrigine asse x [m] e passo [m]        : %f - %f\n",oxminimod,d1);
          printf("\tOrigine asse y [m] e passo [m]        : %f - %f\n",oyminimod,d2);
          printf("\tOrigine asse z [m] e passo [m]        : %f - %f\n",ozminimod,d3);
          printf("\tN. campioni asse x                    : %d\n",nxminimod);
          printf("\tN. campioni asse y                    : %d\n",nyminimod);
          printf("\tN. campioni asse z                    : %d\n",nzminimod);
        }
     }

/***************************************************************************************************/
     for (i=0; i< MAX_PARAM; i++)
       {
         free(ident[i]);
         free(val[i]);
        }
/***************************************************************************************************/
/************* Definizione puntatori per l'ALLOCAZIONE DINAMICA DELLA MEMORIA **********************/
      callocKTAFloat( &xshot, Nshot, 62, myrank, argv);
      callocKTAFloat( &yshot, Nshot, 63, myrank, argv);      
      callocKTAFloat( &zshot, Nshot, 64, myrank, argv);
      contMem += Nshot*3*sizeof(float);
      
      if ( myrank != iope)
      {
         callocKTAFloat( &S, n1*n2*n3, 65, myrank, argv);
         callocKTAFloat( &A, n1*n2*2, 66, myrank, argv);
         callocKTAFloat( &T, n1*n2*2, 67, myrank, argv);
         callocKTAFloat( &Px, n1*n2*2, 68, myrank, argv);
         callocKTAFloat( &Py, n1*n2*2, 69, myrank, argv);
         callocKTAFloat( &Pz, n1*n2*2, 70, myrank, argv);

      } /* end if ( myrank != iope) */     
      
      if (TARGET == 1)
        {
          Ntgt=n1*n2*n3;
          Ntgt1=1;
        }
      else if (TARGET==2)
          Ntgt1=Ntgt;

/***************************************************************************************************/
      /* ROB start - allocazione contigua*/

      if(OUT_A == 1)
         NtgtAA = Ntgt;
      else
         NtgtAA = 1;

      if(OUT_T == 1) 
         NtgtTT = Ntgt;
      else
           NtgtTT = Ntgt;
/*         NtgtTT = 1;      */

     if ( (OUT_UX == 1) || (OUT_UY == 1) || (OUT_UZ == 1) )
     {
      NtgtPPX = Ntgt;
      NtgtPPY = Ntgt;
      NtgtPPZ = Ntgt;
     }
     else
     {
      NtgtPPX = 1;
      NtgtPPY = 1;
      NtgtPPZ = 1;	
     }
      

      AA_size  = NtgtAA * sizeof(float);
      TT_size  = NtgtTT * sizeof(float);
      PPx_size = NtgtPPX * sizeof(float);
      PPy_size = NtgtPPY *sizeof(float);
      PPz_size = NtgtPPZ * sizeof(float);
      N_AATTPP=NtgtAA+NtgtTT+NtgtPPX+NtgtPPY+NtgtPPZ;
      AATTPP_size = AA_size + TT_size + PPx_size + PPy_size + PPz_size + 5 * memoffset * sizeof(float);

      AATTPP=malloc(N_AATTPP*sizeof(float));
      contMem += N_AATTPP*sizeof(float);
      if (AATTPP == NULL)                                               err_and_exit(71, myrank, argv);

      AA = AATTPP;
      TT = AA + NtgtAA + memoffset;
      PPx = TT + NtgtTT + memoffset;
      PPy = PPx + NtgtPPX + memoffset;
      PPz = PPy + NtgtPPY + memoffset;
      
      /* ROB end - allocazione contigua*/
/***************************************************************************************************/
      /* ROB start: impacchettamento*/
      
      requests = calloc(Npackets, sizeof(MPI_Request));
      contMem += Npackets*sizeof(MPI_Request)/2;
      if (requests == NULL)                                             err_and_exit(72, myrank, argv);

      statuses = calloc(Npackets, sizeof(MPI_Status));
      contMem += Npackets*sizeof(MPI_Status)/2;
      if (statuses == NULL)                                             err_and_exit(73, myrank, argv);
      q = (AATTPP_size /  sizeof(float)) / Npackets;
      r = (AATTPP_size /  sizeof(float)) % Npackets;

      Pbuffer = calloc(q + 1, sizeof(float));
      contMem += (q+1)*2;
      if (Pbuffer == NULL)                                              err_and_exit(74, myrank, argv);

      /* ROB end: impacchettamento*/
/***************************************************************************************************/
      callocKTAFloat( &xtgt, Ntgt1, 75, myrank, argv);
      callocKTAFloat( &ytgt, Ntgt1, 76, myrank, argv);
      callocKTAFloat( &ztgt, Ntgt1, 77, myrank, argv);
      contMem += Ntgt1*3*sizeof(float);

      if ( myrank != iope)
       {      
         callocKTAFloat( &Tcorr, n1*n2, 78, myrank, argv);
         callocKTAFloat( &Acorr, n1*n2, 79, myrank, argv);
         callocKTAFloat( &Pxcorr, n1*n2, 80, myrank, argv);
         callocKTAFloat( &Pycorr, n1*n2, 81, myrank, argv);
         callocKTAFloat( &xcorr, n1*n2, 82, myrank, argv);
         callocKTAFloat( &ycorr, n1*n2, 83, myrank, argv);
         callocKTAInt( &Ok1, n1*n2, 84, myrank, argv);
         callocKTAInt( &Ok2, n1*n2, 85, myrank, argv);
         callocKTAInt( &Posx, (n1+3)*(n2+3)*2, 86, myrank, argv);
         callocKTAInt( &Posy, (n1+3)*(n2+3)*2, 87, myrank, argv);   
         /* Allocazione memoria per le derivate parziali della slowness solo se necessario      */
         if (VERSIONE == 1)
          n3ver=n3;
         else
          n3ver=1;
         callocKTAFloat( &Sx, n1*n2*n3ver, 88, myrank, argv);
         callocKTAFloat( &Sy, n1*n2*n3ver, 89, myrank, argv);
         callocKTAFloat( &vettdistx, n3, 127, myrank, argv);
         callocKTAFloat( &vettdisty, n3, 128, myrank, argv);
       } /* if ( myrank != iope) */
     
    callocKTAShortInt( &shortbuf, Ntgt*index_cur, 113, myrank, argv);
    contMem += Ntgt*index_cur*sizeof(short int);       
    
    nRangeA = 1; nRangeT = 1; nRangeCx = 1; nRangeCy = 1; nRangeCz = 1;     
    if ( OUT_A == 1)     	nRangeA  = Nshot*2;
    if ( OUT_T == 1)     	nRangeT  = Nshot*2;
    if ( OUT_UX == 1)     	nRangeCx = Nshot*2;
    if ( OUT_UY == 1)     	nRangeCy = Nshot*2;
    if ( OUT_UZ == 1)     	nRangeCz = Nshot*2;
    
    nRangeTot = nRangeA + nRangeT + nRangeCx + nRangeCy + nRangeCz;
    callocKTAFloat( &rangeA,  nRangeTot,  116, myrank, argv);        
    rangeT  = rangeA + nRangeA;
    rangeCx = rangeT + nRangeT;
    rangeCy = rangeCx + nRangeCx;
    rangeCz = rangeCy + nRangeCy;
          
    callocKTAFloat( &rangeA_recv,  nRangeTot,  116, myrank, argv);        
    rangeT_recv  = rangeA_recv  + nRangeA;
    rangeCx_recv = rangeT_recv  + nRangeT;
    rangeCy_recv = rangeCx_recv + nRangeCx;
    rangeCz_recv = rangeCy_recv + nRangeCy;
    contMem += nRangeTot*sizeof(float)*2;  

/***************************************************************************************************/
/****************** LETTURA DELLA POSIZIONE DELLE SORGENTI *****************/

       fdesc=0;
       strcpy(mode,"rb");
       glob_fopen_(&fdesc, SourceFile, mode, &file_error, strlen(SourceFile), strlen(mode));
       if (file_error != 0)                                            err_and_exit(90, myrank, argv);

       if ((VERBOSE > 1) && (myrank == iope))
                printf("\nLettura file shot : %s\n", SourceFile);
      
       num=Nshot;
       dim = sizeof(float);
       glob_fread_(&fdesc, xshot, &dim, &num, &file_error);
       glob_fread_(&fdesc, yshot, &dim, &num, &file_error);
       glob_fread_(&fdesc, zshot, &dim, &num, &file_error);

       glob_fclose_(&fdesc, &file_error);
       strcpy(mode," ");

       if ((VERBOSE > 1) && (myrank == iope))
                 printf("Letti N. %d punti.\n", Nshot);

/***************************************************************************************************/
   /*----------- controlli iniziali sulle posizioni degli shot ---------------*/
      for (j=0; j<Nshot; j++)
        {
          if (( xshot[j] < o1 )  || ( xshot[j] > o1+(n1-1)*d1 ) )
             {
               fprintf(stderr, "ERROR:(%d : %f, %f, %f) \n", j+1,xshot[j],yshot[j],zshot[j]);
               err_and_exit(91, myrank, argv);
             }
          if (( yshot[j] < o2 ) || ( yshot[j] > o2+(n2-1)*d2 ) )
             {
               fprintf(stderr, "ERROR:(%d : %f, %f, %f) \n", j+1,xshot[j],yshot[j],zshot[j]);
               err_and_exit(92, myrank, argv);
             }
          if (( zshot[j] < o3 ) || ( zshot[j] > o3+(n3-1)*d3 ) )
             {
               fprintf(stderr, "ERROR:(%d : %f, %f, %f) \n", j+1,xshot[j],yshot[j],zshot[j]);
               err_and_exit(93, myrank, argv);
             }
        }

/***************************************************************************************************/
/****************** LETTURA DELLA POSIZIONE DEI PUNTI DEL TARGET *****************/
   if (TARGET == 2)
     {
       fdesc=0;
       strcpy(mode,"rb");

       glob_fopen_(&fdesc, TargetFile, mode, &file_error, strlen(TargetFile), strlen(mode));
       if (file_error != 0)            err_and_exit(94, myrank, argv);

       if ((VERBOSE > 1) && (myrank == iope))
                printf("\nLettura file punti del target : %s\n", TargetFile);

       num=Ntgt;
       dim = sizeof(float);
       glob_fread_(&fdesc, xtgt, &dim, &num, &file_error);
       glob_fread_(&fdesc, ytgt, &dim, &num, &file_error);
       glob_fread_(&fdesc, ztgt, &dim, &num, &file_error);

       glob_fclose_(&fdesc, &file_error);
       strcpy(mode," ");

       if ((VERBOSE > 1) && (myrank == iope))
                printf("Letti N. %d punti.\n", Ntgt);

/***************************************************************************************************/
      /*--------- controlli iniziali sulle posizioni dei punti del target ---------------*/
      for (j=0; j<Ntgt; j++)
      {
          if ( ( xtgt[j] < o1 )  || ( xtgt[j] > o1+(n1-1)*d1 ) )
             {
               fprintf(stderr, "ERROR: target x(%d) \n",j+1);
               err_and_exit(95, myrank, argv);
             }
          if ( ( ytgt[j] < o2 ) || ( ytgt[j] > o2+(n2-1)*d2 ) )
             {
               fprintf(stderr, "ERROR: target y(%d) \n",j+1);
               err_and_exit(96, myrank, argv);
             }
          if ( ( ztgt[j] < o3 ) || ( ztgt[j] > o3+(n3-1)*d3 ) )
             {
               fprintf(stderr, "ERROR: target z(%d) \n",j+1);
               err_and_exit(97, myrank, argv);
             }
       }
   }
   
/* CAN&BN: allocazione della memoria per il buffer di scrittura */
   callocKTAInt( &shotSort,  Nshot, 137, myrank, argv);
   contMem += Nshot*2;
   if ( myrank == iope)
   {
      dimWriteBuf = maxMem - contMem;
      dimWriteBuf = Ntgt * index_cur * (dimWriteBuf/Ntgt/index_cur); /* Fa in modo che la dimensione sia un multiplo di Ntgt*3 */
      writeBufferFloat = calloc( dimWriteBuf, sizeof(float));
      writeBufferShort = (short int *) writeBufferFloat;
      printf("======> Allocati %i elementi per l'array writeBuffer \n", dimWriteBuf);     
      if (writeBufferFloat == NULL)                                        err_and_exit(113, myrank, argv);
   }
/***************************************************************************************************/
/******************  LETTURA DEL MODELLO DI VELOCITA' ************************/
   if ((VERBOSE > 1) && (myrank == iope))
            printf("Reading velocity model : %s\n\n", DataFileIn);
   else
   {
      fdesc=0;
      strcpy(mode,"rb");
      glob_fopen64_(&fdesc, DataFileIn, mode, &file_error, strlen(DataFileIn), strlen(mode));
      if (file_error != 0)                                             err_and_exit(98, myrank, argv);
      off64 =(int64)1;                                     /* R64 lug 2001 */
      origin=0;
      glob_fseek64_(&fdesc,&off64,&origin, &file_error);   /* R64 lug 2001 - rewind vel model */
      dim = sizeof(float);
   
      if ( SOTTOMOD == 1 )
         {
           num=n1*n2*n3;
           glob_fread_(&fdesc,S, &dim, &num, &file_error);
         }
   
      if ( SOTTOMOD == 2 )
         {
          for (k=0; k<n3; k++)
              for (j=0; j<n2; j++)
                  for (i=0;i<n1; i++)
                    {
                      /* R64 lug 2001 */
                      off64 = ( (int64)(i*pxminimod+inxminimod-1) +
                                (int64)(j*pyminimod+inyminimod-1) * (int64)nxmod +
                                (int64)(k*pzminimod+inzminimod-1) * (int64)nxmod * (int64)nymod )*
                                (int64)dim + (int64)1;
                      glob_fseek64_(&fdesc,&off64,&origin, &file_error);     /* R64 lug 2001 */
                      num=1;
                      glob_fread_(&fdesc,&S[i+j*n1+k*n1*n2], &dim, &num, &file_error);
                    }   
         }
   
      glob_fclose64_(&fdesc, &file_error);      /* R64 lug 2001 */
      strcpy(mode," ");
   }
/***************************************************************************************************/
/**************** CHIAMATA FUNZIONE PRINCIPALE: EXECUTE **************************/
   if  (myrank == iope)
   {
   printf("\nCalling main program ....\n");
   execute_iope_(&o1, &o2, &o3, &d1, &d2, &d3, &n1, &n2, &n3, &esize,
            &UPDOWN, 
            &Nshot, xshot, yshot, zshot,

            &TARGET, &Ntgt, &Ntgt1, xtgt, ytgt, ztgt,

            AA, TT, PPx, PPy, PPz,

            OutDir, &LenOutDir, &OUTPUT, &OUT_A, &OUT_T,
            &OUT_UX, &OUT_UY, &OUT_UZ, &VERBOSE,

            &myrank, &size, &iope, &Npackets, &q, &r, Pbuffer, requests, statuses,

            &NtgtAA, &NtgtTT, &NtgtPPX, &NtgtPPY, &NtgtPPZ, 
            
            rangeA, rangeT, rangeCx, rangeCy, rangeCz, rangeA_recv, 
            &nRangeA, &nRangeT, &nRangeCx, &nRangeCy, &nRangeCz,
            
            writeBufferFloat, writeBufferShort, &dimWriteBuf, shotSort, &QUANT_FLAG, &index_cur,
            &index_A, &index_T, &index_UX, &index_UY, &index_UZ);
   }
   else
   {

   execute_(&o1, &o2, &o3, &d1, &d2, &d3, &n1, &n2, &n3, &esize,
            &n3ver, S, Sx, Sy,

            &SOGLIAX, &SOGLIAY, &MODALITA, &APERTURA, &ANGOLO,
            &INIZIO, &UPDOWN, &VERSIONE, &AMPMIN, &DAMP,

            &Nshot, xshot, yshot, zshot,

            &TARGET, &Ntgt, &Ntgt1, xtgt, ytgt, ztgt,

            A,  T,  Px,  Py,  Pz,
            AA, TT, PPx, PPy, PPz,
            xcorr, ycorr, Acorr, Pxcorr, Pycorr,
            Ok2, Posx, Posy, Ok1,

            OutDir, &LenOutDir, &OUTPUT, &OUT_A, &OUT_T,
            &OUT_UX, &OUT_UY, &OUT_UZ, &VERBOSE,Tcorr,

            &myrank, &size, &iope, &Npackets, &q, &r, Pbuffer, requests, statuses,

            &NtgtAA, &NtgtTT, &NtgtPPX, &NtgtPPY, &NtgtPPZ, 
                      
            vettdistx, vettdisty, &maxdisty, &maxdistx, &distxiniz, &zmaxdistx, &distyiniz, &zmaxdisty, &zdistyiniz,
            
            rangeA, rangeT, rangeCx, rangeCy, rangeCz, rangeA_recv, 
            &nRangeA, &nRangeT, &nRangeCx, &nRangeCy, &nRangeCz, &QUANT_FLAG );
   }       
   /* Scrittura dei file con i range */
   if (myrank == iope)
   {
      elemtowrite=Nshot*2;     
      

      if ( (OUT_T == 1) && (QUANT_FLAG == 1))
         writeBinFile( "rangeT.sht", OutDir, myrank, argv, rangeT_recv, elemtowrite, 119);
      
      if ( (OUT_A == 1) && (QUANT_FLAG == 1))
         writeBinFile( "rangeA.sht", OutDir, myrank, argv, rangeA_recv, elemtowrite, 120);
      
      if ( (OUT_UX == 1) && (QUANT_FLAG == 1) && (UPDOWN == 2))
         writeBinFile( "rangeCx.sht", OutDir, myrank, argv, rangeCx_recv, elemtowrite, 114);

      if ( (OUT_UY == 1) && (QUANT_FLAG == 1) && (UPDOWN == 2))
         writeBinFile( "rangeCy.sht", OutDir, myrank, argv, rangeCy_recv, elemtowrite, 115);

      if ( (OUT_UZ == 1) && (QUANT_FLAG == 1) && (UPDOWN == 2))
         writeBinFile( "rangeCz.sht", OutDir, myrank, argv, rangeCz_recv, elemtowrite, 121);
         
/* CAN&BN: gestione della scrittura seriale*/
/*         Scrittura del file di sorting */

         writeBinFile( "sortGreenF.sht", OutDir, myrank, argv, shotSort, Nshot, 122);
   }


/***************************************************************************************************/
/************************** DISALLOCAZIONE MEMORIA *****************************/
      free(xshot);      free(yshot);      free(zshot);
      free(xtgt);       free(ytgt);       free(ztgt);
      
      /* ROB start : deallocazione memoria contigua */ 
      free(AATTPP);
      free(rangeA);
      /* ROB end : deallocazione memoria contigua */

      free(requests);
      free(statuses);
      free(Pbuffer);

      free(HeaderFileIn);
      free(DataFileIn);
      free(TargetFile);
      free(SourceFile);
      free(OutDir);
      
      if(myrank != iope)
      {
         free(S);          free(Sx);         free(Sy);
         free(A);          free(T);
         free(Px);         free(Py);         free(Pz);
         free(Pxcorr);     free(Pycorr);
         free(xcorr);      free(ycorr);
         free(Acorr);
         free(Ok1);        free(Ok2);
         free(Posx);       free(Posy);      
         free(vettdistx);  free(vettdisty);
      }
  
/***************************************************************************************************/

      if ((VERBOSE > 0) && (myrank == iope))
               printf("That's all folks!\n\n");
               
      MPI_Finalize();
      exit(EXIT_SUCCESS);
}



/***************************************************************************************************/
/***************************************************************************************************/
/***************************************************************************************************/


void writeBinFile( char *rangeFileName, char *OutDir, int myrank, char *argv[], float *range, int elemtowrite, int err)
{
	
	char mode[MAX_STRING_LEN];
	char rangeFile[MAX_STRING_LEN];
	int file_error, dim, fdesc;
	
	dim = sizeof(float);
	
	strcpy(mode,"wb");
	
        rangeFile[0]='\0';
        strcpy( rangeFile, OutDir);
        strcat( rangeFile, rangeFileName);
   
        glob_fopen64_(&fdesc, rangeFile, mode, &file_error, strlen(rangeFile), strlen(mode));
        if (file_error != 0)                                                      
                 err_and_exit(err, myrank, argv);
        glob_fwrite_(&fdesc, range, &dim, &elemtowrite, &file_error);
        glob_fclose_(&fdesc, &file_error);
}