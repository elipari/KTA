/****************************************************/
/*                   start.be.c                     */
/****************************************************/
#include "./3_common_defs.h"

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
/* argv[1] = traces parameters                   */
/* argv[2] = migrator parameters                 */
/* argv[3] = acquisition and target files        */
/* argv[4] = output directory                    */
/*************************************************/

#include <mpi.h>

int main(int argc, char *argv[])
{
    FILE *fp;
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    float *grid_el;  	
    int i, j, k,imax, lstr;
    float t,u,v;

    int fidsgy, fpesi;

    char *mode;
    int dim;
    char filename[MAX_STRING_LEN];
    char filenum[20];
    int file_error;

    char st[MAX_STRING_LEN];
    char st1[MAX_STRING_LEN];
    char in[MAX_STRING_LEN];
    char dataformat[MAX_STRING_LEN];
    char title[MAX_STRING_LEN];
    int test,tempRick, esize, n3;

/* -------- Superficie acquisizione ----------- */
    char  acq_file[MAX_STRING_LEN];
    int acq_type;
    TGTptr startacq, curacq;

    int   acq_npan, acq_nt_tot;
    int   acq_nna_max, acq_nnb_max, acq_nnc_max;
    int   acq_rra_max, acq_rrb_max, acq_rrc_max;

    /* cambiamento sistema riferimento */
    float *acq_o1,  *acq_o2,  *acq_o3;
    float *acq_ca1, *acq_ca2, *acq_ca3;
    float *acq_cb1, *acq_cb2, *acq_cb3;
    float *acq_cc1, *acq_cc2, *acq_cc3;

    /* bassa risoluzione */
    float *acq_da, *acq_db, acq_dc;
    int   *acq_na, *acq_nb, acq_nc;
    int   *acq_nt;

    /* alta risoluzione */
    float *acq_dda, *acq_ddb, acq_ddc;
    int   *acq_nna, *acq_nnb, acq_nnc;
    int   *acq_nnt;

/* ------------  Target ------------- */
    char  tg_file[MAX_STRING_LEN];
    int   tg_type;
    TGTptr starttgt, curtgt;

    int   tg_npan, tg_nt_tot;
    int   tg_nna_max, tg_nnb_max, tg_nnc_max;
    float tg_rra, tg_rrb, tg_rrc;
    int   tg_rra_max, tg_rrb_max, tg_rrc_max;

    /* cambiamento sistema riferimento */
    float *tg_o1,  *tg_o2,  *tg_o3;
    float *tg_ca1, *tg_ca2, *tg_ca3;
    float *tg_cb1, *tg_cb2, *tg_cb3;
    float *tg_cc1, *tg_cc2, *tg_cc3;

    /* bassa risoluzione */
    float *tg_da, *tg_db, tg_dc;
    int   *tg_na, *tg_nb, tg_nc;
    int   *tg_nt;

    /* alta risoluzione */
    float *tg_dda, *tg_ddb, tg_ddc;
    int   *tg_nna, *tg_nnb, tg_nnc;
    int   *tg_nnt;

    /* matrici pesi per interpolazione */
    float *tg_weights_lin_a, *tg_weights_lin_b, *tg_weights_lin_c;
    float *tg_weights_cub_b, *tg_weights_cub_c;
    
    float u2, u3, v2, v3;
    
    /* dimensione degli indici delle matrici dei pesi*/
    int   w_a, w_b, w_c;
    
/* -------- Database funzioni Green ------- */
    char dbDir[MAX_STRING_LEN];
    int LendbDir;
    char fpdir[MAX_STRING_LEN];
    int lenfpdir;

    float* SA;
    float* ST;

    float* RA;
    float* RT;
    float* RUz;
    
    /*AVA*/
    float* RVx;
    float* RVy;
    float* RVz;
    float* SVx;
    float* SVy;
    float* SVz;    
    
/* ------------  Migratore ------------- */
    int   bordf;
    float semiap;
    float smussamento;
    float soglia_ampiezze;
    int   imaging;
    int   ava;
    int   verbose;
/* ----- fattore di decimazione delle tracce*/  
    int   datadecimation;
/* ----- Apertura di migrazione-------------*/
    float apeiniz;
    float zetamaxape; 
    int   maxdisty,maxdistx;
    int   distyiniz,distxiniz,zmaxdistx,zmaxdisty;
    int   zdistyiniz;
/*---- parametri di mute*/
    int   muteFlag;
    char  muteFileName[MAX_STRING_LEN];
    int   old_stepz, stepz, dz;
    float dip_mute;
/*------ AVA parametri*/
    float az_dip_min, az_dip_max, az_dip_step, el_dip_min, el_dip_max, el_dip_step;
    float dip_min, dip_max, azimuth_min, azimuth_max , step_dip,  step_az;
    int   n_dip, n_az, tg_nt_ava, n_az_dip, n_el_dip;
/*------ AVO parametri*/
    int  step_off;
    int   n_off, avo;
            
/* ------------   Tracce   ------------- */
    SEGYptr startsgy, cursgy;
    float vel_sup;

    int nf;
    int primobitns, nshotnbytes;
    int primobitxs, xsnbytes;
    int primobitys, ysnbytes;
    int primobitzs, zsnbytes;
    int primobitxr, xrnbytes;
    int primobityr, yrnbytes;
    int primobitzr, zrnbytes;
    int primatr;
    int ntraccefile;
    /*----------- decimazione delle tracce*/
    int ntraccefiledec;
    
    int max_ncamp, max_ncampfilt;
    int ncamp, ncampfilt,maxnrblocchitr;
    int formato;
    float dt;
    float dx;
    float tshift;

    float a11, a12, a21, a22;
    float b1, b2;

    int64 sgy64;

    float* tracce;
    float* traces;
    float* traccia_temp;
    float* traccia_letta;
    char*  traccia_lettac;
    short int* traccia_lettash;    
    float* traccia1;
    float* traccia2;
    float *tr_xr, *tr_yr, *tr_zr;

    int64 *binr_offset;
    int64 *bins_offset;

    float *tr_xs, *tr_ys, *tr_zs;

/* CCC 06/05/2002 MPI- Upgrade tr_ns da integer*4 --> integer*8 per fare un'unico MPI_BCAST di integer
                       in execute */
    int64 *tr_ns;
/*    int *tr_ns; */
    float* tr_weights;

/* ---  Lookup table per arctan             ---*/
    int atn;
    float *attable;
/* ---  Strutture dati per immagini migrate ---*/

    int dimgruppotr;
    int nrgruppitr;
    int maxgruppi;
    int max_ntracce,max_ntracce_old;
    int maxdimbloccotr;
    int nrblocchitr;
    int dimbloccotr;
    int offmin,offmax;

    float *pan_save;
    float* pan_parz;
    float* pan_tot;
    int* cont_rparz;
    int* cont_r;
    int *cont_save;

    int* dbrestart;
    int  isrestart;
    int antialias;
/*--------Apertura di migrazione-------*/
    float *vett_ape;
    float *vett_distx,*vett_disty;
/* --------- parametri di mute--------*/
    int  *vett_depth_in, *vett_off_in;
    float *vett_off;
    int  nElemToReadMute, errCode;
    
/*----- dimensione dell'immagine da salvare */
    int pan1, pan2, pan3;
    /* --AVA -------*/
    int   pan4, pan5;
        
/* ---  Directories --- */
    char *OutDir;
    int LenOutDir;

/* MPI variables */
/* 31/05/2002 */
/* 
   myrank = Rank PE
   size   = Numero totale PE utilizzati
   iope   = size-1 = Rank di I/O PE
   numtracceperpe = Numero tracce per comp PE da elaborare = dimensione gruppo 
*/
    int myrank, size, iope, numtracceperpe;
/*  20/06/2002
    err   = codice d'errore
*/
    int err;
/*  02/07/2002
    statbuf = utilizzato per verificare se esistono directory
*/
    struct stat statbuf;
/*----DeCompressione input---------*/
   char rangefilename[MAX_STRING_LEN];
   float *rangeT, *rangeA, *rangeCz;
   short int *tempfdg;
/*----Sorting input---------*/   
   int *sortfileTAC,*sortfileV;
   int  sort_dim;
/*----Flag di quantizzazione-------*/
   int  quantTAC, quantV, UDTAC, UDV;
   char quantMode[3][MAX_STRING_LEN];


   void interp_weights( float *tg_weights_cub, float *tg_weights_lin, int tg_rr, int j, float u);
   
   void callocKTAFloat ( float **pt, int nelem, int err, int myrank, char *argv[]);
   void callocKTAInt ( int **pt, int nelem, int err, int myrank, char *argv[]);
   void callocKTAShortInt ( short int **pt, int nelem, int err, int myrank, char *argv[]);
   void callocKTAInt64 ( int64 **pt, int nelem, int err, int myrank, char *argv[]);
   void callocKTAChar ( char **pt, int nelem, int err, int myrank, char *argv[]);
   void binsphere(int, int, float *);	
/*AVA*/
   float rangeVx[2], rangeVy[2], rangeVz[2];

   int num_psave , num_pan;   
/* *************************************** INIZIO MIGRATOR ***************************************/
/* Inizializzaione MPI ***************************************************************************/
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    MPI_Comm_size(MPI_COMM_WORLD, &size); 
/* Rank I/O PE */
    iope = size - 1;
/*    iope = 2;
    myrank = 2;
    size =3;*/
/* Controllo argomenti linea di comando **********************************************************/
    if (argc == 2)
    {
         if (strcmp("--help", argv[1]) == 0 )
       {
          if(myrank == iope)
          {
             printf("*** MIGRATOR v. %s ***\n",VERSION);
             printf("Command line template :\n");
             printf("migrator.x (mig1.p <traces par>) (mig2.p <mig par>) (mig3.p <acq par>) (output dir)\n\n");
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
          printf("*** MIGRATOR v. %s ***\n",VERSION);
          printf("migrator.x (mig1.p <traces par>) (mig2.p <mig par>) (mig3.p <acq par>) (output dir)\n\n");
       }
       MPI_Finalize();
       exit(EXIT_FAILURE);
    }

/***************************************************************************************************/

    if(myrank == iope)
    {
       printf("\n");
       printf("*** MIGRATOR v. %s ***\n\n",VERSION);
    }

  if(size <= 1)
  {
                err = 111;
                err_and_exit(err, myrank, argv);
  }

/*****************************************************************************************************/
  /************************ PROCESSING PARAMETERS FILES *********************/

    for (i=0; i< MAX_PARAM; i++)
     {
       ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
       val[i]   = calloc(MAX_STRING_LEN, sizeof(char));
     }


  /* ------------------- start processing migrator parameters ----------------- */
/* Inizio lettura mig2.p <mig par> ********************************************************************/
   fp=fopen(argv[2], "r");
   if (fp ==  NULL)
       {
          err = 1;
          err_and_exit(err, myrank, argv);
        }

   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );

   fclose(fp);
/* Fine lettura mig2.p <mig par> *********************************************************************/

/* Default parametri e loro definizione dal file mig2.p **********************************************/
   /* assegnazione delle variabili */
   strcpy(title,"\0");
   atn = 600;
   bordf   = -1;
   semiap = FLT_MAX;
   smussamento = FLT_MAX;
   soglia_ampiezze = FLT_MAX;
   imaging = -1;
   ava = 0;
   verbose = 1;
   offmin = -1;
   offmax = -1;
/*   isrestart = 0;  */
   antialias=0;
/* CCC 03/05/2002 MPI- Lettura parametri per il calcolo dei blocchi e gruppi */
   numtracceperpe=-1;
/* Clara fattore di decimazione delle tracce*/   
   datadecimation=1;
/* Clara2 apertura variabile con la profondita'*/   
   apeiniz=-1;
   zetamaxape=0.0;
/* Clara3 distanza massima Y (XLINE) e X (ILINE)a cui si migrano i dati*/  
   maxdisty=-1; 
   maxdistx=-1; 
/* Clara4 maxdist x e y  variabile con la profondita'*/
   distyiniz=-1;
   distxiniz=-1;
   zmaxdisty=-1;
   zmaxdistx=-1;
/*Clara4_bis*/
   zdistyiniz=-1;
/* Clara5 inizializzazione parametri di mute*/
   muteFlag=0;   
/* Flag di quantizzazione */
/*   0 => non quantizzato */
/*   1 => quantizzato a 2 byte bottom-up (per ogni shot il range dipende dal punto del target)*/
/*   2 => quantizzato a 2 byte top-down (il range e' unico per ciascuno shot) */
   quantTAC = 0;
   quantV = 0;
   UDTAC = 0;
   UDV = 0;


   strcpy( quantMode[0], "NO");
   strcpy( quantMode[1], "BOTTOM-UP");
   strcpy( quantMode[2], "TOP-DOWN");
/* AVA inizializzazione parametri*/
   dip_min=0.0;
   azimuth_min=0.0;
   dip_max=-1.0;
   azimuth_max=-1.0;
   step_dip=-1.0;
   step_az=-1.0;
   n_dip=1;
   n_az=1;
   n_az_dip=1;
   n_el_dip=1;
/*AVO*/
   avo=0;
   step_off=-1;
   n_off=1;
        
   for (i=0; i<imax; i++)
     {
       if (strcmp("TITLE", ident[i]) == 0 ) /* 0 */
           sscanf(val[i], "%s", title);
       else if (strcmp("ATN", ident[i]) == 0 ) /* 0 */
           sscanf(val[i], "%d", &atn);
       else if (strcmp("BORDF", ident[i]) == 0 ) /* 1 */
           sscanf(val[i], "%d", &bordf);
       else if (strcmp("SEMIAPERTURA", ident[i]) == 0 ) /* 2 */
           sscanf(val[i], "%f", &semiap);
       else if (strcmp("SMUSSAMENTO", ident[i]) == 0 ) /* 3 */
           sscanf(val[i], "%f", &smussamento);
       else if (strcmp("SOGLIA-AMPIEZZE", ident[i]) == 0 ) /* 4 */
           sscanf(val[i], "%f", &soglia_ampiezze);
       else if (strcmp("IMAGING", ident[i]) == 0 ) /* 5 */
           sscanf(val[i], "%d", &imaging);
       else if (strcmp("AVA-ANALYSIS", ident[i]) == 0 ) /* 6 */
           sscanf(val[i], "%d", &ava);
       else if (strcmp("VERBOSE", ident[i]) == 0 ) /* 7 */
           sscanf(val[i], "%d", &verbose);
       else if (strcmp("OFFMIN",ident[i]) == 0)
           sscanf(val[i], "%d", &offmin);
       else if (strcmp("OFFMAX",ident[i]) == 0)
            sscanf(val[i], "%d", &offmax);
       else if (strcmp("RESTART",ident[i]) == 0)
           sscanf(val[i], "%d", &isrestart);
       else if (strcmp("ANTIALIAS",ident[i]) == 0)
           sscanf(val[i], "%d", &antialias);
       else if (strcmp("NUMTRACCEPERPE",ident[i]) == 0)
           sscanf(val[i], "%d", &numtracceperpe);
/* Clara fattore di decimazione delle tracce*/   
       else if (strcmp("DATADECIMATION",ident[i]) == 0)
           sscanf(val[i], "%d", &datadecimation);
/* Clara2 apertura variabile con la profondita'*/   
       else if (strcmp("APERTURAINIZ", ident[i]) == 0 )
           sscanf(val[i], "%f", &apeiniz);
       else if (strcmp("ZETAMAXAPE", ident[i]) == 0 ) 
           sscanf(val[i], "%f", &zetamaxape);
/* Clara3 distanza massima Y (XLINE)e X (ILINE) a cui si migrano i dati*/  
       else if (strcmp("MAXDISTY", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &maxdisty);        
          else if (strcmp("MAXDISTX", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &maxdistx);        
/* Clara4 maxdist x e y variabile con la profondita'*/   
       else if (strcmp("DISTYINIZ", ident[i]) == 0 )
           sscanf(val[i], "%d", &distyiniz);
       else if (strcmp("DISTXINIZ", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &distxiniz);
       else if (strcmp("ZMAXDISTY", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &zmaxdisty);        
       else if (strcmp("ZMAXDISTX", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &zmaxdistx);        
 /* Clara5 parametri di mute'*/
        else if (strcmp("MUTEFLAG", ident[i]) == 0 ) 
           sscanf(val[i], "%d", &muteFlag);        
       else if (strcmp("MUTEFILENAME", ident[i]) == 0 ) 
           sscanf(val[i], "%s", muteFileName);        
 /*Clara4_bis*/
        else if (strcmp("ZDISTYINIZ", ident[i]) == 0 )
           sscanf(val[i], "%d", &zdistyiniz);
 /* AVA lettura parametri*/
         else if (strcmp("DIP_MIN", ident[i]) == 0 )          
           sscanf(val[i], "%f", &dip_min);                   
        else if (strcmp("DIP_MAX", ident[i]) == 0 )        
           sscanf(val[i], "%f", &dip_max);                
         else if (strcmp("AZIMUTH_MIN", ident[i]) == 0 )              
           sscanf(val[i], "%f", &azimuth_min);                   
        else if (strcmp("AZIMUTH_MAX", ident[i]) == 0 )       
           sscanf(val[i], "%f", &azimuth_max);                
         else if (strcmp("STEP_DIP", ident[i]) == 0 )          
           sscanf(val[i], "%f", &step_dip);                   
        else if (strcmp("STEP_AZ", ident[i]) == 0 )       
           sscanf(val[i], "%f", &step_az);                
/*AVO ANALYSIS*/        
        else if (strcmp("AVO-ANALYSIS", ident[i]) == 0 ) /* 6 */
           sscanf(val[i], "%d", &avo);
        else if (strcmp("STEP_OFF", ident[i]) == 0 )       
           sscanf(val[i], "%d", &step_off);               
/* Flag di quantizzazione */
        else if (strcmp("QUANT_TAC", ident[i]) == 0 )       
           sscanf(val[i], "%d", &quantTAC);       
        else if (strcmp("QUANT_V", ident[i]) == 0 )       
           sscanf(val[i], "%d", &quantV);                                
        else if (strcmp("UDTAC", ident[i]) == 0 )       
           sscanf(val[i], "%d", &UDTAC);       
        else if (strcmp("UDV", ident[i]) == 0 )       
           sscanf(val[i], "%d", &UDV);                    
       }

/*******************************************************************************************************/
   /* check dei parametri */
   if ( atn <= 0 )                                                         err_and_exit(107, myrank, argv);
   if ( bordf == -1 )                                                      err_and_exit(2, myrank, argv);   
   if (bordf <= 0)                                                         err_and_exit(3, myrank, argv);
   if ( semiap == FLT_MAX )                                                err_and_exit(4, myrank, argv);
   if ( (semiap <= 0) || (semiap > 90) )                                   err_and_exit(5, myrank, argv);
   if ( smussamento == FLT_MAX )                                           err_and_exit(6, myrank, argv);       
   if ( (smussamento < 0) || (smussamento > 1) )                           err_and_exit( 7, myrank, argv);
   if ( soglia_ampiezze == FLT_MAX )                                       err_and_exit( 8, myrank, argv);   
   if (soglia_ampiezze < 0)                                                err_and_exit( 9, myrank, argv);
   if ( imaging == -1 )                                                    err_and_exit(10, myrank, argv);       
   if ( (imaging < 1) || (imaging > 4) )                                   err_and_exit(11, myrank, argv);
   if ( (ava < 0) || (ava > 1) )                                           err_and_exit( 12, myrank, argv);   
   if ( (ava==1) && ((dip_max==-1)&&(azimuth_max==-1) ))                   err_and_exit( 145, myrank, argv);   
   if ( (ava==1) && ((dip_max<-1)||(dip_max>90) ))                         err_and_exit( 146, myrank, argv);       
   if ( (ava==1) && ((azimuth_max<-1)||(azimuth_max>360) ))                err_and_exit( 147, myrank, argv);   
   if ( (ava==1) && (dip_max!=-1)&& 
                           (step_dip<0 || step_dip>=(dip_max-dip_min) ) )  err_and_exit( 148, myrank, argv);                    
   if ( (ava==1) && (azimuth_max!=-1)&& 
                    (step_az<0 || step_az>=(azimuth_max-azimuth_min) ) )   err_and_exit( 149, myrank, argv); 
   if ( (verbose < 0) || (verbose > 2) )                                   err_and_exit( 13, myrank, argv);
   if ( (offmin >=0) && (offmax>=0) && (offmax < offmin) )                 err_and_exit(14, myrank, argv);
   if ( numtracceperpe <=0)                                                err_and_exit(15, myrank, argv);   
   if ( datadecimation <1)                                                 err_and_exit( 113, myrank, argv);       
   if ( ((apeiniz < 0)&&(apeiniz!= -1)) || (apeiniz > 90)  )               err_and_exit( 114, myrank, argv);   
   if (zetamaxape <0 || zetamaxape >2500)                                  err_and_exit( 115, myrank, argv);   
   if (maxdisty <0 && maxdisty != -1)                                      err_and_exit( 117, myrank, argv);
   if (maxdistx <0 && maxdistx != -1)                                      err_and_exit( 118, myrank, argv);    
   if ( ( (distxiniz < 0) && (distxiniz !=-1) ) || (distxiniz > maxdistx) )  err_and_exit( 119, myrank, argv);       
   if ( ( (distyiniz < 0) && (distyiniz !=-1) ) || (distyiniz > maxdisty) )  err_and_exit( 120, myrank, argv);
   if (zmaxdisty <=0 && zmaxdisty != -1)                                   err_and_exit( 121, myrank, argv);   
   if (zmaxdistx <=0 && zmaxdistx != -1)                                   err_and_exit( 122, myrank, argv);  

   if ((zdistyiniz > zmaxdisty) || (zdistyiniz < 0 && zdistyiniz != -1))   err_and_exit(131, myrank, argv);
   if (muteFlag !=0 && muteFlag != 1)                                      err_and_exit( 125, myrank, argv); 
   if ( (avo==1) && (offmax==-1) )                                         err_and_exit( 162, myrank, argv);   
   if ( (avo==1) && ( (step_off<0) || (step_off>(offmax-offmin)) ) )       err_and_exit( 163, myrank, argv);   
   if ( (avo==1) && ( ava==1) )                                            err_and_exit( 164, myrank, argv);   
   if ( (quantTAC<0) || (quantTAC>1))                                      err_and_exit( 165, myrank, argv);
   if ( (UDTAC<0) || (UDTAC>1))                                            err_and_exit( 166, myrank, argv);
   if ( (ava == 1) && ((quantV<0)||(quantV>1)) )                           err_and_exit( 167, myrank, argv);     
   if ( (ava == 1) && ((UDV<0)||(UDV>1)) )                                 err_and_exit( 168, myrank, argv);     
  	   
  /* ------------------- end processing migrator parameters --------------*/
  /* ------------------- initialization --------------*/
   if ((distxiniz ==-1) && (maxdistx !=-1)) {
   	distxiniz=maxdistx;} 
   if ((distyiniz ==-1) && (maxdisty !=-1)) {
   	distyiniz=maxdisty;}
   if ( (ava==1) && (dip_max!=-1)){
       n_dip = ceil((dip_max-dip_min)/step_dip);}
   if ( (ava==1) && (azimuth_max!=-1)  ){     
       n_az = ceil((azimuth_max-azimuth_min)/step_az); }  
   
   if ( (ava==1) && (el_dip_max!=-1)){
       n_el_dip = ceil((el_dip_max-el_dip_min)/el_dip_step);}
   if ( (ava==1) && (az_dip_max!=-1)  ){     
       n_az_dip = ceil((az_dip_max-az_dip_min)/az_dip_step); }  
       
       
   
   if  (avo==1) {     
       n_off = ceil((offmax-offmin)/step_off); }
   if ( (ava == 1) && (quantV==1) && (UDV==1) )   quantV=2;     
   
   grid_el =(float*)calloc(n_dip+1,sizeof(float *));
   binsphere(n_az,n_dip,grid_el);           

 /*----------- lettura file di mute---------------------------------------*/
   if (muteFlag==1) 
   {
   	fp=fopen(muteFileName, "r");
        if (fp ==  NULL) err_and_exit(126, myrank, argv);
        
        CountMutePar ( fp, &nElemToReadMute, &errCode);
        if ( errCode==1) err_and_exit(127, myrank, argv);
        
        fclose(fp);
    }
    else 
        nElemToReadMute=1;   
/*******************************************************************************************************/
/* CCC 03/05/2002 MPI- Definizione DIMGRUPPOTR e MAXDIMBLOCCOTR del codice originale scalare
                       tramite numtracceperpe */
       dimgruppotr    = numtracceperpe;
       maxdimbloccotr = (size-1)*numtracceperpe;

/*******************************************************************************************************/
  /* ------------------- start processing traces parameters ---------------*/
  /* Inizio lettura mig1.p <traces par> ******************************************************************/
   fp=fopen(argv[1], "r");
   if (fp ==  NULL)  err_and_exit( 16, myrank, argv);
   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
   fclose(fp);
/* Fine lettura mig1.p <traces par> ********************************************************************/
/* Default traces parametri e loro definizione dal file mig1.p *****************************************/
   /* assegnazione delle variabili */
   vel_sup = -1;   
   for (i=0; i<imax; i++)
       if (strcmp("VELSUP", ident[i]) == 0 )  sscanf(val[i], "%f", &vel_sup);
   if (vel_sup <= 0) err_and_exit( 17, myrank, argv);
   startsgy = TracesParams(argv[1]);
   max_ntracce = 0;
   max_ncamp = 0;
   max_ncampfilt = 0;
   cursgy = startsgy;
   while (cursgy != NULL)
     {
       cursgy->ncampfilt = cursgy->ncamp + 2*bordf;
       if (cursgy->ntraccetot > max_ntracce)
         max_ntracce = cursgy->ntraccetot;
       if (cursgy->ncamp > max_ncamp)
         max_ncamp = cursgy->ncamp;
       cursgy = cursgy->nextsgy;
     }
    max_ncampfilt = max_ncamp + 2*bordf;
   /* ------------------- end processing traces parameters --------------*/

   /* --------- start processing acquisition & target parameters --------*/
/* Inizio lettura mig3.p <acq par> *********************************************************************/
   fp=fopen(argv[3], "r");
   if (fp ==  NULL) err_and_exit( 18, myrank, argv);
   imax=0;
   imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
   fclose(fp);
/* Fine lettura mig3.p <acq par> ***********************************************************************/

   /* assegnazione delle variabili */
   strcpy(acq_file, "\0");
   strcpy(tg_file, "\0");
   strcpy(dbDir, "\0");

   for (i=0; i<imax; i++)
     {
       if (strcmp("ACQUISITION-FILE", ident[i]) == 0 ) /* 1 */
           sscanf(val[i], "%s", acq_file);
       else if (strcmp("TARGET-FILE", ident[i]) == 0 ) /* 2 */
           sscanf(val[i], "%s", tg_file);
       else if (strcmp("DATABASE-DIR", ident[i]) == 0 ) /* 3 */
           sscanf(val[i], "%s", dbDir);
     }
/*******************************************************************************************************/
   /* check dei parametri */
   if ( strlen(acq_file) == 0 )  err_and_exit( 19, myrank, argv);
   if ( strlen(acq_file) == 0 )  err_and_exit( 20, myrank, argv);
   if ( strlen(dbDir) == 0 )  err_and_exit( 21, myrank, argv);
/******************************************************************************************************/
     /* Database Directory */
     lstr = strlen(dbDir);
     if (dbDir[lstr-1] != '/')
     {
       if (lstr < (MAX_STRING_LEN-2))
           dbDir[lstr]='\0';  
       else
            err_and_exit( 22, myrank, argv);
     }
/* Controllo esistenza dir */
    err = stat(dbDir, &statbuf);
    if(err != 0) err_and_exit( 109, myrank, argv);
    dbDir[lstr]='/';
    dbDir[lstr+1]='\0';
    LendbDir= strlen(dbDir);
/******************************************************************************************************/
    /* Esposizione a video dei parametri */
    if ((verbose >0) && (myrank == iope))
    {
    printf("\n*** MIGRATOR v. %s ***\n\n",VERSION);
    printf("PARAMETRI ALGORITMO MIGRAZIONE :\n");
    printf("Padding delle tracce per filtraggio [# campioni]                : %d\n", bordf);
    printf("- APERTURA OPERATORE \n");
    printf("Semiapertura operatore di migrazione [gradi decimali]           : %f\n", semiap);
    if (apeiniz!=-1){
    printf("Semiapertura operatore di migrazione iniziale [gradi decimali]  : %f\n", apeiniz);
    printf("Profondita' di transizione ad apertura massima                  : %f\n", zetamaxape);
     }
    printf("- DISTANZA DI MIGRAZIONE \n");
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
    printf("- PARAMETRI DI IMAGING \n");
    printf("Smussamento a coseno rialzato offset lontani (0 - 1)            : %f\n", smussamento);
    printf("Soglia ampiezze ricevitori                                      : %f\n", soglia_ampiezze);
    if (imaging == 1)
    printf("Condizione di imaging                                           : As * Ar\n");
    else if (imaging == 2)
    printf("Condizione di imaging                                           : As / Ar\n");
    else if (imaging == 3)
    printf("Condizione di imaging                                           : As / (Ar + soglia_ampiezze)\n");
    if (ava == 0)
    printf("Analisi angoli riflessione                                      : NO\n");
    else if (ava == 1) {
    printf("Analisi angoli riflessione                                      : SI\n");
      if (dip_max!=-1){
    printf("dip_min                                                         : %f\n", dip_min);
    printf("dip_max                                                         : %f\n", dip_max);
    printf("step_dip                                                        : %f\n", step_dip);}
      if (azimuth_max!=-1){
    printf("azimuth_min                                                     : %f\n", azimuth_min);
    printf("azimuth_max                                                     : %f\n", azimuth_max);
    printf("step_az                                                         : %f\n", step_az);}
    }
    if (avo == 0)
    printf("Analisi AVO                                                     : NO\n");
    else if (avo == 1) {
    printf("Analisi AVO                                                     : SI\n");
    printf("offset minimo                                                   : %d\n", offmin);
    printf("offset massimo                                                  : %d\n", offmax);
    printf("step_off                                                        : %d\n", step_off);
    }
    if (antialias == 0)
    printf("Antialias                                                       : NO\n");
    else if (antialias == 1)
    printf("Antialias                                                       : SI\n");
    printf("Fattore di decimazione dei dati                                 : %d\n", datadecimation);
    if (muteFlag == 1) {
    printf("- MUTE ESTERNO \n");
    printf("Flag attivazione mute esterno                                   : SI\n");
    printf("File mute esterno                                               : %s\n", muteFileName);
    }
    printf("Velocita\' media onde P degli strati superficiali               : %f\n", vel_sup);
    printf("File contenente descrizione superficie acquisizione             : %s\n", acq_file);
    printf("File contenente descrizione target migrazione                   : %s\n", tg_file);
    printf("Directory contenente i files del database                       : %s\n", dbDir);
    printf("- MODALITA' QUANTIZZAZIONE DATABASE \n");
    printf("Modalita' di quantizzazione fdg Tempi                           : %s\n", quantMode[quantTAC]);
    printf("Modalita' di quantizzazione fdg Ampiezze                        : %s\n", quantMode[quantTAC]);
    printf("Modalita' di quantizzazione fdg Cz                              : %s\n", quantMode[quantTAC]);
    if (ava == 1)
    {
    	printf("Modalita' di quantizzazione fdg Vx                              : %s\n", quantMode[quantV]);
    	printf("Modalita' di quantizzazione fdg Vy                              : %s\n", quantMode[quantV]);
    	printf("Modalita' di quantizzazione fdg Vz                              : %s\n", quantMode[quantV]);
    }
    }
/******************************************************************************************************/
/* CCC Controllo esistenza file acq_file e tg_file */
    fp = fopen(acq_file, "r");
    if (fp ==  NULL)  err_and_exit( 23, myrank, argv);
    fclose(fp);
    
    fp = fopen(tg_file, "r");
    if (fp ==  NULL) err_and_exit( 24, myrank, argv);
    fclose(fp);
/******************************************************************************************************/
     /* Acquisition Surface */
     startacq = TargetParams(acq_file, &acq_type);

     acq_npan=0;
     acq_nt_tot=0;
     acq_nna_max=0;
     acq_nnb_max=0;
     acq_nnc_max=0;
     acq_rra_max=0;
     acq_rrb_max=0;
     acq_rrc_max=0;
     acq_dc = 0;
     acq_nc = 0;
     acq_ddc = 0;
     acq_nnc = 0;

     if (verbose > 0 && (myrank == iope)) printf("\nACQUISITION SURFACE : \n");

     if (acq_type == 1)
       {
         acq_npan = 1;
         callocKTAFloat( &acq_o1, acq_npan, 30, myrank, argv); 
         callocKTAFloat( &acq_o2, acq_npan, 31, myrank, argv); 
         callocKTAFloat( &acq_o3, acq_npan, 32, myrank, argv); 

         if (verbose > 0 && (myrank == iope))   printf("----- Set of points -----\n");

         fp = fopen(acq_file, "r");
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
                      acq_o1=realloc(acq_o1, acq_npan * sizeof(float));
                      if(acq_o1 == NULL) err_and_exit( 30, myrank, argv);

                      acq_o2=realloc(acq_o2, acq_npan * sizeof(float));
                      if(acq_o2 == NULL) err_and_exit( 31, myrank, argv);

                      acq_o3=realloc(acq_o3, acq_npan * sizeof(float));
                      if(acq_o3 == NULL) err_and_exit( 32, myrank, argv);
                      
                      sscanf(st,"%f    %f    %f\n", &acq_o1[j], &acq_o2[j],&acq_o3[j] );
                      acq_npan++;
                      j++;
                    }
              }
             else if (test == REMARK)
                fscanf(fp,"%*[^\n]\n");
           }
         fclose(fp);
 
         acq_npan  =j;
         acq_nt_tot=j;

         if (verbose > 0 && (myrank == iope))  printf("Total number of points : %d\n",acq_npan);
       }
     else
       {
         curacq = startacq;
         while (curacq != NULL)
          {
           if ( (acq_type == 3) && (curacq->seq == 1) && (verbose >0) && (myrank == iope))
            {
              printf("----- Volume -----\n");
              printf("o1 = %f - o2 = %f - o3 = %f\n",   curacq->o1, curacq->o2, curacq->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(curacq->ca1)*180/PI, acos(curacq->ca2)*180/PI,
                                                        acos(curacq->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(curacq->cb1)*180/PI, acos(curacq->cb2)*180/PI,
                                                        acos(curacq->cb3)*180/PI);
              printf("cc1 = %f - cc2 = %f - cc3 = %f\n",acos(curacq->cc1)*180/PI, acos(curacq->cc2)*180/PI,
                                                        acos(curacq->cc3)*180/PI);
              printf("la = %f - lb = %f - lc = %f\n",   curacq->la,  curacq->lb,  curacq->lc);
              printf("da = %f - db = %f - dc = %f\n",   curacq->da,  curacq->db,  curacq->dc );
              printf("na = %d - nb = %d - nc = %d\n",   curacq->na,  curacq->nb,  curacq->nc);
              printf("dda = %f - ddb = %f - ddc = %f\n",curacq->dda, curacq->ddb, curacq->ddc );
              printf("nna = %d - nnb = %d - nnc = %d\n",curacq->nna, curacq->nnb, curacq->nnc);
             }
           if ( (acq_type == 2) && (verbose >0)&&(myrank == iope) )
            {
              printf("----- Panel N. %d -----\n",       curacq->seq);
              printf("o1 = %f - o2 = %f - o3 = %f\n",   curacq->o1, curacq->o2, curacq->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(curacq->ca1)*180/PI, acos(curacq->ca2)*180/PI,
                                                        acos(curacq->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(curacq->cb1)*180/PI, acos(curacq->cb2)*180/PI,
                                                        acos(curacq->cb3)*180/PI);
              printf("la = %f - lb = %f\n",             curacq->la, curacq->lb);
              printf("da = %f - db = %f\n",             curacq->da, curacq->db);
              printf("na = %d - nb = %d\n",             curacq->na, curacq->nb);
              printf("dda = %f - ddb = %f\n",           curacq->dda, curacq->ddb);
              printf("nna = %d - nnb = %d\n",           curacq->nna, curacq->nnb);
             }

           acq_npan = acq_npan +1;
           acq_nt_tot = acq_nt_tot + curacq->nt;
           if (curacq->nna>= acq_nna_max)
            acq_nna_max=curacq->nna;
           if (curacq->nnb>= acq_nnb_max)
            acq_nnb_max=curacq->nnb;
           if ( (curacq->da/curacq->dda) >= acq_rra_max)
            acq_rra_max=(int)(curacq->da/curacq->dda);
           if ( (curacq->db/curacq->ddb) >= acq_rrb_max)
            acq_rrb_max=(int)(curacq->db/curacq->ddb);

           if (acq_type == 2)
             {
                acq_dc = 0;
                acq_nc = 1;
                acq_ddc = 0;
                acq_nnc = 1;
             }
           if (acq_type == 3)
             {
               if (curacq->nnc>= acq_nnc_max)
                   acq_nnc_max=curacq->nnc;
               if ( (curacq->dc/curacq->ddc) >= acq_rrc_max)
                   acq_rrc_max=(int)(curacq->dc/curacq->ddc);

               acq_dc = curacq->dc;
               acq_nc = curacq->nc;
               acq_ddc = curacq->ddc;
               acq_nnc = curacq->nnc;
             }
           curacq=curacq->next_tgt;
          }
       }
       
      fflush(stdout);
/******************************************************************************************************/

     /* Target */
     starttgt = TargetParams(tg_file, &tg_type);

     tg_npan=0;
     tg_nt_tot=0;
     tg_nna_max=0;
     tg_nnb_max=0;
     tg_nnc_max=0;
     tg_rra_max=0;
     tg_rrb_max=0;
     tg_rrc_max=0;
     tg_dc = 0;
     tg_nc = 0;
     tg_ddc = 0;
     tg_nnc = 0;

     if (verbose > 0 && (myrank == iope))     printf("\nTARGET : \n");

     if (tg_type == 1)
       {
         tg_npan = 1;
         callocKTAFloat( &tg_o1, tg_npan, 52, myrank, argv);
         callocKTAFloat( &tg_o2, tg_npan, 53, myrank, argv);
         callocKTAFloat( &tg_o3, tg_npan, 54, myrank, argv);

         if (verbose > 0 && (myrank == iope))          printf("----- Set of points -----\n");

         fp = fopen(tg_file, "r");
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
                      tg_o1=realloc(tg_o1, tg_npan * sizeof(float));
                      if(tg_o1 == NULL) err_and_exit( 52, myrank, argv);

                      tg_o2=realloc(tg_o2, tg_npan * sizeof(float));
                      if(tg_o2 == NULL) err_and_exit( 53, myrank, argv);

                      tg_o3=realloc(tg_o3, tg_npan * sizeof(float));
                      if(tg_o3 == NULL) err_and_exit( 54, myrank, argv);

                      sscanf(st,"%f %f %f\n", &tg_o1[j], &tg_o2[j],&tg_o3[j] );
                      tg_npan++;
                      j++;
                    }
              }
             else if (test == REMARK)
                fscanf(fp,"%*[^\n]\n");
           }
         fclose(fp);

         tg_npan=j;
         tg_nt_tot=j;

         w_a = 1;
         w_b = 1;
         w_c = 1;

         pan1 = 1;
         pan2 = 1;
         pan3 = j;

         if (verbose > 0 && (myrank == iope))         printf("Total number of points : %d\n",tg_npan);
       }
     else
       {
         curtgt = starttgt;
         while (curtgt != NULL)
          {
           if ( (tg_type == 3) && (curtgt->seq == 1) && (verbose >0) && (myrank == iope) )
            {
              printf("----- Volume -----\n");
              printf("o1 = %f - o2 = %f - o3 = %f\n",   curtgt->o1, curtgt->o2, curtgt->o3);
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
           if ( (tg_type == 2) && (verbose>0) && (myrank == iope))
            {
              printf("----- Panel N. %d -----\n",     curtgt->seq);
              printf("o1 = %f - o2 = %f - o3 = %f\n",   curtgt->o1, curtgt->o2, curtgt->o3);
              printf("ca1 = %f - ca2 = %f - ca3 = %f\n",acos(curtgt->ca1)*180/PI, acos(curtgt->ca2)*180/PI,
                                                        acos(curtgt->ca3)*180/PI);
              printf("cb1 = %f - cb2 = %f - cb3 = %f\n",acos(curtgt->cb1)*180/PI, acos(curtgt->cb2)*180/PI,
                                                        acos(curtgt->cb3)*180/PI);
              printf("la = %f - lb = %f\n",             curtgt->la, curtgt->lb);
              printf("da = %f - db = %f\n",             curtgt->da, curtgt->db);
              printf("na = %d - nb = %d\n",             curtgt->na, curtgt->nb);
              printf("dda = %f - ddb = %f\n",           curtgt->dda, curtgt->ddb);
              printf("nna = %d - nnb = %d\n",           curtgt->nna, curtgt->nnb);
             }

           tg_npan = tg_npan +1;
           tg_nt_tot = tg_nt_tot + curtgt->nt;
           if (curtgt->nna>= tg_nna_max)
             tg_nna_max=curtgt->nna;
           if (curtgt->nnb>= tg_nnb_max)
             tg_nnb_max=curtgt->nnb;
           if ( ((curtgt->nna-1)/(curtgt->na-1)) >= tg_rra_max)
             tg_rra_max=(int)((curtgt->nna-1)/(curtgt->na-1));
           if ( ((curtgt->nnb-1)/(curtgt->nb-1)) >= tg_rrb_max)
             tg_rrb_max=(int)((curtgt->nnb-1)/(curtgt->nb-1));

           if (tg_type == 2)
             {
                tg_dc = 1;
                tg_nc = 1;
                tg_ddc = 1;
                tg_nnc = 1;

                w_a = ((tg_rra_max+1)*tg_npan);
                w_b = (tg_rrb_max*tg_npan);
                w_c = 1;

                pan1 = tg_nna_max;
                pan2 = tg_nnb_max;
                pan3 = tg_npan;
             }

           if (tg_type == 3)
             {
               if (curtgt->nnc>= tg_nnc_max)
                   tg_nnc_max=curtgt->nnc;
               if ( ((curtgt->nnc-1)/(curtgt->nc-1)) >= tg_rrc_max)
                   tg_rrc_max=(int)((curtgt->nnc-1)/(curtgt->nc-1));
                tg_dc = curtgt->dc;
                tg_nc = curtgt->nc;
                tg_ddc = curtgt->ddc;
                tg_nnc = curtgt->nnc;

                w_a = (tg_rra_max +1);
                w_b = (tg_rrb_max);
                w_c = (tg_rrc_max);

                pan1 = tg_nna_max;
                pan2 = tg_nnb_max;
                pan3 = tg_nnc_max;
             }
           curtgt=curtgt->next_tgt;
          }
       }
   /* --------- end processing acquisition & target parameters -----------*/

   /* ------ setting del numero di gruppi/blocchi di tracce da analizzare --------*/
   /* -------- fattore di decimazione delle tracce -------------------------------*/  
    max_ntracce_old=max_ntracce;
    max_ntracce = max_ntracce/datadecimation ;
         if ( max_ntracce*datadecimation < max_ntracce_old )
           max_ntracce++;
/* CCC 06/05/2002 MPI- Abort se dimensione blocco > numero tracce presenti ---------*/
    if (maxdimbloccotr > max_ntracce)                 err_and_exit(25, myrank, argv);
    
    maxgruppi= (int) (max_ntracce/dimgruppotr) ;
    if ( maxgruppi*dimgruppotr < max_ntracce )
       maxgruppi++;
       
    maxnrblocchitr= (int) max_ntracce/maxdimbloccotr;
    if ( maxnrblocchitr*maxdimbloccotr < max_ntracce )
        maxnrblocchitr++;


   /* ----- end setting --------------------------------------------------*/
   /* -------- AVA--------------------------------------------------------*/
   pan4=1;
   pan5=1;
   if (ava==1) {
   	pan4=n_dip;
   	pan5=n_az;
	   }
   if (avo==1) {
   	pan4=n_off;
   }   


   /* ------------------- start processing output directory --------------*/
   OutDir = calloc(MAX_STRING_LEN, sizeof(char));
   strcpy(OutDir, argv[4]);
   lstr = strlen(OutDir);
   if (OutDir[lstr-1] != '/')
     {
       if (lstr < (MAX_STRING_LEN-2))
           OutDir[lstr]='\0';
       else
          err_and_exit( 26, myrank, argv);
     }
    /*----------- Controllo esistenza dir ----------*/
    err = stat(OutDir, &statbuf);
    if(err != 0)
       err_and_exit( 110, myrank, argv);

    OutDir[lstr]='/';
    OutDir[lstr+1]='\0';
    LenOutDir= strlen(OutDir);


  /************************ CHECKING PARAMETERS VALUES *********************/

      if (max_ncamp <= 0)                     err_and_exit( 27, myrank, argv);
      if (max_ntracce <= 0)                   err_and_exit( 28, myrank, argv);
      if (max_ncampfilt <= 0)                 err_and_exit( 29, myrank, argv);

  /*********************** MEMORY ALLOCATION  ************************ */
      	if (myrank == iope) {printf("sono prima dell'allocazione \n"); fflush(stdout);}
 

    /* ------ACQUISITION---------------------------------------------*/

    if(acq_type != 1) 
    {
       callocKTAFloat( &acq_o1, acq_npan, 30, myrank, argv);
       callocKTAFloat( &acq_o2, acq_npan, 31, myrank, argv);
       callocKTAFloat( &acq_o3, acq_npan, 32, myrank, argv);
    }

    callocKTAFloat( &acq_ca1, acq_npan, 33, myrank, argv);   
    callocKTAFloat( &acq_ca2, acq_npan, 34, myrank, argv);
    callocKTAFloat( &acq_ca3, acq_npan, 35, myrank, argv);
    callocKTAFloat( &acq_cb1, acq_npan, 36, myrank, argv);
    callocKTAFloat( &acq_cb2, acq_npan, 37, myrank, argv);
    callocKTAFloat( &acq_cb3, acq_npan, 38, myrank, argv);
    callocKTAFloat( &acq_cc1, acq_npan, 39, myrank, argv);
    callocKTAFloat( &acq_cc2, acq_npan, 40, myrank, argv);
    callocKTAFloat( &acq_cc3, acq_npan, 41, myrank, argv);
    callocKTAFloat( &acq_da, acq_npan, 42, myrank, argv);
    callocKTAFloat( &acq_db, acq_npan, 43, myrank, argv);
    callocKTAInt( &acq_na, acq_npan, 44, myrank, argv);
    callocKTAInt( &acq_nb, acq_npan, 45, myrank, argv);
    callocKTAInt( &acq_nt, acq_npan, 46, myrank, argv);
    callocKTAFloat( &acq_dda, acq_npan, 47, myrank, argv);
    callocKTAFloat( &acq_ddb, acq_npan, 48, myrank, argv);
    callocKTAInt( &acq_nna, acq_npan, 49, myrank, argv);
    callocKTAInt( &acq_nnb, acq_npan, 50, myrank, argv);
    callocKTAInt( &acq_nnt, acq_npan, 51, myrank, argv);

      	if (myrank == iope) {printf("prima alloc target \n"); fflush(stdout);}

    /* ------TARGET---------------------------------------------*/

    if(tg_type != 1)
    {
       callocKTAFloat( &tg_o1, tg_npan, 52, myrank, argv);
       callocKTAFloat( &tg_o2, tg_npan, 53, myrank, argv);
       callocKTAFloat( &tg_o3, tg_npan, 54, myrank, argv);
    }

    callocKTAFloat( &tg_ca1, tg_npan, 55, myrank, argv);
    callocKTAFloat( &tg_ca2, tg_npan, 56, myrank, argv);
    callocKTAFloat( &tg_ca3, tg_npan, 57, myrank, argv);
    callocKTAFloat( &tg_cb1, tg_npan, 58, myrank, argv);
    callocKTAFloat( &tg_cb2, tg_npan, 59, myrank, argv);
    callocKTAFloat( &tg_cb3, tg_npan, 60, myrank, argv);
    callocKTAFloat( &tg_cc1, tg_npan, 61, myrank, argv);
    callocKTAFloat( &tg_cc2, tg_npan, 62, myrank, argv);
    callocKTAFloat( &tg_cc3, tg_npan, 63, myrank, argv);
    callocKTAFloat( &tg_da, tg_npan, 64, myrank, argv);
    callocKTAFloat( &tg_db, tg_npan, 65, myrank, argv);
    callocKTAInt( &tg_na, tg_npan, 66, myrank, argv);
    callocKTAInt( &tg_nb, tg_npan, 67, myrank, argv);
    callocKTAInt( &tg_nt, tg_npan, 68, myrank, argv);
    callocKTAFloat( &tg_dda, tg_npan, 69, myrank, argv);
    callocKTAFloat( &tg_ddb, tg_npan, 70, myrank, argv);
    callocKTAInt( &tg_nna, tg_npan, 71, myrank, argv);
    callocKTAInt( &tg_nnb, tg_npan, 72, myrank, argv);
    callocKTAInt( &tg_nnt, tg_npan, 73, myrank, argv);

    callocKTAFloat( &tg_weights_lin_a, w_a*tg_npan, 74, myrank, argv);
    callocKTAFloat( &tg_weights_lin_b, w_b*tg_npan, 141, myrank, argv);
    callocKTAFloat( &tg_weights_lin_c, w_c,         142, myrank, argv);

    callocKTAFloat( &tg_weights_cub_b, w_b*16*tg_npan, 143, myrank, argv);
    callocKTAFloat( &tg_weights_cub_c, w_c*16,         144, myrank, argv);

      /*-----AVA---------------------------------------------------- */
     if (ava==1) 
        tg_nt_ava=tg_nt_tot;
     else 
        tg_nt_ava=1;

      	if (myrank == iope) {printf("prima di alloc dbase \n"); fflush(stdout);}

    /* ------DATABASES SORGENTI----------------------------------- */
    callocKTAFloat( &SA, tg_nt_tot, 75, myrank, argv);
    callocKTAFloat( &ST, tg_nt_tot, 76, myrank, argv);
    /* ------AVA----angoli sul target----------------------------------*/
    callocKTAFloat( &SVx, tg_nt_ava, 150, myrank, argv);
    callocKTAFloat( &SVy, tg_nt_ava, 151, myrank, argv);
    callocKTAFloat( &SVz, tg_nt_ava, 152, myrank, argv);

    /* ------RESTART-----------------------------------------------*/

    callocKTAInt( &dbrestart, maxnrblocchitr, 77, myrank, argv);

    /* ------DATABASES RICEVITORI----------------------------------*/
    callocKTAFloat( &RA, tg_nt_tot, 78, myrank, argv);
    callocKTAFloat( &RT, tg_nt_tot, 79, myrank, argv);
    /* ------angoli in superficie----------------------------------*/
    callocKTAFloat( &RUz, tg_nt_tot, 82, myrank, argv);
    /* ------AVA---angoli sul target----------------------------------*/
    callocKTAFloat( &RVx, tg_nt_ava, 153, myrank, argv);
    callocKTAFloat( &RVy, tg_nt_ava, 154, myrank, argv);
    callocKTAFloat( &RVz, tg_nt_ava, 155, myrank, argv);

      	if (myrank == iope) {printf("prima di alloc tracce \n"); fflush(stdout);}
           
    /* ------TRACCE---------------------------------------------*/
    callocKTAFloat( &tracce, max_ncampfilt, 83, myrank, argv);
    callocKTAFloat( &traces, maxdimbloccotr*max_ncamp, 84, myrank, argv);
    callocKTAFloat( &traccia_temp, max_ncamp, 85, myrank, argv);
    callocKTAFloat( &traccia_letta, max_ncamp, 86, myrank, argv);
    callocKTAChar(  &traccia_lettac, max_ncamp*4, 87, myrank, argv);
    callocKTAShortInt( &traccia_lettash, max_ncamp, 112, myrank, argv);
    callocKTAFloat( &traccia1, max_ncamp+bordf, 88, myrank, argv);
    callocKTAFloat( &traccia2, max_ncamp+bordf, 89, myrank, argv);

    /* ------POSIZIONE RICEVITORI--------------------------------*/
    callocKTAFloat( &tr_xr, maxdimbloccotr, 90, myrank, argv);
    callocKTAFloat( &tr_yr, maxdimbloccotr, 91, myrank, argv);
    callocKTAFloat( &tr_zr, maxdimbloccotr, 92, myrank, argv);

    /* ------VALIDITA' RICEVITORI---------------------------------*/
    callocKTAInt64( &binr_offset, maxdimbloccotr, 93, myrank, argv);
    callocKTAInt64( &bins_offset, maxdimbloccotr, 94, myrank, argv);
    
    /* ------POSIZIONE e NUMERO SORGENTI--------------------------*/
    callocKTAFloat( &tr_xs, maxdimbloccotr, 95, myrank, argv);
    callocKTAFloat( &tr_ys, maxdimbloccotr, 96, myrank, argv);
    callocKTAFloat( &tr_zs, maxdimbloccotr, 97, myrank, argv);

/* CCC 06/05/2002 MPI- Upgrade tr_ns da integer*4 --> integer*8 per fare un'unico MPI_BCAST di integer
                       in execute */
/*    tr_ns=calloc(maxdimbloccotr,sizeof(int)); */
    callocKTAInt64( &tr_ns, maxdimbloccotr, 98, myrank, argv);

    /* ------PESI------------------------------------------------*/
    callocKTAFloat( &tr_weights, maxdimbloccotr, 99, myrank, argv);

    /* ------LOOKUP TABLE ATAN-----------------------------------*/
    callocKTAFloat( &attable, atn, 100, myrank, argv);



    /* ------PANNELLO DI MIGRAZIONE------------------------------*/
    num_psave=pan1*pan2*pan4*pan5;
    num_pan=pan1*pan2*pan3*pan4*pan5;
    callocKTAFloat( &pan_save, num_psave, 101, myrank, argv);
    callocKTAFloat( &pan_parz, num_pan, 102, myrank, argv);
    callocKTAFloat( &pan_tot,  num_pan, 103, myrank, argv);
    for (i=0; i<num_pan; i++) pan_tot[i]=0;
    callocKTAInt( &cont_save,  num_psave, 104, myrank, argv);
    callocKTAInt( &cont_rparz, num_pan, 105, myrank, argv);
    callocKTAInt( &cont_r,     num_pan, 106, myrank, argv);
    for (i=0; i<num_pan; i++)   cont_r[i]=0;

    /* ------MODE------------------------------------------------*/
    callocKTAChar( &mode, 5, 108, myrank, argv);
    callocKTAFloat( &vett_ape,  pan1, 116, myrank, argv);
    callocKTAFloat( &vett_distx, pan1, 123, myrank, argv);
    callocKTAFloat( &vett_disty, pan1, 124, myrank, argv);
    callocKTAInt( &vett_depth_in, nElemToReadMute, 128, myrank, argv);
    callocKTAInt( &vett_off_in,   nElemToReadMute, 129, myrank, argv);
    callocKTAFloat( &vett_off,    pan1,            130, myrank, argv);
    /*-------Compressione output--------------------------------*/
    callocKTAFloat( &rangeA,  tg_nt_tot*2, 132, myrank, argv);
    callocKTAFloat( &rangeT,  tg_nt_tot*2, 133, myrank, argv);
    callocKTAFloat( &rangeCz, tg_nt_tot*2, 134, myrank, argv);    
    callocKTAShortInt( &tempfdg,  tg_nt_tot, 135, myrank, argv);
        
  /**************** ASSEGNAZIONE VALORI *******************************************/
    if (acq_type > 1) {

    curacq=startacq;
    i=0;
    while (curacq != NULL)
      {
        acq_o1[i]=curacq->o1;
        acq_o2[i]=curacq->o2;
        acq_o3[i]=curacq->o3;
        acq_ca1[i]=curacq->ca1;
        acq_ca2[i]=curacq->ca2;
        acq_ca3[i]=curacq->ca3;
        acq_cb1[i]=curacq->cb1;
        acq_cb2[i]=curacq->cb2;
        acq_cb3[i]=curacq->cb3;
        acq_cc1[i]=curacq->cc1;
        acq_cc2[i]=curacq->cc2;
        acq_cc3[i]=curacq->cc3;
        acq_da[i]=curacq->da;
        acq_db[i]=curacq->db;
        acq_na[i]=curacq->na;
        acq_nb[i]=curacq->nb;
        acq_nt[i]=curacq->nt;
        acq_dda[i]=curacq->dda;
        acq_ddb[i]=curacq->ddb;
        acq_nna[i]=curacq->nna;
        acq_nnb[i]=curacq->nnb;
        acq_nnt[i]=curacq->nnt;
        curacq = curacq->next_tgt;
        i++;
      }
     }

    if (tg_type > 1) {

    curtgt=starttgt;
    i=0;
    while (curtgt != NULL)
      {
        tg_o1[i]=curtgt->o1;
        tg_o2[i]=curtgt->o2;
        tg_o3[i]=curtgt->o3;
        tg_ca1[i]=curtgt->ca1;
        tg_ca2[i]=curtgt->ca2;
        tg_ca3[i]=curtgt->ca3;
        tg_cb1[i]=curtgt->cb1;
        tg_cb2[i]=curtgt->cb2;
        tg_cb3[i]=curtgt->cb3;
        tg_cc1[i]=curtgt->cc1;
        tg_cc2[i]=curtgt->cc2;
        tg_cc3[i]=curtgt->cc3;
        tg_da[i]=curtgt->da;
        tg_db[i]=curtgt->db;
        tg_na[i]=curtgt->na;
        tg_nb[i]=curtgt->nb;
        tg_nt[i]=curtgt->nt;
        tg_dda[i]=curtgt->dda;
        tg_ddb[i]=curtgt->ddb;
        tg_nna[i]=curtgt->nna;
        tg_nnb[i]=curtgt->nnb;
        tg_nnt[i]=curtgt->nnt;
        tg_rra=curtgt->da/curtgt->dda;
        tg_rrb=curtgt->db/curtgt->ddb;

        curtgt = curtgt->next_tgt;
        i++;
      }
    }

      if ( (tg_type == 2) || ((tg_type == 3) && (starttgt->nnc == starttgt->nc)) )
      { 
      	if (myrank==iope) 
      	    printf("-----MIGRAZIONE CON INTERPOLAZIONE PER PANNELLI DELLE FGREEN-------\n");

        
        curtgt=starttgt;
        i=0;
        while (curtgt != NULL)
         {
            tg_rra=curtgt->da/curtgt->dda;
            tg_rrb=curtgt->db/curtgt->ddb;

            /* INTERPOLAZIONE VERTICALE (ASSE A)*/
            for(k=0; k<=tg_rra; k++)
            {
                   t = k*curtgt->dda/curtgt->da;                   
                   tg_weights_lin_a[i*w_a+k] = (1-t);                    
            }               
            /* INTERPOLAZIONE ORIZZONTALE (ASSE B) */
            for(j=0; j<tg_rrb; j++)
            {  
                   u = j*curtgt->ddb/curtgt->db;  
                   interp_weights( &(tg_weights_cub_b[i*w_b*16]), &(tg_weights_lin_b[i*w_b]), w_b, j, u); 
            } 

            if ( tg_type == 2 )
            {
               curtgt = curtgt->next_tgt;
               i++;
            }
            else
               curtgt = NULL;     
         }
      }

     if ( (tg_type == 3) && (starttgt->nnc != starttgt->nc) )
      {
      	if (myrank==iope) 
            printf("-----MIGRAZIONE CON INTERPOLAZIONE PER VOLUMI DELLE FGREEN-------\n");
            
        curtgt=starttgt;

        tg_rra=curtgt->da/curtgt->dda;
        tg_rrb=curtgt->db/curtgt->ddb;
        tg_rrc=curtgt->dc/curtgt->ddc;

        /* INTERPOLAZIONE VERTICALE (ASSE A)*/
        for(k=0; k<=tg_rra; k++)
        {
              t = k*curtgt->dda/curtgt->da;
              tg_weights_lin_a[0+k] = (1-t);      
        } 
          
        /* INTERPOLAZIONE ORIZZONTALE (ASSI B & C) */
        for (i=0; i<tg_rrc; i++)
        {      
            v = i*curtgt->ddc/curtgt->dc;
            interp_weights( tg_weights_cub_c, tg_weights_lin_c, w_c, i, v); 
        }
        for (j=0; j<tg_rrb; j++)
        {
            u = j*curtgt->ddb/curtgt->db;
            interp_weights( tg_weights_cub_b, tg_weights_lin_b, w_b, j, u);                         
         }

      }

     /*----------- assegnazione vettori di mute------------------------ */
      if (muteFlag==1) 
      {
   	fp=fopen(muteFileName, "r");        
        ReadMutePar ( fp, nElemToReadMute, vett_depth_in, vett_off_in);        
        fclose(fp);
        
        dz = (int)starttgt->dda;
        for ( i=0; i<nElemToReadMute+1; i++)
        {
        	 
        	if ( i == 0 )
        	{     
        		old_stepz = 0;
        		stepz = vett_depth_in[i]/dz + 1;        		
        		dip_mute= 0.0;
        	}
        	else if ( i == (nElemToReadMute ) )
        	{
        		old_stepz += stepz;
        		stepz = pan1 - old_stepz;
        	}
        	else
        	{
        		old_stepz += stepz;
        		stepz = ( vett_depth_in[i] - vett_depth_in[i-1] )/dz + 1;
        		dip_mute= (float)( vett_off_in[i] - vett_off_in[i-1] ) / (float)stepz;
        	}
        	
        	for ( j=old_stepz; j<old_stepz+stepz; j++)
        	{       
        		if ( j == 0 )
        	            vett_off[j] = vett_off_in[0];
        	        else
        		    vett_off[j] = vett_off[j-1] + dip_mute;
        	}
        }
      }
      /* ---------------- lettura file con i range ----------- */
        num_pan = tg_nt_tot*2;
        strcpy(rangefilename,dbDir);
        strcat(rangefilename,"rangeT.sht");
        fp=fopen(rangefilename, "r");
        if (fp ==  NULL) err_and_exit( 136, myrank, argv);
        fread(rangeT,sizeof(float),num_pan,fp);
        fclose(fp); 
          
        strcpy(rangefilename,dbDir);
        strcat(rangefilename,"rangeA.sht"); 
        fp=fopen(rangefilename, "r");
        if (fp ==  NULL) err_and_exit(137, myrank, argv);
        fread(rangeA,sizeof(float),num_pan,fp);
        fclose(fp);
          
        strcpy(rangefilename,dbDir);
        strcat(rangefilename,"rangeCz.sht");         
        fp=fopen(rangefilename, "r");
        if (fp ==  NULL) err_and_exit( 138, myrank, argv);
        fread(rangeCz,sizeof(float),num_pan,fp); 
        fclose(fp); 
        
         callocKTAInt( &sortfileV,   acq_nt_tot, 160, myrank, argv);
         callocKTAInt( &sortfileTAC, acq_nt_tot, 160, myrank, argv);
   
        if (ava==1 && (UDV==1)) {
          /* ---------------- lettura sorting file ----------- */
           strcpy(rangefilename,dbDir);
           strcat(rangefilename,"sortGreenF.sht");
           fp=fopen(rangefilename, "r");
           if (fp ==  NULL)                           err_and_exit( 159, myrank, argv);
           fseek(fp,0,SEEK_END);
           sort_dim=((int)ftell(fp))/4;
           fseek(fp,0,SEEK_SET);           
           fread(sortfileV,sizeof(float),sort_dim,fp);
           fclose(fp);
           if (sort_dim != acq_nt_tot)                   err_and_exit( 161, myrank, argv);
        } else {
           sort_dim = acq_nt_tot;
           for (i=0; i<acq_nt_tot; i++)
                sortfileV[i]=i+1;	
        }
        if (UDTAC==1 ) {
          /* ---------------- lettura sorting file ----------- */
           strcpy(rangefilename,dbDir);
           strcat(rangefilename,"sortGreenF.sht");
           fp=fopen(rangefilename, "r");
           if (fp ==  NULL)                              err_and_exit( 159, myrank, argv);
           fseek(fp,0,SEEK_END);
           sort_dim=((int)ftell(fp))/4;
           fseek(fp,0,SEEK_SET);           
           fread(sortfileTAC,sizeof(float),sort_dim,fp);
           fclose(fp);
           if (sort_dim != acq_nt_tot)                   err_and_exit( 161, myrank, argv);
        } else {
           sort_dim = acq_nt_tot;
           for (i=0; i<acq_nt_tot; i++)
                sortfileTAC[i]=i+1;	
        }
  /************************ HEADER FILES  *********************/

     if(myrank == iope) {
     if (tg_type == 1)
      {
         strcpy(filename,OutDir);
         strcat(filename,"LISTr.H");
         lstr = strlen(filename);

         remove(filename);

         strcpy(st, "title");
         strcpy(st1, "reflectivity");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));

         strcpy(st, "settype");
         seph_put_int_(filename,st,&tg_type,strlen(filename),strlen(st));

         strcpy(st, "nt");
         seph_put_int_(filename,st,&tg_nt_tot,strlen(filename),strlen(st));

         strcpy(st, "dataformat");
         strcpy(st1, "float");
         seph_put_string_(filename,st, st1, strlen(filename),strlen(st),strlen(st1));

         dim = sizeof(float);
         strcpy(st, "esize");
         seph_put_int_(filename,st, &dim, strlen(filename),strlen(st));

         strcpy(st, "in");
         strcpy(st1, "./LISTr.H@");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));

         strcpy(filename,OutDir);
         strcat(filename,"LISTfr.H");
         lstr = strlen(filename);

         remove(filename);

         strcpy(st, "title");
         strcpy(st1, "receiver fold");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));

         strcpy(st, "settype");
         seph_put_int_(filename,st,&tg_type,strlen(filename),strlen(st));

         strcpy(st, "nt");
         seph_put_int_(filename,st,&tg_nt_tot,strlen(filename),strlen(st));

         strcpy(st, "dataformat");
         strcpy(st1, "int");
         seph_put_string_(filename,st, st1, strlen(filename),strlen(st),strlen(st1));

         dim = sizeof(int);
         strcpy(st, "esize");
         seph_put_int_(filename,st, &dim, strlen(filename),strlen(st));

         strcpy(st, "in");
         strcpy(st1, "./LISTfr.H@");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));

      }

     if (tg_type == 2)
      {
       for (k=0; k<tg_npan; k++)
        { 
         strcpy(filename,OutDir);
         strcat(filename,"PAN");
         filenum[0]='\0';
         sprintf(filenum,"%8d",10000000+k+1);
         strcat(filename,filenum);
         strcat(filename,"r.H");
         lstr = strlen(filename);

         remove(filename);

         n3 = 2;
         strcpy( dataformat, "float");
         esize = sizeof(float);
         strcpy(in,"./PAN");
         filenum[0]='\0';
         sprintf(filenum,"%8d",10000000+k+1);
         strcat(in,filenum);
         strcat(in,"r.H@");
    
         writeHeaderMig ( filename, title, tg_type, &tg_o3[k], &tg_o1[k], &tg_o2[k],
                          &tg_ca1[k], &tg_ca2[k], &tg_ca3[k], &tg_cb1[k], &tg_cb2[k], &tg_cb3[k], 
                          &tg_cc1[k], &tg_cc2[k], &tg_cc3[k], &tg_dda[k], &tg_ddb[k], &tg_ddc,  
                          tg_nna[k], tg_nnb[k], n3, pan4, pan5, dataformat, esize, in, bordf, &semiap,     
                          &apeiniz, &zetamaxape, maxdisty, distyiniz, zdistyiniz,   
                          zmaxdisty, maxdistx, distxiniz, zmaxdistx, &smussamento, 
                          &soglia_ampiezze, imaging, ava, &dip_max, &dip_min, &azimuth_min, &azimuth_max,
                          avo, offmax, offmin, muteFlag, muteFileName, datadecimation,
                          antialias);
                       

         strcpy(filename,OutDir);
         strcat(filename,"PAN");
         filenum[0]='\0';
         sprintf(filenum,"%8d",10000000+k+1);
         strcat(filename,filenum);
         strcat(filename,"fr.H");
         lstr = strlen(filename);

         remove(filename);

         esize = sizeof(int);
         n3 = 2;
         strcpy( dataformat, "int");
         strcpy(st, "title");
         strcpy(st1, "receiver fold");
         strcpy(in,"./PAN");
         filenum[0]='\0';
         sprintf(filenum,"%8d",10000000+k+1);
         strcat(in,filenum);
         strcat(in,"r.H@");

         writeHeaderMig ( filename, title, tg_type, &tg_o3[k], &tg_o1[k], &tg_o2[k],
                          &tg_ca1[k], &tg_ca2[k], &tg_ca3[k], &tg_cb1[k], &tg_cb2[k], &tg_cb3[k], 
                          &tg_cc1[k], &tg_cc2[k], &tg_cc3[k], &tg_dda[k], &tg_ddb[k], &tg_ddc,  
                          tg_nna[k], tg_nnb[k], n3, pan4, pan5, dataformat, esize, in, bordf, &semiap,     
                          &apeiniz, &zetamaxape, maxdisty, distyiniz, zdistyiniz,   
                          zmaxdisty, maxdistx, distxiniz, zmaxdistx, &smussamento, 
                          &soglia_ampiezze, imaging, ava, &dip_max, &dip_min, &azimuth_min, &azimuth_max,
                          avo, offmax, offmin, muteFlag, muteFileName, datadecimation,
                          antialias); 
        }
      }

     if (tg_type == 3)
      {
         strcpy(filename,OutDir);
         strcat(filename,"VOLUMEr.H");
         lstr = strlen(filename);

         remove(filename);

         strcpy(in,"VOLUMEr.H@");
         esize = sizeof(float);
         strcpy( dataformat, "float");
        
         writeHeaderMig ( filename, title, tg_type, &tg_o3[0], &tg_o1[0], &tg_o2[0],
                          &tg_ca1[0], &tg_ca2[0], &tg_ca3[0], &tg_cb1[0], &tg_cb2[0], &tg_cb3[0], 
                          &tg_cc1[0], &tg_cc2[0], &tg_cc3[0], &tg_dda[0], &tg_ddb[0], &tg_ddc,  
                          tg_nna[0], tg_nnb[0], tg_nnc, pan4, pan5, dataformat, esize, in, bordf, &semiap,     
                          &apeiniz, &zetamaxape, maxdisty, distyiniz, zdistyiniz,   
                          zmaxdisty, maxdistx, distxiniz, zmaxdistx, &smussamento, 
                          &soglia_ampiezze, imaging, ava, &dip_max, &dip_min, &azimuth_min, &azimuth_max, 
                          avo, offmax, offmin, muteFlag, muteFileName, datadecimation,
                          antialias);
                       
         strcpy(filename,OutDir);
         strcat(filename,"VOLUMEfr.H");
         lstr = strlen(filename);

         remove(filename);

         strcpy(st, "title");
         strcpy(st1, "receiver fold");
         strcpy(in,"VOLUMEfr.H@");
         esize = sizeof(int);
         strcpy( dataformat, "int");

         writeHeaderMig ( filename, title, tg_type, &tg_o3[0], &tg_o1[0], &tg_o2[0],
                          &tg_ca1[0], &tg_ca2[0], &tg_ca3[0], &tg_cb1[0], &tg_cb2[0], &tg_cb3[0], 
                          &tg_cc1[0], &tg_cc2[0], &tg_cc3[0], &tg_dda[0], &tg_ddb[0], &tg_ddc,  
                          tg_nna[0], tg_nnb[0], tg_nnc, pan4, pan5, dataformat, esize, in, bordf, &semiap,     
                          &apeiniz, &zetamaxape, maxdisty, distyiniz, zdistyiniz,   
                          zmaxdisty, maxdistx, distxiniz, zmaxdistx, &smussamento, 
                          &soglia_ampiezze, imaging, ava, &dip_max, &dip_min, &azimuth_min, &azimuth_max,
                          avo, offmax, offmin, muteFlag, muteFileName, datadecimation,
                          antialias);
      }
      }

if (ava==1) {
  /* Conversione gradi -> radianti */
      if ( dip_max != -1) dip_max = dip_max * PI/ 180.0;
      dip_min = dip_min * PI / 180.0;
      if ( azimuth_max != -1) azimuth_max = azimuth_max * PI/ 180.0;
      azimuth_min = azimuth_min * PI / 180.0;
}      
  /************************ LOOP ON SEG-Y FILES  *********************/

    cursgy = startsgy;
    while (cursgy != NULL)
      {
         fidsgy = 1;
         strcpy(mode,"rb");
         lstr = strlen(cursgy->filename);

         glob_fopen64_(&fidsgy, cursgy->filename, mode, &file_error, lstr,2);

         nf = cursgy->seq;
         primobitns    = cursgy->primobitns;
         nshotnbytes = cursgy->nshotnbytes;
         primobitxs    = cursgy->primobitxs;
         xsnbytes = cursgy->xsnbytes;
         primobitys    = cursgy->primobitys;
         ysnbytes = cursgy->ysnbytes;
         primobitzs    = cursgy->primobitzs;
         zsnbytes = cursgy->zsnbytes;
         primobitxr    = cursgy->primobitxr;
         xrnbytes = cursgy->xrnbytes;
         primobityr    = cursgy->primobityr;
         yrnbytes = cursgy->yrnbytes;
         primobitzr    = cursgy->primobitzr;
         zrnbytes = cursgy->zrnbytes;
         primatr= cursgy->primatr;
         ntraccefile = cursgy->ntraccetot;
         ncamp     = cursgy->ncamp;
         ncampfilt = cursgy->ncampfilt;
         formato   = cursgy->formato;
         dt        = cursgy->dt;
         dx        = cursgy->dx;
         tshift    = cursgy->tshift;
         a11 = cursgy->a11;
         a12 = cursgy->a12;
         a21 = cursgy->a21;
         a22 = cursgy->a22;
         b1  = cursgy->b1;
         b2  = cursgy->b2;
        /*--------- decimazione delle tracce-----------*/        
         ntraccefiledec=ntraccefile/datadecimation ;
         if ( ntraccefiledec*datadecimation < ntraccefile )
           ntraccefiledec++;
         
         nrgruppitr= (int) (ntraccefiledec/dimgruppotr);
         if ( nrgruppitr*dimgruppotr < ntraccefiledec )
           nrgruppitr++;

         dimbloccotr=maxdimbloccotr;
         nrblocchitr= (int) ntraccefiledec/dimbloccotr;
         if ( nrblocchitr*dimbloccotr < ntraccefiledec )
           nrblocchitr++;
         
         fseeko64(glob_fp[fidsgy], (int64)0, SEEK_END);
         sgy64 = ftello64(glob_fp[fidsgy]);
         fseeko64(glob_fp[fidsgy], (int64)0, SEEK_SET);

/******************************************************************************************************/

         if (verbose > 0 && (myrank == iope)) {
            printf("\n tg_o2: %f\n",tg_o2[1]);	
            printf("\n FILE SEG-Y N: %d\n",cursgy->seq);
            printf("%d - filename    : %s\n",cursgy->seq,cursgy->filename);
            printf("%d - size (bytes): %.0f\n",cursgy->seq,(double) sgy64);
            printf("--- Binary Trace Header ---\n");
            printf("%d - offset-numshot..: %d\n",cursgy->seq,cursgy->primobitns);
            printf("%d - nbytes-numshot..: %d\n",cursgy->seq,cursgy->nshotnbytes);
            printf("%d - offset-xsource..: %d\n",cursgy->seq,cursgy->primobitxs);
            printf("%d - nbytes-xsource..: %d\n",cursgy->seq,cursgy->xsnbytes);
            printf("%d - offset-ysource..: %d\n",cursgy->seq,cursgy->primobitys);
            printf("%d - nbytes-ysource..: %d\n",cursgy->seq,cursgy->ysnbytes);
            printf("%d - offset-zsource..: %d\n",cursgy->seq,cursgy->primobitzs);
            printf("%d - nbytes-zsource..: %d\n",cursgy->seq,cursgy->zsnbytes);
            printf("%d - offset-xreceiver: %d\n",cursgy->seq,cursgy->primobitxr);
            printf("%d - nbytes-xreceiver: %d\n",cursgy->seq,cursgy->xrnbytes);
            printf("%d - offset-yreceiver: %d\n",cursgy->seq,cursgy->primobityr);
            printf("%d - nbytes-yreceiver: %d\n",cursgy->seq,cursgy->yrnbytes);
            printf("%d - offset-zreceiver: %d\n",cursgy->seq,cursgy->primobitzr);
            printf("%d - nbytes-zreceiver: %d\n",cursgy->seq,cursgy->zrnbytes);
            printf("--- Numero tracce ---\n");
            printf("%d - trace-offset....: %d\n",cursgy->seq,cursgy->primatr);
            printf("%d - ntraccefile.....: %d\n",cursgy->seq,cursgy->ntraccetot);
            printf("--- Offset range  ---\n");
            printf("%d - offset minimo...: %d\n",cursgy->seq,offmin);
            printf("%d - offset massimo..: %d\n",cursgy->seq,offmax);
            printf("--- Formato tracce ---\n");
            printf("%d - ncampioni.......: %d\n",cursgy->seq,cursgy->ncamp);
            printf("%d - ncampfilt.......: %d\n",cursgy->seq,cursgy->ncampfilt);
            printf("%d - formato.........: %d\n",cursgy->seq,cursgy->formato);
            printf("%d - dt..............: %f\n",cursgy->seq,cursgy->dt);
            printf("%d - dx..............: %f\n",cursgy->seq,cursgy->dx);
            printf("%d - tshift..........: %f\n",cursgy->seq,cursgy->tshift);
            printf("--- Cambiamento riferimento ---\n");
            printf("%d - a11 a12.........: %f %f\n",cursgy->seq,cursgy->a11,cursgy->a12);
            printf("%d - a21 a22.........: %f %f\n",cursgy->seq,cursgy->a21,cursgy->a22);
            printf("%d - b1  b2..........: %f %f\n",cursgy->seq,cursgy->b1,cursgy->b2);
            printf("--- Equalizzazione ---\n");
            printf("%d - filepesi........: %s\n",cursgy->seq,cursgy->filepesi);
	   
            printf("\n");
         }
            fflush(stdout);
/******************************************************************************************************/

         if (strlen(cursgy->filepesi) != 0)
           fpesi=1;
         else
           fpesi=0;

         strcpy(fpdir,cursgy->filepesi);
         lenfpdir=strlen(fpdir);
	
/******************************************************************************************************/
/*                                if (myrank == iope) err_and_exit(165, myrank, argv);
           Chiamata alla subroutine principale MAIN */
         execute_(
             &fidsgy, &nf, &primobitns,&nshotnbytes,&dimgruppotr,&maxgruppi,
             &nrgruppitr, &nrblocchitr,&dimbloccotr,&maxdimbloccotr,&maxnrblocchitr,
             &primobitxs, &xsnbytes, &primobitys, &ysnbytes, &primobitzs, &zsnbytes,
             &primobitxr, &xrnbytes, &primobityr, &yrnbytes, &primobitzr, &zrnbytes,
             &primatr,&offmin,&offmax,dbrestart,&isrestart,
             &ncamp, &ncampfilt, &formato, &dt, &dx, &tshift,
             &a11, &a12, &a21, &a22, &b1, &b2,
             &max_ncamp, &max_ncampfilt,
             &max_ntracce,&ntraccefiledec,

             &vel_sup,&fpesi,

             &bordf, &semiap, &smussamento, &soglia_ampiezze,
             &imaging, &verbose, &ava,traces,&antialias,

             tracce,traccia_temp,traccia_letta,traccia_lettac,traccia_lettash,
             traccia1,traccia2,tr_xr,tr_yr,tr_zr,binr_offset,bins_offset,
             tr_weights,tr_xs,tr_ys,tr_zs,tr_ns,

             pan_parz,pan_tot,pan_save,
             cont_rparz,cont_r,cont_save,
             &pan1, &pan2, &pan3, &pan4, &pan5,

             &acq_type,
             acq_o1, acq_o2, acq_o3,
             acq_ca1,acq_ca2,acq_ca3,
             acq_cb1,acq_cb2,acq_cb3,
             acq_cc1,acq_cc2,acq_cc3,
             acq_da, acq_db, acq_na, acq_nb, acq_nt,
             acq_dda,acq_ddb,acq_nna,acq_nnb,acq_nnt,
             &acq_dc, &acq_nc, &acq_ddc, &acq_nnc,
             &acq_npan, &acq_nt_tot,
             &tg_type,
             tg_o1, tg_o2, tg_o3,
             tg_ca1,tg_ca2,tg_ca3,
             tg_cb1,tg_cb2,tg_cb3,
             tg_cc1,tg_cc2,tg_cc3,
             tg_da, tg_db, tg_na, tg_nb, tg_nt,
             tg_dda,tg_ddb,tg_nna,tg_nnb,tg_nnt,
             &tg_dc, &tg_nc, &tg_ddc, &tg_nnc,
             &tg_npan, &tg_nt_tot,
             &w_a, &w_b, &w_c,
             SA,ST,
             RA,RT,RUz,

             &atn, attable,

             dbDir,&LendbDir,OutDir,&LenOutDir,
             fpdir,&lenfpdir,

             &myrank, &size, &iope, &numtracceperpe,
             &datadecimation, vett_ape, &apeiniz, &zetamaxape, &maxdisty, &maxdistx,
             vett_distx, &distxiniz, &zmaxdistx, vett_disty, &distyiniz, &zmaxdisty,
             &muteFlag, vett_off, &zdistyiniz, 
             tg_weights_cub_b, tg_weights_cub_c, tg_weights_lin_b, tg_weights_lin_c, tg_weights_lin_a,
             rangeT, rangeA, rangeCz, tempfdg,
             &dip_max, &dip_min, &azimuth_max, &azimuth_min, 
             SVx, SVy, SVz, RVx, RVy, RVz, &tg_nt_ava, &avo,
             &quantTAC, &quantV, sortfileTAC, sortfileV,grid_el
             ); 

	 printf("--- Equalizzazione ---\n");
	 fflush(stdout);
/******************************************************************************************************/

         glob_fclose64_(&fidsgy, &file_error);
         cursgy = cursgy->nextsgy;

      }  
/******************************************************************************************************/
/* CCC 26/05/2002 Deallocazione memoria */

      free(acq_o1);      free(acq_o2);      free(acq_o3);
      free(acq_ca1);     free(acq_ca2);     free(acq_ca3);
      free(acq_cb1);     free(acq_cb2);     free(acq_cb3);
      free(acq_cc1);     free(acq_cc2);     free(acq_cc3);
      free(acq_da);      free(acq_db);
      free(acq_na);      free(acq_nb);
      free(acq_nt);
      free(acq_dda);     free(acq_ddb);
      free(acq_nna);     free(acq_nnb);
      free(acq_nnt);
      free(tg_o1);       free(tg_o2);       free(tg_o3);
      free(tg_ca1);      free(tg_ca2);      free(tg_ca3);
      free(tg_cb1);      free(tg_cb2);      free(tg_cb3);
      free(tg_cc1);      free(tg_cc2);      free(tg_cc3);
      free(tg_da);       free(tg_db);
      free(tg_na);       free(tg_nb);
      free(tg_nt);
      free(tg_dda);      free(tg_ddb);
      free(tg_nna);      free(tg_nnb);
      free(tg_nnt);
      free(tg_weights_lin_a);
      free(tg_weights_lin_b);
      free(tg_weights_lin_c);
      free(tg_weights_cub_b);
      free(tg_weights_cub_c);
      free(SA);
      free(ST);
      free(dbrestart);
      free(RA);
      free(RT);
      free(RUz);
      if (ava==1) {
      	free(SVx);
      	free(SVy);
      	free(SVz);
      	free(RVx);
      	free(RVy);
      	free(RVz);
      }
      free(sortfileTAC);
      free(sortfileV);
      free(tracce);
      free(traces);
      free(traccia_temp);
      free(traccia_letta);
      free(traccia_lettac);
      free(traccia_lettash);
      free(vett_ape);
      free(vett_distx);
      free(vett_disty);   
      free(vett_off_in);
      free(vett_depth_in);
      free(vett_off);
      free(rangeA);
      free(rangeT);
      free(rangeCz);
      free(tempfdg);         
      free(traccia1);
      free(traccia2);
      free(tr_xr);
      free(tr_yr);
      free(tr_zr);
      free(binr_offset);
      free(bins_offset);
      free(tr_xs);
      free(tr_ys);
      free(tr_zs);
      free(tr_ns);
      free(tr_weights);
      free(attable);
      free(pan_save);
      free(pan_parz);
      free(pan_tot);
      free(cont_save);
      free(cont_rparz);
      free(cont_r);

/******************************************************************************************************/
      if(myrank == iope && (verbose > 0)) printf("That's all folks!\n");
      
      MPI_Finalize(); 
      exit(EXIT_SUCCESS);

}


void interp_weights( float *tg_weights_cub, float *tg_weights_lin, int tg_rr, int j, float u)
{
                   /*-------------------- PESI INTERPOLAZIONE LINEARE------------------------------------------- */
                   tg_weights_lin[0+j] = (1-u);   
                   
                   /*--------------------  PESI INTERPOLAZIONE CUBICA ------------------------------------------ */
                   /* BORDO A SINISTRA */
                   tg_weights_cub[0 +j*4              ] = ((-1.0/6.0*(u-1) + 1.0/2.0)*(u-1) - 1.0/3.0)*(u-1);    
                   tg_weights_cub[1 +j*4              ] = (( 1.0/2.0*(u-1) -       1)*(u-1) - 1.0/2.0)*(u-1) + 1;
                   tg_weights_cub[2 +j*4              ] = ((-1.0/2.0*(u-1) + 1.0/2.0)*(u-1) +     1.0)*(u-1);    
                   tg_weights_cub[3 +j*4              ] =  ( 1.0/6.0*(u-1)*(u-1)            - 1.0/6.0)*(u-1);    
                   
                   /* CENTRO */
                   tg_weights_cub[0 +j*4 + 4*tg_rr] = ((-1.0/6.0*u + 1.0/2.0)*u - 1.0/3.0)*u;                     
                   tg_weights_cub[1 +j*4 + 4*tg_rr] = (( 1.0/2.0*u -       1)*u - 1.0/2.0)*u + 1;  
                   tg_weights_cub[2 +j*4 + 4*tg_rr] = ((-1.0/2.0*u + 1.0/2.0)*u +     1.0)*u;      
                   tg_weights_cub[3 +j*4 + 4*tg_rr] =  ( 1.0/6.0*u*u            - 1.0/6.0)*u;                     
                   
                   /* BORDO DESTRO */
                   tg_weights_cub[0 +j*4 + 8*tg_rr] =  ((-1.0/6.0*(u+1) + 1.0/2.0)*(u+1) - 1.0/3.0)*(u+1);                     
                   tg_weights_cub[1 +j*4 + 8*tg_rr] =  (( 1.0/2.0*(u+1) -       1)*(u+1) - 1.0/2.0)*(u+1) + 1;
                   tg_weights_cub[2 +j*4 + 8*tg_rr] =  ((-1.0/2.0*(u+1) + 1.0/2.0)*(u+1) +     1.0)*(u+1);    
                   tg_weights_cub[3 +j*4 + 8*tg_rr] =   ( 1.0/6.0*(u+1)*(u+1)            - 1.0/6.0)*(u+1);                      
                  
                   /* ULTIMA COLONNA */
                   tg_weights_cub[0 +j*4 + 12*tg_rr] = 0.0;                    
                   tg_weights_cub[1 +j*4 + 12*tg_rr] = 0.0; 
                   tg_weights_cub[2 +j*4 + 12*tg_rr] = 0.0;     
                   tg_weights_cub[3 +j*4 + 12*tg_rr] = 1.0;    
}




void binsphere(int naz,int nel,float *el)
{
float *B,*H,th,ATOT,A;
int i;


B = (float *)calloc(nel+1,sizeof(float *));
H = (float *)calloc(nel+1,sizeof(float *));


B[0]=2*PI/naz;
th=B[0];


ATOT=(B[0]*B[0])/(4*tan(th/2));


A=ATOT/nel;


el[0]=0;

for (i=1;i<=nel;i++)
{
B[i]=(sqrt(fabs(B[i-1]*B[i-1]-4*A*tan(th/2) )));
H[i-1]=(B[i-1]-B[i])/(2*tan(th/2));
el[i]=H[i-1]+el[i-1];

}

for (i=1;i<=nel;i++)
{
el[i]=el[i]/el[nel]*PI/2;
}
}
