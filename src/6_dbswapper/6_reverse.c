/****************************************************/
/*                   reverse.c                      */
/****************************************************/
#include "../0_include/0_common_defs.h"

void reverse(char *Hfile, char *Dfile, char *outHfile, char *outDfile, char *name,
             int sorting, int updown, int nshot, int ntarget, int isrestart, int *sortList)
{
    long i,j,kk;

    char *str, *str1, *str2;

    int fdesc, fdesc2;
    char* mode;
    int dimShort, num, numin, numout;
    int64 in64, in64_1, in64_2;   /* R64 lug 2001 */
    int64 swapped; 
    int origin, cur, endoffile;
    int file_error;
    long len, len1;
    float ff;
    long nBuffIn;
    short int *buffin;
    long nBuffOut;
    short int *buffout;
    int endfile;
    
    int nBuf2Read, nRowBuf, nElem2Write, Lbuf;
    int i1, i2, i3;
    int nBlockSwapped;
    long indx;


/* --------------- Initialization ---------- */
/*===========================================*/

    printf("reverse 1. Inizialization ..\n"); fflush(stdout);
    str  = calloc(MAX_STRING_LEN, sizeof(char));
    str1 = calloc(MAX_STRING_LEN, sizeof(char));
    str2 = calloc(MAX_STRING_LEN, sizeof(char));
    mode = calloc(10, sizeof(char));

    /* Numero di righe in un blocco di lettura */
    nRowBuf = (int) (2.5e8/(float) nshot);  
    
    /* Dimensione del buffer di uscita (numero di elementi) */
    nBuffOut = (long) nRowBuf * (long) nshot;

    /* Dimensione del buffer di ingresso (numero di elementi) */
    nBuffIn  = nRowBuf;
    
    /* Numero di byte per uno short int */
    dimShort = sizeof(short int);

/*------- Allocazione -------*/
/*===========================*/

    buffout  = calloc(nBuffOut,dimShort);
    if ( buffout == NULL)
    {
        fprintf(stderr, "ERROR: cannot allocate buffout \n");
        exit(EXIT_FAILURE);
    }
    else
    {
    	printf("Allocati %10.0f bytes per buffout \n", (float) ((long) dimShort* (long) nBuffOut));
    	if (  dimShort *  nBuffOut > 1000000000)
    	   printf("!!ATTENZIONE: la dimensione dell'array e' maggiore di 1GigaByte \n");
    }
    
    buffin   = calloc(nBuffIn,dimShort);


/* --------------- Open files --------------- */
/*============================================*/

    printf("reverse 2.a Open files ..\n"); fflush(stdout);
    fdesc=0;
    strcpy(mode,"rb");
    glob_fopen64_(&fdesc, Dfile, mode, &file_error, strlen(Dfile), strlen(mode)); /* R64 lug 2001 */
    if (file_error != 0)
     {
        fprintf(stderr, "ERROR: unable to read file ->  %s \n", Dfile);
        exit(EXIT_FAILURE);
      }
      
     printf("reverse 2.b Open files ..\n"); fflush(stdout);
     fdesc2=1;
     if (isrestart == 0)   
        strcpy(mode,"wb");   /* Apertura senza restart */
     else
        strcpy(mode,"rb+");  /* Apertura con restart */         

     glob_fopen64_(&fdesc2, outDfile, mode, &file_error, strlen(outDfile), strlen(mode));
        
     if (file_error != 0)
     {
        fprintf(stderr, "ERROR: unable to write file ->  %s \n", outDfile);
        exit(EXIT_FAILURE);
     }
     
	


/* ----------- Creating SEP Header files -------------------- */
/*===========================================================*/

    len1 = MAX_STRING_LEN;
    strcpy(str, "TITLE");
    seph_get_string_(Hfile, str, str1, &len, strlen(Hfile), strlen(str), len1);
    str1[len]='\0';

    strcpy(str2, "./");
    strcat(str2, name);

    strcpy(str, "title");
    seph_put_string_(outHfile,str,str1,strlen(outHfile),strlen(str),strlen(str1));

    strcpy(str, "updown");
    seph_put_int_(outHfile,str,&sorting,strlen(outHfile),strlen(str));

    if (sorting != updown)
      {
        strcpy(str, "nshot");
        seph_put_int_(outHfile,str,&ntarget,strlen(outHfile),strlen(str));

        strcpy(str, "ntarget");
         seph_put_int_(outHfile,str,&nshot,strlen(outHfile),strlen(str));
      }
    else
      {
        strcpy(str, "nshot");
        seph_put_int_(outHfile,str,&nshot,strlen(outHfile),strlen(str));

        strcpy(str, "ntarget");
         seph_put_int_(outHfile,str,&ntarget,strlen(outHfile),strlen(str));
      }

    strcpy(str, "esize");
    seph_put_int_(outHfile,str, &dimShort,strlen(outHfile),strlen(str));

    strcpy(str, "in");
    seph_put_string_(outHfile,str,str2,strlen(outHfile),strlen(str),strlen(str2));


/* --------------- Sorting ---------------------------- */
/*======================================================*/

    printf("Sorting..\n"); fflush(stdout);
    
    origin        = 0;
    cur           = 1;
    endoffile     = 2;
    nBlockSwapped = 0;

    if (sorting != updown)
      {
      	/* Numero di letture del file in ingresso = 
      	   = numero di ricevitori / numero di ricevitori letti per volta 
      	*/
      	nBuf2Read = ntarget/nRowBuf;
      	
      	if (isrestart == 1)
      	{      	
      		/* Determina il numero blocchi gia' swappati
      		   ========================================= */
      		in64=(int64)1;	
      		
      		/* Posiziona puntatore in fondo al file 
      		   ====================================*/      		
            glob_fseek64_( &fdesc2, &in64, &endoffile, &file_error);	
            
            /* Legge la dimensione del file 
               ============================*/
            glob_ftell64_( &fdesc2, &swapped, &file_error);
            
            /* Riposiziona il puntatore all'inizio del file 
               ============================================*/
            glob_fseek64_( &fdesc2, &in64, &origin, &file_error);
            
            /* Determina il numero di blocchi di righe trasposti 
               prima dell'interruzione 
               =================================================*/
            nBlockSwapped = (int) (swapped / (dimShort*nBuffOut));

            /* Posiziona il puntatore sulla posizione del primo elemento da scrivere 
               =====================================================================*/
            in64=(int64)(nBlockSwapped*dimShort*nBuffOut+1);
            glob_fseek64_( &fdesc2, &in64, &origin, &file_error);
      	}

      	printf(" nBlockSwapped %i nBuf2Read %i \n", nBlockSwapped, nBuf2Read);
      	
      	/* Ciclo sulle letture del file
      	   ============================ */
      	for ( i1=nBlockSwapped; i1<nBuf2Read+1; i1++)
      	{
           printf("Ciclo sulle letture del file: i1=%i di %i \n", i1+1, nBuf2Read+1); fflush( stdout);
           
      	   /* Skip degli elementi gia' letti della prima colonna
      	      ================================================== */
      	   in64 = (int64)((i1*nRowBuf)*dimShort +1);
      	   glob_fseek64_( &fdesc, &in64, &origin, &file_error);
      	   
      	   /* Numero effettivo di ricevitori da leggere
      	      ========================================= */
 	   if ( i1 < nBuf2Read )
 	      Lbuf = nRowBuf;
 	   else
 	      Lbuf = ntarget - nRowBuf * nBuf2Read;
 	      
 	   /* Esce dal ciclo se l'ultima lettura non e' necessaria
 	      ==================================================== */
 	   if ( Lbuf == 0 )
 	      break;
 	   else if ( Lbuf <0 )
 	      printf("ERRORE!!!!!");
 	   
 	   /* Ciclo sugli shot (colonne) 
 	      ========================== */   
      	   for ( i2=0; i2<nshot; i2++)
      	   {
      	      if ( (i2/10000)*10000 == i2)
      	      printf("Ciclo sugli shot: i2=%i di %i \n", i2, nshot); fflush( stdout); 
      	      
      	      /* Lettura di Lbuf ricevitori dello shot (colonna) corrente
      	         ======================================================== */
      	      glob_fread_(&fdesc, buffin, &dimShort, &Lbuf, &file_error);

      	      
      	      /* Skip fino al 1^ ricevitore (riga) da leggere del prossimo shot
      	         ============================================================== */
      	      in64 = (int64)( (ntarget - Lbuf)*dimShort + 1);
      	      glob_fseek64_( &fdesc, &in64, &cur, &file_error);
      	      
      	      /* Ciclo sugli elementi letti che effettua la trasposizione
      	         ======================================================== */
      	      for ( i3=0; i3<Lbuf; i3++)
      	      {
      	      	indx = (long) (sortList[i2]-1) + (long) nshot * (long) i3;
      	         buffout[ indx ] = buffin[i3];
      	      }
      	   }
      	   
      	  /* Scrittura sul file in output
      	     ============================ */
      	   nElem2Write = Lbuf * nshot/10;
      	   for ( i2=0; i2<10; i2++)
      	   {
      	      /* Questo ciclo serve per evitare di scrivere blocchi di dati
      	         piu' lunghi di 2Giga. Lo spezzettamento in 10 parti dovrebbe
      	         essere sufficiente, ma non e' detto che sia la scelta ottima 
      	         ============================================================ */
      	      indx = nElem2Write*i2;
               if ( i2 == 9 ) 
                 nElem2Write = Lbuf*nshot - nElem2Write *9;              	 
                 
      	      printf("# elementi da scrivere %i \n", nElem2Write);
      	      glob_fwrite_(&fdesc2, &(buffout[indx]), &dimShort, &nElem2Write, &file_error);
      	      if ( file_error == 1)
      	         printf("Non ha scritto correttamente \n");
      	   }
      	   
      	}
      }
    else
      {
        printf("Writing data...         "); fflush(stdout);

        dimShort = sizeof(short int);
        num = ntarget;
        origin = 0;

        for (i=1; i<=nshot; i++)
         {

           glob_fread_(&fdesc, buffin, &dimShort, &num, &file_error);
           glob_fwrite_(&fdesc2, buffin, &dimShort, &num, &file_error);
           ff = (float)i / (float)(long)(nshot/10);
           if ( (ff - (float)(long)ff ) == 0)
                  { printf("%d ",(long)ff ); fflush(stdout); }

         }
        printf("\n");
      }

    glob_fclose_(&fdesc, &file_error);
    glob_fclose_(&fdesc2, &file_error);

/* ----------- Freeing memory  -------------------- */

    free(str);
    free(str1);
    free(str2);
    free(mode);
    free(buffin);
    free(buffout);

 }
