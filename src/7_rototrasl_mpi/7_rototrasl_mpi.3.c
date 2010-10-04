#include "../0_include/0_segyheader.h"
#include "../0_include/0_common_defs.h"

FILE* glob_fp[MAX_FILE_NUM];
const float pi=3.14159265358979;
const float eps=0.000000001;
/***************************************************************************/

#include <string.h>
#include <stdio.h>
#include <math.h>

int ReadVelocitySegyFile();
extern void segyopenread_();
extern void segygetheader_();
extern void segygettrace_();
extern void segyclose_();

/*------------------------------------------------------------*/
/*------------- MAIN PROGRAM ---------------------------------*/

int  main(int argc, char* argv[])
{
  char* nome[MAX_PARAM];
  char* val[MAX_PARAM];
  char modellosgy[MAX_STRING_LEN];
  char modellosep[MAX_STRING_LEN];
  char headersep[MAX_STRING_LEN];
  int tipo,campioni,max,i;
  int n_il,n_xl,n_z,ydir;
  int incxl_tgt,incil_tgt,incxl_vel,incil_vel;
  float velbin_il,velbin_xl;
  float dim_il,dim_xl;
  int corner[4][2];
  FILE *setup,*fdescheader,*fp;

  int formato,ntraccetot,ncampioni,j;
  int velsup,imaging;                                  /* CCC 25/07/2002 */ /* ,dimgruppo,dimblocco;   */
  char filetracce[MAX_STRING_LEN],dbasedir[MAX_STRING_LEN];
  char modello[MAX_STRING_LEN],filepesi[MAX_STRING_LEN];
  int firsttrace,antialias;
  float aox,aoy,aoz,tox,toy,toz,xtemp,ytemp;
  float aox1,aoy1,aoz1,tox1,toy1,toz1;
  float ox,oy,oz,ox1,oy1,oz1;
  float la1, la2, lt1, lt2, ltz;
  int na1,na2;
  int nt1,nt2,ntz,nnt1,nnt2,nntz;
  float da1,da2,dt1,dt2,dz;
  int numshot,xsource,ysource,zsource,xreceiver,yreceiver,zreceiver;
  int snumshot,sxsource,sysource,szsource,sxreceiver,syreceiver,szreceiver;
  char offmin[30], offmax[30];
  int restart;
  int bordf,aperturai,modalita,versione,inizio;
  float aperturam,smussamento,sogliaampiezze;
  float ampmin,damp,angolo;
  float dt,dx,tshift,sogliax,sogliay;
  float aztgtdeg, aztgtrad, azmoddeg, azmodrad;
  int ncpu;
  double dmempcpu;
  float memtot,memsh,mega,mempcost;
  float mempcpu,memptot,memptrac;
  float oxminimod,oyminimod,ozminimod;
  int sottomod, nxminimod,nyminimod,nzminimod,pxminimod,pyminimod,pzminimod;
  int nrprimail, nrprimaxl;
/* Clara2 decimazione delle tracce e apertura variabile con la profondita'   */  
  int  datadecimation;
  float aperturainiz, zetamaxape;
 /* Clara3 distanza massima Y (XLINE) e X (INLINE) a cui si migrano i dati*/ 
  int maxdisty,maxdistx;
 /*Clara4 stride numerazione INLINE e XLINE*/
  int strideIL, strideXL;
 /*Clara5 no processing velocity file*/ 
  int procvel,cornerx,cornery;
 /* Clara6 dist variabili e mute*/
  int muteflag, zmaxdistx, zmaxdisty, distxiniz,distyiniz;
  char mutefilename[MAX_STRING_LEN];
/* CCC 25/07/2002 */
  int Npackets, memoffset, numtracceperpe;


  /* Test sintassi di chiamata -----------------------------*/
  if (argc < 2)
    {
      fprintf(stderr,"ERROR : Missing command line parameter.\n");
      fprintf(stderr,"*** ROTOTRASL v. %s ***\n",VERSION);
      fprintf(stderr,"rototrasl.x (par file <param.p>)\n\n");
      exit(EXIT_FAILURE);
    }

  printf("\n");
  printf("*** ROTOTRASL v. %s ***\n\n",VERSION);

  /* inizializzazione dei parametri ai valori di default */

  ydir=1;
  mega=1024*1024;

  numshot  = 9;
  snumshot = 4;

  xsource  = 73;
  sxsource = 4;
  ysource  = 77;
  sysource = 4;
  zsource  = 45;
  szsource = 4;

  xreceiver  = 81;
  sxreceiver = 4;
  yreceiver  = 85;
  syreceiver = 4;
  zreceiver  = 41;
  szreceiver = 4;

  firsttrace = 1;

  tshift=0;

  filepesi[0]=('\0');

  offmin[0]=('\0');
  offmax[0]=('\0');

/* Clara2 decimazione delle tracce e apertura variabile con la profondita'   */     
  datadecimation = 1;
  aperturainiz = 0;
  zetamaxape = 0; 
/* Clara3 distanza massima Y (XLINE)e X(INLINE) a cui si migrano i dati*/  
  maxdisty = -1;
  maxdistx= -1;
 /*Clara4 stride*/
  strideIL = 1;
  strideXL = 1;
 /*Clara5*/
  procvel=1;
  /*Clara6*/
 muteflag=0;
 zmaxdistx=-1;
 zmaxdisty=-1;
 distxiniz=-1;
 distyiniz=-1;
 mutefilename[0]=('\0');
 
  
  inizio    = 10;
  versione  = 2;
  modalita  = 1;
  sogliax   = 0.1;
  sogliay   = 0.1;
  aperturai = 2;
  angolo    = 75;
  ampmin    = 1E-6;
  damp      = 0.95 ;

  sottomod  = 1;
  oxminimod = 0.0;
  oyminimod = 0.0;
  ozminimod = 0.0;

  lt1 = 0;
  lt2 = 0;

  nrprimail = -1;
  nrprimaxl = -2;

  antialias=0;
/* CCC 25/07/2002 */
/*  dimblocco=100000;
  dimgruppo=2500;       */

/* CCC 25/07/2002 */
  Npackets       = 1;
  memoffset      = 0;
  numtracceperpe = 1000;

  for (i=0; i< MAX_PARAM; i++)
    {
      nome[i] = calloc(MAX_STRING_LEN, sizeof(char));
      val[i] = calloc(MAX_STRING_LEN, sizeof(char));
    }

  /* Lettura file di parametri  (in ordine alfabetico) -------------------*/

  fp=fopen(argv[1], "r");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to open file  %s \n", argv[1]);
      exit(EXIT_FAILURE);
    }

  fprintf(stderr, " Lettura file di parametri %s \n", argv[1]);

  max=0;
  max=ReadParams(fp, nome, val, MAX_STRING_LEN , MAX_PARAM );
  fclose(fp);

  printf(" Parametri letti: %d\n\n",max);

  for (i=0; i<max; i++)
    {
      if (strcmp("ANGOLO", nome[i]) == 0 )
        { sscanf(val[i],"%f", &angolo); }
      else if (strcmp("AMPMIN", nome[i]) == 0 )
        { sscanf(val[i],"%f", &ampmin); }
      else if (strcmp("ANTIALIAS", nome[i]) == 0 )
        { sscanf(val[i],"%d", &antialias); }
      else if (strcmp("APERTURAI", nome[i]) == 0 )
        { sscanf(val[i],"%d", &aperturai); }
      else if (strcmp("AZ_TARGET", nome[i]) == 0 )
        { sscanf(val[i],"%f", &aztgtdeg); }
      else if (strcmp("AOX", nome[i]) == 0 )
        { sscanf(val[i],"%f", &aox); }
      else if (strcmp("AOY", nome[i]) == 0 )
        { sscanf(val[i],"%f", &aoy); }
      else if (strcmp("AOZ", nome[i]) == 0 )
        { sscanf(val[i],"%f", &aoz); }
      else if (strcmp("BORDF", nome[i]) == 0 )
        { sscanf(val[i],"%d", &bordf);  }
      else if (strcmp("DAMP", nome[i]) == 0 )
        { sscanf(val[i],"%f", &damp); }
      else if (strcmp("DATABASEDIR", nome[i]) == 0 )
        { sscanf(val[i],"%s", dbasedir); }
/*      else if (strcmp("DIMBLOCCOTRACCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &dimblocco); }                   */
      else if (strcmp("DIM_IL", nome[i])== 0)
        { sscanf(val[i],"%f",&dim_il ); }
      else if (strcmp("DIM_XL", nome[i])== 0)
        { sscanf(val[i],"%f",&dim_xl ); }
      else if (strcmp("DT", nome[i]) == 0 )
        { sscanf(val[i],"%f", &dt); }
      else if (strcmp("DX", nome[i]) == 0 )
        { sscanf(val[i],"%f", &dx); }
      else if (strcmp("FILEMODELLO", nome[i]) == 0 )
        { sscanf(val[i],"%s", modellosgy); }
      else if (strcmp("FILEPESI", nome[i]) == 0 )
        { sscanf(val[i],"%s", filepesi); }
      else if (strcmp("FILETRACCE", nome[i]) == 0 )
        { sscanf(val[i],"%s", filetracce); }
      else if (strcmp("FIRSTTRACE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &firsttrace); }
      else if (strcmp("FORMATO", nome[i]) == 0 )
        { sscanf(val[i],"%d", &formato); }
      else if (strcmp("IMAGING", nome[i]) == 0 )
        { sscanf(val[i],"%d", &imaging); }
      else if (strcmp("INIZIO", nome[i]) == 0 )
        { sscanf(val[i],"%d", &inizio); }
      else if (strcmp("LA1", nome[i]) == 0 )
        { sscanf(val[i],"%f", &la1); }
      else if (strcmp("LA2", nome[i]) == 0 )
        { sscanf(val[i],"%f", &la2); }
      else if (strcmp("LT1", nome[i]) == 0 )
        { sscanf(val[i],"%f",&lt1 ); }
      else if (strcmp("LT2", nome[i]) == 0 )
        { sscanf(val[i],"%f",&lt2 ); }
      else if (strcmp("LTZ", nome[i]) == 0 )
        { sscanf(val[i],"%f", &ltz); }
      else if (strcmp("NA1", nome[i]) == 0 )
        { sscanf(val[i],"%d", &na1); }
      else if (strcmp("NA2", nome[i]) == 0 )
        { sscanf(val[i],"%d", &na2); }
      else if (strcmp("NCAMPIONI", nome[i]) == 0 )
        { sscanf(val[i],"%d", &ncampioni); }
      else if (strcmp("NTRACCETOT", nome[i]) == 0 )
        { sscanf(val[i],"%d", &ntraccetot); }
      else if (strcmp("MEMPRIVATE", nome[i]) == 0 )
        { sscanf(val[i],"%f", &mempcpu);        }
      else if (strcmp("MODALITA", nome[i]) == 0 )
        { sscanf(val[i],"%d", &modalita); }
      else if (strcmp("MODELLO", nome[i]) == 0 )
        { sscanf(val[i],"%s", modello); }
      else if (strcmp("MPTOT", nome[i]) == 0 )
        { sscanf(val[i],"%f", &memptot); }
      else if (strcmp("NCPU", nome[i]) == 0 )
        { sscanf(val[i],"%d", &ncpu); }
      else if (strcmp("NRPRIMAIL", nome[i])== 0)
        { sscanf(val[i],"%d",&nrprimail );}
      else if (strcmp("NRPRIMAXL", nome[i])== 0)
        { sscanf(val[i],"%d",&nrprimaxl );}
      else if (strcmp("NT1", nome[i]) == 0 )
        { sscanf(val[i],"%d", &nt1); }
      else if (strcmp("NT2", nome[i]) == 0 )
        { sscanf(val[i],"%d", &nt2); }
      else if (strcmp("NNT1", nome[i]) == 0 )
        { sscanf(val[i],"%d", &nnt1); }
      else if (strcmp("NNT2", nome[i]) == 0 )
        { sscanf(val[i],"%d", &nnt2); }
      else if (strcmp("NTZ", nome[i]) == 0 )
        { sscanf(val[i],"%d", &ntz); }
      else if (strcmp("NNTZ", nome[i]) == 0 )
        { sscanf(val[i],"%d", &nntz); }
      else if (strcmp("OFFMIN", nome[i]) == 0 )
        { sscanf(val[i],"%s", offmin);  }
      else if (strcmp("OFFMAX", nome[i]) == 0 )
        { sscanf(val[i],"%s", offmax);  }
      else if (strcmp("POS-NUMSHOT", nome[i]) == 0 )
        { sscanf(val[i],"%d", &numshot); }
      else if (strcmp("POS-XSOURCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &xsource); }
      else if (strcmp("POS-YSOURCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &ysource); }
      else if (strcmp("POS-ZSOURCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &zsource); }
      else if (strcmp("POS-XRECEIVER", nome[i]) == 0 )
        { sscanf(val[i],"%d", &xreceiver); }
      else if (strcmp("POS-YRECEIVER", nome[i]) == 0 )
        { sscanf(val[i],"%d", &yreceiver); }
      else if (strcmp("POS-ZRECEIVER", nome[i]) == 0 )
        { sscanf(val[i],"%d", &zreceiver); }
      else if (strcmp("RESTART", nome[i]) == 0 )
        { sscanf(val[i],"%d", &restart);        }
      else if (strcmp("SCAMP_IL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &pxminimod); }
      else if (strcmp("SCAMP_XL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &pyminimod); }
      else if (strcmp("SCAMP_Z", nome[i]) == 0 )
        { sscanf(val[i],"%d", &pzminimod); }
      else if (strcmp("SEMIAPERTURA", nome[i]) == 0 )
        { sscanf(val[i],"%f", &aperturam); }
      else if (strcmp("SIZE-NUMSHOT", nome[i]) == 0 )
        { sscanf(val[i],"%d", &snumshot); }
      else if (strcmp("SIZE-XSOURCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &sxsource); }
      else if (strcmp("SIZE-YSOURCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &sysource); }
      else if (strcmp("SIZE-ZSOURCE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &szsource); }
      else if (strcmp("SIZE-XRECEIVER", nome[i]) == 0 )
        { sscanf(val[i],"%d", &sxreceiver); }
      else if (strcmp("SIZE-YRECEIVER", nome[i]) == 0 )
        { sscanf(val[i],"%d", &syreceiver); }
      else if (strcmp("SIZE-ZRECEIVER", nome[i]) == 0 )
        { sscanf(val[i],"%d", &szreceiver); }
      else if (strcmp("SMUSSAMENTO", nome[i]) == 0 )
        { sscanf(val[i],"%f", &smussamento); }
      else if (strcmp("SOGLIAAMPIEZZE", nome[i]) == 0 )
        { sscanf(val[i],"%f", &sogliaampiezze); }
      else if (strcmp("SOGLIAX", nome[i]) == 0 )
        { sscanf(val[i],"%f", &sogliax); }
      else if (strcmp("SOGLIAY", nome[i]) == 0 )
        { sscanf(val[i],"%f", &sogliay); }
      else if (strcmp("SOTTOMOD", nome[i]) == 0 )
        { sscanf(val[i],"%d", &sottomod); }
      else if (strcmp("TEMP", nome[i]) == 0 )
        { sscanf(val[i],"%s", modellosep); }
      else if (strcmp("TIPOOUT", nome[i]) == 0 )
        { sscanf(val[i],"%d", &tipo); }
      else if (strcmp("TOX", nome[i]) == 0 )
        { sscanf(val[i],"%f", &tox); }
      else if (strcmp("TOY", nome[i]) == 0 )
        { sscanf(val[i],"%f", &toy); }
      else if (strcmp("TOZ", nome[i]) == 0 )
        { sscanf(val[i],"%f", &toz); }
      else if (strcmp("TSHIFT", nome[i]) == 0 )
        { sscanf(val[i],"%f", &tshift); }
      else if (strcmp("VELSUP", nome[i]) == 0 )
        { sscanf(val[i],"%d", &velsup); }
      else if (strcmp("VERSIONE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &versione); }
      else if (strcmp("NPACKETS", nome[i]) == 0 )
        { sscanf(val[i],"%d", &Npackets); }
      else if (strcmp("MEMOFFSET", nome[i]) == 0 )
        { sscanf(val[i],"%d", &memoffset); }
      else if (strcmp("NUMTRACCEPERPE", nome[i]) == 0 )
        { sscanf(val[i],"%d", &numtracceperpe); }
/* Clara2 decimazione tracce e apertura variabile con la profondita'   */   
      else if (strcmp("DATADECIMATION", nome[i]) == 0 )
        { sscanf(val[i],"%d", &datadecimation); }
      else if (strcmp("APERTURAINIZ", nome[i]) == 0 )
        { sscanf(val[i],"%f", &aperturainiz); }
      else if (strcmp("ZETAMAXAPE", nome[i]) == 0 )
        { sscanf(val[i],"%f", &zetamaxape); }
/* Clara3 distanza massima Y (XLINE) e X (INLINE) a cui si migrano i dati*/  
      else if (strcmp("MAXDISTY", nome[i]) == 0 )
        { sscanf(val[i],"%d", &maxdisty); }
      else if (strcmp("MAXDISTX", nome[i]) == 0 )
        { sscanf(val[i],"%d", &maxdistx); }
/* Clara4 stride IL e XL*/  
      else if (strcmp("STRIDEIL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &strideIL); }
       else if (strcmp("STRIDEXL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &strideXL); }
/*Clara5 no processing velocity file*/ 
        else if (strcmp("PROCVEL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &procvel); }
/*Clara6*/
      else if (strcmp("ZMAXDISTX", nome[i]) == 0 )
        { sscanf(val[i],"%d", &zmaxdistx); }
      else if (strcmp("ZMAXDISTY", nome[i]) == 0 )
        { sscanf(val[i],"%d", &zmaxdisty); }
      else if (strcmp("MUTEFLAG", nome[i]) == 0 )
        { sscanf(val[i],"%d", &muteflag); }
      else if (strcmp("MUTEFILENAME", nome[i]) == 0 )
        { sscanf(val[i],"%s", mutefilename); }
       else if (strcmp("DISTYINIZ", nome[i]) == 0 )
        { sscanf(val[i],"%d", &distyiniz); }
        else if (strcmp("DISTXINIZ", nome[i]) == 0 )
        { sscanf(val[i],"%d", &distxiniz); }
    }

 

/*Clara5 no processing velocity file*/ 
if (procvel==1)
{
  /* Processing file modello di velocita' ------------------------------ */

  strcpy(headersep,modellosep);
  strcat(modellosep,"temp.H@");
  strcat(headersep,"temp.H");
  printf(" File di velocita' temporaneo (SEP) in %s \n",headersep);

  /* Richiamo la procedura che scrive il file temporaneo in formato SEP */
  /* e che calcola i parametri geometrici del grid di velocita'         */



  ReadVelocitySegyFile(modellosgy,modellosep,&campioni,&dz,&n_xl,&n_il,
                       &velbin_il,&velbin_xl,corner,&azmodrad,&ydir);

  fp=fopen("veloinfo.p", "w");
	  if (fp ==  NULL)
	    {
	      fprintf(stderr, " \n  ERROR: unable to create file \"veloinfo.p\"\n");
	      exit(EXIT_FAILURE);
	    }
	  fprintf(fp,"velosgy = %s\n",modellosgy);
	  fprintf(fp,"campioni = %d \n",campioni);
	  fprintf(fp,"dz = %f \n",dz);
	  fprintf(fp,"n_xl = %d\n",n_xl);
	  fprintf(fp,"n_il = %d \n",n_il);
	  fprintf(fp,"velbin_il = %f \n",velbin_il);
	  fprintf(fp,"velbin_xl = %f \n",velbin_xl);
	  fprintf(fp,"cornerx = %d \n",corner[0][0]);
	  fprintf(fp,"cornery = %d \n",corner[0][1]);
	  fprintf(fp,"azmodrad = %f \n",azmodrad);
	  fprintf(fp,"ydir = %d \n",ydir);
  fclose(fp);
} else {
  fp=fopen("veloinfo.p", "r");
	  if (fp ==  NULL)
	    {
	      fprintf(stderr, " \n  ERROR: unable to open file veloinfo.p\n");
	      exit(EXIT_FAILURE);
	    }
	
	  fprintf(stderr, " Lettura file di parametri in veloinfo.p \n");
	
	  max=0;
	  max=ReadParams(fp, nome, val, MAX_STRING_LEN , MAX_PARAM );
  fclose(fp);

  printf(" Parametri letti: %d\n\n",max);

  for (i=0; i<max; i++)
    {
      if (strcmp("VELOSGY", nome[i]) == 0 )
        { sscanf(val[i],"%s", &modellosgy); }
      else if (strcmp("N_XL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &n_xl); }
      else if (strcmp("N_IL", nome[i]) == 0 )
        { sscanf(val[i],"%d", &n_il); }
      else if (strcmp("CAMPIONI", nome[i]) == 0 )
        { sscanf(val[i],"%d", &campioni); }
      else if (strcmp("DZ", nome[i]) == 0 )
        { sscanf(val[i],"%f", &dz); }
      else if (strcmp("VELBIN_IL", nome[i]) == 0 )
        { sscanf(val[i],"%f", &velbin_il); }
      else if (strcmp("VELBIN_XL", nome[i]) == 0 )
        { sscanf(val[i],"%f", &velbin_xl); }
      else if (strcmp("CORNERX", nome[i]) == 0 )
        { sscanf(val[i],"%d", &cornerx); }
      else if (strcmp("CORNERY", nome[i]) == 0 )
        { sscanf(val[i],"%d", &cornery); }
      else if (strcmp("AZMODRAD", nome[i]) == 0 )
        { sscanf(val[i],"%f", &azmodrad); }
      else if (strcmp("YDIR", nome[i]) == 0 )
        { sscanf(val[i],"%d", &ydir);  }
      }
      corner[0][0]=cornerx;
      corner[0][1]=cornery;
}
printf("sono qui");
 for (i=0; i< MAX_PARAM; i++)
    {
      free(nome[i]);
      free(val[i]);
    }
    

  ox=(float)corner[0][0];
  oy=(float)corner[0][1];
  oz=0.0;
  n_z=campioni;
  azmoddeg=azmodrad*180/pi;

  incil_vel = (int)rint((double)(velbin_il/dim_il));
  incxl_vel = (int)rint((double)(velbin_xl/dim_xl));

  da1 = la1 / (float)(na1-1);
  da2 = la2 / (float)(na2-1);
  dt1 = lt1 / (float)(nnt1-1);

  if ( fabsf(lt2) <= eps)
    {dt2=0; nt2=1; nnt2=1;}
  else
    {dt2 = lt2 / (float)(nnt2-1);}

  if (tipo==1)
    {aztgtdeg=azmoddeg;
     incxl_tgt = (int)rint((double)(dt1/dim_il));
 /*Clara4*/
     incxl_tgt *= strideXL;
     incil_tgt = 0; }

  if (tipo==2)
    {aztgtdeg=azmoddeg+ydir*90;
     incil_tgt = (int)rint((double)(dt1/dim_xl));
 /*Clara4*/
     incil_tgt *= strideIL;
     incxl_tgt = 0; }

  if (tipo==3)
    {aztgtdeg=azmoddeg;
     incxl_tgt = (int)rint((double)(dt1/dim_il));
     incil_tgt = (int)rint((double)(dt2/dim_xl));
 /*Clara4*/
     incxl_tgt *= strideXL;
     incil_tgt *= strideIL;
      }

  /* scrittura dell'header del file di velocita in formato sep */

  fp=fopen(headersep,"w");
  fprintf(fp,"n1=%d\n",n_xl);
  fprintf(fp,"n2=%d\n",n_il);
  fprintf(fp,"n3=%d\n",campioni);
  fprintf(fp,"d1=%f\n",dim_il*incil_vel);
  fprintf(fp,"d2=%f\n",dim_xl*incxl_vel);
  fprintf(fp,"d3=%f\n",dz);
  fprintf(fp,"o1=%d\n",0);
  fprintf(fp,"o2=%d\n",0);
  fprintf(fp,"o3=%d\n",0);
  fprintf(fp,"esize=4\n");
  fprintf(fp,"sets next: in=%s@\n",headersep);
  fclose(fp);

  /* Controlli sulle dimensioni ---------------------------------------------*/

  if ( fabsf(velbin_il/dim_il-(float)rint((double)(velbin_il/dim_il)))>0.1)
    { printf("** ATTENZIONE: dimensioni IL modello diverse da quelle in input \n"); }
  if ( fabsf(velbin_xl/dim_xl-(float)rint((double)(velbin_xl/dim_xl)))>0.1)
    { printf("** ATTENZIONE: dimensioni XL modello diverse da quelle in input \n"); }

  if( (tipo==1) && (fabsf(dt1/dim_il-(float)rint((double)(dt1/dim_il)))>0.02) )
    { printf("** ATTENZIONE: dimensione bin target non multiplo di IL \n"); }
  if( (tipo==2) && (fabsf(dt1/dim_xl-(float)rint((double)(dt1/dim_xl)))>0.02) )
    { printf("** ATTENZIONE: dimensione bin target non multiplo di XL \n"); }
  if( (tipo==3) && (fabsf(dt1/dim_il-(float)rint((double)(dt1/dim_il)))>0.02) )
    { printf("** ATTENZIONE: dimensione bin target non multiplo di IL \n"); }
  if( (tipo==3) && (fabsf(dt2/dim_xl-(float)rint((double)(dt2/dim_xl)))>0.02) )
    { printf("** ATTENZIONE: dimensione bin target non multiplo di XL \n"); }

  printf( "\n");
  printf( " Dimensione bin IL x XL (da input)  : %f x %f\n", dim_il,dim_xl);
  printf( " Dimensione bin modello di velocita : %f x %f\n", dim_il*incil_vel,dim_xl*incxl_vel);
  printf( " Incremento IL e XL modello velocita: %d x %d\n", incil_vel,incxl_vel);
  printf( "\n");
  if( (tipo==1)|(tipo==3))
    { printf( " Dimensione bin output migrato      : %f x %f\n", dt1, dt2);
      printf( " Incremento IL e XL target          : %d x %d\n", incil_tgt,incxl_tgt); }
  if( tipo==2 )
    { printf( " Dimensione bin output migrato      : %f x %f\n", dt2, dt1);
      printf( " Incremento IL e XL target          : %d x %d\n", incil_tgt,incxl_tgt); }

  /* Calcolo requisiti di memoria -------------------------------------------*/
  /* calcolo delle dimensioni del gruppo di tracce --------------------------*/

  if ( (mempcpu*ncpu) > memptot)
    { mempcpu=floorf(memptot/ncpu); }

  /* con matrici per AVA */
  /* mempcost=((float)(12*nnt1*nnt2*nntz))/mega+((float)(8*nnt1*nnt2))/mega+ */
  /*          ((float)(64*nt1*nt2*ntz))/mega+                                */
  /*          ((float)(32*ncampioni))/mega+((float)(32*bordf))/mega;         */
  /*  memptrac= ((float)(4*ncampioni+8*bordf+32*nt1*nt2*ntz))/mega;          */

 /*   mempcost= ((float)(12*nnt1*nnt2*nntz))/mega+((float)(8*nnt1*nnt2))/mega+ */
/*              ((float)(40*nt1*nt2*ntz))/mega+ */
/*              ((float)(32*ncampioni))/mega+((float)(32*bordf))/mega; */
  mempcost= ((float)(2*nnt1*nnt2*nntz*4))/mega+
            ((float)(7*nt1*nt2*ntz*4))/mega+
            ((float)(6*(ncampioni+2*bordf)*4))/mega;

 /*   memptrac= ((float)(4*ncampioni+8*bordf+20*nt1*nt2*ntz))/mega; */
 /*   dimgruppo= (int)((mempcpu-mempcost)/memptrac); */

 /*   if ( dimgruppo < 1 ) */
/*      { printf("** ATTENZIONE: dimensione gruppo tracce minore di 1 \n");  */
/*        printf("**             decrementare nr CPUs \n"); }   */

/*    if ( dimgruppo > 5000 )  */
/*      {  dimgruppo = 5000; } */

 /*   memsh=(3*(nnt1*nnt2*nntz)*4)/mega+(40*ntraccetot)/mega; */
/*    memtot = memsh + mempcpu*ncpu ; */
/* CCC 25/07/2002 */ 
/*  memsh=(2*(nnt1*nnt2*nntz)*4)/mega+(11*dimblocco*4)/mega+(dimblocco*ncampioni*4)/mega;
  memtot = memsh + mempcost*ncpu ;
  dmempcpu = (double)(mempcpu) * (double)(mega);

  printf(" Dimensione del gruppo di tracce     %d\n",dimgruppo);  
  printf(" Dimensione del blocco di tracce     %d\n",dimblocco);
  printf(" Numero di CPU da impostare                 %d\n", ncpu);
  printf(" Memoria private (singola) utilizzata       %.3f MB\n",mempcost);
  printf(" Memoria shared utilizzata                  %.3f MB\n",memsh);
  printf(" Memoria totale (shared+private) utilizzata %.3f MB\n",memtot);      */

  /**************************************************************************/
  /*                                                                        */
  /* Genera i file di parametri necessari al funzionamento dell'ambiente di */
  /* migrazione                                                             */
  /*                                                                        */
  /**************************************************************************/

  aox1=aox-ox;
  aoy1=aoy-oy;
  xtemp=aox1*sin(azmodrad)+aoy1*cos(azmodrad);
  ytemp=-ydir*aox1*cos(azmodrad)+ydir*aoy1*sin(azmodrad);
  aox1=xtemp;
  aoy1=ytemp;
  aoz1=aoz;

  tox1=tox-ox;
  toy1=toy-oy;
  xtemp=tox1*sin(azmodrad)+toy1*cos(azmodrad);
  ytemp=-ydir*tox1*cos(azmodrad)+ydir*toy1*sin(azmodrad);
  tox1=xtemp;
  toy1=ytemp;
  toz1=toz;

  xtemp=ox*sin(azmodrad)+oy*cos(azmodrad);
  ytemp=-ydir*ox*cos(azmodrad)+ydir*oy*sin(azmodrad);
  ox1=xtemp;
  oy1=ytemp;
  oz1=oz;


  fp=fopen("mig1.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"mig1.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"velsup=  %d\n",velsup);
  fprintf(fp,"filename_1 = %s\n",filetracce);
  fprintf(fp,"offset-numshot_1 = %d\n",numshot);
  fprintf(fp,"nbytes-numshot_1 = %d\n",snumshot);
  fprintf(fp,"offset-xsource_1 = %d\n",xsource);
  fprintf(fp,"nbytes-xsource_1 = %d \n",sxsource);
  fprintf(fp,"offset-ysource_1 = %d\n",ysource);
  fprintf(fp,"nbytes-ysource_1 = %d \n",sysource);
  fprintf(fp,"offset-zsource_1 = %d\n",zsource);
  fprintf(fp,"nbytes-zsource_1 = %d \n",szsource);
  fprintf(fp,"offset-xreceiver_1 = %d\n",xreceiver);
  fprintf(fp,"nbytes-xreceiver_1 = %d\n",sxreceiver);
  fprintf(fp,"offset-yreceiver_1 = %d\n",yreceiver);
  fprintf(fp,"nbytes-yreceiver_1 = %d\n",syreceiver);
  fprintf(fp,"offset-zreceiver_1 = %d\n",zreceiver);
  fprintf(fp,"nbytes-zreceiver_1 = %d\n",szreceiver);
  fprintf(fp,"trace-offset_1 = %d \n",firsttrace);
  fprintf(fp,"ntraccetot_1 = %d \n",ntraccetot);
  fprintf(fp,"ncampioni_1= %d \n",ncampioni);
  fprintf(fp,"formato_1= %d \n",formato);
  fprintf(fp,"dt_1 = %f \n",dt);
  fprintf(fp,"dx_1 = %f \n",dx);
  fprintf(fp,"tshift_1 = %f \n",tshift);
  fprintf(fp,"a11_1 = %f \n",sin(azmodrad));
  fprintf(fp,"a12_1 = %f \n",cos(azmodrad));
  fprintf(fp,"a21_1 = %f \n",-ydir*cos(azmodrad));
  fprintf(fp,"a22_1 = %f \n", ydir*sin(azmodrad));
  fprintf(fp,"b1_1 = %f \n",-ox1);
  fprintf(fp,"b2_1 = %f \n",-oy1);
  fprintf(fp,"filepesi_1 =%s\n",filepesi);
  fclose(fp);

  fp=fopen("mig2.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"mig2.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"bordf = %d \n",bordf);
  fprintf(fp,"semiapertura = %f\n",aperturam);
  fprintf(fp,"smussamento = %f \n",smussamento);
  fprintf(fp,"soglia-ampiezze = %.9f \n",sogliaampiezze);
  fprintf(fp,"imaging= %d \n",imaging);
  fprintf(fp,"ava-analysis = 0 \n");
  fprintf(fp,"offmin =%s \n",offmin);
  fprintf(fp,"offmax =%s \n",offmax);
  fprintf(fp,"restart =%d \n",restart);
  fprintf(fp,"antialias =%d \n",antialias);
/* CCC 25/07/2002 */
/*  fprintf(fp,"dimgruppotracce=%d \n",dimgruppo);
  fprintf(fp,"dimbloccotracce=%d \n",dimblocco);     */
  fprintf(fp,"verbose=2\n");
  fprintf(fp,"NUMTRACCEPERPE=%d \n",numtracceperpe);
/* Clara2 decimazione delle tracce e apertura variabile con la profondita'   */  
  fprintf(fp,"datadecimation = %d\n",datadecimation);
  fprintf(fp,"aperturainiz = %f\n",aperturainiz);
  fprintf(fp,"zetamaxape = %f\n",zetamaxape);
/* Clara3 distanza massima Y (XLINE) a cui si migrano i dati*/  
  fprintf(fp,"maxdisty = %d\n",maxdisty);
  fprintf(fp,"maxdistx = %d\n",maxdistx);
/* Clara6*/
  fprintf(fp,"zmaxdisty = %d\n",zmaxdisty);
  fprintf(fp,"zmaxdistx = %d\n",zmaxdistx);
  fprintf(fp,"distyiniz = %d\n",distyiniz);
  fprintf(fp,"distxiniz = %d\n",distxiniz);
  fprintf(fp,"muteflag = %d\n",muteflag);
  fprintf(fp,"mutefilename = %s\n",mutefilename);

  
  fclose(fp);

  fp=fopen("mig3.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"mig3.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"acquisition-file =./acq.p \n");
  fprintf(fp,"target-file =./tgt.p \n");
  fprintf(fp,"database-dir = %s \n", dbasedir);
  fclose(fp);

  fp=fopen("eik1.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"eik1.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"MODELLO = %s \n",headersep);
  fprintf(fp,"SOTTOMOD= %d \n",sottomod);
  if (sottomod==2)
    {
      fprintf(fp,"oxminimod= %f\n",oxminimod);
      fprintf(fp,"oyminimod= %f\n",oyminimod);
      fprintf(fp,"ozminimod= %f\n",ozminimod);
      fprintf(fp,"pxminimod= %d\n",pxminimod);
      fprintf(fp,"pyminimod= %d\n",pyminimod);
      fprintf(fp,"pzminimod= %d\n",pzminimod);
      fprintf(fp,"nxminimod= %d\n",(int)((n_xl-1)/pxminimod)+1);
      fprintf(fp,"nyminimod= %d\n",(int)((n_il-1)/pyminimod)+1);
      fprintf(fp,"nzminimod= %d\n",(int)((n_z -1)/pzminimod)+1);
    }
  fclose(fp);

  fp=fopen("eik2.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"eik2.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"VERSIONE = %d\n",versione);
  fprintf(fp,"INIZIO   = %d\n",inizio);
  fprintf(fp,"APERTURA = %d\n",aperturai);
  fprintf(fp,"MODALITA = %d\n",modalita);
  fprintf(fp,"SOGLIAX = %f\n",sogliax);
  fprintf(fp,"SOGLIAY = %f\n",sogliay);
  fprintf(fp,"ANGOLO = %f\n",angolo);
  fprintf(fp,"DAMP = %f\n",damp);
  fprintf(fp,"AMPMIN = %.9f\n",ampmin);
  fprintf(fp,"VERBOSE = 2\n");
  fprintf(fp,"NPACKETS = %d\n",Npackets);
  fprintf(fp,"MEMOFFSET = %d\n",memoffset );
  fclose(fp);


  fp=fopen("eik3.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"eik3.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"source-file =./tgt.sht \n");
  fprintf(fp,"target = 2 \n");
  fprintf(fp,"target-file =./acq.sht \n");
  fprintf(fp,"updown = 2 \n");
  fprintf(fp,"output = 2 \n");
  fprintf(fp,"ampiezza = 1 \n");
  fprintf(fp,"travel = 1 \n");
  fprintf(fp,"cosenox = 1 \n");
  fprintf(fp,"cosenoy = 1 \n");
  fprintf(fp,"cosenoz = 1 \n");
  fclose(fp);


  fp=fopen("dbs.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"dbs.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"acquisition = ./acq.p\n");
  fprintf(fp,"acq-interp = 0\n\n");
  fprintf(fp,"target = ./tgt.p\n");
  fprintf(fp,"tgt-interp = 0\n\n");
  fprintf(fp,"sorting = 1\n");
  fprintf(fp,"amplitudes  = 1\n");
  fprintf(fp,"traveltimes = 1\n");
  fprintf(fp,"ux  = 1\n");
  fprintf(fp,"uy  = 1\n");
  fprintf(fp,"uz  = 1\n");
  fprintf(fp,"vx  = 0\n");
  fprintf(fp,"vy  = 0\n");
  fprintf(fp,"vz  = 0\n");
  fprintf(fp,"VERBOSE = 2\n");
  fclose(fp);


  fp=fopen("acq.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"acq.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"settype=2\n");
  fprintf(fp,"o1_1=%f\n",aox1);
  fprintf(fp,"o2_1=%f\n",aoy1);
  fprintf(fp,"o3_1=%f\n",aoz1);
  fprintf(fp,"la_1=%f\n",la1);
  fprintf(fp,"lb_1=%f\n",la2);
  fprintf(fp,"theta_1=0\n");
  fprintf(fp,"phi_1=0\n");
  fprintf(fp,"psi_1=0\n");
  fprintf(fp,"na_1=%d\n",na1);
  fprintf(fp,"nb_1=%d\n",na2);
  fprintf(fp,"nna_1=%d\n",na1);
  fprintf(fp,"nnb_1=%d\n",na2);
  fclose(fp);

  fp=fopen("tgt.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"tgt.p\"\n");
      exit(EXIT_FAILURE);
    }

  if ( fabsf(lt2) <= eps)
    {
      fprintf(fp,"settype=2\n");
      fprintf(fp,"o1_1=%f\n",tox1);
      fprintf(fp,"o2_1=%f\n",toy1);
      fprintf(fp,"o3_1=%f\n",toz1);
      fprintf(fp,"la_1=%f\n",ltz);
      fprintf(fp,"lb_1=%f\n",lt1);
      fprintf(fp,"theta_1=90\n");
      fprintf(fp,"psi_1=90\n");
      fprintf(fp,"phi_1=%f\n",180+ydir*(azmoddeg-aztgtdeg));
      fprintf(fp,"na_1=%d\n",ntz);
      fprintf(fp,"nb_1=%d\n",nt1);
      fprintf(fp,"nna_1=%d\n",nntz);
      fprintf(fp,"nnb_1=%d\n",nnt1);
    }
  else
    {
      fprintf(fp,"settype=3\n");
      fprintf(fp,"o1=%f\n",tox1);
      fprintf(fp,"o2=%f\n",toy1);
      fprintf(fp,"o3=%f\n",toz1);
      fprintf(fp,"la=%f\n",ltz);
      fprintf(fp,"lb=%f\n",lt1);
      fprintf(fp,"lc=%f\n",lt2);
      fprintf(fp,"theta=90\n");
      fprintf(fp,"psi=90\n");
      fprintf(fp,"phi=%f\n",180+ydir*(azmoddeg-aztgtdeg));
      fprintf(fp,"na=%d\n",ntz);
      fprintf(fp,"nb=%d\n",nt1);
      fprintf(fp,"nc=%d\n",nt2);
      fprintf(fp,"nna=%d\n",nntz);
      fprintf(fp,"nnb=%d\n",nnt1);
      fprintf(fp,"nnc=%d\n",nnt2);
    }
  fclose(fp);

  fp=fopen("mig2segy.p", "w");
  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to create file \"mig2segy.p\"\n");
      exit(EXIT_FAILURE);
    }
  fprintf(fp,"az_target= %f\n",aztgtdeg);
  fprintf(fp,"az_model= %f\n",azmoddeg);
  fprintf(fp,"ydir= %d\n",ydir);
  fprintf(fp,"tox= %f\n",tox);
  fprintf(fp,"toy= %f\n",toy);
  fprintf(fp,"toz= %f\n",toz);
  fprintf(fp,"nnt1= %d\n",nnt1);
  fprintf(fp,"nnt2= %d\n",nnt2);
  fprintf(fp,"nntz= %d\n",nntz);
  fprintf(fp,"lt1= %f\n",lt1);
  fprintf(fp,"lt2= %f\n",lt2);
  fprintf(fp,"ltz= %f\n",ltz);
  fprintf(fp,"tipoout= %d\n",tipo);
  if( (tipo==1)|(tipo==2)|(tipo==3))
    {
    fprintf(fp,"nrprimail= %d\n",nrprimail);
    fprintf(fp,"nrprimaxl= %d\n",nrprimaxl);
    fprintf(fp,"incil_tgt= %d\n",incil_tgt);
    fprintf(fp,"incxl_tgt= %d\n",incxl_tgt);
    }
  fclose(fp);

  /* Fine esecuzione -----------------------------------------*/
    printf("\nThat's all folks!\n\n");

}


/*------------------------------------------------------------*/
/*------------------------------------------------------------*/

int ReadVelocitySegyFile(char vel_in[MAX_STRING_LEN],
    char vel_out[MAX_STRING_LEN], int *nsamples, float *dz,int *n_xl,int *n_il,
    float *velbin_il,float *velbin_xl, int corner[4][2], float *azmodrad,int *ydir)

{
  /***************************************************************************/
  /*                                                                         */
  /*      INPUT--->  file di velocita'(segy)                  -vel_in        */
  /*                                                                         */
  /*                                                                         */
  /*      OUTPUT-->  file del volume (trasposto) di velocita' -vel_out       */
  /*                 n.campioni                               -nsamples      */
  /*                 campionamento verticale [m]              -dz            */
  /*                 n. di crossline                          -n_xl          */
  /*                 n. di inline                             -n_il          */
  /*                 bin size crossline [m]                   -velbin_il     */
  /*                 bin size inline [m]                      -velbin_xl     */
  /*                 coordinate vertici grid di velocita'     -corner        */
  /*                 azimut grid di velocita'                 -azmodrad      */
  /*                 elevation data file                      -elev@         */
  /*                                                                         */
  /*                                                                         */
  /***************************************************************************/

  int reelheader[27],traceheader[91];
  short int temp;
  int xcur,ycur,xold,yold,ntracks=2;
  int l,ls,i=0,j=0,k;
  int surf_elev_ind=14;
  float dx1,dy1,dx3,dy3,len_xl,len_in;
  int zmin,zmax,nz_skip=0,nz_adj=0;
  const int o_unit=20,t_unit=30;
  int fdescin=10,fdescout=10;
  float *tracedata, *tmp, *volumedata, *piano;
  int *coord_data, *ground_level;
  FILE *header,*output,*outputsep,*fdescheader,*setup;

  /*************************************************************************/
  /* Apro il file di velocita' in formato seg-y per il calcolo */
  /* del numero di inlines e crosslines */

  segyopenread_(&fdescin,vel_in);
  if (fdescin<1)
    {
      printf("\n\n ERROR:");
      printf(" Unable to open the file \"%s\"... \n",vel_in);
      return(-1);
    }
  else
    {
      printf(" Lettura del file \"%s\"... \n",vel_in);
    }

  /*************************************************************************/
  /* Leggo dal reel header il numero di campioni per traccia ed il passo   */
  /* di campionamento verticale                                            */

  segygetheader_(&fdescin,reelheader);

  temp=(short int)reelheader[7];
#ifdef INTEL
  byteswap(&temp,sizeof(short int),1);
#endif
  *nsamples=temp;

  temp=(short int)reelheader[5];
#ifdef INTEL
  byteswap(&temp,sizeof(short int),1);
#endif
  *dz=temp/1000;

  printf(" Numero di campioni per traccia      : %d\n",*nsamples);
  printf(" Passo di campionamento verticale [m]: %f\n",*dz);

  /*************************************************************************/

  tracedata=(float *)calloc((*nsamples),sizeof(float));

  /* conto il numero di tracce e di inline */

  segygettrace_(&fdescin,traceheader,tracedata);
  xold=traceheader[23];
#ifdef INTEL
  byteswap(&xold,sizeof(int),1);
#endif
  yold=traceheader[24];
#ifdef INTEL
  byteswap(&yold,sizeof(int),1);
#endif
  zmin=traceheader[surf_elev_ind];
#ifdef INTEL
  byteswap(&zmin,sizeof(int),1);
#endif
  zmax=zmin;

  segygettrace_(&fdescin,traceheader,tracedata);
  xcur=traceheader[23];
#ifdef INTEL
  byteswap(&xcur,sizeof(int),1);
#endif
  ycur=traceheader[24];
#ifdef INTEL
  byteswap(&ycur,sizeof(int),1);
  byteswap(&traceheader[surf_elev_ind],sizeof(int),1);
#endif
  zmin=(zmin>traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmin ;
  zmax=(zmax<traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmax ;
  (*n_il)=1;

  if (xcur>xold)
    {
      while(1)
        {
          xold=xcur;
          yold=ycur;
          segygettrace_(&fdescin,traceheader,tracedata);
          if (fdescin<0)
            {
              break;
            }
          ntracks++;
          xcur=traceheader[23];
          ycur=traceheader[24];
#ifdef INTEL
          byteswap(&xcur,sizeof(int),1);
          byteswap(&traceheader[surf_elev_ind],sizeof(int),1);
#endif
          zmin=(zmin>traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmin ;
          zmax=(zmax<traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmax ;
          if (xcur<xold)
            {
              (*n_il)++;
            }
        }
    }


  else if (xcur<xold)
    {
      while(1)
        {
          xold=xcur;
          yold=ycur;
          segygettrace_(&fdescin,traceheader,tracedata);
          if (fdescin<0)
            {
              break;
            }
          ntracks++;
          xcur=traceheader[23];
          ycur=traceheader[24];
#ifdef INTEL
          byteswap(&xcur,sizeof(int),1);
          byteswap(&traceheader[surf_elev_ind],sizeof(int),1);
#endif
          zmin=(zmin>traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmin ;
          zmax=(zmax<traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmax ;
          if (xcur>xold)
            {
              (*n_il)++;
            }
        }
    }


  else if ((xcur==xold)&&(ycur>yold))
    {
      while(1)
        {
          xold=xcur;
          yold=ycur;
          segygettrace_(&fdescin,traceheader,tracedata);
          if (fdescin<0)
            {
              break;
            }
          ntracks++;
          xcur=traceheader[23];
          ycur=traceheader[24];
#ifdef INTEL
          byteswap(&ycur,sizeof(int),1);
          byteswap(&traceheader[surf_elev_ind],sizeof(int),1);
#endif
          zmin=(zmin>traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmin ;
          zmax=(zmax<traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmax ;
          if (ycur<yold)
            {
              (*n_il)++;
            }
        }
    }

  else if((xcur==xold)&&(ycur<yold))
    {
      while(1)
        {
          xold=xcur;
          yold=ycur;
          segygettrace_(&fdescin,traceheader,tracedata);
          if (fdescin<0)
            {
              break;
            }
          ntracks++;
          xcur=traceheader[23];
          ycur=traceheader[24];
#ifdef INTEL
          byteswap(&ycur,sizeof(int),1);
          byteswap(&traceheader[surf_elev_ind],sizeof(int),1);
#endif
          zmin=(zmin>traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmin ;
          zmax=(zmax<traceheader[surf_elev_ind]) ? traceheader[surf_elev_ind] :zmax ;
          if (ycur>yold)
            {
              (*n_il)++;
            }
        }
    }

  /* calcolo il numero di crosslines come numero totale di tracce diviso per il numero di inlines*/

  (*n_xl)=(ntracks)/(*n_il);
  printf(" Numero di inline   : %d\n",*n_il);
  printf(" Numero di crossline: %d\n",*n_xl);


  /* chiusura del file e deallocazione della memoria */
  segyclose_(&fdescin);
  free(tracedata);

  /************************************************************************************************/

  /* riapertura del file per il calcolo dei parametri utili */

  segyopenread_(&fdescin,vel_in);
  segygetheader_(&fdescin,reelheader);

  /* allocazione variabili dinamiche */

  tracedata=(float *)calloc((*nsamples),sizeof(float));
  coord_data=(int *)calloc((*n_xl)*(*n_il)*(2),sizeof(int));
  tmp=(float *)calloc((*nsamples)*(*n_xl),sizeof(float));
  ground_level=(int *)calloc((*n_xl),sizeof(int));
  piano=(float *)calloc(((*n_xl)*(*n_il)),sizeof(float));
  volumedata=(float *)calloc((*n_xl)*(*n_il),sizeof(float));

  /* apertura del file di uscita */

  output=fopen(vel_out,"w");

  for(i=0;i<(*n_il);i++)
    {
      fdescout=fwrite(tmp,sizeof(float),((*n_xl)*(*nsamples)),output);
    }
  rewind(output);

  /************************************************************************************************/

  /* scrittura del file di uscita */

  for (i=0;i<(*n_il);i++)
    {
      fseek(output,i*(*n_xl)*4,SEEK_SET);
      /* printf("Scrittura inline %d\n\n",i+1);*/
      for (j=0;j<(*n_xl);j++)
        {
          segygettrace_(&fdescin,traceheader,tracedata);
          for (k=0;k<(*nsamples);k++)
            {
#ifdef INTEL
              byteswap(&tracedata[k],sizeof(int),1);
#endif
              tmp[k*(*n_xl)+j]=tracedata[k];
            }
          xcur=traceheader[23];
          ycur=traceheader[24];
          zmax=traceheader[surf_elev_ind];
#ifdef INTEL
          byteswap(&xcur,sizeof(int),1);
          byteswap(&ycur,sizeof(int),1);
          byteswap(&zmax,sizeof(int),1);
#endif
          coord_data[2*(i*(*n_xl)+j)]=xcur;
          coord_data[2*(i*(*n_xl)+j)+1]=ycur;
        }
      for (k=0;k<(*nsamples);k++)
        {
          fdescout=fwrite(&tmp[k*(*n_xl)],sizeof(float),(*n_xl),output);
          fseek(output,(*n_xl)*(*n_il-1)*4,SEEK_CUR);
        }
    }
  fclose(output);


  corner[0][0]=coord_data[0];
  corner[0][1]=coord_data[1];
  corner[1][0]=coord_data[2*((*n_xl)-1)];
  corner[1][1]=coord_data[2*((*n_xl)-1)+1];
  corner[3][0]=coord_data[2*(*n_il-1)*(*n_xl)];
  corner[3][1]=coord_data[2*(*n_il-1)*(*n_xl)+1];
  corner[2][0]=coord_data[2*((*n_il-1)*(*n_xl)+(*n_xl)-1)];
  corner[2][1]=coord_data[2*((*n_il-1)*(*n_xl)+(*n_xl)-1)+1];
  printf("\n");
  printf(" Coordinate dei vertici del modello di velocita'\n");
  printf(" Vertice1         x=%d        y=%d\n",corner[0][0],corner[0][1]);
  printf(" Vertice2         x=%d        y=%d\n",corner[1][0],corner[1][1]);
  printf(" Vertice3         x=%d        y=%d\n",corner[2][0],corner[2][1]);
  printf(" Vertice4         x=%d        y=%d\n\n",corner[3][0],corner[3][1]);

  dx1=(float)(corner[1][0]-corner[0][0]);
  dy1=(float)(corner[1][1]-corner[0][1]);
  len_in=sqrt(dx1*dx1+dy1*dy1);
  (*azmodrad)=(float)atan2((double)dx1,(double)dy1);

 /*   printf(" dx %f  dy %f  \n",dx1,dy1); */

  dx3=(float)(corner[3][0]-corner[0][0]);
  dy3=(float)(corner[3][1]-corner[0][1]);
  len_xl=sqrt(dx3*dx3+dy3*dy3);

  if ((dx3*dy1-dy3*dx1)>0)
    {
      *ydir=-1;
    }

  /* stampa a video dei parametri calcolati */

 /*   printf(" dx %f  dy %f  \n",dx3,dy3); */

  printf("\n");
  printf(" Lunghezza dei lati del modello: \n");
  printf(" Lunghezza inline                 %f\n",len_in);
  printf(" Lunghezza crossline              %f\n",len_xl);
  printf(" Azimuth del modello di velocita' %f\n",(*azmodrad)*180/pi);
  printf(" YDir                             %d\n\n",*ydir);

  /* calcolo dei parametri velbin_il ed velbin_xl */

  *velbin_il = len_in/ (float)(*n_xl-1);
  *velbin_xl = len_xl/ (float)(*n_il-1);

  /* scrittura dell'header del file di velocita in formato sep */

  segyclose_(&fdescin);
  close(&fdescout);

}
