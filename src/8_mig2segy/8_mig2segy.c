#include <string.h>
#include <stdio.h>
#include <math.h>
#include "../0_include/0_common_defs.h"

FILE* glob_fp[MAX_FILE_NUM];

int main(int argc, char* argv[])
{
  FILE *fp;
  int fdesc=10,i,j,n,tipoout;
  int imax,nntz,nnt1,nnt2,ydir;
  int reelheader[27];
  int traceheader[91];
  int numtracce,incil_tgt,incxl_tgt;
  int numprimail,numprimaxl,curil,curxl;
  float otx,oty,otz,x0,y0,xcur,ycur,dt1,dt2;
  float aztgtrad,aztgtdeg;
  float *tracedata,dz,lt1,lt2,ltz;
  const float pi=3.14159265358979;
  const float eps=0.000000001;

  char* ident[MAX_PARAM];
  char* val[MAX_PARAM];

  /* Test sintassi chiamata --------------------------------------------- */

  if (argc < 3)
    {
      fprintf(stderr,"ERROR : Missing command line parameter.\n");
      fprintf(stderr,"*** MIG2SEGY v. %s ***\n",VERSION);
      fprintf(stderr,"mig2segy.x (par <mig2segy.p>) (bin mig <MIG.H@>) (segy <x.sgy> \n\n");
      exit(EXIT_FAILURE);
    }

  printf("\n");
  printf("*** MIG2SEGY v. %s ***\n\n",VERSION);

  /* lettura dei parametri dai file di parametri secondari -------------- */

  numprimail=-1;
  numprimaxl=-2;
  tipoout=0;

  for (i=0; i< MAX_PARAM; i++)
    {
      ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
      val[i] = calloc(MAX_STRING_LEN, sizeof(char));
    }

  for (i=0; i< 27; i++)
    reelheader[i]=0;
  for (i=0; i< 91; i++)
    traceheader[i]=0;

  printf(" Apertura file di parametri  %s\n",argv[1]);
  fp=fopen(argv[1], "r");

  if (fp ==  NULL)
    {
      fprintf(stderr, " \n  ERROR: unable to open file %s\n",argv[1]);
      exit(EXIT_FAILURE);
    }

  imax = 0;
  imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
  close(fp);

  for (i=0; i<imax; i++)
    {
     if (strcmp("TOX", ident[i]) == 0 )
       {sscanf(val[i],"%f",&otx ); }
     else if (strcmp("TOY", ident[i]) == 0 )
       {sscanf(val[i],"%f",&oty );}
     else  if (strcmp("TOZ", ident[i]) == 0)
       {sscanf(val[i],"%f",&otz );}
     else if (strcmp("NNT1", ident[i])== 0)
       {sscanf(val[i],"%d",&nnt1 );}
     else if (strcmp("NNT2", ident[i])== 0)
       {sscanf(val[i],"%d",&nnt2 );}
     else if (strcmp("NNTZ", ident[i])== 0)
       {sscanf(val[i],"%d",&nntz );}
     else if (strcmp("AZ_TARGET", ident[i])== 0)
       {sscanf(val[i],"%f",&aztgtdeg ); }
     else if (strcmp("LT1", ident[i])== 0)
       {sscanf(val[i],"%f",&lt1 );}
     else if (strcmp("LT2", ident[i])== 0)
       {sscanf(val[i],"%f",&lt2 );}
     else if (strcmp("LTZ", ident[i])== 0)
       {sscanf(val[i],"%f",&ltz );}
     else if (strcmp("TIPOOUT", ident[i])== 0)
       {sscanf(val[i],"%d",&tipoout );}
     else if (strcmp("NRPRIMAIL", ident[i])== 0)
       {sscanf(val[i],"%d",&numprimail );}
     else if (strcmp("NRPRIMAXL", ident[i])== 0)
       {sscanf(val[i],"%d",&numprimaxl );}
     else if (strcmp("INCIL_TGT", ident[i])== 0)
       {sscanf(val[i],"%d",&incil_tgt );}
     else if (strcmp("INCXL_TGT", ident[i])== 0)
       {sscanf(val[i],"%d",&incxl_tgt );}
     else if (strcmp("YDIR", ident[i])== 0)
       {sscanf(val[i],"%d",&ydir );}

   }


  if ( tipoout==0)
  {
     numprimail=-1;
     numprimaxl=-2;
  }

  aztgtrad=aztgtdeg*pi/180;
  numtracce=nnt1*nnt2;

  dt1= lt1/ (float)(nnt1-1);
  dz=  ltz/ (float)(nntz-1);

  if ( fabsf(lt2) <= eps)
    {dt2=0; nnt2=1;}
  else
    {dt2 = lt2/ (float)(nnt2-1);}

  x0=otx;
  y0=oty;

  printf(" DESCRIZIONE FILE USCITA \n");
  printf(" Numero campioni lato1 : %d\n",nnt1);
  printf(" Numero campioni lato2 : %d\n",nnt2);
  printf(" Numero tracce         : %d\n",numtracce);
  printf(" Passo campionamento 1 : %f\n",dt1);
  printf(" Passo campionamento 2 : %f\n",dt2);
  printf(" Coordinata x prima traccia=%f\n",otx);
  printf(" Coordinata y prima traccia=%f\n",oty);

  remove (argv[3]);
  printf(" Apertura file di uscita  %s\n",argv[3]);

  segyopen_(&fdesc,argv[3]);
  reelheader[7]=nntz;
  reelheader[9]=1;
  reelheader[5]=dz*1000;
  segyputheader_(&fdesc,reelheader);

  printf(" Apertura file di ingresso  %s\n",argv[2]);
  fp=fopen(argv[2],"r");
  if (fp ==  NULL)
  {
    fprintf(stderr, " \n  ERROR: unable to mig file %s\n",argv[2]);
    exit(EXIT_FAILURE);
  }

  tracedata=calloc((nntz),sizeof(float));

  curil=numprimail;
/*  curxl=numprimaxl;    */

  for (i=0;i<nnt2;i++)
    {
  curxl=numprimaxl;

      for (j=0;j<nnt1;j++)
        {
          xcur=x0+j*dt1*sin(aztgtrad)-ydir*i*dt2*cos(aztgtrad);
          ycur=y0+j*dt1*cos(aztgtrad)+ydir*i*dt2*sin(aztgtrad);

          traceheader[21]=(float)rint((double)xcur);
          traceheader[22]=(float)rint((double)ycur);
          traceheader[23]=(float)rint((double)xcur);
          traceheader[24]=(float)rint((double)ycur);
          traceheader[38]=nntz;
          traceheader[39]=dz*1000;
          traceheader[35]=otz;
          traceheader[87]=curil;
          traceheader[88]=curxl;
          if (tipoout==2)
            { curil=curil+incil_tgt; }
          else if (tipoout==1)
            { curxl=curxl+incxl_tgt; }
          else if (tipoout==3)
            { curxl=curxl+incxl_tgt; }
          traceheader[38]=nntz;
          n=fread(tracedata,sizeof(float),nntz,fp);
          segyputtrace_(&fdesc,traceheader,tracedata);
        }

      if (tipoout==2)
          { curil=curil+incil_tgt; }
      else if (tipoout==1)
          { curxl=curxl+incxl_tgt; }
      else if (tipoout==3)
          { curil=curil+incil_tgt; }
    }

  segyclose_(&fdesc);

  printf("\nThat's all folks!\n\n");

}
