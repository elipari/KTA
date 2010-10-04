/* CCC 25/06/2002 - Subroutine che stampa i messaggi di errore per ogni processore e 
                    fa terminare il programma                                              */

void err_and_exit(int err, int myrank, char *argv[])
{
#include "../0_include/0_common_defs.h"
#include <mpi.h>

fprintf(stderr, "myrank = %d --> ",myrank);

switch (err) 
{
   case 1: 
   {
      fprintf(stderr, "ERROR: unable to open file  %s \n", argv[1]);
      break;
   }
   case 2:
   {
      fprintf(stderr, "ERROR: missing parameter MODELLO in file %s\n", argv[1]);
      break;
   }
   case 3:
   {
      fprintf(stderr, "ERROR: wrong value for parameter SOTTOMOD in file %s\n", argv[1]);
      break;
   }
   case 4:
   {
      fprintf(stderr, "ERROR: missing parameter OXMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 5:
   {
      fprintf(stderr, "ERROR: missing parameter OYMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 6:
   {
      fprintf(stderr, "ERROR: missing parameter OZMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 7:
   {
      fprintf(stderr, "ERROR: missing parameter NXMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 8:
   {
      fprintf(stderr, "ERROR: missing parameter NYMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 9:
   {
      fprintf(stderr, "ERROR: missing parameter NZMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 10:
   {  
      fprintf(stderr, "ERROR: missing parameter PXMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 11:
   {
      fprintf(stderr, "ERROR: missing parameter PYMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 12:
   {
      fprintf(stderr, "ERROR: missing parameter PZMINIMOD in file %s\n", argv[1]);
      break;
   }
   case 13:
   {
      fprintf(stderr, "ERROR: unable to open Header file MODELLO of file ->  %s \n", argv[1]);
      break;
   }
   case 14:
   {
      fprintf(stderr, "ERROR: missing parameters in Header file MODELLO of file -> %s\n", argv[1]);
      break;
   }
   case 15:
   {
      fprintf(stderr, "ERROR: negative number of sub-model samples\n");
      break;
   }
   case 16:
   {
      fprintf(stderr, "ERROR: negative sub-sampling steps\n");
      break;
   }
   case 17:
   {
      fprintf(stderr, "ERROR: x sub-model borders out of model\n");
      break;
   }
   case 18:
   {
      fprintf(stderr, "ERROR: y sub-model borders out of model\n");
      break;
   }
   case 19:
   {
      fprintf(stderr, "ERROR: z sub-model borders out of model\n");
      break;
   }
   case 20:
   {
      fprintf(stderr, "ERROR: unable to open file  %s \n", argv[2]);
      break;
   }
   case 21:
   {
      fprintf(stderr, "ERROR: missing parameter VERSIONE in file %s\n", argv[2]);
      break;
   }
   case 22:
   {
      fprintf(stderr, "ERROR: parameter value VERSIONE in file %s\n", argv[2]);
      break;
   }
   case 23:
   {
      fprintf(stderr, "ERROR: missing parameter INIZIO in file %s\n", argv[2]);
      break;
   }
   case 24:
   {
      fprintf(stderr, "ERROR: parameter value INIZIO in file %s\n", argv[2]);
      break;
   }
   case 25:
   {
      fprintf(stderr, "ERROR: missing parameter APERTURA in file %s\n", argv[2]);
      break;
   }
   case 26:
   {
      fprintf(stderr, "ERROR: parameter value APERTURA in file %s\n", argv[2]);
      break;
   }
   case 27:
   {
      fprintf(stderr, "ERROR: missing parameter MODALITA in file %s\n", argv[2]);
      break;
   }
   case 28:
   {
      fprintf(stderr, "ERROR: parameter value MODALITA in file %s\n", argv[2]);
      break;
   } 
   case 29:
   {
      fprintf(stderr, "ERROR: missing parameter SOGLIAX in file %s\n", argv[2]);
      break;
   }
   case 30:
   {
      fprintf(stderr, "ERROR: parameter value SOGLIAX in file %s\n", argv[2]);
      break;
   }
   case 31:
   {
      fprintf(stderr, "ERROR: missing parameter SOGLIAY in file %s\n", argv[2]);
      break;
   }
   case 32:
   {
      fprintf(stderr, "ERROR: parameter value SOGLIAX in file %s\n", argv[2]);
      break;
   }
   case 33:
   {
      fprintf(stderr, "ERROR: missing parameter AMPMIN in file %s\n", argv[2]);
      break;
   }
   case 34:
   {
      fprintf(stderr, "ERROR: parameter value AMPMIN in file %s\n", argv[2]);
      break;
   }
   case 35:
   {
      fprintf(stderr, "ERROR: missing parameter DAMP in file %s\n", argv[2]);
      break;
   }
   case 36:
   {
      fprintf(stderr, "ERROR: parameter value DAMP in file %s\n", argv[2]);
      break;
   }
   case 37:
   {  
      fprintf(stderr, "ERROR: missing parameter ANGOLO in file %s\n", argv[2]);
      break;
   }
   case 38:
   {
      fprintf(stderr, "ERROR: parameter value ANGOLO in file %s\n", argv[2]);
      break;
   }
   case 39:
   {
      fprintf(stderr, "ERROR: parameter value VERBOSE in file %s\n", argv[2]);
      break;
   }
   case 40:
   {
      fprintf(stderr, "ERROR:  parameter value NPACKETS <= 0 in file %s\n", argv[2]);
      break;
   }
   case 41:
   {
      fprintf(stderr, "ERROR:  parameter value MEMOFFSET <= 0 in file %s\n", argv[2]);
      break;
   }
   case 42:
   {
      fprintf(stderr, "ERROR: unable to open file  %s \n", argv[3]);
      break;
   }
   case 43:
   {
      fprintf(stderr, "ERROR: missing parameter SOURCE-FILE in file %s\n", argv[3]);
      break;
   }
   case 44:
   {
      fprintf(stderr, "ERROR: missing parameter TARGET in file %s\n", argv[3]);
      break;
   }
   case 45:
   {
      fprintf(stderr, "ERROR: parameter value TARGET in file %s\n", argv[3]);
      break;
   }
   case 46:
   {
      fprintf(stderr, "ERROR: missing parameter TARGET-FILE in file %s\n", argv[3]);
      break;
   }
   case 47:
   {
      fprintf(stderr, "ERROR: missing parameter UPDOWN in file %s\n", argv[3]);
      break;
   }
   case 48:
   {
      fprintf(stderr, "ERROR: parameter value UPDOWN in file %s\n", argv[3]);
      break;
   }
   case 49:
   {
      fprintf(stderr, "ERROR: missing parameter OUTPUT in file %s\n", argv[3]);
      break;
   }
   case 50:
   {
      fprintf(stderr, "ERROR: parameter value OUTPUT in file %s\n", argv[3]);
      break;
   }
   case 51:
   {
      fprintf(stderr, "ERROR: parameter value OUT_A in file %s\n", argv[3]);
      break;
   }
   case 52:
   {
      fprintf(stderr, "ERROR: parameter value OUT_T in file %s\n", argv[3]);
      break;
   }
   case 53:
   {
      fprintf(stderr, "ERROR: parameter value PX in file %s\n", argv[3]);
      break;
   }
   case 54:
   {
      fprintf(stderr, "ERROR: parameter value PY in file %s\n", argv[3]);
      break;
   }
   case 55:
   {
      fprintf(stderr, "ERROR: parameter value PZ in file %s\n", argv[3]);
      break;
   }
   case 56:
   {
      fprintf(stderr, "ERROR: unable to open SOURCE file of file -> %s \n", argv[3]);
      break;
   }
   case 57:
   {
      fprintf(stderr, "ERROR: unable to open TARGET file of file -> %s \n", argv[3]);
      break;
   }
   case 58:
   {
      fprintf(stderr, "ERROR: output directory name too long : %s\n", argv[4]);
      break;
   }
   case 59:
   {
      fprintf(stderr, "ERROR: number of sources < 1 \n");
      break;
   }
   case 60:
   {
      fprintf(stderr, "ERROR: negative velocity sampling steps \n");
      break;
   }
   case 61:
   {
      fprintf(stderr, "ERROR: negative number of velocity model samples\n");
      break;
   }
   case 62:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array xshot\n");
      break;
   }
   case 63:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array yshot\n");
      break;
   }
   case 64:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array zshot\n");
      break;
   }
   case 65:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array S\n");
      break;
   }
   case 66:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array A\n");
      break;
   }
   case 67:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array T\n");
      break;
   }
   case 68:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Px\n");
      break;
   }
   case 69:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Py\n");
      break;
   }
   case 70:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pz\n");
      break;
   }
   case 71:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array AATTPP\n");
      break;
   }
   case 72:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array requests\n");
      break;
   }
   case 73:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array statuses\n");
      break;
   }
   case 74:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pbuffer\n");
      break;
   }
   case 75:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array xtgt\n");
      break;
   }
   case 76:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array ytgt\n");
      break;
   }
   case 77:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array ztgt\n");
      break;
   }
   case 78:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Tcorr\n");
      break;
   }
   case 79:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Acorr\n");
      break;
   }
   case 80:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pxcorr\n");
      break;
   }
   case 81:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pycorr\n");
      break;
   }
   case 82:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array xcorr\n");
      break;
   }
   case 83:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array ycorr\n");
      break;
   }
   case 84:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Ok1\n");
      break;
   }
   case 85:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Ok2\n");
      break;
   }
   case 86:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Posx\n");
      break;
   }
   case 87:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Posy\n");
      break;
   }
   case 88:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Sx\n");
      break;
   }
   case 89:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Sy\n");
      break;
   }
   case 90:
   {
      fprintf(stderr, "ERROR: unable to open SOURCE file of file -> %s \n", argv[3]);
      break;
   }
   case 91:
   {
      fprintf(stderr, "ERROR: x coordinate out of borders\n");
      break;
   }
   case 92:
   {
      fprintf(stderr, "ERROR: y coordinate out of borders.\n");
      break;
   }
   case 93:
   {
      fprintf(stderr, "ERROR: z coordinate out of borders.\n");
      break;
   }
   case 94:
   {
      fprintf(stderr, "ERROR: unable to open TARGET file of file -> %s \n", argv[3]);
      break;
   }
   case 95:
   {
      fprintf(stderr, "ERROR: target x coordinate out of borders\n");
      break;
   }
   case 96:
   {
      fprintf(stderr, "ERROR: target y coordinate out of borders\n");
      break;
   }
   case 97:
   {
      fprintf(stderr, "ERROR: target z coordinate out of borders\n");
      break;
   }
   case 98:
   {
      fprintf(stderr, "ERROR: unable to open MODEL file of file -> %s \n", argv[1]);
      break;
   }
   case 99:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array ident\n");
      break;
   }
   case 100:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array val\n");
      break;
   }
   case 101:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array HeaderFileIn\n");
      break;
   }
   case 102:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array DataFileIn\n");
      break;
   }
   case 103:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array temp in for n. 1\n");
      break;
   }
   case 104:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array temp in for n. 2\n");
      break;
   }
   case 105:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array SourceFile\n");
      break;
   }
   case 106:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array TargetFile\n");
      break;
   }
   case 107:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Outdir\n");
      break;
   }
   case 108:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array mode in point n.1\n");
      break;
   }
   case 109:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array mode in point n.2\n");
      break;
   }
   case 110:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array mode in point n.3\n");
      break;
   }
   case 111:
   {
      fprintf(stderr, "ERROR: unable to access the output directory\n");
      break;
   }
   case 112:
   {
      fprintf(stderr, "ERROR: to run eikonal-mpi use a number of processors >= 2\n");
      break;
   }
   case 113:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array shortbuf\n");
      break;
   }
   case 114:
   {
      fprintf(stderr, "ERROR: ERROR: cannot open file rangeCx.sht\n");
      break;
   }
   case 115:
   {
      fprintf(stderr, "ERROR: ERROR: cannot open file rangeCy.sht\n");
      break;
   }
   case 116:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array range\n");
      break;
   }
   case 117:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array mode in point n.4\n");
      break;
   }
   case 118:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array rangeFile\n");
      break;
   }
   case 119:
   {
      fprintf(stderr, "ERROR: cannot open file rangeT.sht \n");
      break;
   }
   case 120:
   {
      fprintf(stderr, "ERROR: cannot open file rangeA.sht \n");
      break;
   }
   case 121:
   {
      fprintf(stderr, "ERROR: cannot open file rangeCz.sht\n");
      break;
   }
   case 122:
   {
      fprintf(stderr, "ERROR: cannot open file sortGreenF.sht \n");
      break;
   }
   case 123:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Ttrim\n");
      break;
   }
   case 124:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pxtrim\n");
      break;
   }
   case 125:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pytrim\n");
      break;
   }
   case 126:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array Pztrim\n");
      break;
   }
   case 127:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array vettdistx\n");
      break;
   }
   case 128:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array vettdisty\n");
      break;
   }
   case 129:
   {
      fprintf(stderr, "ERROR: parameter value maxdisty <0 in file %s\n", argv[2]);
      break;
   }
    case 130:
   {
      fprintf(stderr, "ERROR: parameter value maxdistx <0 in file %s\n", argv[2]);
      break;
   }
   case 131:
   {
      fprintf(stderr, "ERROR: parameter value distxiniz not allowed in file %s\n", argv[2]);
      break;
   }
    case 132:
   {
      fprintf(stderr, "ERROR: parameter value distyiniz not allowed in file %s\n", argv[2]);
      break;
   }
   case 133:
   {
      fprintf(stderr, "ERROR: parameter value zmaxdisty not allowed in file %s\n", argv[2]);
      break;
   }
    case 134:
   {
      fprintf(stderr, "ERROR: parameter value zmaxdistx not allowed in file %s\n", argv[2]);
      break;
   }
    case 135:
   {
      fprintf(stderr, "ERROR:parameter value zdistyiniz not allowed in file parameter %s\n", argv[2]);
      break;
   }   
    case 136:
   {
      fprintf(stderr, "ERROR:parameter value QUANT_FLAG  in file %s\n", argv[3]);
      break;
   }   
   case 137:
   {
      fprintf(stderr, "ERROR:cannot allocate memory for array shotSort\n");
      break;
   }
}
        
MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
exit(EXIT_FAILURE);

}

