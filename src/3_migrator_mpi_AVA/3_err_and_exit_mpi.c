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
      fprintf(stderr, "ERROR: unable to open file  %s \n", argv[2]);
      break;
   }
   case 2:
   {
      fprintf(stderr, "ERROR: missing parameter BORDF in file %s\n", argv[2]);
      break;
   }
   case 3:
   {
      fprintf(stderr, "ERROR: parameter value BORDF in file %s\n", argv[2]);
      break;
   }
   case 4:
   {
      fprintf(stderr, "ERROR: missing parameter SEMIAPERTURA in file %s\n", argv[2]);
      break;
   }
   case 5:
   {
      fprintf(stderr,"ERROR: parameter value SEMIAPERTURA in file %s\n", argv[2] );
      break;
   }
   case 6:
   {
      fprintf(stderr, "ERROR: missing parameter SEMIAPERTURA in file %s\n", argv[2]);
      break;
   }
   case 7:
   {
      fprintf(stderr,"ERROR: parameter value SMUSSAMENTO in file %s\n", argv[2] );
      break;
   }
   case 8:
   {
      fprintf(stderr, "ERROR: missing parameter SOGLIA_AMPIEZZE in file %s\n", argv[2]);
      break;
   }
   case 9:
   {
      fprintf(stderr, "ERROR: parameter value SOGLIA_AMPIEZZE in file %s\n", argv[2]);
      break;
   }
   case 10:
   {
      fprintf(stderr, "ERROR: missing parameter IMAGING in file %s\n", argv[2]);
      break;
   }
   case 11:
   {
      fprintf(stderr, "ERROR: parameter value IMAGING in file %s\n", argv[2]);
      break;
   }
   case 12:
   {
      fprintf(stderr, "ERROR: parameter value AVA-ANALYSIS in file %s\n", argv[2]);
      break;
   }
   case 13:
   {
      fprintf(stderr, "ERROR: parameter value VERBOSE in file %s\n", argv[2]);
      break;
   }
   case 14:
   {
      fprintf(stderr, "ERROR: OFFMIN > OFFMAX in file %s\n", argv[2]);
      break;
   }
   case 15:
   {
      fprintf(stderr, "ERROR: missing or wrong parameter NUMTRACCEPERPE in file %s\n", argv[2]);
      break;
   }
   case 16:
   {
      fprintf(stderr, "ERROR: unable to open file  %s \n", argv[1]);
      break;
   }
   case 17:
   {
      fprintf(stderr, "ERROR: missing or wrong parameter VELSUP in file %s\n", argv[1]);
      break;
   }
   case 18:
   {
      fprintf(stderr, "ERROR: unable to open file  %s \n", argv[3]);
      break;
   }
   case 19:
   {
      fprintf(stderr, "ERROR: missing parameter ACQUISITION-FILE in file %s\n", argv[3]);
      break;
   }
   case 20:
   {
      fprintf(stderr, "ERROR: missing parameter TARGET-FILE in file %s\n", argv[3]);
      break;
   }
   case 21:
   {
      fprintf(stderr, "ERROR: missing parameter DATABASE-DIR in file %s\n", argv[3]);
      break;
   }
   case 22:
   {
      fprintf(stderr, "ERROR: database directory DATABASE-DIR name too long in file %s\n", argv[3]);      
      break;
   }
   case 23:
   {
      fprintf(stderr, "ERROR: unable to open file ACQUISITION-FILE indicated in file %s\n", argv[3]);
      break;
   }
   case 24:
   {
      fprintf(stderr, "ERROR: unable to open file TARGET-FILE indicated in file %s\n", argv[3]);
      break;
   }
   case 25:
   {
      fprintf(stderr, "ERROR: NUMTRACCEPERPE * Number of PEs > max_ntracce indicated in file %s\n", argv[1]);
      break;
   }
   case 26:
   {
      fprintf(stderr, "ERROR: output directory name too long : %s\n", argv[4]);
      break;
   }
   case 27:
   {
      fprintf(stderr, "ERROR: MAX_NCAMP non valido\n");
      break;
   }
   case 28:
   {
      fprintf(stderr, "ERROR: MAX_NTRACCE non valido\n");
      break;
   } 
   case 29:
   {
      fprintf(stderr, "ERROR: MAX_NCAMPFILT non valido\n");
      break;
   }
   case 30:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_o1\n");
      break;
   }
   case 31:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_o2\n");
      break;
   }
   case 32:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_o3\n");
      break;
   }
   case 33:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_ca1\n");
      break;
   }
   case 34:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_ca2\n");
      break;
   }
   case 35:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_ca3\n");
      break;
   }
   case 36:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_cb1\n");
      break;
   }
   case 37:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_cb2\n");
      break;
   }
   case 38:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_cb3\n");
      break;
   }
   case 39:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_cc1\n");
      break;
   }
   case 40:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_cc2\n");
      break;
   }
   case 41:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_cc3\n");
      break;
   }
   case 42:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_da\n");
      break;
   }
   case 43:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_db\n");
      break;
   }
   case 44:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_na\n");
      break;
   }
   case 45:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_nb\n");
      break;
   }
   case 46:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_nt\n");
      break;
   }
   case 47:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_dda\n");
      break;
   }
   case 48:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_ddb\n");
      break;
   }
   case 49:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_nna\n");
      break;
   }
   case 50:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_nnb\n");
      break;
   }
   case 51:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array acq_nnt\n");
      break;
   }
   case 52:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_o1\n");
      break;
   }
   case 53:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_o2\n");
      break;
   }
   case 54:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_o3\n");
      break;
   }
   case 55:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_ca1\n");
      break;
   }
   case 56:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_ca2\n");
      break;
   }
   case 57:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_ca3\n");
      break;
   }
   case 58:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_cb1\n");
      break;
   }
   case 59:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_cb2\n");
      break;
   }
   case 60:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_cb3\n");
      break;
   }
   case 61:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_cc1\n");
      break;
   }
   case 62:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_cc2\n");
      break;
   }
   case 63:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_cc3\n");
      break;
   }
   case 64:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_da\n");
      break;
   }
   case 65:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_db\n");
      break;
   }
   case 66:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_na\n");
      break;
   }
   case 67:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_nb\n");
      break;
   }
   case 68:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_nt\n");
      break;
   }
   case 69:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_dda\n");
      break;
   }
   case 70:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_ddb\n");
      break;
   }
   case 71:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_nna\n");
      break;
   }
   case 72:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_nnb\n");
      break;
   }
   case 73:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_nnt\n");
      break;
   }
   case 74:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_weights_lin_a\n");
      break;
   }
   case 75:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array SA\n");
      break;
   }
   case 76:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array ST\n");
      break;
   }
   case 77:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array dbrestart\n");
      break;
   }
   case 78:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RA\n");
      break;
   }
   case 79:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RT\n");
      break;
   }
   case 80:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RUx\n");
      break;
   }
   case 81:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RUy\n");
      break;
   }
   case 82:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RUz\n");
      break;
   }
   case 83:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tracce\n");
      break;
   }
   case 84:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traces\n");
      break;
   }
   case 85:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traccia_temp\n");
      break;
   }
   case 86:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traccia_letta\n");
      break;
   }
   case 87:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traccia_lettac\n");
      break;
   }
   case 88:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traccia1\n");
      break;
   }
   case 89:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traccia2\n");
      break;
   }
   case 90:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_xr\n");
      break;
   }
   case 91:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_yr\n");
      break;
   }
   case 92:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_zr\n");
      break;
   }
   case 93:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array binr_offset\n");
      break;
   }
   case 94:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array bins_offset\n");
      break;
   }
   case 95:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_xs\n");
      break;
   }
   case 96:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_ys\n");
      break;
   }
   case 97:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_zs\n");
      break;
   }
   case 98:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_ns\n");
      break;
   }
   case 99:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tr_weights\n");
      break;
   }
   case 100:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array attable\n");
      break;
   }
   case 101:
   { 
      fprintf(stderr, "ERROR: cannot allocate memory for array pan_save\n");
      break;
   }
   case 102:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array pan_parz\n");
      break;
   }
   case 103:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array pan_tot\n");
      break;
   }
   case 104:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array cont_save\n");
      break;
   }
   case 105:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array cont_rparz\n");
      break;
   }
   case 106:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array cont_r\n");
      break;
   }
   case 107:
   {
      fprintf(stderr,"ERROR: parameter value ATN in file %s\n", argv[2] );
      break;
   }
   case 108:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array mode\n");
      break;
   }
   case 109:
   {
      fprintf(stderr, "ERROR: unable to access the DATABASE-DIR indicated in file %s\n", argv[3]);
      break;
   }
   case 110:
   {
      fprintf(stderr, "ERROR: unable to access the output directory\n");
      break;
   }
   case 111:
   {
      fprintf(stderr, "ERROR: to run migrator-mpi use a number of processors >= 2\n");
      break;
   }

/* Clara lettura 16 bit*/  
    case 112:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array traccia_lettash\n");
      break;
   }
/* Clara fattore di decimazione delle tracce*/  
    case 113:
   {
      fprintf(stderr, "ERROR: parameter value datadecimation <1 in file %s\n", argv[2]);
      break;
   }
/* Clara2 apertura variabile con la profondita'*/   
    case 114:
   {
      fprintf(stderr, "ERROR: invalid parameter value aperturainiz in file %s\n", argv[2]);
      break;
   }
    case 115:
   {
      fprintf(stderr, "ERROR: parameter value zetamaxape <0 o >2500 in file %s\n", argv[2]);
      break;
   }
    case 116:
   {
      fprintf(stderr, "ERROR:cannot allocate memory for array vett_ape\n");
      break;
   }
/* Clara3 distanza massima Y (XLINE) e X (ILINE) a cui si migrano i dati*/  
   case 117:
   {
      fprintf(stderr, "ERROR: parameter value maxdisty <0 in file %s\n", argv[2]);
      break;
   }
    case 118:
   {
      fprintf(stderr, "ERROR: parameter value maxdistx <0 in file %s\n", argv[2]);
      break;
   }
/* Clara4 distanza massima Y (XLINE) e X (ILINE) variabile*/  
   case 119:
   {
      fprintf(stderr, "ERROR: parameter value distxiniz not allowed in file %s\n", argv[2]);
      break;
   }
    case 120:
   {
      fprintf(stderr, "ERROR: parameter value distyiniz not allowed in file %s\n", argv[2]);
      break;
   }
   case 121:
   {
      fprintf(stderr, "ERROR: parameter value zmaxdisty not allowed in file %s\n", argv[2]);
      break;
   }
    case 122:
   {
      fprintf(stderr, "ERROR: parameter value zmaxdistx not allowed in file %s\n", argv[2]);
      break;
   }
    case 123:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array vett_distx\n");
      break;
   }
    case 124:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array vett_disty\n");
      break;
   }
    case 125:
   {
      fprintf(stderr, "ERROR: parameter value muteFlag not allowed in file %s\n", argv[2]);
      break;
   }
   case 126:
   {
      fprintf(stderr, "ERROR: unable to open Mute File\n");
      break;
   }
    case 127:
   {
      fprintf(stderr, "ERROR: parameter values in Mute File not allowed\n");
      break;
   }
    case 128:
   {
      fprintf(stderr, "ERROR:cannot allocate memory for array vett_depth_in\n");
      break;
   } 
   case 129:
   
   {
      fprintf(stderr, "ERROR:cannot allocate memory for array vett_off_in\n");
      break;
   } 
   case 130:
   {
      fprintf(stderr, "ERROR:cannot allocate memory for array vett_off\n");
      break;
   }
/*Clara4_bis*/
   case 131:
   {
      fprintf(stderr, "ERROR:parameter value zdistyiniz not allowed in file parameter %s\n", argv[2]);
      break;
   }
   
   case 132:                                                                              
   {                                                                                       
      fprintf(stderr, "ERROR:cannot allocate memory for array rangeA\n");           
      break;                                                                               
   }                                                                                       
   case 133:                                                                               
                                                                                           
   {                                                                                       
      fprintf(stderr, "ERROR:cannot allocate memory for array rangeT \n");             
      break;                                                                               
   }                                                                                       
   case 134:                                                                               
   {                                                                                       
      fprintf(stderr, "ERROR:cannot allocate memory for array rangeCz\n");                
      break;                                                                               
   }                                                                                       
   case 135:                                                                               
   {                                                                                       
      fprintf(stderr, "ERROR:cannot allocate memory for array tempfdgT\n");                
      break;                                                                               
   } 
   case 136:
   {
      fprintf(stderr, "ERROR: unable to open rangeT.sht\n");
      break;
   } 
   case 137:
   {
      fprintf(stderr, "ERROR: unable to open rangeA.sht\n");
      break;
   }   
   case 138:
   {
      fprintf(stderr, "ERROR: unable to open rangeCz.sht\n");
      break;
   } 
   case 139:                                                                               
   {                                                                                       
      fprintf(stderr, "ERROR:cannot allocate memory for array tempfdgA\n");                
      break;                                                                               
   } 
   case 140:                                                                               
   {                                                                                       
      fprintf(stderr, "ERROR:cannot allocate memory for array tempfdgCz\n");                
      break;                                                                               
   }
   case 141:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_weights_lin_b\n");
      break;
   }   
   case 142:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_weights_lin_c\n");
      break;
   }   
   case 143:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_weights_cub_b\n");
      break;
   }   
   case 144:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array tg_weights_cub_c\n");
      break;
   } 
  /*AVA controllo parametri*/ 
      case 145:
   {
      fprintf(stderr, "ERROR: missing parameters for AVA classification (dip_max, azimuth_max) in file %s\n", argv[2]);
      break;
   }   
   case 146:
   {
      fprintf(stderr, "ERROR: wrong parameter for AVA classification dip_max in file %s\n", argv[2]);
      break;
   }   
   case 147:
   {
      fprintf(stderr, "ERROR: wrong parameter for AVA classification azimuth_max in file %s\n", argv[2]);
      break;
   }           
   case 148:
   {
      fprintf(stderr, "ERROR: wrong parameter for AVA classification step_dip in file %s\n", argv[2]);
      break;
   }   
   case 149:
   {
      fprintf(stderr, "ERROR: wrong parameter for AVA classification step_az in file %s\n", argv[2]);
      break;
   }           
   case 150:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array SVx\n");
      break;
   }
   case 151:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array SVy\n");
      break;
   }
   case 152:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array SVz\n");
      break;
   }
   case 153:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RVx\n");
      break;
   }
   case 154:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RVy\n");
      break;
   }
   case 155:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array RVz\n");
      break;
   }
   case 156:
   {
      fprintf(stderr, "ERROR: unable to open rangeVx.sht\n");
      break;
   } 
   case 157:
   {
      fprintf(stderr, "ERROR: unable to open rangeVy.sht\n");
      break;
   }   
   case 158:
   {
      fprintf(stderr, "ERROR: unable to open rangeVz.sht\n");
      break;
   } 
   case 159:
   {
      fprintf(stderr, "ERROR: unable to open sortGreenF.sht\n");
      break;
   }   
   case 160:
   {
      fprintf(stderr, "ERROR: cannot allocate memory for array sortfile\n");
      break;
   } 
   case 161:
   {
      fprintf(stderr, "ERROR: dimension of SortGreenF.sht and number of acq_points must agree\n");
      break;
   }    
   case 162:
   {
      fprintf(stderr, "ERROR: missing parameter off_max for AVO classification in file %s\n", argv[2]);
      break;
   } 
   case 163:
   {
      fprintf(stderr, "ERROR: wrong value for parameter step_off for AVO classification in file %s\n", argv[2]);
      break;
   }  
   case 164:
   {
      fprintf(stderr, "ERROR: you must choose either AVA or AVO analysis\n");
      break;
   }
   case 165:
   {
      fprintf(stderr, "ERROR: wrong value for parameter QUANT_TAC in file %s\n", argv[2]);
      break;
   }
   case 166:
   {
      fprintf(stderr, "ERROR: wrong value for parameter UDTAC in file %s\n", argv[2]);
      break;
   }
   case 167:
   {
      fprintf(stderr, "ERROR: wrong value for parameter QUANT_V in file %s\n", argv[2]);
      break;
   }
   case 168:
   {
      fprintf(stderr, "ERROR: wrong value for parameter UDV in file %s\n", argv[2]);
      break;
   }


}  

fflush(stderr);
fflush(stdout);        
MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
exit(EXIT_FAILURE);
   
}  
   
   
   
   
   
   
   
   
   
   