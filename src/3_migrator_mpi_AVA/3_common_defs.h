#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <sys/stat.h>

#define REMARK '#'
#define MAX_PARAM 200
#define MAX_STRING_LEN 255
#define MAX_FILE_NUM 20

#define PI 3.1415926
#define EPS 0.001

#ifndef VERSION
  #define VERSION "XXX"
#endif

/*************************************************/
/*              Global definitions               */
/*************************************************/

typedef long long int int64;     /* R64 lug 2001 */

typedef struct filesgy *SEGYptr;

typedef struct filesgy {

      int seq;

      char filename[MAX_STRING_LEN];

      int primobitns, nshotnbytes;

      int primobitxs, xsnbytes;
      int primobitys, ysnbytes;
      int primobitzs, zsnbytes;

      int primobitxr, xrnbytes;
      int primobityr, yrnbytes;
      int primobitzr, zrnbytes;

      int primatr;
      int ntraccetot;
      int nshot;
      int ntracceshot;

      int ncamp;
      int ncampfilt;
      int formato;
      float dt;
      float dx;
      float tshift;

      float a11, a12, a21, a22;
      float b1, b2;

      char filepesi[MAX_STRING_LEN];

      SEGYptr nextsgy;

} SEGY;


typedef struct target *TGTptr;

typedef struct target {
      int seq;
      TGTptr next_tgt;

      int   settype;
      float o1, o2, o3;

      float a1, a2, a3;
      float b1, b2, b3;
      float c1, c2, c3;

      float theta, phi, psi;

      float la, ca1, ca2, ca3;
      float lb, cb1, cb2, cb3;
      float lc, cc1, cc2, cc3;

      float da, db, dc;
      float dda, ddb, ddc;
      int   na, nb, nc, nt;
      int   nna, nnb, nnc, nnt;
} TGT;


/*******************  A_SEP_header.c *******************/
void seph_get_int_(char* file_name, char* name, int* value,
                   long int file_len, long int name_len);
void seph_get_real_(char* file_name, char* name, float* value,
                   long int file_len, long int name_len);
void seph_get_double_(char* file_name, char* name, double* value,
                   long int file_len, long int name_len);
void seph_get_string_(char* file_name, char* name, char* value, long int *ret_len,
                      long int file_len, long int name_len, long int value_len);

void seph_put_int_(char* file_name, char* name, int* value,
                   long int file_len, long int name_len);
void seph_put_real_(char* file_name, char* name, float* value,
                    long int file_len, long int name_len);
void seph_put_double_(char* file_name, char* name, double* value,
                   long int file_len, long int name_len);
void seph_put_string_(char* file_name, char* name, char* value,
                      long int file_len, long int name_len, long int value_len);

/*******************    A_utils.c    *******************/
int ReadParams(FILE *fp, char *ident[], char *val[], int str_len, int vect_len);
long int format_f772c_string(char* source, char *dest, long int source_len);
long int format_c2f77_string(char* string, long int len);
double round(double x);
int byteswap(void *ptr, size_t dim, size_t num);

/*******************    A_glob_file.c    *******************/
void glob_fopen_(int* fdesc, char* file_name, char* mode, int* err,
                int file_len, int mode_len);
void glob_fclose_(int* fdesc, int* err);

void glob_fwrite_(int* fdesc, void *buf, int *dim, int *num, int* err);
void glob_fread_(int* fdesc, void *buf, int *dim, int *num, int* err);
void glob_feof_(int* fdesc, int* val);
void glob_fseek_(int* fdesc, long *offset, int* origin, int* err);


/*********** Begin R64 lug 2001 */
void glob_fopen64_(int* fdesc, char* file_name, char* mode, int* err,
                int file_len, int mode_len);
void glob_fclose64_(int* fdesc, int* err);

void glob_fseek64_(int* fdesc, int64 *off64, int* origin, int* err);
/*********** End  R64 lug 2001 */


void glob_sizereal4_(float *item, int *size);
void glob_sizeinteger_(int *item, int *size);

/*******************    A_parameters.c    *******************/
SEGYptr TracesParams(char *filename);
TGTptr TargetParams(char *filename, int *ptr_settype);


/*void writeHeaderMig (char *filename, char *title, int tg_type, float *tg_o1, float *tg_o2, float *tg_o3,    
                     float *tg_ca1, float *tg_ca2, float *tg_ca3, float *tg_cb1, float *tg_cb2, float *tg_cb3, 
                     float *tg_cc1, float *tg_cc2, float *tg_cc3, float *d1, float *d2, float *d3, int n1,     
                     int n2, int n3, int n4, int n5, char *dataformat, int esize, char *in, int bordf, float *semiap,         
                     float *apeiniz, float *zetamaxape, int maxdisty, int distyiniz, int zdistyiniz,     
                     int zmaxdisty, int maxdistx, int distxiniz, int zmaxdistx, float *smussamento,       
                     float *soglia_ampiezze, int imaging, int ava, float *dip_max, float *dip_min, 
                     float *azimuth_min, float *azimuth_max,
                     int muteFlag, char *muteFileName, int datadecimation, 
                     int antialias);*/
 void writeHeaderMig (char *filename, char *title, int tg_type, float *tg_o1, float *tg_o2, float *tg_o3,    
                     float *tg_ca1, float *tg_ca2, float *tg_ca3, float *tg_cb1, float *tg_cb2, float *tg_cb3, 
                     float *tg_cc1, float *tg_cc2, float *tg_cc3, float *d1, float *d2, float *d3, int n1,     
                     int n2, int n3, int n4, int n5, char *dataformat, int esize, char *in, int bordf, float *semiap,         
                     float *apeiniz, float *zetamaxape, int maxdisty, int distyiniz, int zdistyiniz,     
                     int zmaxdisty, int maxdistx, int distxiniz, int zmaxdistx, float *smussamento,       
                     float *soglia_ampiezze, int imaging, int ava, float *dip_max, float *dip_min, 
                     float *azimuth_min, float *azimuth_max,
                     int   avo, int offmax, int offmin, 
                     int muteFlag, char *muteFileName, int datadecimation, 
                     int antialias) ;