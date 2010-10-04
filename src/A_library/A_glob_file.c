/****************************************************/
/*                    A_glob_file.c                       */
/****************************************************/
#include "../0_include/0_common_defs.h"
#include "../0_include/0_global_vars.h"


/*----------------------- glob_fopen_ -----------------------*/
/*                                                           */
/* void  glob_fopen_(int* fdesc, char* file_name, char* mode,*/
/*                   int file_len, int mode_len)             */
/*                                                           */
/* Opens a file in mode (char* mode) using a globally defined*/
/* file pointer.                                             */
/*                                                           */
/* INPUT                                                     */
/* int* fdesc   : *fdesc is a number from 0 to MAX_FILE_NUM-1*/
/* char* file_name : string containing the file name         */
/* char* mode : string describing the opening mode           */
/*              (see fopen)                                  */
/*                                                           */
/* OUTPUT                                                    */
/*-----------------------------------------------------------*/
void glob_fopen_(int* fdesc, char* file_name, char* mode, int* err,
                int file_len, int mode_len)
{
     char string1[255];
     char string2[255];

     *err = 0;
     format_f772c_string(file_name, string1, file_len);
     format_f772c_string(mode, string2, mode_len);

     glob_fp[*fdesc]=fopen(string1,string2);
     if ( glob_fp[*fdesc] == NULL )
       {
         *err =1;
         fprintf(stderr, "ERROR: unable to open file %s \n", string1);
         exit(EXIT_FAILURE);
       }

}

/*----------------------- glob_fclose_ ----------------------*/
/*-----------------------------------------------------------*/
void glob_fclose_(int* fdesc, int* err)
{
   *err=fclose(glob_fp[*fdesc]);
 }

/*----------------------- glob_fwrite_ ----------------------*/
/*-----------------------------------------------------------*/
void glob_fwrite_(int* fdesc, void *buf, int *dim, int *num, int* err)
{
   size_t check;

   *err=0;
#ifdef INTEL
   byteswap(buf, *dim, *num);
#endif
   check=fwrite((char *)buf, *dim, *num, glob_fp[*fdesc]);
#ifdef INTEL
   byteswap(buf, *dim, *num);
#endif
   if (check != *num) *err=1;
}

/*----------------------- glob_fread_ ----------------------*/
/*-----------------------------------------------------------*/
void glob_fread_(int* fdesc, void *buf, int *dim, int *num, int* err)
{
  size_t check;

  *err=0;
  check=fread((char *)buf, (*dim), (*num), glob_fp[*fdesc]);
  if (check != (*num)) { *err=1; return; }
#ifdef INTEL
  byteswap(buf, *dim, *num);
#endif

}


/*-----------------------  glob_feof_  ----------------------*/
/*-----------------------------------------------------------*/
void glob_feof_(int* fdesc, int* val)
{
   *val = feof(glob_fp[*fdesc]);

}


/*----------------------- glob_fseek_ ----------------------*/
/*  *origin = 0  --> SEEK_SET                               */
/*  *origin = 1  --> SEEK_CUR                               */
/*  *origin = 2  --> SEEK_END                               */
/*-----------------------------------------------------------*/
void glob_fseek_(int* fdesc, long *offset, int* origin, int* err)
{

   if (*origin == 0)

     *err=fseek(glob_fp[*fdesc], *offset-1, SEEK_SET);

   else if (*origin == 1)

     *err=fseek(glob_fp[*fdesc], *offset-1, SEEK_CUR);

   else if (*origin == 2)

     *err=fseek(glob_fp[*fdesc], *offset-1, SEEK_END);

}

/*---------------------- glob_sizereal4_ --------------------*/
/*-----------------------------------------------------------*/
void glob_sizereal4_(float *item, int *size)
{
    *size=sizeof(*item);
 }

/*---------------------- glob_sizeinteger_ --------------------*/
/*-----------------------------------------------------------*/
void glob_sizeinteger_(int *item, int *size)
{
    *size=sizeof(*item);
 }

/*---------------------- glob_sizeshortint_ --------------------*/
/*-----------------------------------------------------------*/
void glob_sizeshortint_(short int *item, int *size)
{
    *size=sizeof(*item);
 }


#ifdef INTEL
/*-------------------       byteswap      -------------------*/
/*-----------------------------------------------------------*/
int byteswap(void *ptr, size_t dim, size_t num)
{

   unsigned char  *ch;
   int i;
   long l;

   ch = calloc(dim, sizeof(char));
   for (l=0; l<num; l++)
     {

       for (i=dim; i>=1; i--)
           ch[dim-i] = *((char *)(ptr+l*dim+i-1));

       for (i=0; i<dim; i++)
           *((char *) (ptr+l*dim+i)) = ch[i];

     }
   free(ch);
   return 0;
}
#endif

/*-------- 64 bit functions ---------------------------------*/
/* R64 lug 2001 */

/*----------------------- glob_fopen64_ ---------------------*/
/*                                                           */
/* void  glob_fopen_(int* fdesc, char* file_name, char* mode,*/
/*                   int file_len, int mode_len)             */
/*                                                           */
/* Opens a file in mode (char* mode) using a globally defined*/
/* file pointer.                                             */
/*                                                           */
/* INPUT                                                     */
/* int* fdesc   : *fdesc is a number from 0 to MAX_FILE_NUM-1*/
/* char* file_name : string containing the file name         */
/* char* mode : string describing the opening mode           */
/*              (see fopen)                                  */
/*                                                           */
/* OUTPUT                                                    */
/*-----------------------------------------------------------*/
void glob_fopen64_(int* fdesc, char* file_name, char* mode, int* err,
                int file_len, int mode_len)
{
     char string1[255];
     char string2[255];

     *err = 0;
     format_f772c_string(file_name, string1, file_len);
     format_f772c_string(mode, string2, mode_len);

     glob_fp[*fdesc]=fopen64(string1,string2);
     if ( glob_fp[*fdesc] == NULL )
       {
         *err =1;
         fprintf(stderr, "ERROR: unable to open file %s \n", string1);
         exit(EXIT_FAILURE);
       }

}

/*----------------------- glob_fclose64_ --------------------*/
/*-----------------------------------------------------------*/
void glob_fclose64_(int* fdesc, int* err)
{
   *err=fclose(glob_fp[*fdesc]);
 }

/*----------------------- glob_fseek64_ --------------------*/
/*  *origin = 0  --> SEEK_SET                               */
/*  *origin = 1  --> SEEK_CUR                               */
/*  *origin = 2  --> SEEK_END                               */
/*----------------------------------------------------------*/

void glob_fseek64_(int* fdesc, int64 *off64, int* origin, int* err)
{

   if (*origin == 0)

     *err=fseeko64(glob_fp[*fdesc], *off64-(int64)1, SEEK_SET);

   else if (*origin == 1)

     *err=fseeko64(glob_fp[*fdesc], *off64-(int64)1, SEEK_CUR);

   else if (*origin == 2)

     *err=fseeko64(glob_fp[*fdesc], *off64-(int64)1, SEEK_END);

}

/*----------------------- glob_ftell64_ ----------------------*/

void glob_ftell64_(int* fdesc, int64 *off64, int* err)
{
     *err=0;
     *off64=ftello64(glob_fp[*fdesc]);
     if (*off64 == -1) *err=-1;

}