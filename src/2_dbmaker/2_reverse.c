/****************************************************/
/*                   reverse.c                      */
/****************************************************/
#include "../0_include/0_common_defs.h"

void reverse(char *Hfile, char *Dfile, int updown, int nshot, int ntarget)
{
    long i,j;

    char *str, *str1, *str2;

    int fdesc;
    char* mode;
    int dim, num;
    long len, len1;
    int file_error;
    float *block, *block2;

    str = calloc(MAX_STRING_LEN, sizeof(char));
    str1 = calloc(MAX_STRING_LEN, sizeof(char));
    str2 = calloc(MAX_STRING_LEN, sizeof(char));
    mode = calloc(10, sizeof(char));

    fdesc=0;
    strcpy(mode,"rb");
    glob_fopen_(&fdesc, Dfile, mode, &file_error, strlen(Dfile), strlen(mode));
    if (file_error != 0)
     {
        fprintf(stderr, "ERROR: unable to read file ->  %s \n", Dfile);
        exit(EXIT_FAILURE);
      }

    dim = sizeof(float);
    block = calloc(ntarget, dim);
    block2 = calloc(nshot*ntarget, dim);
    for (i=1; i<=nshot; i++)
      {
         glob_fread_(&fdesc, block, &dim, &ntarget, &file_error);
         for (j=1; j<=ntarget; j++)
             {
                block2[i+(j-1)*nshot-1] = block[j-1];
              }
       }

    glob_fclose_(&fdesc, &file_error);

    remove(Dfile);

    fdesc=0;
    strcpy(mode,"wb");
    glob_fopen_(&fdesc, Dfile, mode, &file_error, strlen(Dfile), strlen(mode));
    if (file_error != 0)
     {
        fprintf(stderr, "ERROR: unable to write file ->  %s \n", Dfile);
        exit(EXIT_FAILURE);
      }

    num = nshot*ntarget;
    glob_fwrite_(&fdesc, block2, &dim, &num, &file_error);

    glob_fclose_(&fdesc, &file_error);

    /* .......................... */

    len1 = MAX_STRING_LEN;
    strcpy(str, "TITLE");
    seph_get_string_(Hfile, str, str1, &len, strlen(Hfile), strlen(str), len1);
    str1[len]='\0';

    strcpy(str, "IN");
    seph_get_string_(Hfile, str, str2, &len, strlen(Hfile), strlen(str), len1);
    str2[len]='\0';

    remove(Hfile);

    strcpy(str, "title");
    seph_put_string_(Hfile,str,str1,strlen(Hfile),strlen(str),strlen(str1));

    strcpy(str, "updown");
    seph_put_int_(Hfile,str,&updown,strlen(Hfile),strlen(str));

    strcpy(str, "nshot");
    seph_put_int_(Hfile,str,&ntarget,strlen(Hfile),strlen(str));

    strcpy(str, "ntarget");
    seph_put_int_(Hfile,str,&nshot,strlen(Hfile),strlen(str));

    strcpy(str, "esize");
    seph_put_int_(Hfile,str, &dim,strlen(Hfile),strlen(str));

    strcpy(str, "in");
    seph_put_string_(Hfile,str,str2,strlen(Hfile),strlen(str),strlen(str2));


    free(str);
    free(str1);
    free(str2);
    free(mode);
    free(block);
    free(block2);
 }
