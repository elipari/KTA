/******************************************************/
/*                 A_SEP_header.c                     */
/******************************************************/

#include "../0_include/0_common_defs.h"
#include "../0_include/0_global_vars.h"

/*---------------------------------------------------------------------*/
/*      SEPH_get_int_                                                  */
/*---------------------------------------------------------------------*/
void seph_get_int_(char* file_name, char* name, int* value,
                   long int file_len, long int name_len)
{
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    FILE* fp;
    int i, imax;
    char* temp1;
    char* temp2;

    for (i=0; i< MAX_PARAM; i++)
       {
         ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
         val[i] = calloc(MAX_STRING_LEN, sizeof(char));
        }
    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));
    imax=0;

    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);

    fp=fopen(temp1, "r");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }
    imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
    fclose(fp);


    for(i=0; temp2[i]!='\0'; i++) temp2[i]=toupper(temp2[i]);
    i=0;
    while (i<imax)
        {
           if (strcmp(temp2, ident[i]) == 0 )
              {
                sscanf(val[i], "%d", value);
               }
           i++;
         }


    free(temp2);
    free(temp1);
    for (i=MAX_PARAM-1; i>=0; i--)
       {
         free(ident[i]);
         free(val[i]);
         }

}


/*---------------------------------------------------------------------*/
/*      SEPH_get_real_                                                  */
/*---------------------------------------------------------------------*/
void seph_get_real_(char* file_name, char* name, float* value,
                   long int file_len, long int name_len)
{
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    FILE* fp;
    int i, imax;
    char* temp1;
    char* temp2;


    for (i=0; i< MAX_PARAM; i++)
       {
         ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
         val[i] = calloc(MAX_STRING_LEN, sizeof(char));
        }
    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));
    imax=0;



    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);


    fp=fopen(temp1, "r");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }
    imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
    fclose(fp);


    for(i=0; temp2[i]!='\0'; i++) temp2[i]=toupper(temp2[i]);
    i=0;
    while (i<imax)
        {
           if (strcmp(temp2, ident[i]) == 0 )
              {
                sscanf(val[i], "%f", value);
               }
           i++;
         }

    free(temp2);
    free(temp1);
    for (i=0; i< MAX_PARAM; i++)
       {
         free(ident[i]);
         free(val[i]);
        }

}


/*---------------------------------------------------------------------*/
/*      SEPH_get_double_                                                  */
/*---------------------------------------------------------------------*/
void seph_get_double_(char* file_name, char* name, double* value,
                   long int file_len, long int name_len)
{
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    FILE* fp;
    int i, imax;
    char* temp1;
    char* temp2;


    for (i=0; i< MAX_PARAM; i++)
       {
         ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
         val[i] = calloc(MAX_STRING_LEN, sizeof(char));
        }
    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));
    imax=0;



    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);


    fp=fopen(temp1, "r");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }
    imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
    fclose(fp);


    for(i=0; temp2[i]!='\0'; i++) temp2[i]=toupper(temp2[i]);
    i=0;
    while (i<imax)
        {
           if (strcmp(temp2, ident[i]) == 0 )
              {
                sscanf(val[i], "%lf", value);
               }
           i++;
         }

    free(temp2);
    free(temp1);
    for (i=0; i< MAX_PARAM; i++)
       {
         free(ident[i]);
         free(val[i]);
        }

}


/*---------------------------------------------------------------------*/
/*      SEPH_get_string_                                               */
/*---------------------------------------------------------------------*/
void seph_get_string_(char* file_name, char* name, char* value, long int *ret_len,
                      long int file_len, long int name_len, long int value_len )
{
    char* ident[MAX_PARAM];
    char* val[MAX_PARAM];
    FILE* fp;
    int i, imax;
    char* temp1;
    char* temp2;


    for (i=0; i< MAX_PARAM; i++)
       {
         ident[i] = calloc(MAX_STRING_LEN, sizeof(char));
         val[i] = calloc(MAX_STRING_LEN, sizeof(char));
        }
    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));
    imax=0;



    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);


    fp=fopen(temp1, "r");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }
    imax = ReadParams(fp, ident, val, MAX_STRING_LEN , MAX_PARAM );
    fclose(fp);


    for(i=0; temp2[i]!='\0'; i++) temp2[i]=toupper(temp2[i]);
    i=0;
    while (i<imax)
        {
           if (strstr(ident[i], temp2 ) != NULL )
              {
                strcpy(value,val[i]);
               }
           i++;
         }
    *ret_len = strlen(value);
    format_c2f77_string(value, value_len);

    free(temp2);
    free(temp1);
    for (i=0; i< MAX_PARAM; i++)
       {
         free(ident[i]);
         free(val[i]);
        }

}




/*---------------------------------------------------------------------*/
/*      seph_put_int_                                                 */
/*---------------------------------------------------------------------*/
void seph_put_int_(char* file_name, char* name, int* value,
                   long int file_len, long int name_len)
{
    FILE* fp;

    char* temp1;
    char* temp2;

    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));

    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);


    fp=fopen(temp1, "a");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }

    fseek(fp,0,SEEK_END);
    fprintf(fp,"%s=%d\n", temp2, *value);

    fclose(fp);
    free(temp2);
    free(temp1);
}


/*---------------------------------------------------------------------*/
/*      seph_put_real_                                                 */
/*---------------------------------------------------------------------*/
void seph_put_real_(char* file_name, char* name, float* value,
                    long int file_len, long int name_len)
{
    FILE* fp;

    char* temp1;
    char* temp2;

    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));

    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);


    fp=fopen(temp1, "a");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }

    fseek(fp,0,SEEK_END);
    fprintf(fp,"%s=%f\n", temp2, *value);

    fclose(fp);
    free(temp2);
    free(temp1);
}


/*---------------------------------------------------------------------*/
/*      seph_put_double_                                                 */
/*---------------------------------------------------------------------*/
void seph_put_double_(char* file_name, char* name, double* value,
                   long int file_len, long int name_len)
{
    FILE* fp;

    char* temp1;
    char* temp2;

    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));

    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);


    fp=fopen(temp1, "a");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }

    fseek(fp,0,SEEK_END);
    fprintf(fp,"%s=%g\n", temp2, *value);

    fclose(fp);
    free(temp2);
    free(temp1);
}


/*---------------------------------------------------------------------*/
/*      seph_put_string_                                                 */
/*---------------------------------------------------------------------*/
void seph_put_string_(char* file_name, char* name, char* value,
                      long int file_len, long int name_len, long int value_len)
{
    FILE* fp;

    char* temp1;
    char* temp2;
    char* temp3;

    temp1 = calloc(MAX_STRING_LEN, sizeof(char));
    temp2 = calloc(MAX_STRING_LEN, sizeof(char));
    temp3 = calloc(MAX_STRING_LEN, sizeof(char));

    format_f772c_string(file_name, temp1, file_len);
    format_f772c_string(name, temp2, name_len);
    format_f772c_string(value, temp3, value_len);

    fp=fopen(temp1, "a");
    if (fp ==  NULL)
       {
          fprintf(stderr, "ERROR: unable to open file  %s \n", temp1);
          exit(EXIT_FAILURE);
        }

    fseek(fp,0,SEEK_END);
    fprintf(fp,"%s=\"%s\"\n", temp2, temp3);

    fclose(fp);

    free(temp3);
    free(temp2);
    free(temp1);
}

/*---------------------------------------------------------------------*/
/*      writeHeader                                                    */
/*---------------------------------------------------------------------*/

void writeHeaderMig (char *filename, char *title, int tg_type, float *tg_o1, float *tg_o2, float *tg_o3,    
                     float *tg_ca1, float *tg_ca2, float *tg_ca3, float *tg_cb1, float *tg_cb2, float *tg_cb3, 
                     float *tg_cc1, float *tg_cc2, float *tg_cc3, float *d1, float *d2, float *d3, int n1,     
                     int n2, int n3, int n4, int n5, char *dataformat, int esize, char *in, int bordf, float *semiap,         
                     float *apeiniz, float *zetamaxape, int maxdisty, int distyiniz, int zdistyiniz,     
                     int zmaxdisty, int maxdistx, int distxiniz, int zmaxdistx, float *smussamento,       
                     float *soglia_ampiezze, int imaging, int ava, float *dip_max, float *dip_min, 
                     float *azimuth_min, float *azimuth_max,
                     int muteFlag, char *muteFileName, int datadecimation, 
                     int antialias) 
{
        
    char st[MAX_STRING_LEN];
    char st1[MAX_STRING_LEN];


         strcpy(st, "title");
         seph_put_string_(filename,st,title,strlen(filename),strlen(st),strlen(title));

         strcpy(st, "settype");
         seph_put_int_(filename,st,&tg_type,strlen(filename),strlen(st));

         strcpy(st, "o1");
         seph_put_real_(filename,st,tg_o1,strlen(filename),strlen(st));

         printf("===== O2 %f \n", *tg_o2);
         strcpy(st, "o2");
         seph_put_real_(filename,st,tg_o2,strlen(filename),strlen(st));

         strcpy(st, "o3");
         seph_put_real_(filename,st,tg_o3,strlen(filename),strlen(st));


         strcpy(st, "ca1");
         seph_put_real_(filename,st,tg_ca1,strlen(filename),strlen(st));

         strcpy(st, "ca2");
         seph_put_real_(filename,st,tg_ca2,strlen(filename),strlen(st));

         strcpy(st, "ca3");
         seph_put_real_(filename,st,tg_ca3,strlen(filename),strlen(st));

         strcpy(st, "cb1");
         seph_put_real_(filename,st,tg_cb1,strlen(filename),strlen(st));

         strcpy(st, "cb2");
         seph_put_real_(filename,st,tg_cb2,strlen(filename),strlen(st));

         strcpy(st, "cb3");
         seph_put_real_(filename,st,tg_cb3,strlen(filename),strlen(st));

         strcpy(st, "cc1");
         seph_put_real_(filename,st,tg_cc1,strlen(filename),strlen(st));

         strcpy(st, "cc2");
         seph_put_real_(filename,st,tg_cc2,strlen(filename),strlen(st));

         strcpy(st, "cc3");
         seph_put_real_(filename,st,tg_cc3,strlen(filename),strlen(st));

         strcpy(st, "d1");
         seph_put_real_(filename,st, d1, strlen(filename),strlen(st));

         strcpy(st, "d2");
         seph_put_real_(filename,st, d2, strlen(filename),strlen(st));
         
         strcpy(st, "d3");
         seph_put_real_(filename,st, d3, strlen(filename),strlen(st));
         
         strcpy(st, "n1");
         seph_put_int_(filename,st, &n1, strlen(filename),strlen(st));

         strcpy(st, "n2");
         seph_put_int_(filename,st, &n2, strlen(filename),strlen(st));
         
         strcpy(st, "n3");
         seph_put_int_(filename,st, &n3, strlen(filename),strlen(st));
         
         if (ava == 1) {
           strcpy(st, "n4");
           seph_put_int_(filename,st, &n4, strlen(filename),strlen(st));
        
           strcpy(st, "n5");
           seph_put_int_(filename,st, &n5, strlen(filename),strlen(st));
         }

         strcpy(st, "dataformat");
         seph_put_string_(filename, st, dataformat, strlen(filename),strlen(st),strlen(dataformat));

         strcpy(st, "esize");
         seph_put_int_(filename,st, &esize, strlen(filename),strlen(st));

         strcpy(st, "in");
         seph_put_string_(filename,st,in,strlen(filename),strlen(st),strlen(in));
         
         strcpy(st, "bordf");
         seph_put_int_(filename,st,&bordf,strlen(filename),strlen(st));
         
         strcpy(st, "semiap");
         seph_put_real_(filename,st, semiap, strlen(filename),strlen(st));    
               
         strcpy(st, "apeiniz");
         seph_put_real_(filename,st, apeiniz, strlen(filename),strlen(st));
         
         strcpy(st, "zetamaxape");
         seph_put_real_(filename,st, zetamaxape, strlen(filename),strlen(st));

         strcpy(st, "maxdisty");
         seph_put_int_(filename,st, &maxdisty, strlen(filename),strlen(st));
         
         strcpy(st, "distyiniz");
         seph_put_int_(filename,st, &distyiniz, strlen(filename),strlen(st));
         
         strcpy(st, "zdistyiniz");
         seph_put_int_(filename,st, &zdistyiniz, strlen(filename),strlen(st)); 
             
         strcpy(st, "zmaxdisty");
         seph_put_int_(filename,st, &zmaxdisty, strlen(filename),strlen(st));
         
         strcpy(st, "maxdistx");
         seph_put_int_(filename,st, &maxdistx, strlen(filename),strlen(st));
         
         strcpy(st, "distxiniz");
         seph_put_int_(filename,st, &distxiniz, strlen(filename),strlen(st));
         
         strcpy(st, "zmaxdistx");
         seph_put_int_(filename,st, &zmaxdistx, strlen(filename),strlen(st));
         
         strcpy(st, "smussamento");
         seph_put_real_(filename,st, smussamento, strlen(filename),strlen(st));  
            
         strcpy(st, "soglia_ampiezze");
         seph_put_real_(filename,st, soglia_ampiezze, strlen(filename),strlen(st));
         
         strcpy(st, "datadecimation");
         seph_put_int_(filename,st, &datadecimation, strlen(filename),strlen(st));
         
         strcpy(st, "imaging");
         if (imaging == 1)
             strcpy(st1,"As * Ar");
         else if (imaging == 2)
             strcpy(st1,"Ar / As");
         else if (imaging == 3)
             strcpy(st1,"Ar / (As + soglia_ampiezze)");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));
         
         strcpy(st, "AVA");         
         if (ava == 0)
             strcpy(st1,"NO");    
         else if (ava == 1)
             strcpy(st1,"SI");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));
         
         if ((ava == 1)&&(*dip_max!=-1)) {
            strcpy(st,"min_dip");
            seph_put_real_(filename,st,dip_min,strlen(filename),strlen(st));	         
            strcpy(st,"max_dip");
            seph_put_real_(filename,st,dip_max,strlen(filename),strlen(st));	         
         } else {
            strcpy(st, "class. per dip");
            strcpy(st1,"NO");
            seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));         
         }	         
         if ((ava == 1)&&(*azimuth_max!=-1)) {
            strcpy(st,"min_azimuth");
            seph_put_real_(filename,st,azimuth_min,strlen(filename),strlen(st));	         
            strcpy(st,"max_azimuth");
            seph_put_real_(filename,st,azimuth_max,strlen(filename),strlen(st));	         
         } else {
            strcpy(st, "class. per azimuth");
            strcpy(st1,"NO");
            seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));         
         }         
         strcpy(st, "ANTIALIAS");                      
         if (antialias == 0)
             strcpy(st1,"NO");    
         else if (antialias == 1)
             strcpy(st1,"SI");
         seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));

         if (muteFlag == 1) {
            strcpy(st, "MUTE_ESTERNO");         
            strcpy(st1,"SI");
            seph_put_string_(filename,st,st1,strlen(filename),strlen(st),strlen(st1));
            strcpy(st, "MUTE_FILE");         
            seph_put_string_(filename,st,muteFileName,strlen(filename),strlen(st),strlen(muteFileName));         
         }
    

}