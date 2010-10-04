
void swap_(void *fbuffer, int *buffersize, int *n) {
  int i,j;
  unsigned char *buffer;
  unsigned char swappedbuffer[*buffersize * (*n)];

  buffer = fbuffer;
   
  memcpy(swappedbuffer, buffer, (*buffersize) * (*n));
  for (j=0; j<(*n); j++) {
    for( i=0; i<*buffersize; i++ ) {
      buffer[i+j*(*buffersize) ] = swappedbuffer[*buffersize+j*(*buffersize) -1-i];
    }   
  }
}

