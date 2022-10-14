#ifdef CRAY90
  #include <fortran.h>
  int BACIO
         (int * mode, int * start, int *newpos, int * size, int * no, 
          int * nactual, int * fdes, 
          _fcd fcd_fname, _fcd fcd_datary) { 
  char *fname, *datary;
  int namelen;
#endif
#ifdef HP
  int bacio
         (int * mode, int * start, int *newpos, int * size, int * no, 
          int * nactual, int * fdes, const char *fname, char *datary, 
          int  namelen, int  datanamelen) {
#endif
#ifdef SGI
  int bacio_
         (int * mode, int * start, int *newpos, int * size, int * no, 
          int * nactual, int * fdes, const char *fname, char *datary, 
          int  namelen, int  datanamelen) {
#endif
#ifdef LINUX
  int bacio_
         (int * mode, int * start, int *newpos, int * size, int * no, 
          int * nactual, int * fdes, const char *fname, char *datary, 
          int  namelen, int  datanamelen) {
#endif
  int i, j, jret, seekret;
  char *realname, *tempchar;
  int tcharval;
  size_t count;

/* Initialization(s) */
  *nactual = 0;

/* Check for illegal combinations of options */
  if (( BAOPEN_RONLY & *mode) && (BAOPEN_WONLY & *mode) ) {
     #ifdef VERBOSE
       printf("illegal -- trying to open both read only and write only\n");
     #endif
     return -1;
  }
  if ( (BAREAD & *mode ) && (BAWRITE & *mode) ) {
     #ifdef VERBOSE
       printf("illegal -- trying to both read and write in the same call\n");
     #endif
     return -2;
  }

/* This section handles Fortran to C translation of strings so as to */
/*   be able to open the files Fortran is expecting to be opened.    */
  #ifdef CRAY90
    namelen = _fcdlen(fcd_fname);
    fname   = _fcdtocp(fcd_fname);
  #endif
  if ( (BAOPEN_RONLY & *mode) | (BAOPEN_WONLY & *mode) | 
       (BAOPEN_RW & *mode) ) {
    #ifdef VERBOSE
      printf("Will be opening a file %s %d\n", fname, namelen); fflush(stdout);
      printf("Strlen %d namelen %d\n", strlen(fname), namelen); fflush(stdout);
    #endif
    realname = (char *) malloc( namelen * sizeof(char) ) ;
    if (realname == NULL) { 
      #ifdef VERBOSE
        printf("failed to mallocate realname %d = namelen\n", namelen);
        fflush(stdout);
      #endif
      return -3;
    }
    tempchar = (char *) malloc(sizeof(char) * 1 ) ;
    i = 0;
    j = 0;
    *tempchar = fname[i];
    tcharval = *tempchar;
    while (i == j && i < namelen ) {
       fflush(stdout); 
       if ( isgraph(tcharval) ) {
         realname[j] = fname[i];
         j += 1;
       }
       i += 1;
       *tempchar = fname[i];
       tcharval = *tempchar;
    }
    #ifdef VERBOSE
      printf("i,j = %d %d\n",i,j); fflush(stdout);
    #endif
    realname[j] = '\0';
  } 
   
/* Open files with correct read/write and file permission. */
  if (BAOPEN_RONLY & *mode) {
    #ifdef VERBOSE
      printf("open read only %s\n", realname);
    #endif
     *fdes = open(realname, O_RDONLY | O_CREAT , S_IRWXU | S_IRWXG | S_IRWXO );
  }
  else if (BAOPEN_WONLY & *mode ) {
    #ifdef VERBOSE
      printf("open write only %s\n", realname);
    #endif
     *fdes = open(realname, O_WRONLY | O_CREAT , S_IRWXU | S_IRWXG | S_IRWXO );
  }
  else if (BAOPEN_RW & *mode) {
    #ifdef VERBOSE
      printf("open read-write %s\n", realname);
    #endif
     *fdes = open(realname, O_RDWR | O_CREAT , S_IRWXU | S_IRWXG | S_IRWXO );
  }
  else {
    #ifdef VERBOSE
      printf("no openings\n");
    #endif
  }
  if (*fdes < 0) {
    #ifdef VERBOSE
      printf("error in file descriptor! *fdes %d\n", *fdes);
    #endif
    return -4;
  }
  else {
    #ifdef VERBOSE
      printf("file descriptor = %d\n",*fdes );
    #endif
  }


/* Read data as requested */
  if (BAREAD & *mode && (BAOPEN_WONLY & *mode) ) {
    #ifdef VERBOSE
      printf("Error, trying to read while in write only mode!\n");
    #endif
    return -5;
  }
  else if (BAREAD & *mode ) {
  /* Read in some data */
    if (! (*mode & NOSEEK) ) {
      seekret = lseek(*fdes, *start, SEEK_SET);
      if (seekret == -1) {
        #ifdef VERBOSE
          printf("error in seeking to %d\n",*start);
        #endif
        return -6;
      }
      #ifdef VERBOSE
      else {
         printf("Seek successful, seek ret %d, start %d\n", seekret, *start);
      }
      #endif
    }
    #ifdef CRAY90
      datary = _fcdtocp(fcd_datary);
    #endif
    if (datary == NULL) {
      printf("Massive catastrophe -- datary pointer is NULL\n");
      return -666;
    }
    #ifdef VERBOSE
      printf("file descriptor, datary = %d %d\n", *fdes, (int) datary);
    #endif
    count = (size_t) *no;
    jret = read(*fdes, (void *) datary, count);
    if (jret != *no) {
      #ifdef VERBOSE
        printf("did not read in the requested number of bytes\n");
        printf("read in %d bytes instead of %d \n",jret, *no);
      #endif
    }  
    else {
    #ifdef VERBOSE
      printf("read in %d bytes requested \n", *no);
    #endif
    }
    *nactual = jret;
    *newpos = *start + jret;
  }
/* Done with reading */
 
/* See if we should be writing */
  if ( BAWRITE & *mode && BAOPEN_RONLY & *mode ) {
    #ifdef VERBOSE
      printf("Trying to write on a read only file \n");
    #endif
     return -7;
  }
  else if ( BAWRITE & *mode ) {
    if (! (*mode & NOSEEK) ) {
      seekret = lseek(*fdes, *start, SEEK_SET);
      if (seekret == -1) {
      #ifdef VERBOSE
        printf("error in seeking to %d\n",*start);
      #endif
        return -8;
      }
    }
    #ifdef CRAY90
      datary = _fcdtocp(fcd_datary);
    #endif
    if (datary == NULL) {
      printf("Massive catastrophe -- datary pointer is NULL\n");
      return -666;
    }
    #ifdef VERBOSE
      printf("write file descriptor, datary = %d %d\n", *fdes, (int) datary);
    #endif
    count = (size_t) *no;
    jret = write(*fdes, (void *) datary, count);
    if (jret != *no) {
    #ifdef VERBOSE
      printf("did not write out the requested number of bytes\n");
      printf("wrote %d bytes instead\n", jret);
    #endif
      *nactual = jret;
      *newpos = *start + jret;
    }
    else {
    #ifdef VERBOSE
       printf("wrote %d bytes \n", jret);
    #endif
       *nactual = jret;
       *newpos = *start + jret;
    }
  }
/* Done with writing */
    

/* Close file if requested */
  if (BACLOSE & *mode ) {
    jret = close(*fdes);
    if (jret != 0) { 
    #ifdef VERBOSE
      printf("close failed! jret = %d\n",jret);
    #endif
      return -9;
    }
  }
/* Done closing */

/* Check that if we were reading or writing, that we actually got what */
/*  we expected, else return a -10.  Return 0 (success) if we're here  */
/*  and weren't reading or writing */
  if ( (*mode & BAREAD || *mode & BAWRITE) && (*nactual != *no) ) {
    return -10;
  }
  else {
    return 0;
  }
} 
