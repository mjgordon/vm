#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"

#define MODE_WAITING 0
#define MODE_NB      1
#define MODE_INT8    2
#define MODE_INT12   3
#define MODE_INT16   4

const char *modeNames[5];

outputBuffer buffer8;
outputBuffer buffer12;
outputBuffer buffer16;

FILE *write_ptr;

int flagUnrollStack = 0;
int flagOutputPrint = 0;
int flagOutputFile = 0;

int mode = MODE_WAITING;




// Called at startup, sets up different sized output nb buffers, and the modes to use them
// Also opens the output file
void initializeIO(char* filenameOutput) {
  buffer8 = newOutputBuffer(2);
  buffer12 = newOutputBuffer(3);
  buffer16 = newOutputBuffer(4);
  
  modeNames[0] = "WAITING";
  modeNames[1] = "NB : ";
  modeNames[2] = "INT8 : ";
  modeNames[3] = "INT12 : ";
  modeNames[4] = "INT16 : ";

  write_ptr = fopen(filenameOutput,"wb+");
}


// Called at shutdown, closes the output file
void finishIO() {
  fclose(write_ptr);
}


// Receices one nb from the runtime. Will set the mode, or insert into a buffer, and print if necessary
void output(uint8_t nb) {
  int flag = 0;

  int output = 0;
  
  switch(mode) {
  case MODE_WAITING:
    mode = nb;
    break;
  case MODE_NB:
    flag = 1;
    output = nb;
    break;
  case MODE_INT8:
    flag = getInt(nb,&buffer8,&output);
    break;
  case MODE_INT12:
    flag = getInt(nb,&buffer12,&output);
    break;
  case MODE_INT16:
    flag = getInt(nb,&buffer16,&output);
    break;
  }

  if (flag) {
    if (flagOutputPrint) {
      printf("%s%i\n",modeNames[mode],output);
    }
    if (flagOutputFile) {
      fwrite(&output,1,1,write_ptr);
    }
    mode = MODE_WAITING;
  }
}


//Checks if the buffer is full, sets the output value if so
int getInt(uint8_t nb,outputBuffer *buffer, int *value) {
  buffer->nbs[buffer->position] = nb;
  buffer->position += 1;
  if (buffer->position == buffer->size) {
    int output = 0;
    for (int i = 0; i < buffer->size; i++) {
      output += (buffer->nbs[i] << (i * 4));
    }
    buffer->position = 0;
    *value = output;
    return(1);
  }
  else {
    return(0);
  }
}


// Creates an output buffer of a given nb size
outputBuffer newOutputBuffer(uint8_t size) {
  outputBuffer b;
  b.size = size;
  return b;
}

void writeArray(char* name, uint64_t* data, int size) {
  FILE *array_file;
  array_file = fopen(name,"wb+");
  fwrite(data,8,size,array_file);
  fclose(array_file);
}

char* createOutputFilename(char* filepath, char* suffix) {
  char* path = dirname(strdup(filepath));
  char* file = basename(strdup(filepath));
  file = remove_ext(file,'.',0);
  char* output = strdup(path);
  output = concat(output,"/");
  output = concat(output, file);
  output = concat(output,"-analysis/");
  output = concat(output,file);
  output = concat(output,suffix);
  return(output);
}

// remove_ext: removes the "extension" from a file spec.
//   mystr is the string to process.
//   dot is the extension separator.
//   sep is the path separator (0 means to ignore).
// Returns an allocated string identical to the original but
//   with the extension removed. It must be freed when you're
//   finished with it.
// If you pass in NULL or the new string can't be allocated,
//   it returns NULL.

char* remove_ext (char* mystr, char dot, char sep) {
  char *retstr, *lastdot, *lastsep;

  // Error checks and allocate string.

  if (mystr == NULL)
    return NULL;
  if ((retstr = malloc (strlen (mystr) + 1)) == NULL)
    return NULL;

  // Make a copy and find the relevant characters.
  strcpy (retstr, mystr);
  lastdot = strrchr (retstr, dot);
  lastsep = (sep == 0) ? NULL : strrchr (retstr, sep);

  // If it has an extension separator.
  if (lastdot != NULL) {
    // and it's before the extenstion separator.
    if (lastsep != NULL) {
      if (lastsep < lastdot) {
	// then remove it.
	*lastdot = '\0';
      }
    } else {
      // Has extension separator with no path separator.
      *lastdot = '\0';
    }
  }
  return retstr;
}

char* concat(const char *s1, const char *s2) {
    char *result = malloc(strlen(s1) + strlen(s2) + 1); // +1 for the null-terminator
    // in real code you would check for errors in malloc here
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}
