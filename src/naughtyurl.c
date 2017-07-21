#include <stdio.h>
#include <string.h>
#include "R.h"
#include "Rinternals.h"

#define tag_scheme    0x10
#define tag_user      0x11
#define tag_password  0x12
#define tag_host      0x13
#define tag_port      0x14
#define tag_path      0x15
#define tag_query     0x16
#define tag_fragment  0x17
#define tag_skip      0x18

int scheme_to_nextstate(char* url, int length, int i, unsigned char *state){

  int colon = -1;


  if(i + 1 == length){
    url[i] = tag_skip; // nuke ':' and leave
    return i;
  }

  switch(url[i + 1]){
    case '#':
    case '?':
      url[i] = tag_skip;
      break;
    case '/':
      if(url[i+2] != '/'){
        // http:/bob => path = bob
        url[i+1] = tag_path;
        *state = tag_path;
        return i + 1;
      }
      url[i++] = tag_skip; // overwrite colon with skip tag
      url[i++] = tag_skip; // overwrite first slash with skip tag
    default:
      // is this a user name or a hostname ?
      for(int j = i+1; j < length; j++) {
        switch(url[j]){
          case ':':
            colon = j;
            break;
          case '@':
            url[i] = tag_user;
            *state = tag_user;
            //return (colon > 0 ? colon : j) - 1;
            return i;
          case '?':
          case '/':
          case '#':
            url[i] = tag_host;
            *state = tag_host;
            //return (colon > 0 ? colon : j) - 1;
            return i;
        }

      }
  }

  // Never found a host terminator, so all of it is host
  url[i] = tag_host;
  //return length;
  return i;
}

// url -> naughty urls is an in-place transform
void naughty(char* url){
  int i, length=strlen(url);
  unsigned char state = tag_scheme;

  int last_tag = 0, seen_any = 0;
  unsigned char last_state = tag_scheme;

  for(i = 0; i < length; i++) {
    switch(url[i]) {
      case ':':
        switch(state){
          case tag_scheme:
            i = scheme_to_nextstate(url, length, i, &state);
            break;
          case tag_user:
            url[i] = tag_password;
            state  = tag_password;
            break;
          case tag_host:
            url[i] = tag_port;
            state  = tag_port;
            break;
        }
        break;
      case '/':
        if(state == tag_host || state == tag_port) {
          url[i] = tag_path;
          state  = tag_path;
        }
        break;
      case '@':
        if(state == tag_user || state == tag_password) {
          url[i] = tag_host;
          state  = tag_host;
        }
        break;
      case '?':
        if(state < tag_query) {
          url[i] = tag_query;
          state  = tag_query;
        }
        break;
      case '#':
        if(state < tag_fragment) {
          url[i] = tag_fragment;
          state  = tag_fragment;
        }
        break;
      default:
        seen_any = 1;
        break;
    }

    if(state != last_state) {
      if(seen_any == 0){
        url[last_tag] = tag_skip;
      }

      last_tag = i, seen_any = 0;
      last_state = state;


    }

  }

  if(seen_any == 0){
    url[last_tag] = tag_skip;
  }



}

int suffix(char* buffer, unsigned char state){
  switch(state){
    case tag_scheme:
      *buffer = ':';
      return 1;
    case tag_user:
    case tag_password:
      *buffer = '@';
      return 1;
  }
  return 0;
}


int unnaughty(const char* naughty_url, char* buffer){

  int i = 0, j = 0, state = tag_scheme;

  while(naughty_url[i]) {

    switch(naughty_url[i]) {
      case tag_skip:
        i++;
        continue;
      case tag_user:
        buffer[j++] = ':';
        buffer[j++] = '/';
        buffer[j] = '/';
        state = tag_user;
        break;
      case tag_password:
        buffer[j] = ':';
        state = tag_password;
        break;
      case tag_host:
        if(state == tag_scheme){
          buffer[j++] = ':';
          buffer[j++] = '/';
          buffer[j] = '/';
        }
        else {
          buffer[j] = '@';
        }
        state = tag_host;
        break;
      case tag_port:
        j += suffix(buffer +j, state);
        buffer[j] = ':';
        state = tag_port;
        break;
      case tag_path:
        j += suffix(buffer +j, state);
        buffer[j] = '/';
        state = tag_path;
        break;
      case tag_query:
        j += suffix(buffer +j, state);
        buffer[j] = '?';
        state = tag_query;
        break;
      case tag_fragment:
        j += suffix(buffer +j, state);
        buffer[i] = '#';
        state = tag_fragment;
        break;
      default:
        buffer[j] = naughty_url[i];
    }
    i++;
    j++;
  }
  buffer[j++] = 0;
  return j;
}



SEXP do_naughty(SEXP urls) {

  int n = XLENGTH(urls);

  for(int i = 0; i < n; i++){
    const char * url = CHAR(STRING_ELT(urls, i));
    naughty((char *) url);
  }

  SEXP nclass;
  PROTECT(nclass = mkString("naughty"));
  Rf_setAttrib(urls, R_ClassSymbol, nclass);
  UNPROTECT(1);

  return urls;
}


SEXP do_unnaughty(SEXP naughty_urls) {

  int n = XLENGTH(naughty_urls);
  int unnaughy_length;

  char buffer[1024*1024]; //sometimes data: URLS are big

  for(int i = 0; i < n; i++){
    SEXP el = STRING_ELT(naughty_urls, i);
    const char* naughty_url = CHAR(el);
    int naughty_length = LENGTH(el);
    int unnaughty_length = unnaughty(naughty_url, buffer);
    if(unnaughty_length <= naughty_length) {
        memcpy((char *) naughty_url, buffer, unnaughty_length);
    }
    else {
      SET_STRING_ELT(naughty_urls, i, mkChar(buffer));
    }
  }

  Rf_setAttrib(naughty_urls, R_ClassSymbol, R_NilValue);

  return naughty_urls;
}

SEXP vmatrix(SEXP naughty_urls, SEXP rows, SEXP cols) {

  int N = rows == R_NilValue ? XLENGTH(naughty_urls) : XLENGTH(rows);
  int P = cols == R_NilValue ? 8: XLENGTH(cols);

  SEXP out = PROTECT(allocMatrix(STRSXP, N, P));

  char buffer[1024*1024]; //sometimes data: URLS are big


  for(int i = 0; i < N; i++){
    int record = rows == R_NilValue ? i : INTEGER(rows)[i] - 1;
    int start = 0, end = 0;
    int field;
    const char* naughty_url = CHAR(STRING_ELT(naughty_urls, record));

    if(naughty_url[0] == 0) continue; // blank URL

    for(int j = 0; j < P; j++){
      field = cols == R_NilValue ? tag_scheme + j : INTEGER(cols)[j];
      //find beginning of field
      if(field != tag_scheme){
        while(naughty_url[start] && naughty_url[start] >= tag_skip) start++;
        if(naughty_url[start] == 0) break;
        if(naughty_url[start] != field) continue;
        start = start + 1; //first non-thing character
      }
      if(naughty_url[start] == 0) break; //not found, end of url


      //find end of field
      end = start + 1;
      while(naughty_url[end] > tag_skip) end++;
      int field_length = end - start;

      if(field_length > 0) {

        memcpy(buffer, naughty_url + start, field_length);
        buffer[field_length] = 0;
        SET_STRING_ELT(out, i + j*N, mkChar(buffer));

      }

      if(naughty_url[end] == 0) break; //end of url
      start = end;


    }
  }



  UNPROTECT(1);
  return out;
}




int write_sexp_to_buff(char* buffer, SEXP el){
  const char* text = CHAR(el);
  int len = strlen(text);
  if(len == 0) buffer[-1] = tag_skip;
    else
  memcpy(buffer, text, len);

  return len;
}


SEXP splice(SEXP naughty_urls, SEXP cols, SEXP values) {
  char buffer[1024*1024];
  SEXP naughty_url = STRING_ELT(naughty_urls, 0);
  const char* text = CHAR(naughty_url);
  int naughty_length = strlen(text);

  int P = XLENGTH(cols);
  int i = 0, j = 0, k = 0;

  int* fields = INTEGER(cols);

  while(text[i]){

    switch(text[i]){
      case tag_scheme:
      case tag_user:
      case tag_password:
      case tag_host:
      case tag_port:
      case tag_path:
      case tag_query:
      case tag_fragment:
        if(k < P && text[i] >= fields[k]) {

          buffer[j++] = fields[k];
          j += write_sexp_to_buff(buffer+j, STRING_ELT(values, k));


          // if replacing, walk to the next tag;
          if(text[i] == fields[k]){
            do{i++;} while(text[i] >=  tag_skip);
          }

          k++;
          continue;
        }
        //otherwise text[i] < fields[k] so fall through
      default: //non state text
        buffer[j++] = text[i++];


    }
  }

  //Fill in fields on the end.
  for(;k < P; k++){
    buffer[j++] = fields[k];
    j += write_sexp_to_buff(buffer+j, STRING_ELT(values, k));
  }

  buffer[j++] = 0;

  if(j <= naughty_length){
    memcpy((char*) text, buffer, j);
  }
  else {
    SET_STRING_ELT(naughty_urls, 0, mkChar(buffer));
  }

  return naughty_urls;
}

SEXP vmatrix_set(SEXP naughty_urls, SEXP rows, SEXP cols, SEXP values) {

  int N = rows == R_MissingArg ? XLENGTH(naughty_urls) : XLENGTH(rows);
  int P = XLENGTH(cols);

  int V = XLENGTH(values);

  if(V > N*P) {
    Rf_warning("data length is longer than LHS");
  }else if ((N*P) % V > 0){
    Rf_warning("data length [%d] is not a sub-multiple or multiple of the number of rows [%d]", V, N);

  }

  SEXP sexp = PROTECT(allocVector(STRSXP, 1));
  SEXP vals = PROTECT(allocVector(STRSXP, P));

  for(int i = 0; i< N; i++){
    int record = rows == R_NilValue ? i : INTEGER(rows)[i] - 1;

    SET_STRING_ELT(sexp, 0, STRING_ELT(naughty_urls, record));

    for(int j = 0; j < P; j++){
      SET_STRING_ELT(vals, j, STRING_ELT(values, (record + j*P) % V));
    }

    SET_STRING_ELT(naughty_urls, record, STRING_ELT(splice(sexp, cols, vals), 0));


  }
  UNPROTECT(2);


  return naughty_urls;
}

