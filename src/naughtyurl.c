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
            return (colon > 0 ? colon : j) - 1;
          case '?':
          case '/':
          case '#':
            url[i] = tag_host;
            *state = tag_host;
            return (colon > 0 ? colon : j) - 1;
        }

      }
  }

  // Never found a host terminator, so all of it is host
  url[i] = tag_host;
  return length;

}

// url -> naughty urls is an in-place transform
void naughty(char* url){
  int i, length=strlen(url);
  unsigned char state = tag_scheme;

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
          state  = 6;
        }
        break;
      case '#':
        if(state < tag_fragment) {
          url[i] = tag_fragment;
          state = 7;
        }
        break;
      default:
        break;
    }
  }



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
        buffer[j] = ':';
        state = tag_port;
        break;
      case tag_path:
        buffer[j] = '/';
        state = tag_path;
        break;
      case tag_query:
        buffer[j] = '?';
        state = tag_query;
        break;
      case tag_fragment:
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


  return urls;
}


SEXP do_unnaughty(SEXP naughty_urls) {

  int n = XLENGTH(naughty_urls);
  int unnaughy_length;

  char buffer[1024*1024]; //sometimes data: URLS are big

  for(int i = 0; i < n; i++){
    const char* naughty_url = CHAR(STRING_ELT(naughty_urls, i));
    int unnaughty_length = unnaughty(naughty_url, buffer);
    memcpy((char *) naughty_url, buffer, unnaughty_length);
  }

  return naughty_urls;
}


