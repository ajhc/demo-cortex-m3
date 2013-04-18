#ifndef __HEXPARSER_H__
#define __HEXPARSER_H__

/**
 Returns the number of parsed digits
 or 0 if the first char was a whitespace char
 or -1 if a non-whitespace non-hex digit was encountered in the first 8 chars
 modifies the char pointer to point to the next non-whitespace char after the parsed int.
*/
inline int parseHex(char **str, unsigned int *output) {

  int digit = 0;
  unsigned int r = 0;
  for (digit = 0; digit<8; digit++) {
    unsigned char ch = **str;
    if (ch >= '0' && ch <= '9') {
      r <<= 4;
      r |= ch - '0';

    } else if (ch >= 'a' && ch <= 'f') {
      r <<= 4;
      r |= ch - ('a' - 10);

    } else if (ch >= 'A' && ch <= 'F') {
      r <<= 4;
      r |= ch - ('A' - 10);

    } else if (ch == ' ' || ch == 0) {      
      break; // Normal for ints shorter than 32 bit

    } else {
      return -1; // invalid char found
    }

    (*str)++;
  }

  *output = r;

  while (**str && **str == ' ') {
    (*str)++; // Skip over whitespace.
  }

  return digit;
}

#endif
