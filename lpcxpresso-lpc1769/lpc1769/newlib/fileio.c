#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "board.h"
#include "uarts.h"
#include "usbwrapper.h"

#include "fat_sd/ff.h"

#define f_tell(fp) ((fp)->fptr)
#define f_size(fp) ((fp)->fsize)

const unsigned int UART_FD[10] = {
  [2] = IO_DEBUG_TX
  
#ifdef IO_WATCHDOG_TX
  ,[3] = IO_WATCHDOG_TX
#endif
  
#ifdef IO_CHILLER_TX
  ,[4] = IO_CHILLER_TX
#endif
};

const char SD[] = "/sd/";

char fsMounted;
FATFS fatFsInstance;

FATFS *getFat() {
  if (!fsMounted) {
    FRESULT mr = f_mount(0, &fatFsInstance);
    if (mr) {
      fiprintf(stderr, "Failed to initialize FAT layer (f_mount() said: %d\n\r", mr);
      return 0;
    }
    fsMounted = 1;
  }
  return &fatFsInstance;
}

typedef struct {
  FIL fil;
  char append;
} FatFile;

#define MAX_FAT_FILES 4
FatFile fatFiles[MAX_FAT_FILES]; // fd = 10 + index
char fatFilesOpen = 0;

int _open(const char *name, int flags, int mode) {

  if (!strcmp(name, "/dev/stdin")) {
    return 0;

  } else if (!strcmp(name, "/dev/stdout")) {
    return 1;
    
  } else if (!strcmp(name, "/dev/stderr")) {
    return 2;

  } else if (!strcmp(name, "/dev/watchdog")) {
    return 3;

  } else if (!strcmp(name, "/dev/chiller")) {
    return 4;

  } else if (!strncmp(name, SD, sizeof(SD)-1)) {

    if (fatFilesOpen >= MAX_FAT_FILES) {
      errno = EMFILE;
      return -3;
    }
    
    if (!getFat()) {
      errno = ENODEV;
      return -4;
    }

    const char *ffn = name+sizeof(SD)-2; // chop off "/sd", but leave the leading /

    char fflags = 0;
    if (flags & O_RDONLY) {
      fflags |= FA_READ;

    } else if (flags & O_WRONLY) {
      fflags |= FA_WRITE;

    } else /* if (flags & O_RDWR) */ {
      fflags |= FA_READ | FA_WRITE;      
    }

    if (flags & O_CREAT) {
      fflags |= FA_OPEN_ALWAYS;

    } else if (flags & O_TRUNC) {
      fflags |= FA_CREATE_ALWAYS;    

    } else {
      fflags |= FA_OPEN_EXISTING;          
    }

    int fd = fatFilesOpen;
    FRESULT or = f_open(&fatFiles[fd].fil, ffn, fflags);
    
    if (!or) {      
      fatFilesOpen++;
      fatFiles[fd].append = flags & O_APPEND;      
      errno = 0;
      return fd+10;
    }

    if (or == FR_EXIST) {
      errno = EEXIST;

    } else if (or == FR_TOO_MANY_OPEN_FILES) {
      errno = ENFILE;

    } else if (or == FR_NO_FILE || or == FR_NO_PATH || or == FR_INVALID_NAME) {
      errno = ENOENT;

    } else {
      fiprintf(stderr, "Failed to open FAT file %s: %d\n\r", ffn, or);
      errno = EIO;
    }

    return -5;
  }

  return -1;
}

int _close(int file) {
  if (file < 10) {
    return -1; // We don't support closing a device.

  } else {
    int fd = file-10;

    if (fd < fatFilesOpen) {
      FRESULT cr = f_close(&fatFiles[fd].fil);
      if (cr) {
	fiprintf(stderr, "Failed to close FAT file %d: %d\n\r", fd, cr);
	errno = EIO;
	return -2;

      } else {
	for (int i=fd;i<fatFilesOpen-1;i++) {
	  fatFiles[i] = fatFiles[i+1];
	}
	fatFilesOpen--;
	errno = 0;
	return 0;
      }
    }
    
    return -1;
  }
}

int _fstat(int file, struct stat *st) {
  if (file < 10) {
    st->st_mode = S_IFCHR;
    errno = 0;
    return 0; 

  } else {

    int fd = file-10;
    if (fd < fatFilesOpen) {
      st->st_size = f_size(&fatFiles[fd].fil);
      errno = 0;
      return 0;
    }
    
    errno = EINVAL;
    return -1;
  }
}

int _isatty(int file) {
  return file <= 2 ? 1 : 0; // stderr, stdout, stdin are all terminals, of sorts.
}

int _lseek(int file, int ptr, int dir) {
  int fd = file-10;
  if (fd < fatFilesOpen) {
    
    int cur = f_tell(&fatFiles[fd].fil);

    if (dir == SEEK_CUR) {
      ptr -= cur;

    } else if (dir == SEEK_END) {
      ptr = f_size(&fatFiles[fd].fil) - ptr;

    } /* else if (dir == SEEK_SET) {
      // ptr is ready to go.
    }
    */

    if (ptr == cur) {
      errno = 0;
      return ptr;
    }

    FRESULT sr = f_lseek(&fatFiles[fd].fil, ptr);
    if (sr) {
      fiprintf(stderr, "Failed to seek FAT file %d: %d\n\r", fd, sr);
      errno = EINVAL;
      return -1;
    } else {
      errno = 0;
      return ptr;
    }   
  }

  errno = EINVAL;
  return -1;
}

int _read(int file, char *ptr, int len) {
  if (len == 0) return 0;

  if (file == 0 || file == 1) {
    // Notice: We don't read from USB with stdio, implement the usbLine callback in stead

  } else if (file < 10) {
    // Notice: We don't read from UARTS with stdio, implement the handleUart?Line callbacks in stead

  } else {
    int fd = file-10;

    if (fd < fatFilesOpen) {
      unsigned int res;
      FRESULT rr = f_read(&fatFiles[fd].fil, 
			  ptr, len, &res);
      if (rr) {
	fiprintf(stderr, "Failed to read FAT file %d: %d\n\r", fd, rr);
	errno = EINVAL;
	return -1;
      } else {
	errno = 0;
	return res;
      }   
    }
  }

  errno = EINVAL;
  return -1;
}

int _write(int file, char *ptr, int len) {
  if (len == 0) return 0;

  if (file == 0 || file == 1) {
    usbSend(ptr, len);
    return len;

  } else if (file < 10) {
    unsigned int port = UART_FD[file];
    if (port) {
      int totalSent = 0;
      while (len > 0) {
	int sent = sendUART(port, ptr, len);
	len -= sent;
	ptr += sent;
	totalSent += sent;
      }
      return totalSent;
    } else {
      return -1;
    }

  } else {
    int fd = file-10;

    if (fd < fatFilesOpen) {
      unsigned int res;
      if (fatFiles[fd].append) {
	_lseek(file, 0, SEEK_END);
      }
      FRESULT rr = f_write(&fatFiles[fd].fil, ptr, len, &res);
      if (rr) {
	fiprintf(stderr, "Failed to write FAT file %d (bytes: %d): %d\n\r", fd, len, rr);
	errno = EINVAL;
	return -1;
      } else {
	errno = 0;
	return res;
      }   
    }
  }  
  errno = EINVAL;
  return -1;
}
