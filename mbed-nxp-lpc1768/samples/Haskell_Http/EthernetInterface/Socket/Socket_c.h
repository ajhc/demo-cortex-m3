/* Copyright (C) 2012 mbed.org, MIT License
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
#ifndef SOCKET_C_H_
#define SOCKET_C_H_

#include "lwip/sockets.h"
#include "lwip/netdb.h"

struct mySocket {
	int _sock_fd;
	int _blocking;
	struct timeval _timeout;
};

int socket_init_socket(struct mySocket *sock, int type);
int socket_wait_readable(struct mySocket *sock, struct timeval *timeout);
int socket_wait_writable(struct mySocket *sock, struct timeval *timeout);
int socket_close(struct mySocket *sock);

//DNS
inline struct hostent *gethostbyname(const char *name) {
  return lwip_gethostbyname(name);
}

inline int gethostbyname_r(const char *name, struct hostent *ret, char *buf, size_t buflen, struct hostent **result, int *h_errnop) {
  return lwip_gethostbyname_r(name, ret, buf, buflen, result, h_errnop);
}

#endif /* SOCKET_C_H_ */
