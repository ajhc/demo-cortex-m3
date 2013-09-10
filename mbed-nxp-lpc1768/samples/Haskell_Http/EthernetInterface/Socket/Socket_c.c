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
#include "Socket/Socket_c.h"
#include <string.h>

void socket_set_blocking(struct mySocket *sock, int blocking, struct timeval *timeout) {
    sock->_blocking = blocking;
    sock->_timeout = *timeout;
}

int socket_init_socket(struct mySocket *sock, int type) {
	sock->_sock_fd = -1;
	sock->_blocking = 1;
	sock->_timeout.tv_sec = 1;
	sock->_timeout.tv_usec = 500;

	if (sock->_sock_fd != -1)
		return -1;
    
	int fd = lwip_socket(AF_INET, type, 0);
	if (fd < 0)
		return -1;
    
	sock->_sock_fd = fd;
	return 0;
}

int socket_select(struct mySocket *sock, struct timeval *timeout, int read, int write) {
    fd_set fdSet;
    FD_ZERO(&fdSet);
    FD_SET(sock->_sock_fd, &fdSet);
    
    fd_set* readset  = (read ) ? (&fdSet) : (NULL);
    fd_set* writeset = (write) ? (&fdSet) : (NULL);
    
    int ret = lwip_select(FD_SETSIZE, readset, writeset, NULL, timeout);
    return (ret <= 0 || !FD_ISSET(sock->_sock_fd, &fdSet)) ? (-1) : (0);
}

int socket_wait_readable(struct mySocket *sock, struct timeval *timeout) {
	return socket_select(sock, timeout, 1, 0);
}

int socket_wait_writable(struct mySocket *sock, struct timeval *timeout) {
	return socket_select(sock, timeout, 0, 1);
}

int socket_close(struct mySocket *sock) {
	if (sock->_sock_fd < 0)
		return -1;
    
	lwip_close(sock->_sock_fd);
	sock->_sock_fd = -1;

	return 0;
}
