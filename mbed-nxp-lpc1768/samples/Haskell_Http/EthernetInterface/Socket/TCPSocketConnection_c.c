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
#include "TCPSocketConnection_c.h"
#include <string.h>

int tcp_socket_connection_connect(struct myTCPSocketConnection *tcp, const char* host, const int port) {
	tcp->_is_connected = 0;

	if (socket_init_socket(&tcp->sock, SOCK_STREAM) < 0)
		return -1;
    
	if (endpoint_set_address(&tcp->ep, host, port) != 0)
		return -1;
    
	if (lwip_connect(tcp->sock._sock_fd, (const struct sockaddr *) &tcp->ep._remoteHost,
	      sizeof(tcp->ep._remoteHost)) < 0) {
		socket_close(&tcp->sock);
		return -1;
	}
	tcp->_is_connected = 1;
    
	return 0;
}

int tcp_socket_connection_is_connected(struct myTCPSocketConnection *tcp) {
	return tcp->_is_connected;
}

int tcp_socket_connection_send(struct myTCPSocketConnection *tcp, char* data, int length) {
	if ((tcp->sock._sock_fd < 0) || !tcp->_is_connected)
		return -1;
    
	if (!tcp->sock._blocking) {
		if (socket_wait_writable(&tcp->sock, &tcp->sock._timeout) != 0)
			return -1;
	}
    
	int n = lwip_send(tcp->sock._sock_fd, data, length, 0);
	tcp->_is_connected = (n != 0);
    
	return n;
}

// -1 if unsuccessful, else number of bytes written
int tcp_socket_connection_send_all(struct myTCPSocketConnection *tcp, char* data, int length) {
	if ((tcp->sock._sock_fd < 0) || !tcp->_is_connected)
		return -1;
    
	size_t writtenLen = 0;
	while (writtenLen < length) {
		if (!tcp->sock._blocking) {
			// Wait for socket to be writeable
			if (socket_wait_writable(&tcp->sock, &tcp->sock._timeout) != 0)
				return writtenLen;
		}
        
		int ret = lwip_send(tcp->sock._sock_fd, data + writtenLen, length - writtenLen, 0);
		if (ret > 0) {
			writtenLen += ret;
			continue;
		} else if (ret == 0) {
			tcp->_is_connected = 0;
			return writtenLen;
		} else {
			return -1; //Connnection error
		}
	}
	return writtenLen;
}

int tcp_socket_connection_receive(struct myTCPSocketConnection *tcp, char* data, int length) {
	if ((tcp->sock._sock_fd < 0) || !tcp->_is_connected)
		return -1;
    
	if (!tcp->sock._blocking) {
		if (socket_wait_readable(&tcp->sock, &tcp->sock._timeout) != 0)
			return -1;
	}
	
	int n = lwip_recv(tcp->sock._sock_fd, data, length, 0);
	tcp->_is_connected = (n != 0);
    
	return n;
}

// -1 if unsuccessful, else number of bytes received
int tcp_socket_connection_receive_all(struct myTCPSocketConnection *tcp, char* data, int length) {
	if ((tcp->sock._sock_fd < 0) || !tcp->_is_connected)
		return -1;
    
	size_t readLen = 0;
	while (readLen < length) {
		if (!tcp->sock._blocking) {
			//Wait for socket to be readable
			if (socket_wait_readable(&tcp->sock, &tcp->sock._timeout) != 0)
				return readLen;
		}
        
		int ret = lwip_recv(tcp->sock._sock_fd, data + readLen, length - readLen, 0);
		if (ret > 0) {
			readLen += ret;
		} else if (ret == 0) {
			tcp->_is_connected = 0;
			return readLen;
		} else {
			return -1; //Connnection error
		}
	}
	return readLen;
}

int tcp_socket_connection_close(struct myTCPSocketConnection *tcp) {
	return socket_close(&tcp->sock);
}
