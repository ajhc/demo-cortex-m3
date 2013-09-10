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
#include "Socket/Endpoint_c.h"
#include <string.h>

void endpoint_reset_address(struct myEndpoint *ep) {
	memset(&ep->_remoteHost, 0, sizeof(struct sockaddr_in));
	ep->_ipAddress[0] = '\0';
}

void endpoint_init(struct myEndpoint *ep) {
	endpoint_reset_address(ep);
}

int endpoint_set_address(struct myEndpoint *ep, const char* host, const int port) {
	//Resolve DNS address or populate hard-coded IP address
	struct hostent *server = gethostbyname(host);
	if (server == NULL)
		return -1; //Could not resolve address
    
	endpoint_reset_address(ep);
    
	// Set IP address
	memcpy((char*) &ep->_remoteHost.sin_addr.s_addr,
	    (char*) server->h_addr_list[0], server->h_length);
	ep->_remoteHost.sin_family = AF_INET;
    
	// Set port
	ep->_remoteHost.sin_port = htons(port);
    
	return 0;
}

char* endpoint_get_address(struct myEndpoint *ep) {
	if ((ep->_ipAddress[0] == '\0') && (ep->_remoteHost.sin_addr.s_addr != 0))
		inet_ntoa_r(ep->_remoteHost.sin_addr, ep->_ipAddress, sizeof(ep->_ipAddress));
	return ep->_ipAddress;
}

int endpoint_get_port(struct myEndpoint *ep) {
	return ntohs(ep->_remoteHost.sin_port);
}
