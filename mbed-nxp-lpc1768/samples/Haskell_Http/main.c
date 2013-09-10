/* Copyright 2012 Adam Green (http://mbed.org/users/AdamGreen/)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
/* Blink LED without using the mbed library. */
#include "LPC17xx.h"
#include "jhc_rts_header.h"
#include "c_extern.h"

int main() 
{
#if 0
	ethernet_interface_init_dhcp();
	ethernet_interface_connect(12000);

	struct myTCPSocketConnection sock;
	tcp_socket_connection_connect(&sock, "mbed.org", 80);
	char http_cmd[] = "GET /media/uploads/mbed_official/hello.txt HTTP/1.0\n\n";
	tcp_socket_connection_send_all(&sock, http_cmd, sizeof(http_cmd)-1);

	char buffer[300];
	int ret;
	while (true) {
		ret = tcp_socket_connection_receive(&sock, buffer, sizeof(buffer)-1);
		if (ret <= 0)
			break;
		buffer[ret] = '\0';
	}
#endif

	/* Call Haskell code */
	int hsargc = 1;
	char *hsargv = "q";
	char **hsargvp = &hsargv;

	hs_init(&hsargc, &hsargvp);
	_amain();
	hs_exit();

	for (;;) {}
	/* NOTREACHED */

	return 0;
}
