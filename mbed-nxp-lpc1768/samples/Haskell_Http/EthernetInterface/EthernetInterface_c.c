/* EthernetInterface_c.c */
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
#include <string.h>
#include "EthernetInterface_c.h"

#include "lwip/inet.h"
#include "lwip/netif.h"
#include "netif/etharp.h"
#include "lwip/dhcp.h"
#include "arch/lpc17_emac.h"
#include "lpc_phy.h"
#include "lwip/tcpip.h"

// #include "mbed.h"

/* TCP/IP and Network Interface Initialisation */
static struct netif lpcNetif;

struct mySemaphore {
	osSemaphoreId _osSemaphoreId;
	osSemaphoreDef_t _osSemaphoreDef;
#ifdef CMSIS_OS_RTX
	uint32_t _semaphore_data[2];
#endif
};

static struct mySemaphore tcpip_inited;
static struct mySemaphore netif_inited;

static char ip_addr[16];
static int connected;
static int use_dhcp = 0;

// Semaphore
static void init_mySemaphore(struct mySemaphore *sem, int32_t count) {
#ifdef CMSIS_OS_RTX
	memset(sem->_semaphore_data, 0, sizeof(sem->_semaphore_data));
	sem->_osSemaphoreDef.semaphore = sem->_semaphore_data;
#endif
	sem->_osSemaphoreId = osSemaphoreCreate(&sem->_osSemaphoreDef, count);
}
int32_t wait_mySemaphore(struct mySemaphore *sem, uint32_t millisec) {
	return osSemaphoreWait(sem->_osSemaphoreId, millisec);
}

osStatus release_mySemaphore(struct mySemaphore *sem) {
	return osSemaphoreRelease(sem->_osSemaphoreId);
}

// Callbacks
static void tcpip_init_done(void *arg) {
	release_mySemaphore(&tcpip_inited);
}
static void netif_status_callback(struct netif *netif) {
	strcpy(ip_addr, inet_ntoa(netif->ip_addr));
	connected = netif_is_up(netif) ? 1 : 0;
	release_mySemaphore(&netif_inited);
}

static void init_netif(ip_addr_t *ipaddr, ip_addr_t *netmask, ip_addr_t *gw) {
	init_mySemaphore(&tcpip_inited, 0);
	init_mySemaphore(&netif_inited, 0);

	tcpip_init(tcpip_init_done, NULL);
	wait_mySemaphore(&tcpip_inited, osWaitForever);
    
	memset((void*) &lpcNetif, 0, sizeof(lpcNetif));
	netif_add(&lpcNetif, ipaddr, netmask, gw, NULL, lpc_enetif_init, tcpip_input);
	netif_set_default(&lpcNetif);
	netif_set_status_callback(&lpcNetif, netif_status_callback);
}

int ethernet_interface_init_dhcp() {
	use_dhcp = 1;
	init_netif(NULL, NULL, NULL);
	return 0;
}

int ethernet_interface_init_staticip(const char* ip, const char* mask, const char* gateway) {
	use_dhcp = 0;
	ip_addr_t ip_n, mask_n, gateway_n;
	inet_aton(ip, &ip_n);
	inet_aton(mask, &mask_n);
	inet_aton(gateway, &gateway_n);
	init_netif(&ip_n, &mask_n, &gateway_n);
	return 0;
}

int ethernet_interface_connect(unsigned int timeout_ms) {
	NVIC_SetPriority(ENET_IRQn, ((0x01 << 3) | 0x01));
	NVIC_EnableIRQ(ENET_IRQn);

	if (use_dhcp) {
		dhcp_start(&lpcNetif);
	} else {
		netif_set_up(&lpcNetif);
	}

	// -1: error, 0: timeout
	int inited = wait_mySemaphore(&netif_inited, timeout_ms);
	return (inited > 0) ? (0) : (-1);
}

int ethernet_interface_disconnect() {
	if (use_dhcp) {
		dhcp_release(&lpcNetif);
		dhcp_stop(&lpcNetif);
	} else {
		netif_set_down(&lpcNetif);
	}

	NVIC_DisableIRQ(ENET_IRQn);

	return 0;
}

char* ethernet_interface_getIPAddress() {
	return (connected) ? (ip_addr) : (NULL);
}
