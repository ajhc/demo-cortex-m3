#include <stdio.h>
#include <string.h>

#include "usbwrapper.h"
#include "ringbuffer.h"

#include "lpcusb/usbapi.h"
#include "lpcusb/usbdebug.h"
#include "usbcfg.h"


// This buffer only needs to be large enough to allow a few packets to be assembled
#define USB_TX_BUFFER_ORDER 9

RING_BUFFER(usbTxBuffer, USB_TX_BUFFER_ORDER, char) IN_IRAM1;

char usbLineBuffer[USB_LINE_BUFFER_SIZE] IN_IRAM1;
int usbLineLength;

// Callback called when a full line has been buffered by the USB CDC layer.
void WEAK usbLine(char *line, unsigned int lineSize) {
  fprintf(stderr, "Ignoring line from USB: %s\n\r", line);
}


// data structure for GET_LINE_CODING / SET_LINE_CODING class requests
typedef struct {
    unsigned int  baudRate;
    unsigned char charFormat;
    unsigned char parityType;
    unsigned char dataBits;
} TLineCoding;
static TLineCoding lineCoding = {115200, 0, 0, 8};

static unsigned char bulkBuffer[64];
static unsigned char classRequestData[8];

static void USBFrameHandler(U16 wFrame) {
  // Data available, enable NAK interrupt on bulk in
  if (!rbIsEmpty(&usbTxBuffer)) {
    USBHwNakIntEnable(INACK_BI);
  }
}

/**
    Interrupt handler
    Simply calls the USB ISR
 */
void USB_IRQHandler(void) {
  USBHwISR();
}


/**
    Local function to handle the USB-CDC class requests

    @param [in] pSetup
    @param [out] piLen
    @param [out] ppbData
 */
static BOOL HandleClassRequest(TSetupPacket *setupPacket, int *answerLength, unsigned char **answer) {
  switch (setupPacket->bRequest) {
    // set line coding
    case SET_LINE_CODING:
      memcpy((unsigned char *)&lineCoding, *answer, 7);
      *answerLength = 7;
      break;

    // get line coding
    case GET_LINE_CODING:
      *answer = (unsigned char *)&lineCoding;
      *answerLength = 7;
      break;

    // set control line state
    case SET_CONTROL_LINE_STATE:
      // bit0 = DTR, bit = RTS
      break;

    default:
      return FALSE;
  }
  return TRUE;
}

void usbSend(const char *data, unsigned int dataSize) {
  
  while (dataSize--) {    
    while (rbIsFull(&usbTxBuffer)) ; // Wait for the buffer to empty enough
    rbLock(&usbTxBuffer);
    RB_WRITE(usbTxBuffer, *(data++));
    rbUnlock(&usbTxBuffer);
  }  
  
  // Only send all the full packets we can.
  while (rbLength(&usbTxBuffer) > 63) ;
}

void usbSendFlush(const char *data, unsigned int dataSize) {

  while (dataSize--) {
    while (rbIsFull(&usbTxBuffer)) ; // Wait for the buffer to empty
    rbLock(&usbTxBuffer);
    RB_WRITE(usbTxBuffer, *(data++));
    rbUnlock(&usbTxBuffer);
  }  

  usbFlush();
}

void usbFlush() {
  while (!rbIsEmpty(&usbTxBuffer)) ;    
}


// Internal functions called by the USB CDC layer

// Read up to dataSize chars from the transmit buffer
static inline int usbPopForTransmit(unsigned char *data, int dataSize) {
  int res = 0;
  while (dataSize && !rbIsEmpty(&usbTxBuffer)) {
    dataSize--;
    res++;
    rbLock(&usbTxBuffer);
    *data = RB_READ(usbTxBuffer);
    rbUnlock(&usbTxBuffer);
    data++;
  }
  
  return res;
}

// Write data from the USB buffer to the receive buffer
inline void usbPushReceived(unsigned char *data, int dataSize) {
  while (dataSize--) {
    unsigned char ch = *(data++);

    if (ch == '\r' || ch == '\n' || usbLineLength == USB_LINE_BUFFER_SIZE) {
      usbLineBuffer[usbLineLength] = 0;
      usbLine(usbLineBuffer, usbLineLength);
      usbLineLength=0;

    } else {
      usbLineBuffer[usbLineLength++] = ch;
    }
  }
}


/**
    Local function to handle incoming bulk data

    @param [in] endPoint
    @param [in] endPointStatus
 */
static void bulkOut(unsigned char endPoint, unsigned char endPointStatus) {
  int bytesReady = USBHwEPRead(endPoint, bulkBuffer, sizeof(bulkBuffer));
  usbPushReceived(bulkBuffer, bytesReady);
}


/**
    Local function to handle outgoing bulk data

    @param [in] endPoint
    @param [in] endPointStatus
 */
static void bulkIn(unsigned char endPoint, unsigned char endPointStatus)
{
  if (rbIsEmpty(&usbTxBuffer)) {
    // no more data, disable further NAK interrupts until next USB frame
    USBHwNakIntEnable(0);
    return;
  }

  int bytesReady = usbPopForTransmit(bulkBuffer, MAX_PACKET_SIZE);
  if (bytesReady > 0) {
    USBHwEPWrite(endPoint, bulkBuffer, bytesReady);
  }
}

void usbWrapperInit() {
  usbLineLength = 0;
  rbInit(&usbTxBuffer, USB_TX_BUFFER_ORDER);

  // initialize stack
  USBInit();

  USBRegisterDescriptors(USB_DESCRIPTORS);

  USBRegisterRequestHandler(REQTYPE_TYPE_CLASS, HandleClassRequest, classRequestData);

  // register endpoint handlers
  USBHwRegisterEPIntHandler(INT_IN_EP, NULL);
  USBHwRegisterEPIntHandler(BULK_IN_EP, bulkIn);
  USBHwRegisterEPIntHandler(BULK_OUT_EP, bulkOut);

  USBHwRegisterFrameHandler(USBFrameHandler);

  // enable bulk-in interrupts on NAKs
  USBHwNakIntEnable(INACK_BI);
  NVIC_EnableIRQ(USB_IRQn);

  // connect to bus
  USBHwConnect(TRUE);
}


