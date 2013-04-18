#ifndef __USBCFG_H__
#define __USBCFG_H__

#define INT_IN_EP       0x81
#define BULK_OUT_EP     0x05
#define BULK_IN_EP      0x82

#define MAX_PACKET_SIZE 64

#define LE_WORD(x)      ((x)&0xFF),((x)>>8)

// CDC definitions
#define CS_INTERFACE            0x24
#define CS_ENDPOINT             0x25

#define SET_LINE_CODING         0x20
#define GET_LINE_CODING         0x21
#define SET_CONTROL_LINE_STATE  0x22

static const unsigned char USB_DESCRIPTORS[] = {

// device descriptor
  0x12,
  DESC_DEVICE,
  LE_WORD(0x0101),            // bcdUSB
  0x02,                       // bDeviceClass
  0x00,                       // bDeviceSubClass
  0x00,                       // bDeviceProtocol
  MAX_PACKET_SIZE0,           // bMaxPacketSize
  LE_WORD(0x0000),            // idVendor
  LE_WORD(0x05aa),            // idProduct
  LE_WORD(0x0100),            // bcdDevice
  0x01,                       // iManufacturer
  0x02,                       // iProduct
  0x03,                       // iSerialNumber
  0x01,                       // bNumConfigurations

// configuration descriptor
  0x09,
  DESC_CONFIGURATION,
  LE_WORD(147),               // wTotalLength
  0x02,                       // bNumInterfaces
  0x01,                       // bConfigurationValue
  0x00,                       // iConfiguration
  0xC0,                       // bmAttributes
  0x32,                       // bMaxPower
// control class interface
  0x09,
  DESC_INTERFACE,
  0x00,                       // bInterfaceNumber
  0x00,                       // bAlternateSetting
  0x01,                       // bNumEndPoints
  0x02,                       // bInterfaceClass
  0x02,                       // bInterfaceSubClass
  0x01,                       // bInterfaceProtocol, linux requires value of 1 for the cdc_acm module
  0x00,                       // iInterface
// header functional descriptor
  0x05,
  CS_INTERFACE,
  0x00,
  LE_WORD(0x0110),
// call management functional descriptor
  0x05,
  CS_INTERFACE,
  0x01,
  0x01,                       // bmCapabilities = device handles call management
  0x01,                       // bDataInterface
// ACM functional descriptor
  0x04,
  CS_INTERFACE,
  0x02,
  0x02,                       // bmCapabilities
// union functional descriptor
  0x05,
  CS_INTERFACE,
  0x06,
  0x00,                       // bMasterInterface
  0x01,                       // bSlaveInterface0
// notification EP
  0x07,
  DESC_ENDPOINT,
  INT_IN_EP,                  // bEndpointAddress
  0x03,                       // bmAttributes = intr
  LE_WORD(8),                 // wMaxPacketSize
  0x0A,                       // bInterval
// data class interface descriptor
  0x09,
  DESC_INTERFACE,
  0x01,                       // bInterfaceNumber
  0x00,                       // bAlternateSetting
  0x02,                       // bNumEndPoints
  0x0A,                       // bInterfaceClass = data
  0x00,                       // bInterfaceSubClass
  0x00,                       // bInterfaceProtocol
  0x00,                       // iInterface
// data EP OUT
  0x07,
  DESC_ENDPOINT,
  BULK_OUT_EP,                // bEndpointAddress
  0x02,                       // bmAttributes = bulk
  LE_WORD(MAX_PACKET_SIZE),   // wMaxPacketSize
  0x00,                       // bInterval
// data EP in
  0x07,
  DESC_ENDPOINT,
  BULK_IN_EP,                 // bEndpointAddress
  0x02,                       // bmAttributes = bulk
  LE_WORD(MAX_PACKET_SIZE),   // wMaxPacketSize
  0x00,                       // bInterval

// string descriptors
  0x04, DESC_STRING, LE_WORD(0x0409), // Language for strings: English - US

  36, DESC_STRING, 'O', 0, 'p', 0, 'e', 0, 'n', 0, ' ', 0, 'S', 0, 'p', 0, 'a', 0, 'c', 0, 'e', 0, ' ', 0, 'A', 0, 'a', 0, 'r', 0, 'h', 0, 'u', 0, 's', 0, 
  20, DESC_STRING, 'P', 0, 'h', 0, 'o', 0, 't', 0, 'o', 0, 'n', 0, 'S', 0, 'a', 0, 'w', 0, 
  40, DESC_STRING, 'h', 0, 't', 0, 't', 0, 'p', 0, ':', 0, '/', 0, '/', 0, 'p', 0, 's', 0, 'a', 0, 'w', 0, '.', 0, 'o', 0, 's', 0, 'a', 0, 'a', 0, '.', 0, 'd', 0, 'k', 0, 
  30, DESC_STRING, 'S', 0, 'e', 0, 'r', 0, 'i', 0, 'a', 0, 'l', 0, ' ', 0, 'c', 0, 'o', 0, 'n', 0, 't', 0, 'r', 0, 'o', 0, 'l', 0, 

// terminating zero
  0
};

#endif
