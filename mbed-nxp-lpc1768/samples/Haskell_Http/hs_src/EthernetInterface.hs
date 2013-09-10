module EthernetInterface where
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "c_extern.h ethernet_interface_init_dhcp" c_ethernet_interface_init_dhcp :: IO Int
foreign import ccall "c_extern.h ethernet_interface_connect" c_ethernet_interface_connect :: CUInt -> IO Int
foreign import ccall "c_extern.h ethernet_interface_disconnect" c_ethernet_interface_disconnect :: IO Int
foreign import ccall "c_extern.h ethernet_interface_getIPAddress" c_ethernet_interface_getIPAddress :: IO CString

ethernetInitDhcp :: IO Int
ethernetInitDhcp = c_ethernet_interface_init_dhcp

ethernetConnect :: CUInt -> IO Int
ethernetConnect = c_ethernet_interface_connect

ethernetDisconnect :: IO Int
ethernetDisconnect = c_ethernet_interface_disconnect

ethernetGetIpAddress :: IO String
ethernetGetIpAddress = c_ethernet_interface_getIPAddress >>= peekCString
