module TCPSocketConnection where
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

foreign import primitive "const.sizeof(struct myTCPSocketConnection)" myTCPSocketConnectionT_size :: Int
newtype {-# CTYPE "struct myTCPSocketConnection" #-} TCPSocketConnectionT = TCPSocketConnectionT ()

foreign import ccall "c_extern.h tcp_socket_connection_connect" c_tcp_socket_connection_connect :: Ptr TCPSocketConnectionT -> CString -> Int -> IO Int
foreign import ccall "c_extern.h tcp_socket_connection_send_all" c_tcp_socket_connection_send_all :: Ptr TCPSocketConnectionT -> CString -> Int -> IO Int
foreign import ccall "c_extern.h tcp_socket_connection_receive" c_tcp_socket_connection_receive :: Ptr TCPSocketConnectionT -> CString -> Int -> IO Int
foreign import ccall "c_extern.h tcp_socket_connection_close" c_tcp_socket_connection_close :: Ptr TCPSocketConnectionT -> IO Int

tcpSocketConnection_connect :: String -> Int -> IO (Ptr TCPSocketConnectionT)
tcpSocketConnection_connect host port = do
  tag <- mallocBytes myTCPSocketConnectionT_size
  withCString host (\cs -> c_tcp_socket_connection_connect tag cs port)
  return tag

tcpSocketConnection_send_all :: Ptr TCPSocketConnectionT -> String -> IO Int
tcpSocketConnection_send_all tag dat = do
  let len = length dat
  withCString dat (\cs -> c_tcp_socket_connection_send_all tag cs len)

tcpSocketConnection_receive :: Ptr TCPSocketConnectionT -> IO String
tcpSocketConnection_receive tag =
  let n = 100
  in allocaArray0 n $ \cs -> do
    r <- c_tcp_socket_connection_receive tag cs n
    if r <= 0 then return ""
      else peekCStringLen (cs, r)

tcpSocketConnection_close :: Ptr TCPSocketConnectionT -> IO Int
tcpSocketConnection_close tag = do
  r <- c_tcp_socket_connection_close tag
  free tag
  return r
