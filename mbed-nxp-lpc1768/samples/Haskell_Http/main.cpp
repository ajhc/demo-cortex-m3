#include "mbed.h"
#include "TextLCD.h"
#include "EthernetInterface.h"

TextLCD lcd(p24, p26, p27, p28, p29, p30); // rs, e, d0-d3

int main() {
    lcd.printf("Start!\n");

    EthernetInterface eth;
    eth.init(); //Use DHCP
    eth.connect();
    lcd.printf("IP:%s\n", eth.getIPAddress());
    
    TCPSocketConnection sock;
    sock.connect("mbed.org", 80);
    
    char http_cmd[] = "GET /media/uploads/mbed_official/hello.txt HTTP/1.0\n\n";
    sock.send_all(http_cmd, sizeof(http_cmd)-1);
    
    char buffer[300];
    int ret;
    while (true) {
        ret = sock.receive(buffer, sizeof(buffer)-1);
        if (ret <= 0)
            break;
        buffer[ret] = '\0';
        lcd.cls();
        lcd.printf("Len:%d\n\"%s\"\n", ret, buffer);
    }
      
    sock.close();
    eth.disconnect();
    
    while(1) {}
}
