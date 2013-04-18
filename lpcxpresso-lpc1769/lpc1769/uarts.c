#include "board.h"
#include "uarts.h"
#include "stdio.h"

#include "ringbuffer.h"

#define UART_TX_ORDER 8
#define UART_LINE_BUFFER_SIZE (1<<7)

void WEAK handleUart0Line(const char *line, int lineLength) {
  fprintf(stderr, "Ignoring line from UART0: %s\r\n", line); 
}
void WEAK handleUart1Line(const char *line, int lineLength) {
  fprintf(stderr, "Ignoring line from UART1: %s\r\n", line); 
}
void WEAK handleUart2Line(const char *line, int lineLength) {
  fprintf(stderr, "Ignoring line from UART2: %s\r\n", line); 
}
void WEAK handleUart3Line(const char *line, int lineLength) {
  fprintf(stderr, "Ignoring line from UART3: %s\r\n", line); 
}


typedef struct {
  RingBufferControl tx;
  char txArray[1<<(UART_TX_ORDER)];

  char rx[UART_LINE_BUFFER_SIZE];
  unsigned int rxLength;

  volatile FlagStatus txInterrupt;
  LPC_UART_TypeDef *uart;
  unsigned int error;
} UartRingBuffer;


#ifdef USE_UART0
UartRingBuffer uart0 IN_IRAM1;
#endif

#ifdef USE_UART1
UartRingBuffer uart1 IN_IRAM1;
#endif

#ifdef USE_UART2
UartRingBuffer uart2 IN_IRAM1;
#endif

#ifdef USE_UART3
UartRingBuffer uart3 IN_IRAM1;
#endif

UartRingBuffer *UART_BUFFER[4] = {
#ifdef USE_UART0
  [0] = &uart0,
#endif
#ifdef USE_UART1
  [1] = &uart1,
#endif
#ifdef USE_UART2
  [2] = &uart2,
#endif
#ifdef USE_UART3
  [3] = &uart3,
#endif
};

LPC_UART_TypeDef* UARTS[] = {LPC_UART0, (LPC_UART_TypeDef *)LPC_UART1, LPC_UART2, LPC_UART3};

// Perhaps this should live in the board-config.h file in stead?
uint32_t BAUD[] = { 
  115200, // Debug
  9600,   // Not in use
  9600,   // Chiller
  9600    // Watchdog
};


// Configures a pair of pins to use as an UART
void configUart(const uint32_t txpin, const uint32_t rxpin) {
  const int uartNumber = IO_CHAN(txpin);
  LPC_UART_TypeDef *uart = UARTS[uartNumber];
  UartRingBuffer *ub = UART_BUFFER[uartNumber];

  UART_CFG_Type uartConfig;
  UART_ConfigStructInit(&uartConfig);
  uartConfig.Baud_rate = BAUD[uartNumber];
  UART_Init(uart, &uartConfig);

  /* Initialize FIFOConfigStruct to default state:
   * 				- FIFO_DMAMode = DISABLE
   * 				- FIFO_Level = UART_FIFO_TRGLEV0
   * 				- FIFO_ResetRxBuf = ENABLE
   * 				- FIFO_ResetTxBuf = ENABLE
   * 				- FIFO_State = ENABLE
   */
  UART_FIFO_CFG_Type fifoConfig;
  UART_FIFOConfigStructInit(&fifoConfig);
  UART_FIFOConfig(uart, &fifoConfig);
  UART_TxCmd(uart, ENABLE);

  ub->error = 0;
  ub->uart = uart;

  /*
   * Do not enable transmit interrupt here, since it is handled by
   * UART_Send() function, just to reset Tx Interrupt state for the
   * first time
   */
  ub->txInterrupt = RESET;

  // Reset the buffers.
  rbInit(&ub->tx, UART_TX_ORDER);
  ub->rxLength = 0;

  // Enable UART Transmit
  UART_TxCmd(uart, ENABLE);

  /* Enable UART Rx interrupt */
  UART_IntConfig(uart, UART_INTCFG_RBR, ENABLE);
  /* Enable UART line status interrupt */
  UART_IntConfig(uart, UART_INTCFG_RLS, ENABLE);

  IRQn_Type irq;
  if (uartNumber == 0) {
    irq = UART0_IRQn;

  } else if (uartNumber == 1) {
    irq = UART1_IRQn;

  } else if (uartNumber == 2) {
    irq = UART2_IRQn;

  } else /* if (uart == LPC_UART3)*/ {
    irq = UART3_IRQn;
  } 

  NVIC_SetPriority(irq, GROUP_PRIORITY_SERIAL); 

  /* Enable Interrupt for UART channel */
  NVIC_EnableIRQ(irq);

  configPin(txpin);
  configPin(rxpin);
}


void initUARTs() {

#ifdef IO_DEBUG_TX
  configUart(IO_DEBUG_TX, IO_DEBUG_RX);
#endif
  
#ifdef IO_WATCHDOG_TX
  configUart(IO_WATCHDOG_TX, IO_WATCHDOG_RX);
#endif
  
#ifdef IO_CHILLER_TX
  configUart(IO_CHILLER_TX, IO_CHILLER_RX);
#endif
}


/*----------------- INTERRUPT SERVICE ROUTINES --------------------------*/
void UART_IntTransmit(UartRingBuffer *ub) {
  // Disable THRE interrupt
  UART_IntConfig(ub->uart, UART_INTCFG_THRE, DISABLE);

  /* Wait for FIFO buffer empty, transfer UART_TX_FIFO_SIZE bytes
   * of data or break whenever ring buffers are empty */
  /* Wait until THR empty */
  while (UART_CheckBusy(ub->uart) == SET);

  while (!rbIsEmpty(&ub->tx)) {
    unsigned char ch = ub->txArray[ub->tx.start];
    
    if (UART_Send(ub->uart, &ch, 1, NON_BLOCKING)){
      rbRead(&ub->tx); // actually pop the char from the ring
    } else {
      break;
    }
  }

  /* If there is no more data to send, disable the transmit
     interrupt - else enable it or keep it enabled */
  if (rbIsEmpty(&ub->tx)) {
    UART_IntConfig(ub->uart, UART_INTCFG_THRE, DISABLE);
    // Reset Tx Interrupt state
    ub->txInterrupt = RESET;
  } else {
    // Set Tx Interrupt state
    ub->txInterrupt = SET;
    UART_IntConfig(ub->uart, UART_INTCFG_THRE, ENABLE);
  }
}

void UART_IntErr(UartRingBuffer *ub, uint8_t bLSErrType) {
  ub->error = bLSErrType;
}


#ifdef USE_UART0
void UART0_IRQHandler(void) {
   /* Determine the interrupt source */
   uint32_t intsrc = UART_GetIntId(LPC_UART0);
   uint32_t tmp = intsrc & UART_IIR_INTID_MASK;

   // Receive Line Status
   if (tmp == UART_IIR_INTID_RLS){
     // Check line status
     uint32_t tmp1 = UART_GetLineStatus(LPC_UART0);
     // Mask out the Receive Ready and Transmit Holding empty status
     tmp1 &= (UART_LSR_OE | UART_LSR_PE | UART_LSR_FE | UART_LSR_BI | UART_LSR_RXFE);
     // If any error exist
     if (tmp1) {
       UART_IntErr(&uart0, tmp1);
     }
   }
   
   // Receive Data Available or Character time-out
   if ((tmp == UART_IIR_INTID_RDA) || (tmp == UART_IIR_INTID_CTI)){

     while (1) {
       // Call UART read function in UART driver
       unsigned char ch;
       if (UART_Receive(uart0.uart, &ch, 1, NON_BLOCKING)){
      
	 if (ch == '\n' || ch == '\r' || uart0.rxLength >= UART_LINE_BUFFER_SIZE) {
	   uart0.rx[uart0.rxLength] = 0;
	   handleUart0Line(uart0.rx, uart0.rxLength);
	   uart0.rxLength = 0;
	 } else {
	   uart0.rx[uart0.rxLength++] = ch;
	 }
       } else { // no more data
	 break;
       }
     }

   }

   // Transmit Holding Empty
   if (tmp == UART_IIR_INTID_THRE){
     UART_IntTransmit(&uart0);
   }   
}
#endif


#ifdef USE_UART1
void UART1_IRQHandler(void) {
   /* Determine the interrupt source */
   uint32_t intsrc = UART_GetIntId(LPC_UART1);
   uint32_t tmp = intsrc & UART_IIR_INTID_MASK;

   // Receive Line Status
   if (tmp == UART_IIR_INTID_RLS){
     // Check line status
     uint32_t tmp1 = UART_GetLineStatus(LPC_UART1);
     // Mask out the Receive Ready and Transmit Holding empty status
     tmp1 &= (UART_LSR_OE | UART_LSR_PE | UART_LSR_FE | UART_LSR_BI | UART_LSR_RXFE);
     // If any error exist
     if (tmp1) {
       UART_IntErr(&uart1, tmp1);
     }
   }
   
   // Receive Data Available or Character time-out
   if ((tmp == UART_IIR_INTID_RDA) || (tmp == UART_IIR_INTID_CTI)){
     while (1) {
       // Call UART read function in UART driver
       unsigned char ch;
       if (UART_Receive(uart1.uart, &ch, 1, NON_BLOCKING)){
      
	 if (ch == '\n' || ch == '\r' || uart1.rxLength >= UART_LINE_BUFFER_SIZE) {
	   uart1.rx[uart1.rxLength] = 0;
	   handleUart1Line(uart1.rx, uart1.rxLength);
	   uart1.rxLength = 0;
	 } else {
	   uart1.rx[uart1.rxLength++] = ch;
	 }
       } else { // no more data
	 break;
       }
     }

   }

   // Transmit Holding Empty
   if (tmp == UART_IIR_INTID_THRE){
     UART_IntTransmit(&uart1);
   }   
}
#endif


#ifdef USE_UART2
void UART2_IRQHandler(void) {
   /* Determine the interrupt source */
   uint32_t intsrc = UART_GetIntId(LPC_UART2);
   uint32_t tmp = intsrc & UART_IIR_INTID_MASK;

   // Receive Line Status
   if (tmp == UART_IIR_INTID_RLS){
     // Check line status
     uint32_t tmp1 = UART_GetLineStatus(LPC_UART2);
     // Mask out the Receive Ready and Transmit Holding empty status
     tmp1 &= (UART_LSR_OE | UART_LSR_PE | UART_LSR_FE | UART_LSR_BI | UART_LSR_RXFE);
     // If any error exist
     if (tmp1) {
       UART_IntErr(&uart2, tmp1);
     }
   }
   
   // Receive Data Available or Character time-out
   if ((tmp == UART_IIR_INTID_RDA) || (tmp == UART_IIR_INTID_CTI)){
     while (1) {
       // Call UART read function in UART driver
       unsigned char ch;
       if (UART_Receive(uart2.uart, &ch, 1, NON_BLOCKING)){
      
	 if (ch == '\n' || ch == '\r' || uart2.rxLength >= UART_LINE_BUFFER_SIZE) {
	   uart2.rx[uart2.rxLength] = 0;
	   handleUart2Line(uart2.rx, uart2.rxLength);
	   uart2.rxLength = 0;
	 } else {
	   uart2.rx[uart2.rxLength++] = ch;
	 }
       } else { // no more data
	 break;
       }
     }

   }

   // Transmit Holding Empty
   if (tmp == UART_IIR_INTID_THRE){
     UART_IntTransmit(&uart2);
   }   
}
#endif


#ifdef USE_UART3
void UART3_IRQHandler(void) {
   /* Determine the interrupt source */
   uint32_t intsrc = UART_GetIntId(LPC_UART3);
   uint32_t tmp = intsrc & UART_IIR_INTID_MASK;

   // Receive Line Status
   if (tmp == UART_IIR_INTID_RLS){
     // Check line status
     uint32_t tmp1 = UART_GetLineStatus(LPC_UART3);
     // Mask out the Receive Ready and Transmit Holding empty status
     tmp1 &= (UART_LSR_OE | UART_LSR_PE | UART_LSR_FE | UART_LSR_BI | UART_LSR_RXFE);
     // If any error exist
     if (tmp1) {
       UART_IntErr(&uart3, tmp1);
     }
   }
   
   // Receive Data Available or Character time-out
   if ((tmp == UART_IIR_INTID_RDA) || (tmp == UART_IIR_INTID_CTI)){
     while (1) {
       // Call UART read function in UART driver
       unsigned char ch;
       if (UART_Receive(uart3.uart, &ch, 1, NON_BLOCKING)){
      
	 if (ch == '\n' || ch == '\r' || uart3.rxLength >= UART_LINE_BUFFER_SIZE) {
	   uart3.rx[uart3.rxLength] = 0;
	   handleUart3Line(uart3.rx, uart3.rxLength);
	   uart3.rxLength = 0;
	 } else {
	   uart3.rx[uart3.rxLength++] = ch;
	 }
       } else { // no more data
	 break;
       }
     }

   }

   // Transmit Holding Empty
   if (tmp == UART_IIR_INTID_THRE){
     UART_IntTransmit(&uart3);
   }   
}
#endif



uint32_t UARTSend(UartRingBuffer *ub, char *txbuf, uint32_t buflen)  {
  uint8_t *data = (uint8_t *) txbuf;
  uint32_t bytes = 0;
  
  /* Temporarily lock out UART transmit interrupts during this
     write so the UART transmit interrupt won't cause problems
     with the index values */
  UART_IntConfig(ub->uart, UART_INTCFG_THRE, DISABLE);
  
  /* Loop until transmit run buffer is full or until n_bytes
     expires */
  while ((buflen > 0) && (!rbIsFull(&ub->tx)) ) {
    /* Write data from buffer into ring buffer */
    ub->txArray[rbWrite(&ub->tx)] = *data++;
    
    bytes++;
    buflen--;
  }
  
  /*
   * Check if current Tx interrupt enable is reset,
   * that means the Tx interrupt must be re-enabled
   * due to call UART_IntTransmit() function to trigger
   * this interrupt type
   */
  if (ub->txInterrupt == RESET) {
    UART_IntTransmit(ub);

  } else {
    /*
     * Otherwise, re-enables Tx Interrupt
     */
    UART_IntConfig(ub->uart, UART_INTCFG_THRE, ENABLE);
  }
  
  return bytes;
}

void UARTFlush(UartRingBuffer *ub) {
  // wait for current transmission complete - THR must be empty
  while (UART_CheckBusy(ub->uart));
}


// Abstracted, public API
uint32_t sendUART(unsigned int txPin, char *txbuf, uint32_t buflen) {
  return UARTSend(UART_BUFFER[IO_CHAN(txPin)], txbuf, buflen);
}

void flushUART(unsigned int txPin) {
  UARTFlush(UART_BUFFER[IO_CHAN(txPin)]);
}

uint32_t errorUART(unsigned int rxPin) {
  return UART_BUFFER[IO_CHAN(rxPin)]->error;
}

