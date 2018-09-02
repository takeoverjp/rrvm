#include <stdio.h>

#define UART_STATUS (0x80000000+0x00)
#define UART_INTEN  (0x80000000+0x04)
#define UART_TXBUF  (0x80000000+0x08)
#define UART_RXBUF  (0x80000000+0x0C)

#define UART_TXFULL_MASK  (0x0300)
#define UART_RXEMPTY_MASK (0x0001)

const char *buf = "Hello World !\r\n";

int
main (void)
{
  for (int i = 0; buf[i] != 0x0; i++)
    {
#if 0
      while (((*(volatile int *)(UART_STATUS)) & UART_TXFULL_MASK) > 0);
#endif
      *(volatile int *)(UART_TXBUF) = buf[i];
    }

  return 0;
}
