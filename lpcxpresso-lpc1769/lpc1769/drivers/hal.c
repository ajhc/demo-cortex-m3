#include <stdio.h>
#include <cpu_def.h>
#include <LPC17xx.h>

#define IRQ_TABLE_SIZE  (16+33)

__attribute__ ((section(".irqarea")))
irq_handler_t *irq_handler_table_start[IRQ_TABLE_SIZE];
void *irq_context_table_start[IRQ_TABLE_SIZE];

void **irq_context_table = irq_context_table_start;
irq_handler_t **irq_handler_table = irq_handler_table_start;
unsigned int irq_table_size = IRQ_TABLE_SIZE; 

void disable_irq(unsigned int irqnum)
{
  NVIC_DisableIRQ(irqnum);
  __memory_barrier();
}
void enable_irq(unsigned int irqnum)
{
  __memory_barrier();
  NVIC_EnableIRQ(irqnum);
}

int
request_irq(unsigned int irqnum, irq_handler_t *handler, unsigned long flags,
            const char *name, void *context)
{
  unsigned int irqidx=irq_irqnum2irqidx(irqnum);
  
  if (irqidx>=irq_table_size) 
    return -1;
  
  disable_irq(irqnum);
  irq_handler_table[irqidx]=handler;
  irq_context_table[irqidx]=context;
  enable_irq(irqnum);
  
  return 0;
}

void free_irq(unsigned int irqnum, void *context)
{
  unsigned int irqidx=irq_irqnum2irqidx(irqnum);

  if (irqidx>=irq_table_size) 
    return;
  
  disable_irq(irqnum);
  irq_handler_table[irqidx]=NULL;
  irq_context_table[irqidx]=NULL;
}