#ifndef _IRQ_GENERIC_H
#define _IRQ_GENERIC_H

#ifndef _WITHIN_CPU_DEF_H
#error irq_generic.h cannot be included directly, use cpu_def.h
#endif /*_WITHIN_CPU_DEF_H*/

#ifdef WITH_IRQ_RETVAL

typedef int irqreturn_t;
#define IRQ_NONE       0
#define IRQ_HANDLED    1
#define IRQ_RETVAL(x)  ((x)!=IRQ_NONE?IRQ_HANDLED:IRQ_NONE)

#else /*WITH_IRQ_RETVAL*/

typedef void irqreturn_t;
#define IRQ_NONE       ((void)0)
#define IRQ_HANDLED    ((void)1)
#define IRQ_RETVAL(x)  ((x)!=IRQ_NONE?IRQ_HANDLED:IRQ_NONE)

#endif /*WITH_IRQ_RETVAL*/


#ifndef IRQ_HANDLER_FNC
#ifdef WITH_IRQ_HANDLER_ARGS

typedef irqreturn_t (irq_handler_t)(int, void *);
#define IRQ_HANDLER_FNC(M_fnc_name) \
  irqreturn_t M_fnc_name(int __irq_handler_irqidx, void *__irq_handler_context)

#define irq_handler_get_irqidx() (__irq_handler_irqidx)
#define irq_handler_get_context() (__irq_handler_context)

#else /*WITH_IRQ_HANDLER_ARGS*/

typedef irqreturn_t (irq_handler_t)(void);
#define IRQ_HANDLER_FNC(M_fnc_name) \
  irqreturn_t M_fnc_name(void)

/* irq_handler_get_* have to be CPU specific */

/* Typical interrupt processing when context and irqidx
   are not resolved by assembler/C IRQ routine for
   common to all vectors/irqidx */

#define irq_handler_get_irqidx() \
        irq_arch_get_irqidx()
#define irq_handler_get_context() \
        (irq_context_table[irq_handler_get_irqidx()])

#endif /*WITH_IRQ_HANDLER_ARGS*/
#endif /*IRQ_HANDLER_FNC*/

#ifndef irq_irqidx2irqnum

#ifndef IRQ_IRQIDX_OFFSET
#define IRQ_IRQIDX_OFFSET 0
#endif /*IRQ_IRQIDX_OFFSET*/

#define irq_irqnum2irqidx(x) ((x) + IRQ_IRQIDX_OFFSET)
#define irq_irqidx2irqnum(x) ((x) - IRQ_IRQIDX_OFFSET)

#endif /*irq_irqidx2irqnum*/

#ifndef irq_handler_get_irqnum
#define irq_handler_get_irqnum() irq_irqidx2irqnum(irq_handler_get_irqidx())
#endif /*irq_handler_get_irqnum*/

/* for compatabilty with Linux kernel naming */
#define irq_handler_get_dev() irq_handler_get_context()


/*
Next variables are typically defined by CPU support

extern void **irq_context_table;
extern irq_handler_t **irq_handler_table;
extern int irq_table_size; 
*/

#define IRQF_TRIGGER_NONE       0x00000000
#define IRQF_TRIGGER_RISING     0x00000001
#define IRQF_TRIGGER_FALLING    0x00000002
#define IRQF_TRIGGER_HIGH       0x00000004
#define IRQF_TRIGGER_LOW        0x00000008
#define IRQF_TRIGGER_MASK       (IRQF_TRIGGER_HIGH | IRQF_TRIGGER_LOW | \
                                 IRQF_TRIGGER_RISING | IRQF_TRIGGER_FALLING)

extern int
request_irq(unsigned int irqnum, irq_handler_t *handler, unsigned long flags,
            const char *name, void *context);

extern void free_irq(unsigned int irqnum, void *context);

extern void disable_irq(unsigned int irqnum);
extern void enable_irq(unsigned int irqnum);
extern int set_irq_type(unsigned int irqnum, unsigned int type);
extern int set_irq_priority(int irqnum, int level);

#endif /*_IRQ_GENERIC_H*/
