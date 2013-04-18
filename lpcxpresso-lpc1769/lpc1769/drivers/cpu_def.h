#ifndef _ARM_CPU_DEF_H
#define _ARM_CPU_DEF_H

#ifndef CODE
  #define CODE
#endif

#ifndef XDATA
  #define XDATA
#endif

#ifndef DATA
  #define DATA
#endif

struct pt_regs {
        long uregs[18];
};

#define ARM_cpsr        uregs[16]
#define ARM_pc          uregs[15]
#define ARM_lr          uregs[14]
#define ARM_sp          uregs[13]
#define ARM_ip          uregs[12]
#define ARM_fp          uregs[11]
#define ARM_r10         uregs[10]
#define ARM_r9          uregs[9]
#define ARM_r8          uregs[8]
#define ARM_r7          uregs[7]
#define ARM_r6          uregs[6]
#define ARM_r5          uregs[5]
#define ARM_r4          uregs[4]
#define ARM_r3          uregs[3]
#define ARM_r2          uregs[2]
#define ARM_r1          uregs[1]
#define ARM_r0          uregs[0]
#define ARM_ORIG_r0     uregs[17]

struct undef_hook {
        struct undef_hook *next;
        unsigned long instr_mask;
        unsigned long instr_val;
        unsigned long cpsr_mask;
        unsigned long cpsr_val;
        int (*fn)(struct pt_regs *regs, unsigned int instr);
};

int register_undef_hook(struct undef_hook *hook);

/* Low level CPU specific IRQ handling code */

#if !defined(__thumb__)
/* Regular 32-bit ARM architecture */

#define WITH_IRQ_HANDLER_ARGS

#define sti()                                                   \
        ({                                                      \
                unsigned long temp;                             \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ sti\n"                \
"       bic     %0, %0, #128\n"                                 \
"       msr     cpsr_c, %0"                                     \
        : "=r" (temp)                                           \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define cli()                                                   \
        ({                                                      \
                unsigned long temp;                             \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ cli\n"                \
"       orr     %0, %0, #128\n"                                 \
"       msr     cpsr_c, %0"                                     \
        : "=r" (temp)                                           \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define save_and_cli(flags)                                     \
        ({                                                      \
                unsigned long temp;                             \
                (void) (&temp == &flags);                       \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ save_and_cli\n"       \
"       orr     %1, %0, #128\n"                                 \
"       msr     cpsr_c, %1"                                     \
        : "=r" (flags), "=r" (temp)                             \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define save_flags(flags)                                       \
        ({                                                      \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ save_flags\n"         \
        : "=r" (flags)                                          \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define restore_flags(flags)                                    \
        __asm__ __volatile__(                                   \
        "msr    cpsr_c, %0              @ restore_flags\n"      \
        :                                                       \
        : "r" (flags)                                           \
        : "memory", "cc")


/* FIQ handling code */

#define fiq_sti()                                               \
        ({                                                      \
                unsigned long temp;                             \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ sti\n"                \
"       bic     %0, %0, #64\n"                                  \
"       msr     cpsr_c, %0"                                     \
        : "=r" (temp)                                           \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define fiq_cli()                                               \
        ({                                                      \
                unsigned long temp;                             \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ cli\n"                \
"       orr     %0, %0, #64\n"                                  \
"       msr     cpsr_c, %0"                                     \
        : "=r" (temp)                                           \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define fiq_save_and_cli(flags)                                 \
        ({                                                      \
                unsigned long temp;                             \
                (void) (&temp == &flags);                       \
        __asm__ __volatile__(                                   \
        "mrs    %0, cpsr                @ save_and_cli\n"       \
"       orr     %1, %0, #192\n"                                 \
"       msr     cpsr_c, %1"                                     \
        : "=r" (flags), "=r" (temp)                             \
        :                                                       \
        : "memory", "cc");                                      \
        })

#elif defined(__thumb2__) || defined (__ARM_ARCH_6M__)
/* ARM Cortex-M3 architecture */

/* The interrupts are not delivered with argument,
   it is retrieved independent way - irq_arch_get_irqidx */
#undef WITH_IRQ_HANDLER_ARGS

/* Offset between first interrupt source and exception table base */
#define IRQ_IRQIDX_OFFSET 16

#define sti()                                                   \
        ({                                                      \
        __asm__ __volatile__(                                   \
        "cpsie  i                       @ sti\n"                \
        : : : "memory", "cc");                                  \
        })

#define cli()                                                   \
        ({                                                      \
        __asm__ __volatile__(                                   \
        "cpsid  i                       @ cli\n"                \
        : : : "memory", "cc");                                  \
        })

#define save_and_cli(flags)                                     \
        ({                                                      \
                unsigned long temp;                             \
                (void) (&temp == &flags);                       \
        __asm__ __volatile__(                                   \
        "mrs    %0, primask             @ save_and_cli\n"       \
"       cpsid  i\n"                                             \
        : "=r" (flags)                                          \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define save_flags(flags)                                       \
        ({                                                      \
                unsigned long temp;                             \
                (void) (&temp == &flags);                       \
        __asm__ __volatile__(                                   \
        "mrs    %0, primask             @ save_flags\n"         \
        : "=r" (flags)                                          \
        :                                                       \
        : "memory", "cc");                                      \
        })

#define restore_flags(flags)                                    \
        ({                                                      \
        __asm__ __volatile__(                                   \
        "msr    primask, %0            @ restore_flags\n"       \
        :                                                       \
        : "r" (flags)                                           \
        : "memory", "cc");                                      \
        })

#define irq_arch_get_irqidx()                                   \
        ({                                                      \
        unsigned long ipsr;                                     \
        __asm__ __volatile__(                                   \
        "mrs    %0, ipsr               @ get irqidx\n"          \
        : "=r" (ipsr) );                                        \
        ipsr;                                                   \
        })

#else /*defined(__thumb__)*/

#define WITH_IRQ_HANDLER_ARGS
/* Regular ARM architecture in THUMB mode */

void irq_fnc_sti(void);
#define sti irq_fnc_sti
void irq_fnc_cli(void);
#define cli irq_fnc_cli
unsigned long irq_fnc_save_and_cli(void);
#define save_and_cli(_flags) ((_flags)=irq_fnc_save_and_cli())
unsigned long irq_fnc_save_flags(void);
#define save_flags(_flags) ((_flags)=irq_fnc_save_flags())
void irq_fnc_restore_flags(unsigned long flags);
#define restore_flags irq_fnc_restore_flags

#endif /*defined(__thumb__)*/

void __cpu_coherent_range(unsigned long start, unsigned long end);

static inline void flush_icache_range(unsigned long start, unsigned long end)
{
	__cpu_coherent_range(start, end);
}

/* atomic access routines */

//typedef unsigned long atomic_t;

static inline void atomic_clear_mask(unsigned long mask, volatile unsigned long *addr)
{
        unsigned long flags;

        save_and_cli(flags);
        *addr &= ~mask;
        restore_flags(flags);
}

static inline void atomic_set_mask(unsigned long mask, volatile unsigned long *addr)
{
        unsigned long flags;

        save_and_cli(flags);
        *addr |= mask;
        restore_flags(flags);
}

static inline void set_bit(int nr, volatile unsigned long *addr)
{
        unsigned long flags;

        save_and_cli(flags);
        *addr |= 1<<nr;
        restore_flags(flags);
}

static inline void clear_bit(int nr, volatile unsigned long *addr)
{
        unsigned long flags;

        save_and_cli(flags);
        *addr &= ~(1<<nr);
        restore_flags(flags);
}

static inline int test_bit(int nr, volatile unsigned long *addr)
{
        return ((*addr) & (1<<nr))?1:0;
}

static inline int test_and_set_bit(int nr, volatile unsigned long *addr)
{
        unsigned long flags;
	long m=(1<<nr);
        long r;

        save_and_cli(flags);
        r=*addr;
	*addr=r|m;
        restore_flags(flags);
        return r&m?1:0;
}

#if defined(__thumb2__) || defined (__ARM_ARCH_6M__)

/* DMB, DSB, ISB */

#define __memory_barrier() \
 __asm__ __volatile__("dmb": : : "memory")

#else /* old plain ARM architecture */

#define __memory_barrier() \
 __asm__ __volatile__("": : : "memory")

#endif

/*masked fields macros*/

#define __val2mfld(mask,val) (((mask)&~((mask)<<1))*(val)&(mask))
#define __mfld2val(mask,val) (((val)&(mask))/((mask)&~((mask)<<1)))

static inline void outb(unsigned int port, int val) {
  *(volatile unsigned char *)(port)=val;
}

static inline unsigned char inb(unsigned int port) {
  return *(volatile unsigned char *)(port);
}

#define _WITHIN_CPU_DEF_H
#include <irq_generic.h>
#undef _WITHIN_CPU_DEF_H

extern void **irq_context_table;
extern irq_handler_t **irq_handler_table;
extern unsigned int irq_table_size; 

/* Arithmetic functions */
#if 0
/* ARM v5E architecture - DSP extension */

#define sat_add_slsl(__x,__y) \
    __asm__ ("	qadd	%0,%0,%2\n" \
      : "=r"(__x) \
      : "0" ((long)__x), "r" ((long)__y) : "cc"); \

#define sat_sub_slsl(__x,__y) \
    __asm__ ("	qsub	%0,%0,%2\n" \
      : "=r"(__x) \
      : "0" ((long)__x), "r" ((long)__y) : "cc"); \

#elif !defined(__thumb__)
/* Regular 32-bit ARM architecture */

#define sat_add_slsl(__x,__y) \
    __asm__ ("	adds	%0,%2\n" \
	"	eorvs	%0,%2,#0x80000000\n" \
	"	sbcvs	%0,%0,%2\n" \
      : "=r"(__x) \
      : "0" ((long)__x), "r" ((long)__y) : "cc"); \

#define sat_sub_slsl(__x,__y) \
    __asm__ ("	subs	%0,%2\n" \
	"	eorvs	%0,%2,#0x80000000\n" \
	"	sbcvs	%0,%0,%2\n" \
      : "=r"(__x) \
      : "0" ((long)__x), "r" ((long)__y) : "cc"); \

#elif defined(__thumb2__) || defined (__ARM_ARCH_6M__)

#define sat_add_slsl(__x,__y) \
    __asm__ ("	adds	%0,%2\n" \
	"	itt	vs\n" \
	"	eorsvs	%0,%3,%2\n" \
	"	sbcsvs	%0,%0,%2\n" \
      : "=r"(__x) \
      : "0" ((long)__x), "r" ((long)__y), "r" (0x80000000): "cc"); \

#define sat_sub_slsl(__x,__y) \
    __asm__ ("	subs	%0,%2\n" \
	"	itt	vs\n" \
	"	eorsvs	%0,%3,%2\n" \
	"	sbcsvs	%0,%0,%2\n" \
      : "=r"(__x) \
      : "0" ((long)__x), "r" ((long)__y), "r" (0x80000000) : "cc"); \

#endif

#endif /* _ARM_CPU_DEF_H */
