char jhc_c_compile[] = "gcc /tmp/jhc_63o7Jm/rts/profile.c /tmp/jhc_63o7Jm/rts/rts_support.c /tmp/jhc_63o7Jm/rts/gc_none.c /tmp/jhc_63o7Jm/rts/jhc_rts.c /tmp/jhc_63o7Jm/lib/lib_cbits.c /tmp/jhc_63o7Jm/rts/gc_jgc.c /tmp/jhc_63o7Jm/rts/stableptr.c -I/tmp/jhc_63o7Jm/cbits -I/tmp/jhc_63o7Jm hs_main.c -o hs_main.c '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC'";
char jhc_command[] = "jhc -fffi -C -o hs_main.c hs_src/Main.hs";
char jhc_version[] = "jhc 0.8.1 (-0)";

#include "jhc_rts_header.h"
#include <c_extern.h>

enum {
    CJhc_Prim_Prim_$BE = 1,
    CJhc_Prim_Prim_$LR = 0,
    CJhc_Prim_Prim_$x3a = 0,
    CJhc_Type_Word_Word32 = 0
};
struct sCJhc_Prim_Prim_$x3a A_ALIGNED;
struct sCJhc_Type_Word_Word32 A_ALIGNED;

struct sCJhc_Prim_Prim_$x3a {
    sptr_t a1;
    sptr_t a2;
};

struct sCJhc_Type_Word_Word32 {
    uint32_t a1;
};
void jhc_hs_init(void) ;
void _amain(void) ;
static void b__main(gc_t gc) A_STD;
static void ftheMain(gc_t gc) A_STD;
/* CAFS */
/* (HcNode CJhc.Type.Word.Word32 [Left Op {primCOp = BinOp Shl bits32 bits32, primRetTy = bits32}(1,13)],1) */
static const struct sCJhc_Type_Word_Word32 _c1 = {.a1 = 1 << 13};
#define c1 (TO_SPTR_C(P_WHNF, (sptr_t)&_c1))
/* (HcNode CJhc.Type.Word.Word32 [Left Op {primCOp = BinOp Or bits32 bits32, primRetTy = bits32}(Op {primCOp = BinOp Shl bits32 bits32, primRetTy = bits32}(1,13),Op {primCOp = BinOp Shl bits32 bits32, primRetTy = bits32}(1,15))],2) */
static const struct sCJhc_Type_Word_Word32 _c2 = {.a1 = (1 << 13) | (1 << 15)};
#define c2 (TO_SPTR_C(P_WHNF, (sptr_t)&_c2))
/* (HcNode CJhc.Type.Word.Word32 [Left Op {primCOp = BinOp Shl bits32 bits32, primRetTy = bits32}(1,15)],3) */
static const struct sCJhc_Type_Word_Word32 _c3 = {.a1 = 1 << 15};
#define c3 (TO_SPTR_C(P_WHNF, (sptr_t)&_c3))
/* (HcNode CJhc.Type.Word.Word32 [Left 0],4) */
static const struct sCJhc_Type_Word_Word32 _c4 = {.a1 = 0};
#define c4 (TO_SPTR_C(P_WHNF, (sptr_t)&_c4))
/* (HcNode CJhc.Prim.Prim.: [Right 4,Left &("CJhc.Prim.Prim.[]")],5) */
static const struct sCJhc_Prim_Prim_$x3a _c5 = {.a1 = c4, .a2 = (sptr_t)SET_RAW_TAG(CJhc_Prim_Prim_$BE)};
#define c5 (TO_SPTR_C(P_WHNF, (sptr_t)&_c5))
/* (HcNode CJhc.Prim.Prim.: [Right 3,Right 5],6) */
static const struct sCJhc_Prim_Prim_$x3a _c6 = {.a1 = c3, .a2 = c5};
#define c6 (TO_SPTR_C(P_WHNF, (sptr_t)&_c6))
/* (HcNode CJhc.Prim.Prim.: [Right 2,Right 6],7) */
static const struct sCJhc_Prim_Prim_$x3a _c7 = {.a1 = c2, .a2 = c6};
#define c7 (TO_SPTR_C(P_WHNF, (sptr_t)&_c7))
/* (HcNode CJhc.Prim.Prim.: [Right 1,Right 7],8) */
static const struct sCJhc_Prim_Prim_$x3a _c8 = {.a1 = c1, .a2 = c7};
#define c8 (TO_SPTR_C(P_WHNF, (sptr_t)&_c8))

const void * const nh_stuff[] = {
&_c1, &_c2, &_c3, &_c4, &_c5, &_c6, &_c7, &_c8, NULL
};


void 
jhc_hs_init(void)
{
}

void 
_amain(void)
{
        return (void)b__main(saved_gc);
}

static void A_STD
b__main(gc_t gc)
{
        return ftheMain(gc);
}

static void A_STD
ftheMain(gc_t gc)
{
        fR$__fControl_Monad_forever__2:;
        {   sptr_t v10;
            v10 = c8;
            fJhc_Monad_72__go__3:;
            {   wptr_t v100000 = eval(gc,v10);
                if (SET_RAW_TAG(CJhc_Prim_Prim_$BE) == v100000) {
                    SET_RAW_TAG(CJhc_Prim_Prim_$LR);
                } else {
                    sptr_t v115160440;
                    sptr_t v12;
                    /* ("CJhc.Prim.Prim.:" ni12 ni115160440) */
                    v12 = ((struct sCJhc_Prim_Prim_$x3a*)v100000)->a1;
                    v115160440 = ((struct sCJhc_Prim_Prim_$x3a*)v100000)->a2;
                    {   uint32_t v216085086;
                        gc_frame0(gc,1,v115160440);
                        wptr_t v100002 = eval(gc,v12);
                        v216085086 = ((struct sCJhc_Type_Word_Word32*)v100002)->a1;
                        *((volatile uint32_t *)(1073809420)) = v216085086;
                        saved_gc = gc;
                        (void)Delay((uint32_t)500000);
                        v10 = v115160440;
                        goto fJhc_Monad_72__go__3;
                    }
                }
            }
            goto fR$__fControl_Monad_forever__2;
        }
        return;
}
