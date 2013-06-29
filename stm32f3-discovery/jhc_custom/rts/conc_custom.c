#include "rts/conc.h"

static unsigned int *stk_ctrl = (unsigned int *) 0xe000e010;

void jhc_mutex_init(jhc_mutex_t *mutex) {};

void
jhc_mutex_lock(jhc_mutex_t *mutex)
{
	*mutex = *stk_ctrl;
	*stk_ctrl = *mutex & ~0x1;
}

void
jhc_mutex_unlock(jhc_mutex_t *mutex)
{
	*stk_ctrl = *mutex;
}
