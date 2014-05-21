#include <opencv2/core/core_c.h>
#include "ZefUtil.h"

void zef_free_mat(void* p)
{
    void* x = p;
    cvFree(&x);
}