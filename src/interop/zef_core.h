#include <opencv2/core/core_c.h>
#include <opencv2/imgproc/imgproc_c.h>

#ifdef __cplusplus
extern "C"
{
#endif

/* Construction / Destruction  */

CvMat* zef_create_mat(int rows, int cols, int type);

void zef_release_mat(CvMat* p);

CvMat* zef_create_roi(const CvMat* src, const CvRect* rect);

/* Matrix Conversion */

CvMat* zef_rgb_to_gray(const CvMat* src);

CvMat* zef_convert_scale(const CvMat* src, int destDepth, double scale);

/* Type Utility */

int zef_get_mat_depth(const CvMat* mat);

int zef_get_mat_channel_count(const CvMat* mat);

int zef_make_mat_type(int matDepth, int matChanCount);

int zef_get_width(const CvMat* mat);

int zef_get_height(const CvMat* mat);

/* Math */

int zef_mat_eq(CvMat* a, CvMat* b);

void zef_abs(const CvMat* src, CvMat* dst);

void zef_set(CvMat* img, double v);

#ifdef __cplusplus
}
#endif

