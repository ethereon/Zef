#include <opencv2/core/core_c.h>
#include <opencv2/imgproc/imgproc_c.h>

/* Memory Management */

void zef_free_mat(CvMat* p);

/* Matrix Conversion */

CvMat* zef_rgb_to_gray(const CvMat* src);

CvMat* zef_convert_scale(const CvMat* src, int destDepth, double scale);

/* Type Utility */

int zef_get_mat_depth(int matType);

int zef_get_mat_channel_count(int matType);

int zef_make_mat_type(int matDepth, int matType);

int zef_get_width(const CvMat* mat);

int zef_get_height(const CvMat* mat);

/* Math */

int zef_abs(const CvMat* src, CvMat* dst);

void zef_set(CvMat* img, double v);
