#include <opencv2/core/core_c.h>
#include <opencv2/imgproc/imgproc_c.h>

void zef_free_mat(CvMat* p);

CvMat* zef_rgb_to_gray(const CvMat* src);

CvMat* zef_convert_scale(const CvMat* src, int destDepth, double scale);
