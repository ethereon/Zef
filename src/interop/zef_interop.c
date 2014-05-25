#include "zef_interop.h"

void zef_free_mat(CvMat* p)
{
    void* x = p;
    cvFree(&x);
}

CvMat* zef_rgb_to_gray(const CvMat* src)
{
    CvSize srcSize = cvGetSize(src);
    int srcType = cvGetElemType(src);
    assert(CV_MAT_CN(srcType)==3);
    const int outputType = CV_MAKETYPE(CV_MAT_DEPTH(srcType), 1);
    CvMat* out = cvCreateMat(srcSize.height,  //Rows
                             srcSize.width,   //Cols
                             outputType);
    cvCvtColor(src, out, CV_RGB2GRAY);
    return out;
}

CvMat* zef_convert_scale(const CvMat* src, int destDepth, double scale)
{
    CvSize srcSize = cvGetSize(src);
    int srcType = cvGetElemType(src);
    int destType = CV_MAKE_TYPE(destDepth, CV_MAT_CN(srcType));
    CvMat* out = cvCreateMat(srcSize.height,  //Rows
                             srcSize.width,   //Cols
                             destType);
    cvConvertScale(src, out, scale, 0);
    return out;
}

int zef_get_mat_depth(int matType)
{
    return CV_MAT_DEPTH(matType);
}

int zef_get_mat_channel_count(int matType)
{
    return CV_MAT_CN(matType);
}

int zef_make_mat_type(int matDepth, int matType)
{
    return CV_MAKETYPE(matDepth, matType);
}

int zef_get_width(const CvMat* mat)
{
    return cvGetSize(mat).width;
}

int zef_get_height(const CvMat* mat)
{
    return cvGetSize(mat).height;
}
