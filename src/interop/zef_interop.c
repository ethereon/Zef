#include "zef_interop.h"

CvMat* zef_create_mat(int rows, int cols, int type)
{
    return cvCreateMat(rows, cols, type);
}

void zef_release_mat(CvMat* p)
{
    CvMat* x = p;
    cvReleaseMat(&x);
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

int zef_get_mat_depth(const CvMat* mat)
{
    return CV_MAT_DEPTH(cvGetElemType(mat));
}

int zef_get_mat_channel_count(const CvMat* mat)
{
    return CV_MAT_CN(cvGetElemType(mat));
}

int zef_make_mat_type(int matDepth, int matChanCount)
{
    return CV_MAKETYPE(matDepth, matChanCount);
}

int zef_get_width(const CvMat* mat)
{
    return cvGetSize(mat).width;
}

int zef_get_height(const CvMat* mat)
{
    return cvGetSize(mat).height;
}

void zef_abs(const CvMat* src, CvMat* dst)
{
    cvAbs(src, dst);
}

void zef_set(CvMat* img, double v)
{
    cvSet(img, cvScalar(v, v, v, v), NULL);
}
