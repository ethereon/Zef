#include "zef_core.h"
#include <opencv2/core/core.hpp>
#include <stdint.h>
#include <algorithm>

CvMat* zef_create_mat(int rows, int cols, int type)
{
    return cvCreateMat(rows, cols, type);
}

void zef_release_mat(CvMat* p)
{
    CvMat* x = p;
    cvReleaseMat(&x);
}

CvMat* zef_create_roi(const CvMat* src, const CvRect* rect)
{
    CvSize srcSize = cvGetSize(src);
    CvMat* roi = cvCreateMatHeader(srcSize.height, //Rows
                                   srcSize.width,  //Cols
                                   cvGetElemType(src));
    cvGetSubRect(src, roi, *rect);
    return roi;
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
    //We need to wrap cvAbs since it's a macro.
    cvAbs(src, dst);
}

void zef_set(CvMat* img, double v)
{
    cvSet(img, cvScalar(v, v, v, v), NULL);
}

static bool almost_equal(const float& vA, const float& vB)
{
    static const float epsilon = 1E-5;
    return std::abs(vA - vB) < epsilon;
}

int zef_mat_eq(CvMat* a, CvMat* b)
{
    cv::Mat matA = cv::cvarrToMat(a);
    cv::Mat matB = cv::cvarrToMat(b);
    int depthA = zef_get_mat_depth(a);
    int depthB = zef_get_mat_depth(b);
    if(depthA!=depthB)
    {
        return 0;
    }
    if(depthA==CV_8U)
    {
        return std::equal(matA.begin<uint8_t>(), matA.end<uint8_t>(), matB.begin<uint8_t>());
    }
    else if(depthA==CV_32F)
    {
        return std::equal(matA.begin<float>(), matA.end<float>(), matB.begin<float>(), almost_equal);
    }
    assert(0 && "Unexpected matrix type encountered");
    return 0;
}
