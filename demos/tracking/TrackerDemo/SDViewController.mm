//
//  SDViewController.m
//  TrackerDemo
//
//  Created by Saumitro Dasgupta on 6/5/14.
//  Copyright (c) 2014 Saumitro Dasgupta. All rights reserved.
//

#import "SDViewController.h"
#include "MeanShiftTracker_stub.h"
#import <opencv2/opencv.hpp>

@interface SDViewController ()
@property (strong) NSOperationQueue* opQueue;
@end

@implementation SDViewController

- (id)init
{
    if(!(self = [super initWithNibName:@"TrackerView" bundle:nil])) return nil;
    hs_init(NULL, NULL);
    _opQueue = [[NSOperationQueue alloc] init];
    [_opQueue setMaxConcurrentOperationCount:1];
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
}

-(void) viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
    [_opQueue addOperationWithBlock:^
     {
         while(1)
         {
             [self track];
         }
     }];
}

-(void) track
{
    //Load the demo video
    NSString* videoPath = [[NSBundle mainBundle] pathForResource:@"tracking-test" ofType:@"mp4"];
    std::string filename = [videoPath cStringUsingEncoding:NSUTF8StringEncoding];
    cv::VideoCapture capture(filename);
    cv::Mat frame;
    if(!capture.isOpened())
    {
        NSLog(@"Failed to open video file!");
        return;
    }
    bool didStart = false;
    int hsize = 16;
    float hranges[] = {0,180};
    const float* phranges = hranges;
    cv::Mat roi_hist;
    cv::Rect trackRect = cv::Rect(390, 210, 25, 25);
    
    //Track
    while(1)
    {
        capture >> frame;
        if(frame.empty())
        {
            return;
        }
        cv::Mat hsv, hue, mask;
        cv::cvtColor(frame, hsv, cv::COLOR_BGR2HSV);
        cv::inRange(hsv, cv::Scalar(0, 0, 0), cv::Scalar(10, 255, 255), mask);
        int ch[] = {0, 0};
        hue.create(hsv.size(), hsv.depth());
        cv::mixChannels(&hsv, 1, &hue, 1, ch, 1);
        if(!didStart)
        {
            cv::Mat trackROI(hue, trackRect);
            cv::Mat maskROI(mask, trackRect);
            cv::calcHist(&trackROI, 1, 0, maskROI, roi_hist, 1, &hsize, &phranges);
            cv::normalize(roi_hist, roi_hist, 0, 255, cv::NORM_MINMAX);
            didStart = true;
        }
        cv::Mat backproj;
        cv::calcBackProject(&hue, 1, 0, roi_hist, backproj, &phranges);
        CvMat bpMat = backproj;
        
        //Call our Haskell implementation of mean shift.
        meanShift(&bpMat, &trackRect);
        
        //Display tracking result.
        cv::rectangle(frame, trackRect, cv::Scalar(0,255,0), 2);
        cv::cvtColor(frame, frame, cv::COLOR_BGR2RGB);
        dispatch_async(dispatch_get_main_queue(),
                       ^{
                           [(UIImageView*)[self view] setImage:MatToUIImage(frame)];
                       });
    }
}

UIImage* MatToUIImage(const cv::Mat& cvMat)
{
    NSData *data = [NSData dataWithBytes:cvMat.data length:cvMat.elemSize()*cvMat.total()];
    CGColorSpaceRef colorSpace;
    if (cvMat.elemSize() == 1)
    {
        colorSpace = CGColorSpaceCreateDeviceGray();
    }
    else
    {
        colorSpace = CGColorSpaceCreateDeviceRGB();
    }
    
    CGDataProviderRef provider = CGDataProviderCreateWithCFData((__bridge CFDataRef)data);
    CGImageRef imageRef = CGImageCreate(cvMat.cols,                                 //width
                                        cvMat.rows,                                 //height
                                        8,                                          //bits per component
                                        8 * cvMat.elemSize(),                       //bits per pixel
                                        cvMat.step[0],                              //bytesPerRow
                                        colorSpace,                                 //colorspace
                                        kCGImageAlphaNone|kCGBitmapByteOrderDefault,//bitmap info
                                        provider,                                   //CGDataProviderRef
                                        NULL,                                       //decode
                                        false,                                      //should interpolate
                                        kCGRenderingIntentDefault                   //intent
                                        );
    UIImage *finalImage = [UIImage imageWithCGImage:imageRef];
    CGImageRelease(imageRef);
    CGDataProviderRelease(provider);
    CGColorSpaceRelease(colorSpace);
    return finalImage;
}

@end
