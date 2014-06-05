//
//  SDAppDelegate.m
//  TrackerDemo
//
//  Created by Saumitro Dasgupta on 6/4/14.
//  Copyright (c) 2014 Saumitro Dasgupta. All rights reserved.
//

#import "SDAppDelegate.h"
#import "SDViewController.h"
#include "MeanShiftTracker_stub.h"

@interface SDAppDelegate ()
@property (strong) SDViewController* viewController;
@end

@implementation SDAppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    _viewController = [[SDViewController alloc] init];
    _window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    [_window setRootViewController:_viewController];
    [_window makeKeyAndVisible];
    return YES;
}

@end
