import pstylize
import os
import sys
import time
import logging
import torch
from mlfuncts import *
from cmdfuncts import *


def check_gpu():
    gpu_supported = False
    try:
        torch.cuda.init()
        if(torch.cuda.is_available()):
            gpu_supported = True
            print("CUDA Available : ",torch.cuda.is_available())
            print("CUDA Devices : ",torch.cuda.device_count())
            print("CUDA Arch List : ",torch.cuda.get_arch_list())
            for x in range(torch.cuda.device_count()):
                print("CUDA Capabilities : ",torch.cuda.get_device_capability(x))
                print("CUDA Device Name : ",torch.cuda.get_device_name(x))
                # logging.info("CUDA Device Name : " + torch.cuda.get_device_name(x))
                print("CUDA Device Memory : ",torch.cuda.mem_get_info(x))
                print("CUDA Device Properties : ",torch.cuda.get_device_properties(x))
                # print(torch.cuda.memory_summary(x))
    except:
        print("No supported GPUs detected")

    print("GPU Support : ", gpu_supported);
    return gpu_supported

def show_elapsed(from_time):
    elapsed = time.time() - from_time
    print("Elapsed time = %f secs" % (elapsed))
    hour = elapsed // 3600
    elapsed %= 3600
    minutes = elapsed // 60
    elapsed %= 60
    seconds = elapsed
    print("Elapsed time = %d hours %d mins %d secs" % (hour, minutes, seconds))

def do_train(opts = None):
    is_gpu_available = check_gpu()
    
    if opts == None:
        opts = TTrain(dataset="/train/unsplash/256",
            style_image="style-images/gig.jpg",
            model_name="gig-256",
            model_dir="models",
            checkpoint_model_dir="",
            model_ext = ".pth",
            net="vgg19",
            logfile="",
            epochs=2,
            limit=0,
            batch_size=8,
            log_interval=500,
            checkpoint_interval=1000,
            image_size=256,
            seed=42,
            content_weight=1e5,
            style_weight=1e10,
            lr=1e-3,
            style_scale=1.0,
            force_size=True,
            ignore_gpu=False,
			cuda=True,
            log_event_api=False)

#    check_paths(args)
    trial_batch = opts.batch_size
    start = time.time()

    while(1):
        oom = False
        try:
            print("Trying batch of ", trial_batch)
            if opts.ignore_gpu:
                train(opts, False, trial_batch)
            else:
                train(opts, is_gpu_available, trial_batch)
        except RuntimeError as e:
            print("Hit exception handler")
            if trial_batch > 0:
                oom = True
            else:
                print(e)
                return(1)
        else:
            break

        if oom:
            trial_batch -= 1
            if is_gpu_available and not opts.ignore_gpu:
                torch.cuda.empty_cache()
            if trial_batch == 0:
                print("No batch size found to run current training session (style image too large)")
                return(1)

    show_elapsed(start)
    
def do_stylize(opts = None):
    is_gpu_available = check_gpu()
    
    if opts == None:
        opts = TStylize( content_image = "input-images\\haywain.jpg",
            content_image_raw = None,
            output_image = "output-images\\test-dae-sketch1-512.jpg",
            model = "test-dae-sketch1-512",
            # model = "mosaic-vgg16-1010-512",
            model_dir = "models",
            model_ext = ".pth",
            logfile = "",
            content_scale = 1,
            cuda = True,
            ignore_gpu = False,
            export_onnx = False,
            add_model_ext = True,
            log_event_api = False)

    start = time.time()
    if opts.ignore_gpu:
        stylize(opts, False)
    else:
        stylize(opts, is_gpu_available)
    show_elapsed(start)

def do_test(opts = None):
    # is_gpu_available = check_gpu()
    
    if opts == None:
        opts = TStylize( content_image = "input-images\\haywain.jpg",
            content_image_raw = None,
            output_image = "output-images\\test-dae-sketch1-512.jpg",
            model = "test-dae-sketch1-512",
            # model = "mosaic-vgg16-1010-512",
            model_dir = "models",
            model_ext = ".pth",
            logfile = "",
            content_scale = 1,
            cuda = True,
            ignore_gpu = False,
            export_onnx = False,
            add_model_ext = True,
            log_event_api = False
            )

    for k, v in opts.items():
        print(k, '=', v)

def delphi_test():
    is_gpu_available = check_gpu()

    style = TDelphiStylize()

    for i in pstylize.GetPropertyList():
        print(i, '=', pstylize.GetProperty(i))

    start = time.time()

    if style.ignore_gpu:
        rval = stylize(style, False)
    else:
        rval = stylize(style, is_gpu_available)

    show_elapsed(start)
    return (rval)
    
class TStylize(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self
        
class TTrain(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self
        
class TProperties:
    def __getattr__(Self, Key):
        return props.GetProperty(Key)

    def __setattr__(Self, Key, Value):
        props.SetProperty(Key, Value)

    def __repr__(Self):
        tmp = ""
        for i in props.GetPropertyList():
            if tmp:
                tmp = tmp + ", "
            tmp = tmp + i + " = " + str(getattr(Self,i))
        return tmp

class TDelphiStylize:
    def __getattr__(Self, Key):
        return pstylize.GetProperty(Key)

    def __setattr__(Self, Key, Value):
        pstylize.SetProperty(Key, Value)

    def __repr__(Self):
        tmp = ""
        for i in pstylize.GetPropertyList():
            if tmp:
                tmp = tmp + ", "
            tmp = tmp + i + " = " + str(getattr(Self,i))
        return tmp

if __name__ == "__main__":
    do_train()
#    do_stylize()
#    do_test()
#    delphi_test()
