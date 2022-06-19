import os
import sys
sys.path.append(os.getcwd() + '\pysrc')
for p in sys.path:
    print(p)

import argparse
from argparse import Namespace
import time
import logging
import torch
from mlfuncts import *
from cmdfuncts import *


def check_gpu():
    try:
        torch.cuda.init()
        if(torch.cuda.is_available()):
            gpu_supported = 1
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
        gpu_supported = 0

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
    use_gpu = check_gpu()
    
    if opts == None:
        opts = TProperties(epochs=1,
            limit=0,
            batch_size=4,
            force_size=1,
            dataset="/train/unsplash/256",
            style_image="style-images/mona.jpg",
            model_name="test",
            save_model_dir="models",
            checkpoint_model_dir=None,
            image_size=256,
            style_size=None,
            logfile=None,
            ignore_gpu=1,
            seed=42,
            content_weight=1e5,
            style_weight=1e10,
            lr=1e-3,
            net="vgg19",
            log_interval=500,
            checkpoint_interval=1000)

#    check_paths(args)
    trial_batch = opts.batch_size
    start = time.time()

    while(1):
        oom = False
        try:
            print("Trying batch of ", trial_batch)
            train(opts, use_gpu, trial_batch)
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
            if use_gpu:
                torch.cuda.empty_cache()
            if trial_batch == 0:
                print("No batch size found to run current training session (style image too large)")
                return(1)

    show_elapsed(start)
    
def do_stylize(opts = None):
    use_gpu = check_gpu()
    
    if opts == None:
        opts = TProperties( content_image = "input-images\haywain.jpg",
            content_image_raw = None,
            output_image = "output-images\output2.jpg",
            model = "mosaic-vgg16-1010-512",
            model_dir = "models",
            content_scale = 1,
            cuda = 0,
            ignore_gpu = 0,
            export_onnx = None,
            movie = None,
            add_model_ext = 1,
            logfile = None)

    start = time.time()
    stylize(opts, use_gpu)
    show_elapsed(start)

class TProperties:
  def __init__(self, **kwargs):
    self.__dict__.update(kwargs)

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

if __name__ == "__main__":
    do_stylize()
#    do_train()
    